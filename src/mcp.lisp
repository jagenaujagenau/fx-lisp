;;;; mcp.lisp — MCP client, port of core/mcp + builtins/mcp.zig (scoped).
;;;;
;;;; Configuration lives at ~/.fx/mcp.json in fx's format:
;;;;   {"mcp": {"<name>": {"type": "local"|"stdio", "command": [...] | "cmd",
;;;;            "args": [...], "environment": {...}, "enabled": true}}}
;;;; Only the stdio transport is ported (sse/http are rejected); servers
;;;; start lazily on first use and speak newline-delimited JSON-RPC 2.0
;;;; with the standard initialize handshake.
;;;;
;;;; Like fx, dynamic tool schemas stay out of the main prompt:
;;;; mcp_search_tools searches bounded metadata, mcp_select_tool advertises
;;;; one exact tool (named mcp_<server>_<tool>) for the next model step,
;;;; and mcp_features exposes resources and prompts. Elicitation, auth,
;;;; completions, and subscriptions are not ported.

(in-package #:fx.mcp)

(defparameter +protocol-version+ "2024-11-05")
(defparameter +startup-timeout-seconds+ 15)
(defparameter +operation-timeout-seconds+ 30)
(defparameter +max-search-limit+ 25)
(defparameter +max-result-bytes+ 16384)

(define-condition mcp-error (error)
  ((message :initarg :message :reader mcp-error-message))
  (:report (lambda (c s) (format s "mcp: ~a" (mcp-error-message c)))))

(defun %fail (fmt &rest args)
  (error 'mcp-error :message (apply #'format nil fmt args)))

;;; ------------------------------------------------------------------ config

(defstruct server-config name command args environment enabled)

(defun config-path ()
  (merge-pathnames "mcp.json" (fx.util:fx-dir)))

(defun %parse-command (object)
  "Port of parseCommandSpec: command is a string (+ args) or an array."
  (let ((command (fx.json:jget object "command"))
        (args (fx.json:jget object "args")))
    (cond
      ((stringp command) (values command (or args '())))
      ((and (consp command) (every #'stringp command))
       (values (first command) (rest command)))
      (t (%fail "server config needs a command")))))

(defun load-config ()
  "Parse ~/.fx/mcp.json into server-config structs (stdio only)."
  (let ((path (config-path)))
    (unless (probe-file path) (return-from load-config '()))
    (parse-config
     (handler-case (fx.json:decode (fx.util:read-file-string path))
       (error () (%fail "could not parse ~a" path))))))

(defun parse-config (root)
  (let* ((servers (fx.json:jget root "mcp"))
         (configs '()))
      (when (hash-table-p servers)
        (maphash
         (lambda (name entry)
           (when (hash-table-p entry)
             (let ((type (or (fx.json:jget entry "type") "local")))
               (when (member type '("local" "stdio") :test #'string=)
                 (multiple-value-bind (command args) (%parse-command entry)
                   (push (make-server-config
                          :name name
                          :command command
                          :args args
                          :environment (fx.json:jget entry "environment")
                          :enabled (fx.json:jbool entry "enabled" t))
                         configs))))))
         servers))
      (sort configs #'string< :key #'server-config-name)))

;;; --------------------------------------------------------------- transport

(defstruct connection config process input output (next-id 0) tools server-info)

(defvar *connections* (make-hash-table :test #'equal))

(defun shutdown-connections ()
  (maphash (lambda (name connection)
             (declare (ignore name))
             (let ((process (connection-process connection)))
               (when (and process (sb-ext:process-alive-p process))
                 (ignore-errors (sb-ext:process-kill process 15)))))
           *connections*)
  (clrhash *connections*))

(defun %merged-environment (environment)
  (if (hash-table-p environment)
      (let ((extra '()))
        (maphash (lambda (k v)
                   (when (stringp v)
                     (push (format nil "~a=~a" k v) extra)))
                 environment)
        (append extra (sb-ext:posix-environ)))
      (sb-ext:posix-environ)))

(defun %read-message (connection deadline)
  "Read one JSON-RPC message line, waiting until DEADLINE (internal time)."
  (let ((stream (connection-output connection)))
    (loop
      (when (listen stream)
        (let ((line (read-line stream nil nil)))
          (unless line (%fail "server ~a closed its stdout"
                              (server-config-name (connection-config connection))))
          (when (plusp (length (string-trim " " line)))
            (return (handler-case (fx.json:decode line)
                      (error () (%fail "server sent malformed JSON")))))))
      (unless (sb-ext:process-alive-p (connection-process connection))
        (%fail "server ~a exited"
               (server-config-name (connection-config connection))))
      (when (> (get-internal-real-time) deadline)
        (%fail "timed out waiting for server ~a"
               (server-config-name (connection-config connection))))
      (sleep 0.02))))

(defun %send (connection message)
  (let ((stream (connection-input connection)))
    (write-string (fx.json:encode-to-string message) stream)
    (write-char #\Newline stream)
    (force-output stream)))

(defun %request (connection method &optional params
                            (timeout +operation-timeout-seconds+))
  "Send a request and wait for its response, servicing interleaved traffic."
  (let ((id (incf (connection-next-id connection)))
        (deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (%send connection
           (fx.json:make-jobject "jsonrpc" "2.0" "id" id "method" method
                                 "params" (or params :omit)))
    (loop
      (let* ((message (%read-message connection deadline))
             (message-id (fx.json:jget message "id"))
             (method-name (fx.json:jget message "method")))
        (cond
          ((eql message-id id)
           (let ((err (fx.json:jget message "error")))
             (when err
               (%fail "~a failed: ~a" method
                      (or (fx.json:jget err "message")
                          (fx.json:encode-to-string err))))
             (return (fx.json:jget message "result"))))
          ;; Server-initiated request: refuse politely so the stream
          ;; stays healthy (elicitation/roots are not ported).
          ((and method-name message-id)
           (%send connection
                  (fx.json:make-jobject
                   "jsonrpc" "2.0" "id" message-id
                   "error" (fx.json:make-jobject
                            "code" -32601 "message" "method not supported"))))
          ;; Notifications (logging, progress) are skipped.
          (t nil))))))

(defun %notify (connection method &optional params)
  (%send connection
         (fx.json:make-jobject "jsonrpc" "2.0" "method" method
                               "params" (or params :omit))))

(defun %start (config)
  (let ((process (sb-ext:run-program
                  (server-config-command config)
                  (server-config-args config)
                  :search t :wait nil
                  :input :stream :output :stream :error nil
                  :environment (%merged-environment
                                (server-config-environment config)))))
    (unless (and process (sb-ext:process-alive-p process))
      (%fail "could not start server ~a" (server-config-name config)))
    (let ((connection (make-connection
                       :config config
                       :process process
                       :input (sb-ext:process-input process)
                       :output (sb-ext:process-output process))))
      (let ((result (%request connection "initialize"
                              (fx.json:make-jobject
                               "protocolVersion" +protocol-version+
                               "capabilities" (fx.json:make-jobject)
                               "clientInfo" (fx.json:make-jobject
                                             "name" "fx-lisp"
                                             "version" "0.1.0"))
                              +startup-timeout-seconds+)))
        (setf (connection-server-info connection) result))
      (%notify connection "notifications/initialized")
      connection)))

(defun connect (name)
  "Return a live connection to the configured server NAME, starting it
lazily and restarting it if its process died."
  (let ((existing (gethash name *connections*)))
    (when (and existing (sb-ext:process-alive-p (connection-process existing)))
      (return-from connect existing))
    (remhash name *connections*)
    (let ((config (find name (load-config)
                        :key #'server-config-name :test #'string=)))
      (unless config (%fail "unknown MCP server: ~a" name))
      (unless (server-config-enabled config)
        (%fail "MCP server ~a is disabled" name))
      (setf (gethash name *connections*) (%start config)))))

;;; ------------------------------------------------------------ tool catalog

(defun server-tools (name)
  "tools/list for one server, cached on the connection."
  (let ((connection (connect name)))
    (or (connection-tools connection)
        (setf (connection-tools connection)
              (fx.json:jget (%request connection "tools/list") "tools")))))

(defun dynamic-tool-name (server tool)
  (format nil "mcp_~a_~a" server tool))

(defun %parse-dynamic-name (name)
  "Split mcp_<server>_<tool> against the configured server list."
  (when (fx.util:string-prefix-p "mcp_" name)
    (let ((rest (subseq name 4)))
      (loop for config in (load-config)
            for server = (server-config-name config)
            for prefix = (concatenate 'string server "_")
            when (fx.util:string-prefix-p prefix rest)
              return (values server (subseq rest (length prefix)))))))

(defun all-tool-entries ()
  "(server tool-object) pairs across all enabled servers; failures noted."
  (let ((entries '()) (failures '()))
    (dolist (config (load-config))
      (when (server-config-enabled config)
        (handler-case
            (dolist (tool (server-tools (server-config-name config)))
              (push (list (server-config-name config) tool) entries))
          (error (e)
            (push (format nil "~a: ~a" (server-config-name config) e)
                  failures)))))
    (values (nreverse entries) (nreverse failures))))

;;; -------------------------------------------------------- dynamic dispatch

(defun %flatten-result-content (result)
  "Render a tools/call result's content blocks as text."
  (let ((blocks (fx.json:jget result "content")))
    (with-output-to-string (out)
      (when (fx.json:jbool result "isError")
        (write-string "tool reported an error: " out))
      (dolist (block blocks)
        (let ((type (fx.json:jget block "type")))
          (cond
            ((equal type "text") (write-line (or (fx.json:jget block "text") "") out))
            ((equal type "resource")
             (let ((resource (fx.json:jget block "resource")))
               (format out "resource ~a:~%~a~%"
                       (fx.json:jget resource "uri")
                       (or (fx.json:jget resource "text") "(binary)"))))
            (t (format out "(~a content omitted)~%" (or type "unknown")))))))))

(defun %bound (text)
  (if (> (length text) +max-result-bytes+)
      (concatenate 'string (subseq text 0 +max-result-bytes+)
                   (format nil "~%(truncated at ~d bytes)" +max-result-bytes+))
      text))

(defun call-dynamic-tool (server tool args)
  (let ((connection (connect server)))
    (%bound (%flatten-result-content
             (%request connection "tools/call"
                       (fx.json:make-jobject
                        "name" tool
                        "arguments" (or args (fx.json:make-jobject))))))))

(defun select-tool (dynamic-name)
  "Register mcp_<server>_<tool> as an executable tool; returns its schema."
  (multiple-value-bind (server tool) (%parse-dynamic-name dynamic-name)
    (unless server
      (%fail "unknown dynamic tool ~a; discover names with mcp_search_tools"
             dynamic-name))
    (let ((entry (find tool (server-tools server)
                       :key (lambda (candidate) (fx.json:jget candidate "name"))
                       :test #'string=)))
      (unless entry
        (%fail "server ~a does not provide tool ~a" server tool))
      (let ((schema (or (fx.json:jget entry "inputSchema")
                        (fx.json:make-jobject "type" "object")))
            (description (or (fx.json:jget entry "description") "")))
        (fx.tools:register-tool
         (fx.tools::make-tool-spec
          :name dynamic-name
          :description (format nil "[MCP ~a] ~a" server description)
          :schema schema
          :requires-approval t
          :action-label (format nil "Calling ~a" dynamic-name)
          :call (lambda (args) (call-dynamic-tool server tool args))))
        (fx.json:encode-to-string
         (fx.json:make-jobject "selected" dynamic-name
                               "server" server
                               "description" description
                               "input_schema" schema))))))

;;; ----------------------------------------------------------- search tool

(defun search-tools (query &key (limit 8))
  (let ((limit (min (max limit 1) +max-search-limit+))
        (terms (remove "" (fx.util:split-lines
                           (substitute #\Newline #\Space
                                       (string-downcase query)))
                       :test #'string=)))
    (multiple-value-bind (entries failures) (all-tool-entries)
      (let* ((scored
               (loop for (server tool) in entries
                     for name = (or (fx.json:jget tool "name") "")
                     for description = (or (fx.json:jget tool "description") "")
                     for haystack = (string-downcase
                                     (format nil "~a ~a ~a ~a" server name
                                             description
                                             (fx.json:encode-to-string
                                              (or (fx.json:jget tool "inputSchema")
                                                  ""))))
                     for hits = (count-if (lambda (term)
                                            (search term haystack))
                                          terms)
                     when (plusp hits)
                       collect (list hits server name description)))
             (ranked (sort scored #'> :key #'first))
             (shown (subseq ranked 0 (min limit (length ranked)))))
        (with-output-to-string (out)
          (if shown
              (progn
                (dolist (row shown)
                  (destructuring-bind (hits server name description) row
                    (declare (ignore hits))
                    (format out "~a — ~a~%"
                            (dynamic-tool-name server name)
                            (fx.compaction:compact-line description 200))))
                (format out "~%Select one with mcp_select_tool to make it callable.")
                (when (> (length ranked) (length shown))
                  (format out "~%more_available: true (~d more)"
                          (- (length ranked) (length shown)))))
              (format out "no dynamic tools matched~@[ (no MCP servers configured in ~a)~]"
                      (unless entries (config-path))))
          (dolist (failure failures)
            (format out "~%server unavailable — ~a" failure)))))))

;;; ---------------------------------------------------------- features tool

(defun features (action server &key uri prompt arguments)
  (let ((connection (connect server)))
    (cond
      ((equal action "resource_list")
       (%bound
        (with-output-to-string (out)
          (let ((resources (fx.json:jget (%request connection "resources/list")
                                         "resources")))
            (if resources
                (dolist (resource resources)
                  (format out "~a — ~a~%"
                          (fx.json:jget resource "uri")
                          (or (fx.json:jget resource "name") "")))
                (write-string "(no resources)" out))))))
      ((equal action "resource_read")
       (unless uri (%fail "resource_read requires uri"))
       (%bound
        (with-output-to-string (out)
          (dolist (content (fx.json:jget
                            (%request connection "resources/read"
                                      (fx.json:make-jobject "uri" uri))
                            "contents"))
            (format out "~a~%" (or (fx.json:jget content "text") "(binary)"))))))
      ((equal action "prompt_list")
       (%bound
        (with-output-to-string (out)
          (let ((prompts (fx.json:jget (%request connection "prompts/list")
                                       "prompts")))
            (if prompts
                (dolist (entry prompts)
                  (format out "~a — ~a~%"
                          (fx.json:jget entry "name")
                          (or (fx.json:jget entry "description") "")))
                (write-string "(no prompts)" out))))))
      ((equal action "prompt_get")
       (unless prompt (%fail "prompt_get requires prompt"))
       (%bound
        (with-output-to-string (out)
          (dolist (message (fx.json:jget
                            (%request connection "prompts/get"
                                      (fx.json:make-jobject
                                       "name" prompt
                                       "arguments" (or arguments :omit)))
                            "messages"))
            (format out "~a: ~a~%"
                    (fx.json:jget message "role")
                    (let ((content (fx.json:jget message "content")))
                      (if (hash-table-p content)
                          (or (fx.json:jget content "text") "(non-text)")
                          content)))))))
      (t (%fail "action ~a is not ported (resource/prompt completions and templates are out of scope)"
                action)))))

;;; ---------------------------------------------------------- tool registry

(defmacro %tool-body (&body body)
  `(handler-case (progn ,@body)
     (mcp-error (e)
       (error 'fx.tools:tool-error :message (princ-to-string e)))))

(fx.tools::define-tool "mcp_search_tools"
    (:description "Search bounded metadata for configured MCP/dynamic tools without loading every dynamic schema into the main prompt. Include the configured server alias and requested use case in the query; refine the use case when more_available is true. When to use: you need a specialized external/MCP capability but do not know its exact tool name. When NOT to use: the needed capability is already advertised directly, or ordinary local inspection, execution, web, or user interaction can handle the work."
     :schema (fx.tools::schema
              :properties
              `(("query" "type" "string" "description"
                 "Keyword query over dynamic tool name, description, server, input schema, and tags.")
                ("limit" "type" "integer" "description"
                 "Optional maximum results to return. Defaults to 8 and is capped."))
              :required '("query"))
     :requires-approval nil
     :action-label "Searching MCP tools")
    (args)
  (%tool-body
   (search-tools (or (fx.json:jget args "query")
                     (%fail "mcp_search_tools requires a query"))
                 :limit (or (fx.json:jget args "limit") 8))))

(fx.tools::define-tool "mcp_select_tool"
    (:description "Exact-select one configured MCP/dynamic tool by name so its executable schema is advertised on the next model step. When to use: after discovering the exact specialized tool name in configured metadata. When NOT to use: guessing partial names, selecting built-in tools, or executing the dynamic tool directly."
     :schema (fx.tools::schema
              :properties
              `(("name" "type" "string" "description"
                 "Exact dynamic MCP tool name discovered in configured metadata, such as mcp_server_tool."))
              :required '("name"))
     :requires-approval nil
     :action-label "Selecting MCP tool")
    (args)
  (%tool-body
   (select-tool (or (fx.json:jget args "name")
                    (%fail "mcp_select_tool requires a name")))))

(fx.tools::define-tool "mcp_features"
    (:description "List and read MCP resources and prompts on a configured server. When to use: the task needs a server's advertised resources or prompt templates. When NOT to use: dynamic tool execution (use mcp_select_tool), local files, or unconfigured servers."
     :schema (fx.tools::schema
              :properties
              `(("action" "type" "string"
                 "enum" ("resource_list" "resource_read" "prompt_list" "prompt_get")
                 "description" "Exact MCP feature operation.")
                ("server" "type" "string" "description"
                 "Exact configured MCP server name.")
                ("uri" "type" "string" "description"
                 "Exact discovered resource URI for resource_read.")
                ("prompt" "type" "string" "description"
                 "Exact discovered prompt name for prompt_get.")
                ("arguments" "type" "object" "description"
                 "String-valued prompt arguments for prompt_get."))
              :required '("action" "server"))
     :requires-approval nil
     :action-label "Using MCP feature")
    (args)
  (%tool-body
   (features (or (fx.json:jget args "action") (%fail "action is required"))
             (or (fx.json:jget args "server") (%fail "server is required"))
             :uri (fx.json:jget args "uri")
             :prompt (fx.json:jget args "prompt")
             :arguments (fx.json:jget args "arguments"))))
