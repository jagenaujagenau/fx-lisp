;;;; gateway.lisp — Vercel AI Gateway client, port of gateway/client.zig.
;;;;
;;;; The Zig client speaks the gateway's v3 language-model protocol over a
;;;; native HTTP stack. This port uses the same gateway's OpenAI-compatible
;;;; /v1/chat/completions endpoint with SSE streaming, transported through
;;;; a curl subprocess so stock SBCL needs no TLS library.

(in-package #:fx.gateway)

;;; Usage ledger — port of session_usage: every completed request records
;;; its token usage per model, across the agent loop, subagent threads,
;;; and web_search worker calls.

(defvar *usage-ledger* (make-hash-table :test #'equal))
(defvar *usage-mutex* (sb-thread:make-mutex :name "fx-usage-ledger"))

(defun record-usage (model usage)
  (when (and model (hash-table-p usage))
    (sb-thread:with-mutex (*usage-mutex*)
      (let ((entry (or (gethash model *usage-ledger*)
                       (setf (gethash model *usage-ledger*)
                             (list :requests 0 :prompt 0 :completion 0)))))
        (incf (getf entry :requests))
        (incf (getf entry :prompt)
              (or (fx.json:jget usage "prompt_tokens") 0))
        (incf (getf entry :completion)
              (or (fx.json:jget usage "completion_tokens") 0))
        (setf (gethash model *usage-ledger*) entry)))))

(defun usage-summary ()
  "List of (model requests prompt-tokens completion-tokens), stable order."
  (sb-thread:with-mutex (*usage-mutex*)
    (sort (loop for model being the hash-keys of *usage-ledger*
                  using (hash-value entry)
                collect (list model (getf entry :requests)
                              (getf entry :prompt) (getf entry :completion)))
          #'string< :key #'first)))

(defun reset-usage ()
  (sb-thread:with-mutex (*usage-mutex*)
    (clrhash *usage-ledger*)))

(define-condition gateway-error (error)
  ((message :initarg :message :reader gateway-error-message))
  (:report (lambda (c s)
             (format s "gateway error: ~a" (gateway-error-message c)))))

(defun %gateway-headers (api-key)
  (list "Content-Type: application/json"
        (format nil "Authorization: Bearer ~a" api-key)
        "HTTP-Referer: https://github.com/vercel-labs/fx"
        "ai-gateway-protocol-version: 0.0.1"
        "User-Agent: fx-lisp/0.1.0"))

(defun %curl-args (url headers &key body-file)
  (append (list "-sS" "-N" "--max-time" "600")
          (loop for h in headers append (list "-H" h))
          (when body-file
            (list "-X" "POST" "--data-binary"
                  (concatenate 'string "@" body-file)))
          (list url)))

(defun %temp-body-file (content)
  (let ((path (format nil "~agateway-req-~a.json"
                      (namestring (fx.util:fx-dir)) (random-id 4))))
    (write-file-string path content)
    path))

;;; ------------------------------------------------------------- streaming

(defstruct %tool-call-acc id name (arguments ""))

(defun %merge-tool-call-delta (accs delta)
  "DELTA is one element of choices[0].delta.tool_calls."
  (let* ((index (or (fx.json:jget delta "index") 0))
         (acc (or (gethash index accs)
                  (setf (gethash index accs) (make-%tool-call-acc))))
         (fn (fx.json:jget delta "function")))
    (let ((id (fx.json:jget delta "id")))
      (when id (setf (%tool-call-acc-id acc) id)))
    (when fn
      (let ((name (fx.json:jget fn "name"))
            (args (fx.json:jget fn "arguments")))
        (when name (setf (%tool-call-acc-name acc) name))
        (when args
          (setf (%tool-call-acc-arguments acc)
                (concatenate 'string (%tool-call-acc-arguments acc) args)))))))

(defun chat-completion (&key api-key model messages tools on-delta extra
                             (base-url fx.config:*gateway-base-url*))
  "Streamed chat completion. MESSAGES and TOOLS are decoded-JSON values.
ON-DELTA, when given, is called with each content text fragment. EXTRA is
an optional hash-table of additional request-body fields (e.g. provider
search options). Returns (values content tool-calls finish-reason usage
extras) where TOOL-CALLS is a list of plists (:id :name :arguments) and
EXTRAS carries provider-executed metadata such as citations."
  (let* ((request (fx.json:make-jobject
                   "model" model
                   "messages" messages
                   "tools" (or tools :omit)
                   "stream" t
                   "stream_options" (fx.json:make-jobject "include_usage" t)))
         (body (progn
                 (when extra
                   (maphash (lambda (k v) (fx.json:jput request k v)) extra))
                 (fx.json:encode-to-string request)))
         (body-file (%temp-body-file body))
         (url (concatenate 'string base-url "/v1/chat/completions"))
         (content (make-string-output-stream))
         (tool-accs (make-hash-table))
         (finish-reason nil)
         (usage nil)
         (extras (fx.json:make-jobject))
         (raw-tail (make-string-output-stream))
         (saw-event nil))
    (unwind-protect
         (let ((process (sb-ext:run-program
                         "curl" (%curl-args url (%gateway-headers api-key)
                                            :body-file body-file)
                         :search t :wait nil :output :stream :error :stream)))
           (with-open-stream (out (sb-ext:process-output process))
             (loop for line = (read-line out nil nil)
                   while line
                   do (let ((line (string-right-trim '(#\Return) line)))
                        (cond
                          ((string-prefix-p "data: " line)
                           (let ((payload (subseq line 6)))
                             (unless (string= payload "[DONE]")
                               (setf saw-event t)
                               (let* ((event (fx.json:decode payload))
                                      (choice (first (fx.json:jget event "choices")))
                                      (delta (fx.json:jget choice "delta")))
                                 (let ((u (fx.json:jget event "usage")))
                                   (when u (setf usage u)))
                                 ;; Provider-executed search metadata
                                 ;; (Perplexity-style) rides on the chunks.
                                 (dolist (key '("citations" "search_results"))
                                   (let ((v (fx.json:jget event key)))
                                     (when v (fx.json:jput extras key v))))
                                 (let ((fr (fx.json:jget choice "finish_reason")))
                                   (when fr (setf finish-reason fr)))
                                 (let ((text (fx.json:jget delta "content")))
                                   (when (stringp text)
                                     (write-string text content)
                                     (when on-delta (funcall on-delta text))))
                                 (dolist (tc (fx.json:jget delta "tool_calls"))
                                   (%merge-tool-call-delta tool-accs tc))))))
                          ((plusp (length line))
                           (write-string line raw-tail)
                           (write-char #\Newline raw-tail))))))
           (sb-ext:process-wait process)
           (let ((stderr (with-open-stream (err (sb-ext:process-error process))
                           (with-output-to-string (s)
                             (loop for line = (read-line err nil nil)
                                   while line do (write-line line s)))))
                 (code (sb-ext:process-exit-code process)))
             (unless saw-event
               (let ((tail (get-output-stream-string raw-tail)))
                 (error 'gateway-error
                        :message (%describe-failure tail stderr code))))))
      (ignore-errors (delete-file body-file)))
    (record-usage model usage)
    (values (get-output-stream-string content)
            (%finish-tool-calls tool-accs)
            finish-reason
            usage
            extras)))

(defun %finish-tool-calls (accs)
  (let ((indices (sort (loop for k being the hash-keys of accs collect k) #'<)))
    (loop for index in indices
          for acc = (gethash index accs)
          collect (list :id (or (%tool-call-acc-id acc)
                                (format nil "call_~d" index))
                        :name (%tool-call-acc-name acc)
                        :arguments (%tool-call-acc-arguments acc)))))

(defun %describe-failure (body stderr exit-code)
  (or (ignore-errors
       (let ((parsed (fx.json:decode body)))
         (or (fx.json:jget* parsed "error" "message")
             (fx.json:jget parsed "error"))))
      (let ((trimmed (string-trim '(#\Space #\Newline) body)))
        (and (plusp (length trimmed)) trimmed))
      (let ((trimmed (string-trim '(#\Space #\Newline) stderr)))
        (and (plusp (length trimmed)) trimmed))
      (format nil "request failed (curl exit ~a)" exit-code)))

;;; ---------------------------------------------------------------- catalog

(defun list-models (&key api-key (base-url fx.config:*gateway-base-url*))
  "Fetch model ids from the gateway catalog, like /model in fx."
  (let* ((url (concatenate 'string base-url "/v1/models"))
         (out (with-output-to-string (s)
                (sb-ext:run-program "curl"
                                    (%curl-args url (%gateway-headers api-key))
                                    :search t :output s :error nil))))
    (handler-case
        (loop for entry in (fx.json:jget (fx.json:decode out) "data")
              for id = (fx.json:jget entry "id")
              when id collect id)
      (error ()
        (error 'gateway-error :message (%describe-failure out "" nil))))))
