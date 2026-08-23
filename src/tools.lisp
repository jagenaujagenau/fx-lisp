;;;; tools.lisp — builtin tools, ported from builtins/tools.zig and src/tools/.
;;;;
;;;; Ported set: filesystem tools, terminal (exec-only variant, matching the
;;;; Zig terminalExecOnlySpec), memory. Descriptions are verbatim from Zig.

(in-package #:fx.tools)

(define-condition tool-error (error)
  ((message :initarg :message :reader tool-error-message))
  (:report (lambda (c s) (write-string (tool-error-message c) s))))

(defun fail (fmt &rest args)
  (error 'tool-error :message (apply #'format nil fmt args)))

(defstruct tool-spec
  name description schema requires-approval action-label call)

(defvar *tool-registry* '())

(defun register-tool (spec)
  "Add or replace a tool in the registry."
  (setf *tool-registry*
        (append (remove (tool-spec-name spec) *tool-registry*
                        :key #'tool-spec-name :test #'string=)
                (list spec)))
  spec)

(defun unregister-tool (name)
  (setf *tool-registry*
        (remove name *tool-registry* :key #'tool-spec-name :test #'string=)))

(defmacro define-tool (name (&key description schema requires-approval action-label)
                       (args-var) &body body)
  `(register-tool (make-tool-spec
                   :name ,name
                   :description ,description
                   :schema ,schema
                   :requires-approval ,requires-approval
                   :action-label ,action-label
                   :call (lambda (,args-var)
                           (declare (ignorable ,args-var))
                           ,@body))))

(defun builtin-tools () *tool-registry*)

(defun find-tool (name)
  (find name *tool-registry* :key #'tool-spec-name :test #'string=))

(defun gateway-tool-definitions ()
  "Tool definitions in OpenAI chat-completions format."
  (mapcar (lambda (spec)
            (fx.json:make-jobject
             "type" "function"
             "function" (fx.json:make-jobject
                         "name" (tool-spec-name spec)
                         "description" (tool-spec-description spec)
                         "parameters" (tool-spec-schema spec))))
          *tool-registry*))

(defun execute-tool (name args)
  "Run tool NAME with decoded-JSON ARGS. Returns a result string.
Signals TOOL-ERROR on failure."
  (let ((spec (find-tool name)))
    (unless spec (fail "unknown tool: ~a" name))
    (funcall (tool-spec-call spec) args)))

;;; ------------------------------------------------------------ schema helpers

(defun schema (&key properties required)
  (fx.json:make-jobject
   "type" "object"
   "properties" (let ((table (make-hash-table :test #'equal)))
                  (loop for (name . plist) in properties
                        do (setf (gethash name table)
                                 (apply #'fx.json:make-jobject plist)))
                  table)
   "required" (or required :omit)
   "additionalProperties" :false))

(defparameter +path-description+
  "File path relative to the workspace root, or an external path using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy.")

;;; ------------------------------------------------------------- arg helpers

(defun arg-string (args key &optional default)
  (let ((v (fx.json:jget args key)))
    (cond ((stringp v) v)
          ((null v) default)
          (t (fail "argument ~a must be a string" key)))))

(defun arg-int (args key &optional default)
  (let ((v (fx.json:jget args key)))
    (cond ((integerp v) v)
          ((null v) default)
          (t (fail "argument ~a must be an integer" key)))))

(defun required-string (args key)
  (or (arg-string args key)
      (fail "missing required argument: ~a" key)))

(defun resolved-path (args &optional (key "path") (default "."))
  (expand-path (or (arg-string args key) default)))

;;; -------------------------------------------------------------- glob match

(defun %segment-match-p (pattern segment &optional (p-idx 0) (s-idx 0))
  "fnmatch on one path segment: * and ? only."
  (cond
    ((>= p-idx (length pattern)) (>= s-idx (length segment)))
    ((char= (char pattern p-idx) #\*)
     (loop for k from s-idx to (length segment)
           thereis (%segment-match-p pattern segment (1+ p-idx) k)))
    ((>= s-idx (length segment)) nil)
    ((or (char= (char pattern p-idx) #\?)
         (char= (char pattern p-idx) (char segment s-idx)))
     (%segment-match-p pattern segment (1+ p-idx) (1+ s-idx)))
    (t nil)))

(defun %split-segments (path)
  (remove "" (fx.util:split-lines (substitute #\Newline #\/ path)) :test #'string=))

(defun glob-match-p (pattern path)
  "Match PATH against glob PATTERN. Supports *, ?, and ** across segments.
A bare-name pattern like *.md also matches in any directory."
  (labels ((match (ps ss)
             (cond
               ((null ps) (null ss))
               ((string= (first ps) "**")
                (or (match (rest ps) ss)
                    (and ss (match ps (rest ss)))))
               ((null ss) nil)
               ((%segment-match-p (first ps) (first ss))
                (match (rest ps) (rest ss)))
               (t nil))))
    (let ((ps (%split-segments pattern))
          (ss (%split-segments path)))
      (or (match ps ss)
          ;; Basename-only patterns match anywhere, like fx's glob.
          (and (= (length ps) 1) ss
               (%segment-match-p (first ps) (car (last ss))))))))

(defun walk-files (root fn)
  "Call FN with each regular file's path relative to ROOT. Skips .git."
  (let ((root-dir (parse-namestring
                   (if (string-suffix-p "/" root) root
                       (concatenate 'string root "/")))))
    (labels ((walk (dir rel)
               (dolist (entry (ignore-errors
                               (directory (merge-pathnames "*.*" dir)
                                          :resolve-symlinks nil)))
                 (let* ((name (file-namestring entry)))
                   (if (and name (plusp (length name)))
                       (funcall fn (if (string= rel "") name
                                       (concatenate 'string rel "/" name)))
                       ;; A directory pathname has no file-namestring.
                       (let ((dirname (car (last (pathname-directory entry)))))
                         (when (and (stringp dirname)
                                    (not (string= dirname ".git")))
                           (walk entry (if (string= rel "") dirname
                                           (concatenate 'string rel "/" dirname))))))))))
      (walk root-dir ""))))

;;; ------------------------------------------------------------------- tools

(define-tool "list_files"
    (:description "List directory entries from one directory level without reading file contents. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: inspect a known folder, confirm names, or choose the next path before reading. When NOT to use: recursive discovery, content search, file counts, or shell ls."
     :schema (schema :properties
                     `(("path" "type" "string" "description"
                        "Optional path relative to the workspace root, or an external path; defaults to current directory.")))
     :requires-approval nil
     :action-label "Listing")
    (args)
  (let* ((path (resolved-path args))
         (dir (parse-namestring (if (string-suffix-p "/" path) path
                                    (concatenate 'string path "/"))))
         (entries '()))
    (unless (probe-file dir) (fail "directory not found: ~a" path))
    (dolist (entry (directory (merge-pathnames "*.*" dir) :resolve-symlinks nil))
      (let ((name (file-namestring entry)))
        (if (and name (plusp (length name)))
            (push name entries)
            (let ((dirname (car (last (pathname-directory entry)))))
              (when (stringp dirname)
                (push (concatenate 'string dirname "/") entries))))))
    (if entries
        (join-lines (sort entries #'string<))
        "(empty directory)")))

(define-tool "glob_files"
    (:description "Find file paths matching a glob pattern, with mode=count for exact path counts without listing entries. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: locate files by name, extension, or directory pattern; narrow path or pattern if candidate caps appear. When NOT to use: search file contents, read files, run find, or count non-file concepts."
     :schema (schema :properties
                     `(("pattern" "type" "string" "description"
                        "Glob pattern to match, such as src/**/*.zig or *.md.")
                       ("path" "type" "string" "description"
                        "Optional search root; defaults to current directory.")
                       ("mode" "type" "string" "enum" ("matches" "count")
                        "description"
                        "Use matches to return sample paths, or count to return an exact matching path count without listing entries."))
                     :required '("pattern"))
     :requires-approval nil
     :action-label "Matching")
    (args)
  (let* ((pattern (required-string args "pattern"))
         (root (resolved-path args))
         (mode (arg-string args "mode" "matches"))
         (cap 200)
         (matches '())
         (count 0))
    (walk-files root
                (lambda (rel)
                  (when (glob-match-p pattern rel)
                    (incf count)
                    (when (<= count cap) (push rel matches)))))
    (cond
      ((string= mode "count") (format nil "~d matching path~:p" count))
      ((zerop count) "no matching paths")
      (t (format nil "~a~@[~%(~d more not shown)~]"
                 (join-lines (sort (nreverse matches) #'string<))
                 (when (> count cap) (- count cap)))))))

(define-tool "grep_files"
    (:description "Search text files for a literal substring, optionally narrowed by path/include, with output modes for matching lines, files-with-matches, or counts plus head_limit/offset pagination and bounded context_lines for matches mode. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. Use include as the type/path filter, such as *.zig. When to use: find exact symbols, strings, TODOs, or usage sites. When NOT to use: regex is not supported; avoid unknown-concept exploration, filename lookup, known-path reads, and shell grep; do not repeat the same or equivalent search after a caller search only finds a definition."
     :schema (schema :properties
                     `(("pattern" "type" "string" "description"
                        "Literal plain-text pattern to search for.")
                       ("path" "type" "string" "description"
                        "Optional search root; defaults to current directory.")
                       ("include" "type" "string" "description"
                        "Optional glob pattern applied to candidate file paths before reading files, such as *.zig or src/**/*.ts.")
                       ("case_insensitive" "type" "boolean" "description"
                        "Search case-insensitively when true.")
                       ("mode" "type" "string"
                        "enum" ("matches" "files_with_matches" "count")
                        "description"
                        "Use matches for line matches, files_with_matches for unique matching paths, or count for exact matching-line and matching-file counts.")
                       ("head_limit" "type" "integer" "description"
                        "Optional positive maximum results to return.")
                       ("offset" "type" "integer" "description"
                        "Optional zero-based result offset for pagination.")
                       ("context_lines" "type" "integer" "description"
                        "Optional non-negative lines of context around each match in matches mode."))
                     :required '("pattern"))
     :requires-approval nil
     :action-label "Searching")
    (args)
  (let* ((pattern (required-string args "pattern"))
         (root (resolved-path args))
         (include (arg-string args "include"))
         (ci (fx.json:jbool args "case_insensitive"))
         (mode (arg-string args "mode" "matches"))
         (head-limit (arg-int args "head_limit" 100))
         (offset (arg-int args "offset" 0))
         (context (min (arg-int args "context_lines" 0) 10))
         (needle (if ci (string-downcase pattern) pattern))
         (results '())
         (line-count 0)
         (file-count 0)
         (emitted 0)
         (seen 0))
    (walk-files root
                (lambda (rel)
                  (when (or (null include) (glob-match-p include rel))
                    (let* ((full (expand-path rel root))
                           (content (ignore-errors (read-file-string full))))
                      (when (and content (not (find #\Nul content)))
                        (let ((lines (split-lines content))
                              (file-hit nil))
                          (loop for line in lines
                                for lineno from 1
                                for haystack = (if ci (string-downcase line) line)
                                when (search needle haystack)
                                  do (incf line-count)
                                     (setf file-hit t)
                                     (when (string= mode "matches")
                                       (incf seen)
                                       (when (and (> seen offset)
                                                  (< emitted head-limit))
                                         (incf emitted)
                                         (if (plusp context)
                                             (loop for cl from (max 1 (- lineno context))
                                                     to (min (length lines) (+ lineno context))
                                                   do (push (format nil "~a:~d:~a" rel cl
                                                                    (nth (1- cl) lines))
                                                            results))
                                             (push (format nil "~a:~d:~a" rel lineno line)
                                                   results)))))
                          (when file-hit
                            (incf file-count)
                            (when (string= mode "files_with_matches")
                              (incf seen)
                              (when (and (> seen offset) (< emitted head-limit))
                                (incf emitted)
                                (push rel results))))))))))
    (cond
      ((string= mode "count")
       (format nil "~d matching line~:p in ~d file~:p" line-count file-count))
      ((null results) "no matches")
      (t (join-lines (nreverse results))))))

(define-tool "read_file"
    (:description "Read one UTF-8 text file with bounded line-numbered output and optional start_line/line_count range. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: inspect an exact known path before editing or explaining code. When NOT to use: list directories, search many files, read binary data, or bypass dedicated search tools."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+)
                       ("start_line" "type" "integer" "description"
                        "Optional 1-based first line to return. Defaults to 1.")
                       ("line_count" "type" "integer" "description"
                        "Optional positive number of lines to return. Defaults to the normal read cap and is bounded."))
                     :required '("path"))
     :requires-approval nil
     :action-label "Reading")
    (args)
  (let* ((path (resolved-path args "path" nil))
         (start (max 1 (arg-int args "start_line" 1)))
         (count (min 2000 (max 1 (arg-int args "line_count" 2000)))))
    (unless (probe-file path) (fail "file not found: ~a" path))
    (let* ((lines (split-lines (read-file-string path)))
           (total (length lines))
           (end (min total (1- (+ start count)))))
      (if (> start total)
          (format nil "(file has only ~d line~:p)" total)
          (with-output-to-string (out)
            (loop for lineno from start to end
                  do (format out "~6d  ~a~%" lineno (nth (1- lineno) lines)))
            (when (< end total)
              (format out "(~d more line~:p not shown)" (- total end))))))))

(define-tool "write_file"
    (:description "Create or overwrite a file using complete contents. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: add a new file or intentionally replace an entire generated/small file. When NOT to use: targeted edits to existing files, partial replacements, deleting files, or unapproved external paths."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+)
                       ("content" "type" "string" "description"
                        "Complete file contents to write."))
                     :required '("path" "content"))
     :requires-approval t
     :action-label "Writing")
    (args)
  (let ((path (resolved-path args "path" nil))
        (content (or (arg-string args "content")
                     (fail "missing required argument: content"))))
    (write-file-string path content)
    (format nil "wrote ~d byte~:p to ~a" (length content) path)))

(define-tool "edit_file"
    (:description "Edit an existing file by replacing one exact old_string occurrence with new_string. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: make a focused patch after reading the file. When NOT to use: broad rewrites, ambiguous repeated text, generated formatting, missing files, or cross-file refactors."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+)
                       ("old_string" "type" "string" "description"
                        "Exact text to find in the file. Must match exactly once.")
                       ("new_string" "type" "string" "description"
                        "Text to replace old_string with."))
                     :required '("path" "old_string" "new_string"))
     :requires-approval t
     :action-label "Editing")
    (args)
  (let* ((path (resolved-path args "path" nil))
         (old (required-string args "old_string"))
         (new (or (arg-string args "new_string")
                  (fail "missing required argument: new_string"))))
    (unless (probe-file path) (fail "file not found: ~a" path))
    (when (zerop (length old)) (fail "old_string must not be empty"))
    (let* ((content (read-file-string path))
           (first-hit (search old content)))
      (unless first-hit (fail "old_string not found in ~a" path))
      (when (search old content :start2 (+ first-hit (length old)))
        (fail "old_string matches more than once in ~a; make it unique" path))
      (write-file-string path
                         (concatenate 'string
                                      (subseq content 0 first-hit)
                                      new
                                      (subseq content (+ first-hit (length old)))))
      (format nil "edited ~a" path))))

(define-tool "delete_file"
    (:description "Delete a file or empty directory after the user request clearly requires removal. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: remove obsolete files, generated artifacts, or empty folders. When NOT to use: clean up uncertain state, delete non-empty trees, or modify contents that should be edited instead."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+))
                     :required '("path"))
     :requires-approval t
     :action-label "Deleting")
    (args)
  (let ((path (resolved-path args "path" nil)))
    (let ((truename (probe-file path)))
      (unless truename (fail "path not found: ~a" path))
      (if (null (pathname-name truename))
          (progn
            (handler-case (sb-ext:delete-directory truename)
              (error () (fail "directory not empty or undeletable: ~a" path)))
            (format nil "deleted directory ~a" path))
          (progn
            (delete-file truename)
            (format nil "deleted ~a" path))))))

(define-tool "rename_file"
    (:description "Rename or move a file while preserving its contents. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: fulfill explicit rename, relocation, or organization requests. When NOT to use: copy-and-delete workflows, overwriting destinations, content edits, or unapproved external paths."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+)
                       ("new_path" "type" "string" "description"
                        "Destination path for the rename or move."))
                     :required '("path" "new_path"))
     :requires-approval t
     :action-label "Renaming")
    (args)
  (let ((from (resolved-path args "path" nil))
        (to (expand-path (required-string args "new_path"))))
    (unless (probe-file from) (fail "path not found: ~a" from))
    (when (probe-file to) (fail "destination already exists: ~a" to))
    (ensure-directories-exist to)
    (rename-file from to)
    (format nil "renamed ~a to ~a" from to)))

(define-tool "copy_file"
    (:description "Copy one file without modifying the source. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: duplicate templates, fixtures, or examples before editing the copy. When NOT to use: move files, overwrite uncertain destinations, clone directories, or read file contents."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+)
                       ("new_path" "type" "string" "description"
                        "Destination path for the copy."))
                     :required '("path" "new_path"))
     :requires-approval t
     :action-label "Copying")
    (args)
  (let ((from (resolved-path args "path" nil))
        (to (expand-path (required-string args "new_path"))))
    (unless (probe-file from) (fail "path not found: ~a" from))
    (when (probe-file to) (fail "destination already exists: ~a" to))
    (ensure-directories-exist to)
    (with-open-file (in from :element-type '(unsigned-byte 8))
      (with-open-file (out to :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-does-not-exist :create)
        (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
          (loop for n = (read-sequence buffer in)
                while (plusp n)
                do (write-sequence buffer out :end n)))))
    (format nil "copied ~a to ~a" from to)))

(define-tool "create_folder"
    (:description "Create a new directory, including needed parent folders. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: prepare a path for new files or requested project structure. When NOT to use: create files, inspect directories, clean existing folders, or make speculative structure not requested by the task."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+))
                     :required '("path"))
     :requires-approval t
     :action-label "Creating")
    (args)
  (let* ((path (resolved-path args "path" nil))
         (dir (if (string-suffix-p "/" path) path
                  (concatenate 'string path "/"))))
    (ensure-directories-exist dir)
    (format nil "created ~a" path)))

(define-tool "file_info"
    (:description "Inspect file or directory metadata, including type, size, and modified time. Paths may be workspace-relative or external using an absolute path, ~/..., or a relative workspace escape such as ../...; external access is subject to permission policy. When to use: check existence or distinguish files from directories before acting. When NOT to use: read contents, list child entries, search code, or infer git status."
     :schema (schema :properties
                     `(("path" "type" "string" "description" ,+path-description+))
                     :required '("path"))
     :requires-approval nil
     :action-label "Inspecting")
    (args)
  (let* ((path (resolved-path args "path" nil))
         (truename (probe-file path)))
    (unless truename (fail "path not found: ~a" path))
    (if (null (pathname-name truename))
        (format nil "~a: directory" path)
        (multiple-value-bind (sec min hour day month year)
            (decode-universal-time (file-write-date truename) 0)
          (format nil "~a: file, ~d bytes, modified ~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                  path
                  (with-open-file (in truename :element-type '(unsigned-byte 8))
                    (file-length in))
                  year month day hour min sec)))))

(define-tool "terminal"
    (:description "Run one captured command and return its result."
     :schema (schema :properties
                     `(("action" "type" "string" "enum" ("exec"))
                       ("command" "type" "string" "description" "Command to run.")
                       ("cwd" "type" "string" "description"
                        "Working directory; defaults to the workspace."))
                     :required '("action" "command"))
     :requires-approval t
     :action-label "Running")
    (args)
  (let ((action (arg-string args "action" "exec")))
    (unless (string= action "exec")
      (fail "this build supports only action=exec (the Zig durable terminal is not ported)")))
  (let* ((command (required-string args "command"))
         (cwd (expand-path (or (arg-string args "cwd") ".")))
         (shell (or (getenv "SHELL") "/bin/sh"))
         (stdout (make-string-output-stream))
         (process (sb-ext:run-program shell (list "-c" command)
                                      :directory cwd
                                      :output stdout
                                      :error stdout
                                      :wait t))
         (exit-code (sb-ext:process-exit-code process))
         (output (get-output-stream-string stdout))
         (cap 30000))
    (when (> (length output) cap)
      (setf output (concatenate 'string
                                (subseq output 0 cap)
                                (format nil "~%(output truncated at ~d bytes)" cap))))
    (format nil "exit code: ~d~%~a" exit-code output)))

;;; ---------------------------------------------------------- semantic_search
;;; Port of tools/filesystem/semantic_search.zig: lexical concept-keyword
;;; ranking over workspace files (not embeddings). Limits are the Zig
;;; dispatch defaults: 100 shown results (2x retained), 100 KB read cap per
;;; file, 2000-char sample lines, 2000 walked entries.

(defparameter +search-ignored-dirs+
  '(".git" ".zig-cache" "zig-out" "node_modules" ".next" "dist" "build"
    "coverage"))
(defparameter +search-max-list-entries+ 100)
(defparameter +search-max-file-bytes+ (* 100 1024))
(defparameter +search-max-line-chars+ 2000)
(defparameter +search-walk-entry-cap+ 2000)

(defparameter +search-stop-words+
  '("a" "an" "the" "is" "are" "was" "were" "in" "on" "at" "to" "for" "of"
    "and" "or" "not" "it" "this" "that" "with" "from" "by" "as" "do" "does"
    "how" "what" "where" "when" "why" "which"))

(defun split-search-keywords (query)
  "Port of splitSearchKeywords: split on punctuation/space, keep words of
2+ chars that are not stop words, cap at 16."
  (let ((splitters '(#\Space #\Tab #\, #\. #\; #\: #\? #\!))
        (keywords '())
        (start nil))
    (loop for i from 0 to (length query)
          for ch = (and (< i (length query)) (char query i))
          do (if (and ch (not (member ch splitters)))
                 (unless start (setf start i))
                 (when start
                   (let ((word (subseq query start i)))
                     (when (and (>= (length word) 2)
                                (not (member word +search-stop-words+
                                             :test #'string-equal))
                                (< (length keywords) 16))
                       (push word keywords)))
                   (setf start nil))))
    (nreverse keywords)))

(defun %score-file-for-keywords (path keywords)
  "Return (values score sample-line sample-line-number) or NIL for
oversized/non-text files."
  (let ((content (handler-case (read-file-string path)
                   (error () (return-from %score-file-for-keywords nil)))))
    (when (or (> (length content) +search-max-file-bytes+)
              (find #\Nul content))
      (return-from %score-file-for-keywords nil))
    (let ((total 0) (best-line "") (best-number 0) (best-score 0)
          (basename (let ((slash (position #\/ path :from-end t)))
                      (if slash (subseq path (1+ slash)) path))))
      (loop for line in (split-lines content)
            for number from 1
            do (let ((line-score (count-if (lambda (kw)
                                             (search kw line :test #'char-equal))
                                           keywords)))
                 (when (plusp line-score)
                   (incf total line-score)
                   (when (> line-score best-score)
                     (setf best-score line-score
                           best-line line
                           best-number number)))))
      (dolist (kw keywords)
        (when (search kw basename :test #'char-equal)
          (incf total 3)))
      (values total best-line best-number))))

(defun %search-path-ignored-p (rel)
  (some (lambda (segment) (member segment +search-ignored-dirs+ :test #'string=))
        (%split-segments rel)))

(define-tool "semantic_search"
    (:description "Lexically search workspace files for concept keywords when exact symbols are unknown, ranking likely files for follow-up reads. This is not embedding or true semantic search. When to use: explore unfamiliar concepts, features, or responsibilities. When NOT to use: exact symbols, literal text, file names, counts, or narrow known-path inspection."
     :schema (schema :properties
                     `(("query" "type" "string" "description"
                        "Concept keywords describing what to find, such as retry backoff policy.")
                       ("path" "type" "string" "description"
                        "Optional search root relative to the workspace root, or an external path; defaults to current directory."))
                     :required '("query"))
     :requires-approval nil
     :action-label "Searching")
    (args)
  (block search
    (let* ((query (required-string args "query"))
         (root (resolved-path args))
         (keywords (split-search-keywords query)))
    (when (null keywords)
      (return-from search (format nil "[search] empty query~%")))
    (let ((results '())
          (walked 0)
          (walk-capped nil)
          (result-capped nil)
          (retained-cap (* 2 +search-max-list-entries+))
          (file-root-p (let ((truename (probe-file root)))
                         (and truename (pathname-name truename)))))
      (if file-root-p
          (multiple-value-bind (score line number)
              (%score-file-for-keywords root keywords)
            (when (and score (plusp score))
              (push (list score root line number) results)))
          (block walk
            (walk-files root
                        (lambda (rel)
                          (when (>= (incf walked) +search-walk-entry-cap+)
                            (setf walk-capped t)
                            (return-from walk))
                          (unless (%search-path-ignored-p rel)
                            (multiple-value-bind (score line number)
                                (%score-file-for-keywords
                                 (expand-path rel root) keywords)
                              (when (and score (plusp score))
                                (when (>= (length results) retained-cap)
                                  (setf result-capped t)
                                  (return-from walk))
                                (push (list score rel line number)
                                      results))))))))
      (setf results (sort results
                          (lambda (a b)
                            (if (/= (first a) (first b))
                                (> (first a) (first b))
                                (string< (second a) (second b))))))
      (with-output-to-string (out)
        (let ((shown (subseq results 0 (min (length results)
                                            +search-max-list-entries+))))
          (if (null shown)
              (format out "[search] no results for: ~a~%" query)
              (progn
                (format out "[search] ~d results for: ~a~%"
                        (length shown) query)
                (dolist (entry shown)
                  (destructuring-bind (score path line number) entry
                    (declare (ignore score))
                    (format out "~a:~d: ~a~%" path number
                            (subseq line 0 (min (length line)
                                                +search-max-line-chars+)))))
                (when (> (length results) (length shown))
                  (format out "... and ~d more~%"
                          (- (length results) (length shown))))))
          (when walk-capped
            (format out "... results may be incomplete; traversal cap reached before all files were searched~%"))
          (when result-capped
            (format out "... results may be incomplete; result cap reached before all matching files were scored~%"))))))))

;;; ---------------------------------------------------------------- web_fetch
;;; Port of tools/web: url_policy.zig (scheme/credential/private-address
;;; checks), fetch.zig (curl transport here), html_to_markdown.zig (reduced).

(defparameter +max-url-bytes+ 2000)
(defparameter +max-fetch-output-bytes+ 48000)

(defun %private-host-p (host)
  (let ((host (string-downcase (string-trim "[]" host))))
    (or (member host '("localhost" "0.0.0.0" "::1" "::") :test #'string=)
        (string-suffix-p ".localhost" host)
        (string-suffix-p ".local" host)
        (string-prefix-p "fc" host) (string-prefix-p "fd" host)
        (string-prefix-p "fe80:" host)
        (let ((octets (ignore-errors
                       (mapcar #'parse-integer (%split-segments
                                                (substitute #\/ #\. host))))))
          (and octets (= 4 (length octets)) (every #'integerp octets)
               (let ((a (first octets)) (b (second octets)))
                 (or (= a 127) (= a 10) (= a 0)
                     (and (= a 192) (= b 168))
                     (and (= a 169) (= b 254))
                     (and (= a 172) (<= 16 b 31)))))))))

(defun %validate-url (url)
  "Port of url_policy.normalize: http(s) only, bounded, no credentials,
no private hosts. Returns the host or signals TOOL-ERROR."
  (when (> (length url) +max-url-bytes+) (fail "url too long"))
  (let ((scheme-end (search "://" url)))
    (unless (and scheme-end
                 (member (string-downcase (subseq url 0 scheme-end))
                         '("http" "https") :test #'string=))
      (fail "unsupported url scheme; only http and https are allowed"))
    (when (some (lambda (c) (or (char= c #\Space) (< (char-code c) #x21)))
                url)
      (fail "url contains whitespace or control bytes"))
    (let* ((start (+ scheme-end 3))
           (end (or (position-if (lambda (c) (member c '(#\/ #\? #\#)))
                                 url :start start)
                    (length url)))
           (authority (subseq url start end)))
      (when (zerop (length authority)) (fail "url is missing a host"))
      (when (find #\@ authority) (fail "credentialed urls are not allowed"))
      (let* ((colon (position #\: authority :from-end t))
             (host (if (and colon (every #'digit-char-p
                                         (subseq authority (1+ colon))))
                       (subseq authority 0 colon)
                       authority)))
        (when (%private-host-p host)
          (fail "non-public address rejected: ~a" host))
        host))))

(defun %decode-entities (text)
  (let ((replacements '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                        ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'")
                        ("&nbsp;" . " "))))
    (loop for (entity . char) in replacements
          do (loop for hit = (search entity text)
                   while hit
                   do (setf text (concatenate 'string
                                              (subseq text 0 hit) char
                                              (subseq text (+ hit (length entity)))))))
    text))

(defun %strip-block (html tag)
  "Remove <tag ...>...</tag> blocks, case-insensitively."
  (let ((open (format nil "<~a" tag))
        (close (format nil "</~a>" tag))
        (lower (string-downcase html)))
    (loop for start = (search open lower)
          while start
          do (let ((end (search close lower :start2 start)))
               (setf html (concatenate 'string
                                       (subseq html 0 start)
                                       (if end (subseq html (+ end (length close))) ""))
                     lower (string-downcase html))))
    html))

(defun html-to-markdown (html)
  "Reduced port of html_to_markdown.zig: strip script/style, keep block
structure as newlines, headings as #, links as [text](href), drop other tags."
  (let ((html (%strip-block (%strip-block html "script") "style"))
        (out (make-string-output-stream))
        (pending-href nil)
        (i 0))
    (loop while (< i (length html))
          do (let ((c (char html i)))
               (if (char= c #\<)
                   (let* ((end (or (position #\> html :start i) (length html)))
                          (closing (and (< (1+ i) (length html))
                                        (char= (char html (1+ i)) #\/)))
                          (inner (subseq html (if closing (+ i 2) (1+ i)) end))
                          (tag (string-downcase
                                (subseq inner 0
                                        (or (position-if
                                             (lambda (ch)
                                               (member ch '(#\Space #\Tab #\Newline #\/)))
                                             inner)
                                            (length inner))))))
                     (cond
                       ((member tag '("p" "div" "br" "tr" "li" "ul" "ol"
                                      "table" "section" "article" "blockquote")
                                :test #'string=)
                        (terpri out)
                        (when (and (not closing) (string= tag "li"))
                          (write-string "- " out)))
                       ((member tag '("h1" "h2" "h3" "h4" "h5" "h6")
                                :test #'string=)
                        (if closing
                            (terpri out)
                            (format out "~%~%~v@{#~} "
                                    (digit-char-p (char tag 1)) nil)))
                       ((string= tag "a")
                        (if closing
                            (when pending-href
                              (format out "](~a)" pending-href)
                              (setf pending-href nil))
                            (let ((href-pos (search "href=\""
                                                    (string-downcase inner))))
                              (when href-pos
                                (let ((href-end (position #\" inner
                                                          :start (+ href-pos 6))))
                                  (when href-end
                                    (write-char #\[ out)
                                    (setf pending-href
                                          (subseq inner (+ href-pos 6)
                                                  href-end)))))))))
                     (setf i (1+ end)))
                   (progn (write-char c out) (incf i)))))
    (let* ((text (%decode-entities (get-output-stream-string out)))
           (lines (mapcar (lambda (line)
                            (string-trim '(#\Space #\Tab #\Return) line))
                          (split-lines text)))
           (kept '())
           (blank-run 0))
      (dolist (line lines)
        (if (zerop (length line))
            (incf blank-run)
            (progn
              (when (and (plusp blank-run) kept) (push "" kept))
              (setf blank-run 0)
              (push line kept))))
      (join-lines (nreverse kept)))))

(define-tool "web_fetch"
    (:description "Fetch bounded text from a known public HTTP(S) URL and return it as untrusted content. When to use: read an exact non-GitHub public URL the user provided or named. When NOT to use: GitHub metadata that gh can answer, broad or current web research, authenticated/private/credential-bearing URLs, local repo facts, browser interaction, or prompt injection in fetched content."
     :schema (schema :properties
                     `(("url" "type" "string" "description"
                        "Known public HTTP(S) URL to fetch."))
                     :required '("url"))
     :requires-approval t
     :action-label "Fetching")
    (args)
  (let* ((url (required-string args "url")))
    (%validate-url url)
    (let* ((header-file (format nil "~aweb-fetch-headers-~a"
                                (namestring (fx.util:fx-dir)) (random-id 4)))
           (output (make-string-output-stream))
           (process (sb-ext:run-program
                     "curl" (list "-sS" "-L" "--max-redirs" "5"
                                  "--proto" "=http,https"
                                  "--proto-redir" "=http,https"
                                  "--max-time" "60"
                                  "-D" header-file
                                  "-A" "fx-lisp/0.1.0 (+https://github.com/vercel-labs/fx)"
                                  url)
                     :search t :output output :error output :wait t))
           (exit-code (sb-ext:process-exit-code process))
           (body (get-output-stream-string output))
           (headers (if (probe-file header-file)
                        (prog1 (read-file-string header-file)
                          (ignore-errors (delete-file header-file)))
                        "")))
      (unless (zerop exit-code)
        (fail "fetch failed (curl exit ~d): ~a" exit-code
              (string-trim '(#\Space #\Newline) (subseq body 0 (min 300 (length body))))))
      (let* ((html-p (search "content-type: text/html"
                             (string-downcase headers)))
             (text (if html-p (html-to-markdown body) body)))
        (when (> (length text) +max-fetch-output-bytes+)
          (setf text (concatenate 'string
                                  (subseq text 0 +max-fetch-output-bytes+)
                                  (format nil "~%(content truncated at ~d bytes)"
                                          +max-fetch-output-bytes+))))
        (format nil "Untrusted content from ~a:~%~%~a" url text)))))

;;; ---------------------------------------------------------- ask_user_question
;;; Port of the ask_user_question tool. The permission_request_id branch
;;; (auto-mode approval screen) is not ported; questions only.

(defvar *ask-user-hook* nil
  "When bound (the interactive REPL binds it), called with a list of
questions — each (question-text ((label description) ...)) — and returns
the list of chosen labels. NIL means no interactive user is available.")

(define-tool "ask_user_question"
    (:description "Ask the user 1-4 multiple-choice questions in interactive runs only when a concrete decision blocks progress after local files, git state, or tool output cannot answer it. When to use: choose among precise, mutually exclusive paths before acting, especially destructive or user-preference decisions. When NOT to use: discoverable facts, GitHub handles unless account/private-access specific, trivial yes/no checks, open-ended discussion, or noninteractive runs; noninteractive runs should surface a blocker in freeform text instead."
     :schema (fx.json:make-jobject
              "type" "object"
              "properties"
              (fx.json:make-jobject
               "questions"
               (fx.json:make-jobject
                "type" "array" "minItems" 1 "maxItems" 4
                "items"
                (fx.json:make-jobject
                 "type" "object"
                 "properties"
                 (fx.json:make-jobject
                  "question" (fx.json:make-jobject
                              "type" "string" "description"
                              "Specific blocking decision shown to the user; do not ask for facts tools can inspect.")
                  "options" (fx.json:make-jobject
                             "type" "array" "minItems" 2 "maxItems" 6
                             "items" (fx.json:make-jobject
                                      "type" "object"
                                      "properties"
                                      (fx.json:make-jobject
                                       "label" (fx.json:make-jobject
                                                "type" "string" "description"
                                                "Short precise action label, 1-5 words.")
                                       "description" (fx.json:make-jobject
                                                      "type" "string" "description"
                                                      "Optional one-line consequence or scope of this option."))
                                      "required" '("label"))))
                 "required" '("question" "options"))))
              "required" '("questions"))
     :requires-approval nil
     :action-label "Asking")
    (args)
  (let ((questions
          (loop for entry in (or (fx.json:jget args "questions")
                                 (fail "ask_user_question requires questions"))
                collect (let ((text (fx.json:jget entry "question"))
                              (options (fx.json:jget entry "options")))
                          (unless (and (stringp text) (consp options)
                                       (>= (length options) 2))
                            (fail "each question needs text and at least 2 options"))
                          (list text
                                (loop for option in options
                                      collect (list (or (fx.json:jget option "label")
                                                        (fail "options require a label"))
                                                    (fx.json:jget option "description"))))))))
    (when (> (length questions) 4) (fail "at most 4 questions per call"))
    (let ((hook *ask-user-hook*))
      (unless hook
        (fail "no interactive user is available; surface the blocker in freeform text instead"))
      (let ((answers (funcall hook questions)))
        (fx.json:encode-to-string
         (fx.json:make-jobject
          "answers"
          (loop for (text nil) in questions
                for answer in answers
                collect (fx.json:make-jobject "question" text
                                              "answer" answer))))))))

(define-tool "memory"
    (:description "Save, list, or clear durable user preferences for future fx sessions. When to use: the user explicitly asks to remember, forget, save, or recall a preference. When NOT to use: store task notes, secrets, project facts, temporary context, or anything the user did not ask to persist."
     :schema (schema :properties
                     `(("action" "type" "string" "enum" ("save" "list" "clear"))
                       ("text" "type" "string" "description"
                        "Preference text to save; required for action=save."))
                     :required '("action"))
     :requires-approval nil
     :action-label "Memorizing")
    (args)
  (let ((action (required-string args "action"))
        (path (merge-pathnames "memory.jsonl" (fx-dir))))
    (cond
      ((string= action "save")
       (let ((text (required-string args "text")))
         (with-open-file (out path :direction :output
                                   :if-exists :append
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
           (write-string (fx.json:encode-to-string
                          (fx.json:make-jobject "at" (iso8601-now) "text" text))
                         out)
           (terpri out))
         "saved"))
      ((string= action "list")
       (if (probe-file path)
           (let ((entries (remove "" (split-lines (read-file-string path))
                                  :test #'string=)))
             (if entries
                 (join-lines
                  (mapcar (lambda (line)
                            (or (ignore-errors
                                 (fx.json:jget (fx.json:decode line) "text"))
                                line))
                          entries))
                 "(no saved preferences)"))
           "(no saved preferences)"))
      ((string= action "clear")
       (when (probe-file path) (delete-file path))
       "cleared")
      (t (fail "unknown memory action: ~a" action)))))
