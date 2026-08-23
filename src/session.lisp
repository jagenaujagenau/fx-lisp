;;;; session.lisp — JSONL session store, port of core/session/session_store.zig.
;;;;
;;;; Sessions live under ~/.fx/sessions/<id>.jsonl, one JSON event per line,
;;;; with a "latest" pointer file mirroring session_latest_pointer.zig.

(in-package #:fx.session)

(defstruct session id path)

(defun sessions-dir ()
  (let ((dir (merge-pathnames "sessions/" (fx-dir))))
    (ensure-directories-exist dir)
    dir))

(defun %session-path (id)
  (merge-pathnames (format nil "~a.jsonl" id) (sessions-dir)))

(defun %latest-pointer-path ()
  (merge-pathnames "latest" (sessions-dir)))

(defun make-new-session ()
  (let* ((id (format nil "~a-~a"
                     (substitute #\- #\: (substitute #\- #\T (iso8601-now)))
                     (random-id 4)))
         (session (make-session :id id :path (%session-path id))))
    (write-file-string (%latest-pointer-path) id)
    (append-event session
                  (fx.json:make-jobject "kind" "session_start"
                                        "at" (iso8601-now)
                                        "cwd" (namestring *default-pathname-defaults*)))
    session))

(defun append-event (session event)
  (fx.json:jput event "at" (or (fx.json:jget event "at") (iso8601-now)))
  (with-open-file (out (session-path session)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create
                       :external-format :utf-8)
    (write-string (fx.json:encode-to-string event) out)
    (terpri out))
  (values))

(defun load-session (id)
  (let ((path (%session-path id)))
    (unless (probe-file path)
      (error "session not found: ~a" id))
    (make-session :id id :path path)))

(defun session-events (session)
  (when (probe-file (session-path session))
    (loop for line in (split-lines (read-file-string (session-path session)))
          when (plusp (length line))
            collect (fx.json:decode line))))

(defun session-messages (session)
  "Rebuild the chat message list from logged message events."
  (loop for event in (session-events session)
        when (equal (fx.json:jget event "kind") "message")
          collect (fx.json:jget event "message")))

(defun latest-session-id ()
  (let ((path (%latest-pointer-path)))
    (when (probe-file path)
      (string-trim '(#\Space #\Newline) (read-file-string path)))))

(defun list-sessions ()
  (sort (loop for path in (directory (merge-pathnames "*.jsonl" (sessions-dir)))
              collect (pathname-name path))
        #'string>))
