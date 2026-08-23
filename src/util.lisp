;;;; util.lisp — small shared helpers (paths, files, strings, ids).

(in-package #:fx.util)

(defun getenv (name)
  (let ((v (sb-ext:posix-getenv name)))
    (and v (plusp (length v)) v)))

(defun home-dir ()
  (or (getenv "HOME") "/tmp"))

(defun fx-dir ()
  "Directory for fx state, mirroring the Zig version's ~/.fx."
  (let ((dir (merge-pathnames ".fx/" (parse-namestring (concatenate 'string (home-dir) "/")))))
    (ensure-directories-exist dir)
    dir))

(defun expand-path (path &optional (base (namestring *default-pathname-defaults*)))
  "Expand ~/..., keep absolute paths, resolve relative paths against BASE."
  (cond
    ((zerop (length path)) base)
    ((and (>= (length path) 2) (string= "~/" path :end2 2))
     (concatenate 'string (home-dir) (subseq path 1)))
    ((string= path "~") (home-dir))
    ((char= (char path 0) #\/) path)
    (t (namestring (merge-pathnames path (parse-namestring
                                          (if (string-suffix-p "/" base)
                                              base
                                              (concatenate 'string base "/"))))))))

(defun read-file-string (path)
  (with-open-file (in path :direction :input
                           :external-format '(:utf-8 :replacement #\?))
    (let ((out (make-string-output-stream))
          (buffer (make-string 65536)))
      (loop for n = (read-sequence buffer in)
            while (plusp n)
            do (write-string buffer out :end n))
      (get-output-stream-string out))))

(defun write-file-string (path content)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string content out))
  (values))

(defun split-lines (string)
  (let ((lines '()) (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
            do (push (subseq string start i) lines)
               (setf start (1+ i)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defun join-lines (lines)
  (format nil "~{~a~^~%~}" lines))

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun string-suffix-p (suffix string)
  (and (<= (length suffix) (length string))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun starts-with-any (string prefixes)
  (some (lambda (p) (string-prefix-p p string)) prefixes))

(defun iso8601-now ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

(defvar *id-random-state* (make-random-state t))

(defun random-id (&optional (bytes 8))
  "Hex id like fx session ids."
  (with-output-to-string (out)
    (dotimes (_ bytes)
      (format out "~2,'0x" (random 256 *id-random-state*)))))
