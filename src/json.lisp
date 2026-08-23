;;;; json.lisp — minimal JSON codec, no external dependencies.
;;;;
;;;; Mapping: object -> EQUAL hash-table, array -> list, string -> string,
;;;; number -> integer/double, true -> T, false -> :FALSE, null -> :NULL.

(in-package #:fx.json)

(define-condition json-parse-error (error)
  ((message :initarg :message :reader json-parse-error-message))
  (:report (lambda (c s)
             (format s "JSON parse error: ~a" (json-parse-error-message c)))))

(defun json-null () :null)
(defun json-false () :false)
(defun json-true-p (v) (and v (not (eq v :false)) (not (eq v :null))))

;;; ----------------------------------------------------------------- decoding

(defstruct (%reader (:constructor %make-reader (string)))
  (string "" :type simple-string)
  (pos 0 :type fixnum))

(defun %peek (r)
  (let ((s (%reader-string r)) (p (%reader-pos r)))
    (if (< p (length s)) (char s p) nil)))

(defun %next (r)
  (let ((c (%peek r)))
    (unless c (error 'json-parse-error :message "unexpected end of input"))
    (incf (%reader-pos r))
    c))

(defun %skip-ws (r)
  (loop for c = (%peek r)
        while (and c (member c '(#\Space #\Tab #\Newline #\Return)))
        do (incf (%reader-pos r))))

(defun %expect (r char)
  (let ((c (%next r)))
    (unless (char= c char)
      (error 'json-parse-error
             :message (format nil "expected '~a', got '~a' at ~d" char c (%reader-pos r))))))

(defun %expect-literal (r literal value)
  (loop for ch across literal do (%expect r ch))
  value)

(defun %parse-string (r)
  (%expect r #\")
  (with-output-to-string (out)
    (loop
      (let ((c (%next r)))
        (case c
          (#\" (return))
          (#\\ (let ((e (%next r)))
                 (case e
                   (#\" (write-char #\" out))
                   (#\\ (write-char #\\ out))
                   (#\/ (write-char #\/ out))
                   (#\b (write-char #\Backspace out))
                   (#\f (write-char #\Page out))
                   (#\n (write-char #\Newline out))
                   (#\r (write-char #\Return out))
                   (#\t (write-char #\Tab out))
                   (#\u (let ((code (parse-integer (%reader-string r)
                                                   :start (%reader-pos r)
                                                   :end (+ (%reader-pos r) 4)
                                                   :radix 16)))
                          (incf (%reader-pos r) 4)
                          ;; surrogate pair
                          (if (and (<= #xD800 code #xDBFF)
                                   (eql (%peek r) #\\))
                              (progn
                                (%next r) (%expect r #\u)
                                (let ((low (parse-integer (%reader-string r)
                                                          :start (%reader-pos r)
                                                          :end (+ (%reader-pos r) 4)
                                                          :radix 16)))
                                  (incf (%reader-pos r) 4)
                                  (write-char (code-char (+ #x10000
                                                            (ash (- code #xD800) 10)
                                                            (- low #xDC00)))
                                              out)))
                              (write-char (code-char code) out))))
                   (t (error 'json-parse-error
                             :message (format nil "bad escape \\~a" e))))))
          (t (write-char c out)))))))

(defun %parse-number (r)
  (let ((start (%reader-pos r))
        (s (%reader-string r))
        (floatp nil))
    (loop for c = (%peek r)
          while (and c (or (digit-char-p c) (member c '(#\- #\+ #\. #\e #\E))))
          do (when (member c '(#\. #\e #\E)) (setf floatp t))
             (incf (%reader-pos r)))
    (let ((text (subseq s start (%reader-pos r))))
      (if floatp
          (let ((*read-default-float-format* 'double-float))
            (coerce (read-from-string text) 'double-float))
          (parse-integer text)))))

(defun %parse-value (r)
  (%skip-ws r)
  (let ((c (%peek r)))
    (unless c (error 'json-parse-error :message "unexpected end of input"))
    (case c
      (#\{ (%parse-object r))
      (#\[ (%parse-array r))
      (#\" (%parse-string r))
      (#\t (%expect-literal r "true" t))
      (#\f (%expect-literal r "false" :false))
      (#\n (%expect-literal r "null" :null))
      (t (%parse-number r)))))

(defun %parse-object (r)
  (%expect r #\{)
  (let ((table (make-hash-table :test #'equal)))
    (%skip-ws r)
    (when (eql (%peek r) #\})
      (%next r)
      (return-from %parse-object table))
    (loop
      (%skip-ws r)
      (let ((key (%parse-string r)))
        (%skip-ws r)
        (%expect r #\:)
        (setf (gethash key table) (%parse-value r)))
      (%skip-ws r)
      (let ((c (%next r)))
        (case c
          (#\} (return table))
          (#\, nil)
          (t (error 'json-parse-error
                    :message (format nil "expected ',' or '}', got '~a'" c))))))))

(defun %parse-array (r)
  (%expect r #\[)
  (%skip-ws r)
  (when (eql (%peek r) #\])
    (%next r)
    (return-from %parse-array nil))
  (let ((items '()))
    (loop
      (push (%parse-value r) items)
      (%skip-ws r)
      (let ((c (%next r)))
        (case c
          (#\] (return (nreverse items)))
          (#\, nil)
          (t (error 'json-parse-error
                    :message (format nil "expected ',' or ']', got '~a'" c))))))))

(defun decode (string)
  "Decode a JSON STRING into Lisp data."
  (let ((r (%make-reader (coerce string 'simple-string))))
    (prog1 (%parse-value r)
      (%skip-ws r))))

;;; ----------------------------------------------------------------- encoding

(defun %encode-string (string out)
  (write-char #\" out)
  (loop for c across string
        do (case c
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Newline (write-string "\\n" out))
             (#\Return (write-string "\\r" out))
             (#\Tab (write-string "\\t" out))
             (#\Backspace (write-string "\\b" out))
             (#\Page (write-string "\\f" out))
             (t (if (< (char-code c) #x20)
                    (format out "\\u~4,'0x" (char-code c))
                    (write-char c out)))))
  (write-char #\" out))

(defun encode (value out)
  "Encode VALUE as JSON to stream OUT."
  (etypecase value
    (string (%encode-string value out))
    (hash-table
     (write-char #\{ out)
     (let ((first t))
       (maphash (lambda (k v)
                  (if first (setf first nil) (write-char #\, out))
                  (%encode-string (if (stringp k) k (princ-to-string k)) out)
                  (write-char #\: out)
                  (encode v out))
                value))
     (write-char #\} out))
    (null (write-string "[]" out))
    (cons
     (write-char #\[ out)
     (loop for (item . rest) on value
           do (encode item out)
              (when rest (write-char #\, out)))
     (write-char #\] out))
    (integer (format out "~d" value))
    (real (let ((*read-default-float-format* 'double-float))
            (format out "~f" (coerce value 'double-float))))
    (symbol
     (cond ((eq value t) (write-string "true" out))
           ((eq value :false) (write-string "false" out))
           ((eq value :null) (write-string "null" out))
           (t (%encode-string (string-downcase (symbol-name value)) out))))))

(defun encode-to-string (value)
  (with-output-to-string (out)
    (encode value out)))

;;; ------------------------------------------------------------------ helpers

(defun jget (object key &optional default)
  "Get KEY from a decoded JSON object, or DEFAULT. :NULL counts as missing."
  (if (hash-table-p object)
      (let ((v (gethash key object default)))
        (if (eq v :null) default v))
      default))

(defun jget* (object &rest keys)
  "Nested jget along KEYS."
  (loop for key in keys
        do (setf object (jget object key)))
  object)

(defun jbool (object key &optional default)
  (let ((v (jget object key :missing)))
    (case v
      (:missing default)
      ((:false :null) nil)
      (t (and v t)))))

(defun make-jobject (&rest pairs)
  "Build an EQUAL hash-table from alternating key/value PAIRS.
Pairs whose value is :OMIT are skipped."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          unless (eq v :omit)
            do (setf (gethash k table) v))
    table))

(defun jput (object key value)
  (setf (gethash key object) value)
  object)
