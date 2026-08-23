;;;; compaction.lisp — history compaction, port of core/session/session.zig
;;;; (compactHistory, buildCompactedSummaryText, compressSummaryLines,
;;;; formatCompactedContinuationMessage).
;;;;
;;;; fx compacts deterministically: older turns are folded into one summary
;;;; turn listing recent user requests, assistant outcomes, and tool
;;;; evidence; the most recent turns stay verbatim. This port works on the
;;;; flat OpenAI-style message list the Lisp agent keeps: a "turn" is a user
;;;; message plus the assistant/tool messages that follow it, and the
;;;; summary is carried as a user message starting with the continuation
;;;; preamble so it survives round-trips through the session store.

(in-package #:fx.compaction)

;; Verbatim strings and limits from session.zig.
(defparameter +continuation-preamble+
  "This session is being continued from earlier compacted context. The summary below covers the earlier portion of the conversation.")
(defparameter +recent-messages-note+
  "Recent conversation turns are preserved verbatim.")
(defparameter +direct-resume-instruction+
  "Continue the conversation from where it left off without asking the user to repeat context. Resume directly.")
(defparameter +max-summary-chars+ 1200)
(defparameter +max-summary-lines+ 24)
(defparameter +max-line-chars+ 160)

(defparameter *default-max-history-turns* 8
  "Same default as the Zig app runtime's SessionRuntime max_history_turns.")

(defun max-history-turns ()
  (let ((configured (fx.json:jget (fx.config:load-settings) "max_history_turns")))
    (if (and (integerp configured) (plusp configured))
        configured
        *default-max-history-turns*)))

(defun %role (message) (fx.json:jget message "role"))

(defun summary-message-p (message)
  (and (equal (%role message) "user")
       (let ((content (fx.json:jget message "content")))
         (and (stringp content)
              (fx.util:string-prefix-p +continuation-preamble+ content)))))

(defun group-turns (messages)
  "Group flat messages into turns: each user message starts a turn and
collects the assistant/tool messages after it. Returns (values summary-msg
turns) where SUMMARY-MSG is a prior compaction message or NIL."
  (let ((summary nil)
        (turns '())
        (current nil))
    (dolist (message messages)
      (cond
        ((and (null summary) (null turns) (null current)
              (summary-message-p message))
         (setf summary message))
        ((equal (%role message) "user")
         (when current (push (nreverse current) turns))
         (setf current (list message)))
        (t
         (push message current))))
    (when current (push (nreverse current) turns))
    (values summary (nreverse turns))))

;;; ------------------------------------------------------------ line helpers

(defun compact-line (text max-chars)
  "Collapse TEXT to one bounded line (port of compactLineText)."
  (let ((out (make-string-output-stream))
        (pending-space nil)
        (wrote nil))
    (loop for ch across text
          do (if (member ch '(#\Space #\Tab #\Newline #\Return))
                 (when wrote (setf pending-space t))
                 (progn
                   (when pending-space
                     (write-char #\Space out)
                     (setf pending-space nil))
                   (write-char ch out)
                   (setf wrote t))))
    (let ((line (get-output-stream-string out)))
      (if (> (length line) max-chars)
          (concatenate 'string (subseq line 0 (max 0 (- max-chars 3))) "...")
          line))))

(defun %compress-summary-lines (lines)
  "Dedup, bound line count and total size (port of compressSummaryLines)."
  (let ((seen '())
        (kept '())
        (chars 0)
        (omitted 0))
    (dolist (line lines)
      (let ((normalized (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
        (cond
          ((zerop (length normalized)))
          ((member normalized seen :test #'string=) (incf omitted))
          ((or (>= (length kept) +max-summary-lines+)
               (> (+ chars (length normalized) 1) +max-summary-chars+))
           (incf omitted))
          (t (push normalized seen)
             (push normalized kept)
             (incf chars (1+ (length normalized)))))))
    (when (plusp omitted)
      (push (format nil "- ~d line~:p omitted from summary." omitted) kept))
    (fx.util:join-lines (nreverse kept))))

;;; ----------------------------------------------------------- summary build

(defun %prior-summary-body (summary-message)
  "Extract the summary block from a prior continuation message."
  (let* ((content (fx.json:jget summary-message "content"))
         (start (search "Conversation summary:" content))
         (end (search +recent-messages-note+ content)))
    (when start
      (string-trim '(#\Space #\Newline)
                   (subseq content start (or end (length content)))))))

(defun %prior-removed-count (summary-body)
  "Recover the cumulative removed-turn count from a prior summary."
  (or (and summary-body
           (let ((hit (search "- Earlier turns compacted: " summary-body)))
             (when hit
               (ignore-errors
                (parse-integer summary-body
                               :start (+ hit (length "- Earlier turns compacted: "))
                               :junk-allowed t)))))
      0))

(defun %turn-user-text (turn)
  (let ((message (first turn)))
    (and (equal (%role message) "user")
         (fx.json:jget message "content"))))

(defun %collect-limited (turns extract header limit)
  "Header line plus up to LIMIT compacted lines from EXTRACT over TURNS."
  (let ((lines '()) (added 0))
    (dolist (turn turns)
      (when (>= added limit) (return))
      (dolist (text (funcall extract turn))
        (when (>= added limit) (return))
        (let ((line (compact-line (or text "") (- +max-line-chars+ 4))))
          (when (plusp (length line))
            (when (zerop added) (push header lines))
            (push (format nil "  - ~a" line) lines)
            (incf added)))))
    (nreverse lines)))

(defun build-summary-text (prior-summary-body prior-removed removed-turns)
  "Port of buildCompactedSummaryText: structured extractive summary."
  (let ((lines '()))
    (push "Conversation summary:" lines)
    (push (format nil "- Earlier turns compacted: ~d"
                  (+ prior-removed (length removed-turns)))
          lines)
    (when prior-summary-body
      (push "- Previously compacted context:" lines)
      (dolist (line (fx.util:split-lines prior-summary-body))
        (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
          (when (and (plusp (length trimmed))
                     (not (string= trimmed "Conversation summary:"))
                     (not (fx.util:string-prefix-p "- Earlier turns compacted:"
                                                   trimmed)))
            (push (format nil "  ~a" trimmed) lines)))))
    (setf lines (nreverse lines))
    (setf lines
          (append lines
                  (%collect-limited removed-turns
                                    (lambda (turn)
                                      (let ((text (%turn-user-text turn)))
                                        (and text (list text))))
                                    "- Recent user requests:" 4)
                  (%collect-limited removed-turns
                                    (lambda (turn)
                                      (loop for message in (rest turn)
                                            when (and (equal (%role message) "assistant")
                                                      (stringp (fx.json:jget message "content")))
                                              collect (fx.json:jget message "content")))
                                    "- Assistant outcomes:" 3)
                  (%collect-limited removed-turns
                                    (lambda (turn)
                                      (loop for message in (rest turn)
                                            when (equal (%role message) "tool")
                                              collect (fx.json:jget message "content")))
                                    "- Tool execution evidence:" 4)))
    (when (<= (length lines) 2)
      (setf lines (append lines (list "- Earlier conversation context compacted."))))
    (%compress-summary-lines lines)))

(defun continuation-message-text (summary)
  "Port of formatCompactedContinuationMessage."
  (format nil "~a~%~%~a~%~%~a~%~a"
          +continuation-preamble+ summary
          +recent-messages-note+ +direct-resume-instruction+))

;;; ------------------------------------------------------------- compaction

(defun %preserved-recent-turn-count (max-turns)
  "Port of preservedRecentTurnCount."
  (if (<= max-turns 2) 1 (min (1- max-turns) 4)))

(defun compact-messages (messages &key (max-turns (max-history-turns)) force)
  "Compact MESSAGES when the turn count exceeds MAX-TURNS (or always, with
FORCE). Returns (values messages compacted-p removed-count)."
  (multiple-value-bind (summary-message turns) (group-turns messages)
    (let* ((turn-count (length turns))
           (keep (if force 1 (%preserved-recent-turn-count max-turns))))
      (if (or (and (not force) (<= turn-count max-turns))
              (<= turn-count keep)
              (zerop turn-count))
          (values messages nil 0)
          (let* ((removed (subseq turns 0 (- turn-count keep)))
                 (preserved (subseq turns (- turn-count keep)))
                 (prior-body (and summary-message
                                  (%prior-summary-body summary-message)))
                 (summary (build-summary-text
                           prior-body
                           (%prior-removed-count prior-body)
                           removed))
                 (head (fx.json:make-jobject
                        "role" "user"
                        "content" (continuation-message-text summary))))
            (values (cons head (reduce #'append preserved :from-end t
                                                          :initial-value '()))
                    t
                    (length removed)))))))
