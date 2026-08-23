;;;; permissions.lisp — permission engine, port of core/permissions.
;;;;
;;;; Ported semantics:
;;;; - Modes ask/auto/yolo (types.zig PermissionMode).
;;;; - Rules from settings.json "permission": a bare action string, or a map
;;;;   of permission name -> action | {pattern -> action} (config_runtime.zig
;;;;   parsePermissionConfig). The last matching rule wins.
;;;; - Tool names collapse to permission names: read_file->read,
;;;;   write_file/edit_file->edit, list_files->list, glob_files->glob,
;;;;   grep_files->grep, terminal->bash, web_fetch->web_fetch.
;;;; - bash allow rules with wildcards match only static commands (no shell
;;;;   metacharacters), like isStaticCommand in permissions.zig.
;;;; - Session grants ("always"): exact command for bash, canonical domain
;;;;   for web_fetch, workspace tree pattern for path tools.

(in-package #:fx.permissions)

(defstruct rule permission pattern action)

(defstruct engine
  (mode :ask)
  (rules '())
  (grants '())
  (workspace (namestring *default-pathname-defaults*)))

(defparameter +yolo-warning+ "YOLO enabled: fx permission checks disabled")

(defun permission-name-for-tool (tool-name)
  (cond
    ((string= tool-name "read_file") "read")
    ((or (string= tool-name "write_file") (string= tool-name "edit_file")) "edit")
    ((string= tool-name "list_files") "list")
    ((string= tool-name "glob_files") "glob")
    ((string= tool-name "grep_files") "grep")
    ((string= tool-name "terminal") "bash")
    (t tool-name)))

;;; ------------------------------------------------------------ rule parsing

(defun %parse-action (string)
  (cond ((equal string "allow") :allow)
        ((equal string "ask") :ask)
        ((equal string "deny") :deny)
        (t nil)))

(defun parse-rules (permission-value)
  "Parse the decoded settings.json \"permission\" value into a rule list."
  (let ((rules '()))
    (cond
      ((stringp permission-value)
       (let ((action (%parse-action permission-value)))
         (when action
           (push (make-rule :permission "*" :pattern "*" :action action) rules))))
      ((hash-table-p permission-value)
       (maphash
        (lambda (permission value)
          (cond
            ((stringp value)
             (let ((action (%parse-action value)))
               (when action
                 (push (make-rule :permission permission :pattern "*"
                                  :action action) rules))))
            ((hash-table-p value)
             (maphash (lambda (pattern action-string)
                        (let ((action (%parse-action action-string)))
                          (when action
                            (push (make-rule :permission permission
                                             :pattern pattern
                                             :action action) rules))))
                      value))))
        permission-value)))
    (nreverse rules)))

(defun load-rules ()
  (parse-rules (fx.json:jget (fx.config:load-settings) "permission")))

;;; ---------------------------------------------------------------- matching

(defun wildcard-match-p (pattern candidate &optional (p 0) (c 0))
  "Full-string match with * (any run) and ? (any one char)."
  (cond
    ((>= p (length pattern)) (>= c (length candidate)))
    ((char= (char pattern p) #\*)
     (loop for k from c to (length candidate)
           thereis (wildcard-match-p pattern candidate (1+ p) k)))
    ((>= c (length candidate)) nil)
    ((or (char= (char pattern p) #\?)
         (char= (char pattern p) (char candidate c)))
     (wildcard-match-p pattern candidate (1+ p) (1+ c)))
    (t nil)))

(defun %directory-tree-pattern-p (pattern)
  (fx.util:string-suffix-p "/**" pattern))

(defun %directory-tree-match-p (pattern candidate)
  "\"dir/**\" matches dir itself and anything under it."
  (and (%directory-tree-pattern-p pattern)
       (let ((root (subseq pattern 0 (- (length pattern) 3))))
         (or (string= candidate root)
             (fx.util:string-prefix-p (concatenate 'string root "/") candidate)))))

(defun target-match-p (pattern candidate)
  (or (%directory-tree-match-p pattern candidate)
      (wildcard-match-p pattern candidate)))

(defun static-command-p (command &optional allow-wildcards)
  "True when COMMAND has no shell metacharacters (port of isStaticCommand)."
  (and (plusp (length command))
       (loop for ch across command
             never (or (member ch '(#\; #\| #\& #\$ #\` #\< #\> #\( #\) #\Newline))
                       (and (not allow-wildcards)
                            (member ch '(#\* #\?)))))))

(defun %rule-matches-permission-p (rule-permission permission)
  (or (string= rule-permission "*")
      (string= rule-permission permission)))

(defun %rule-target-matches-p (permission action pattern candidate)
  "bash allow rules are stricter: wildcards only match static commands
(port of ruleTargetMatches)."
  (if (and (eq action :allow) (string= permission "bash"))
      (if (find-if (lambda (ch) (member ch '(#\* #\?))) pattern)
          (and (static-command-p pattern t)
               (static-command-p candidate)
               (wildcard-match-p pattern candidate))
          (string= pattern candidate))
      (target-match-p pattern candidate)))

(defun evaluate-rules (rules permission candidate)
  "Return :allow/:ask/:deny from the last matching rule, or NIL."
  (let ((matched nil))
    (dolist (rule rules matched)
      (when (and (%rule-matches-permission-p (rule-permission rule) permission)
                 (%rule-target-matches-p permission (rule-action rule)
                                         (rule-pattern rule) candidate))
        (setf matched (rule-action rule))))))

;;; ----------------------------------------------------------------- targets

(defun url-domain (url)
  "Canonical domain from a URL, or NIL (port of webFetchDomainTargetForUrl)."
  (let ((scheme-end (search "://" url)))
    (when scheme-end
      (let* ((start (+ scheme-end 3))
             (end (or (position-if (lambda (c) (member c '(#\/ #\? #\#)))
                                   url :start start)
                      (length url)))
             (authority (subseq url start end)))
        (when (and (plusp (length authority))
                   (not (find #\@ authority)))
          (let* ((colon (position #\: authority :from-end t))
                 (host (if (and colon (every #'digit-char-p
                                             (subseq authority (1+ colon))))
                           (subseq authority 0 colon)
                           authority)))
            (string-downcase host)))))))

(defun tool-target (tool-name args)
  "Extract the permission target string for a tool call."
  (let ((permission (permission-name-for-tool tool-name)))
    (cond
      ((string= permission "bash")
       (or (fx.json:jget args "command") ""))
      ((string= permission "web_fetch")
       (or (url-domain (or (fx.json:jget args "url") "")) ""))
      (t
       (let ((path (fx.json:jget args "path")))
         (if path (fx.util:expand-path path) ""))))))

(defun %inside-workspace-p (engine path)
  (let ((root (string-right-trim "/" (engine-workspace engine))))
    (or (string= path root)
        (fx.util:string-prefix-p (concatenate 'string root "/") path))))

;;; ------------------------------------------------------------------ grants

(defun grant-pattern (engine tool-name target)
  "The pattern an \"always\" answer stores (port of suggestedSessionGrants)."
  (let ((permission (permission-name-for-tool tool-name)))
    (cond
      ((string= permission "bash") target)
      ((string= permission "web_fetch") target)
      ((and (plusp (length target)) (%inside-workspace-p engine target))
       (concatenate 'string (string-right-trim "/" (engine-workspace engine)) "/**"))
      ((plusp (length target))
       (let ((slash (position #\/ target :from-end t)))
         (if (and slash (plusp slash))
             (concatenate 'string (subseq target 0 slash) "/**")
             target)))
      (t "*"))))

(defun add-grant (engine tool-name target)
  (let ((permission (permission-name-for-tool tool-name))
        (pattern (grant-pattern engine tool-name target)))
    (pushnew (cons permission pattern) (engine-grants engine) :test #'equal)
    pattern))

(defun grant-allows-p (engine tool-name target)
  (let ((permission (permission-name-for-tool tool-name)))
    (loop for (grant-permission . pattern) in (engine-grants engine)
          thereis (and (string= grant-permission permission)
                       (if (string= permission "bash")
                           (string= pattern target)
                           (target-match-p pattern target))))))

;;; ---------------------------------------------------------------- decision

(defun decide (engine tool-name target requires-approval)
  "Return :allow, :ask, or :deny for one tool call.
Precedence: deny rule, session grant, allow/ask rule, then mode."
  (let* ((permission (permission-name-for-tool tool-name))
         (rule-decision (evaluate-rules (engine-rules engine) permission target)))
    (cond
      ((eq rule-decision :deny) :deny)
      ((eq (engine-mode engine) :yolo) :allow)
      ((grant-allows-p engine tool-name target) :allow)
      ((eq rule-decision :allow) :allow)
      ((eq rule-decision :ask) :ask)
      ((not requires-approval) :allow)
      ;; auto mode approves workspace-scoped path mutations; bash and
      ;; web_fetch still ask.
      ((and (eq (engine-mode engine) :auto)
            (not (member permission '("bash" "web_fetch") :test #'string=))
            (plusp (length target))
            (%inside-workspace-p engine target))
       :allow)
      (t :ask))))

;;; ------------------------------------------------------------------ status

(defun mode-label (mode)
  (string-downcase (symbol-name mode)))

(defun parse-mode (string)
  (cond ((equal string "ask") :ask)
        ((equal string "auto") :auto)
        ((equal string "yolo") :yolo)
        (t nil)))

(defun make-default-engine ()
  (let ((settings (fx.config:load-settings)))
    (make-engine :mode (or (parse-mode (fx.json:jget settings "permission_mode"))
                           :ask)
                 :rules (parse-rules (fx.json:jget settings "permission"))
                 :grants '()
                 :workspace (namestring *default-pathname-defaults*))))

(defun status-text (engine)
  (with-output-to-string (out)
    (format out "mode: ~a~%" (mode-label (engine-mode engine)))
    (when (eq (engine-mode engine) :yolo)
      (format out "~a~%" +yolo-warning+))
    (format out "rules:~%")
    (if (engine-rules engine)
        (dolist (rule (engine-rules engine))
          (format out "  ~a ~a -> ~a~%"
                  (rule-permission rule) (rule-pattern rule)
                  (string-downcase (symbol-name (rule-action rule)))))
        (format out "  (none)~%"))
    (format out "session grants:~%")
    (if (engine-grants engine)
        (loop for (permission . pattern) in (engine-grants engine)
              do (format out "  ~a ~a~%" permission pattern))
        (format out "  (none)~%"))))
