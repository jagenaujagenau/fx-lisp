;;;; cli.lisp — the fx command line, port of core/cli and the REPL surface.
;;;;
;;;; Usage:
;;;;   fx                 interactive REPL
;;;;   fx ask "prompt"    one-shot noninteractive turn
;;;;   fx setup           store an API key
;;;;   fx models          list gateway models
;;;;   fx sessions        list stored sessions
;;;;   fx help / version

(in-package #:fx.cli)

(defparameter +version+ "0.1.0-lisp")

(defun say (fmt &rest args)
  (apply #'format t fmt args)
  (fresh-line)
  (force-output))

(defun %require-api-key ()
  (multiple-value-bind (key source) (fx.config:resolve-api-key)
    (declare (ignore source))
    (unless key
      (say "~a" fx.config:missing-credential-message)
      (sb-ext:exit :code 1))
    key))

(defun %interactive-approval (tool-name label target)
  (format t "~&approve? ~a  [y]es once / [a]lways / [N]o " label)
  (force-output)
  (let ((answer (string-downcase
                 (string-trim " " (or (read-line *standard-input* nil "") "")))))
    (cond
      ((member answer '("y" "yes") :test #'string=) :once)
      ((member answer '("a" "always") :test #'string=)
       (when fx.agent:*engine*
         (say "(will not ask again this session for: ~a ~a)"
              (fx.permissions:permission-name-for-tool tool-name)
              (fx.permissions:grant-pattern fx.agent:*engine* tool-name target)))
       :always)
      (t nil))))

(defun %interactive-questions (questions)
  "Prompt for each ask_user_question entry; returns chosen labels."
  (loop for (text options) in questions
        collect (progn
                  (say "~%~a" text)
                  (loop for (label description) in options
                        for index from 1
                        do (say "  ~d. ~a~@[ — ~a~]" index label description))
                  (format t "choose [1-~d]: " (length options))
                  (force-output)
                  (let* ((line (string-trim " " (or (read-line *standard-input*
                                                               nil "")
                                                    "")))
                         (index (ignore-errors (parse-integer line))))
                    (cond
                      ((and index (<= 1 index (length options)))
                       (first (nth (1- index) options)))
                      ((plusp (length line)) line)
                      (t (first (first options))))))))

(defun %print-help ()
  (say "fx ~a — tiny coding agent (Common Lisp port)" +version+)
  (say "")
  (say "  fx                start interactive session")
  (say "  fx ask \"...\"      run one noninteractive turn")
  (say "  fx resume [id]    resume a stored session (default: latest)")
  (say "  fx setup          store an AI Gateway API key")
  (say "  fx models         list available models")
  (say "  fx sessions       list stored sessions")
  (say "  fx version        print version")
  (say "")
  (say "Slash commands inside the REPL: /help /model /models /mode /permissions /skills /mcp /hooks /status /new /resume /usage /compact /sessions /version /exit"))

(defun setup ()
  (format t "Paste your Vercel AI Gateway API key: ")
  (force-output)
  (let ((key (string-trim " " (or (read-line *standard-input* nil "") ""))))
    (if (plusp (length key))
        (progn
          (fx.util:write-file-string
           (merge-pathnames "api-key.json" (fx.util:fx-dir))
           (fx.json:encode-to-string (fx.json:make-jobject "api_key" key)))
          (say "Saved to ~a" (merge-pathnames "api-key.json" (fx.util:fx-dir))))
        (say "No key entered."))))

(defun models ()
  (let ((key (%require-api-key)))
    (handler-case
        (dolist (id (fx.gateway:list-models :api-key key))
          (say "~a" id))
      (fx.gateway:gateway-error (e) (say "~a" e)))))

(defun sessions ()
  (let ((ids (fx.session:list-sessions)))
    (if ids
        (dolist (id ids) (say "~a" id))
        (say "(no sessions)"))))

(defun ask (prompt &key model)
  "One noninteractive turn, like `fx ask`. Mutating tools are auto-approved
only when FX_AUTO_APPROVE=1; otherwise they are denied."
  (let* ((key (%require-api-key))
         (model (fx.config:resolve-model model))
         (session (fx.session:make-new-session))
         (fx.agent:*engine*
           (let ((engine (fx.permissions:make-default-engine)))
             (when (fx.util:getenv "FX_AUTO_APPROVE")
               (setf (fx.permissions:engine-mode engine) :yolo))
             engine))
         (fx.agent:*approval-hook*
           (lambda (name label target)
             (declare (ignore name target))
             (say "(denied without a tty: ~a)" label)
             nil)))
    (fx.hooks:load-user-hooks)
    (handler-case
        (fx.agent:run-turn :session session :model model
                           :api-key key :user-input prompt :messages '())
      (fx.gateway:gateway-error (e) (say "~a" e) (sb-ext:exit :code 1)))
    (values)))

(defun %handle-slash (line model-box messages-box session-box)
  "Handle a slash command. Returns :exit, :handled, or NIL."
  (let* ((trimmed (string-trim " " line))
         (space (position #\Space trimmed))
         (command (if space (subseq trimmed 0 space) trimmed))
         (arg (if space (string-trim " " (subseq trimmed space)) "")))
    (cond
      ((member command '("/exit" "/quit") :test #'string=) :exit)
      ((string= command "/help") (%print-help) :handled)
      ((string= command "/models") (models) :handled)
      ((string= command "/sessions") (sessions) :handled)
      ((string= command "/model")
       (if (plusp (length arg))
           (progn (setf (car model-box) arg)
                  (fx.config:save-settings
                   (fx.json:jput (fx.config:load-settings) "model" arg))
                  (say "model set to ~a" arg))
           (say "model: ~a" (car model-box)))
       :handled)
      ((string= command "/skills")
       (let* ((space (position #\Space arg))
              (subcommand (if (plusp (length arg))
                              (if space (subseq arg 0 space) arg)
                              "list"))
              (rest-arg (if space (string-trim " " (subseq arg space)) "")))
         (cond
           ((string= subcommand "list")
            (let ((skills (fx.skills:skills)))
              (if skills
                  (dolist (skill skills)
                    (say "~a (~a) — ~a"
                         (fx.skills:skill-name skill)
                         (fx.skills:skill-source skill)
                         (fx.compaction:compact-line
                          (fx.skills:skill-description skill) 100)))
                  (say "(no skills installed; add directories with a SKILL.md under .fx/skills or ~~/.fx/skills)"))))
           ((string= subcommand "show")
            (let ((skill (fx.skills:find-skill rest-arg)))
              (if skill
                  (progn (say "~a — ~a" (fx.skills:skill-name skill)
                              (fx.skills:skill-location skill))
                         (say "~a" (fx.skills:read-skill-chunk skill)))
                  (say "unknown skill: ~a" rest-arg))))
           ((string= subcommand "reload")
            (say "~d skill~:p" (length (fx.skills:skills :reload t))))
           ((string= subcommand "path")
            (say "~a" (fx.skills:managed-skills-dir)))
           ((member subcommand '("add" "install") :test #'string=)
            (if (plusp (length rest-arg))
                (handler-case
                    (let ((installed (fx.skills:install-from-source rest-arg)))
                      (if installed
                          (say "Installed: ~{~a~^, ~}" installed)
                          (say "No skills found (no SKILL.md files).")))
                  (fx.tools:tool-error (e) (say "~a" e)))
                (say "usage: /skills add <repo|path|url|owner/repo@skill>")))
           ((string= subcommand "create")
            (handler-case
                (say "Created ~a" (fx.skills:create-skill-template rest-arg))
              (fx.tools:tool-error (e) (say "~a" e))))
           ((string= subcommand "remove")
            (handler-case
                (say "Removed skill '~a'."
                     (fx.skills:remove-managed-skill rest-arg))
              (fx.tools:tool-error (e) (say "~a" e))))
           (t (say "usage: /skills [list|add|install|show|create|remove|reload|path] [name|url|path]"))))
       :handled)
      ((string= command "/mode")
       (if (plusp (length arg))
           (let ((mode (fx.modes:lookup arg)))
             (if mode
                 (progn
                   (setf fx.agent:*mode* arg
                         (fx.permissions:engine-mode fx.agent:*engine*)
                         (fx.modes:mode-spec-permission-mode mode))
                   (say "mode: ~a (~a) — permissions ~a~@[, read-only tools~]"
                        (fx.modes:mode-spec-name mode) arg
                        (fx.permissions:mode-label
                         (fx.modes:mode-spec-permission-mode mode))
                        (eq (fx.modes:mode-spec-tool-policy mode) :read-only)))
                 (say "unknown mode ~a; available: ~{~a~^, ~}" arg
                      (mapcar #'fx.modes:mode-spec-id (fx.modes:modes)))))
           (dolist (mode (fx.modes:modes))
             (say "~:[ ~;*~] ~a — ~a"
                  (string= (fx.modes:mode-spec-id mode) fx.agent:*mode*)
                  (fx.modes:mode-spec-id mode)
                  (fx.modes:mode-spec-description mode))))
       :handled)
      ((string= command "/hooks")
       (let ((hooks (fx.hooks:list-hooks)))
         (if hooks
             (loop for (kind name) in hooks
                   do (say "~a — ~a" kind name))
             (say "(no hooks registered; add providers in ~a or .fx/hooks.lisp)"
                  (merge-pathnames "hooks.lisp" (fx.util:fx-dir)))))
       :handled)
      ((string= command "/mcp")
       (let* ((space (position #\Space arg))
              (subcommand (if (plusp (length arg))
                              (if space (subseq arg 0 space) arg)
                              "list"))
              (rest-arg (if space (string-trim " " (subseq arg space)) "")))
         (handler-case
             (cond
               ((string= subcommand "list")
                (let ((configs (fx.mcp:load-config)))
                  (if configs
                      (dolist (config configs)
                        (say "~a — ~a~{ ~a~}~@[ (disabled)~]"
                             (fx.mcp:server-config-name config)
                             (fx.mcp:server-config-command config)
                             (fx.mcp:server-config-args config)
                             (not (fx.mcp:server-config-enabled config))))
                      (say "(no MCP servers configured in ~a)"
                           (fx.mcp:config-path)))))
               ((string= subcommand "tools")
                (if (plusp (length rest-arg))
                    (dolist (tool (fx.mcp:server-tools rest-arg))
                      (say "~a — ~a"
                           (fx.mcp:dynamic-tool-name rest-arg
                                                     (fx.json:jget tool "name"))
                           (fx.compaction:compact-line
                            (or (fx.json:jget tool "description") "") 120)))
                    (say "usage: /mcp tools <server>")))
               ((string= subcommand "reload")
                (fx.mcp:shutdown-connections)
                (say "MCP connections reset"))
               ((string= subcommand "path")
                (say "~a" (fx.mcp:config-path)))
               (t (say "usage: /mcp [list|tools <server>|reload|path]")))
           (fx.mcp:mcp-error (e) (say "~a" e))))
       :handled)
      ((string= command "/status")
       (say "model: ~a" (car model-box))
       (say "mode: ~a" fx.agent:*mode*)
       (say "permissions: ~a" (fx.permissions:mode-label
                               (fx.permissions:engine-mode fx.agent:*engine*)))
       (say "workspace: ~a" (namestring *default-pathname-defaults*))
       (say "skills: ~d" (length (fx.skills:skills)))
       (say "history: ~d message~:p" (length (car messages-box)))
       :handled)
      ((member command '("/new" "/reset") :test #'string=)
       (setf (car messages-box) '())
       (when (string= command "/new")
         (setf (car session-box) (fx.session:make-new-session)))
       (say "started a fresh session context")
       :handled)
      ((string= command "/version")
       (say "fx ~a" +version+)
       :handled)
      ((string= command "/resume")
       (let ((id (if (plusp (length arg)) arg (fx.session:latest-session-id))))
         (if (null id)
             (say "(no stored sessions)")
             (handler-case
                 (let* ((session (fx.session:load-session id))
                        (messages (fx.session:session-messages session)))
                   (setf (car session-box) session
                         (car messages-box) messages)
                   (say "resumed session ~a (~d message~:p)"
                        id (length messages)))
               (error (e) (say "~a" e)))))
       :handled)
      ((string= command "/usage")
       (let ((rows (fx.gateway:usage-summary)))
         (if rows
             (progn
               (loop for (model requests prompt completion) in rows
                     do (say "~a — ~d request~:p, ~d prompt + ~d completion tokens"
                             model requests prompt completion))
               (say "total: ~d tokens"
                    (loop for (nil nil prompt completion) in rows
                          sum (+ prompt completion))))
             (say "(no gateway requests yet this session)")))
       :handled)
      ((string= command "/compact")
       (multiple-value-bind (compacted compacted-p removed)
           (fx.compaction:compact-messages (car messages-box) :force t)
         (if compacted-p
             (progn (setf (car messages-box) compacted)
                    (say "Context compacted. (~d older turn~:p folded into the summary)"
                         removed))
             (say "Nothing to compact yet.")))
       :handled)
      ((string= command "/permissions")
       (if (plusp (length arg))
           (let ((mode (fx.permissions:parse-mode arg)))
             (if mode
                 (progn
                   (setf (fx.permissions:engine-mode fx.agent:*engine*) mode)
                   (fx.config:save-settings
                    (fx.json:jput (fx.config:load-settings)
                                  "permission_mode" arg))
                   (say "permission mode set to ~a" arg)
                   (when (eq mode :yolo)
                     (say "~a" fx.permissions:+yolo-warning+)))
                 (say "usage: /permissions [ask|auto|yolo]")))
           (format t "~a" (fx.permissions:status-text fx.agent:*engine*)))
       :handled)
      ((string-prefix-p "/" command)
       (say "unknown command ~a — try /help" command)
       :handled)
      (t nil))))

(defun repl (&key model session-id)
  (let* ((key (%require-api-key))
         (model-box (list (fx.config:resolve-model model)))
         (session-box (list (if session-id
                                (fx.session:load-session session-id)
                                (fx.session:make-new-session))))
         (messages-box (list (if session-id
                                 (fx.session:session-messages
                                  (car session-box))
                                 '())))
         (fx.agent:*engine* (fx.permissions:make-default-engine))
         (fx.agent:*mode* fx.modes:+default-mode-id+)
         (fx.agent:*approval-hook* #'%interactive-approval)
         (fx.tools:*ask-user-hook* #'%interactive-questions))
    (fx.hooks:load-user-hooks)
    (when session-id
      (say "resumed session ~a (~d message~:p)"
           session-id (length (car messages-box))))
    (say "fx ~a — model ~a — permissions ~a — /help for commands"
         +version+ (car model-box)
         (fx.permissions:mode-label (fx.permissions:engine-mode fx.agent:*engine*)))
    (loop
      (format t "~&> ")
      (force-output)
      (let ((line (read-line *standard-input* nil nil)))
        (when (null line) (return))
        (let ((trimmed (string-trim " " line)))
          (case (if (zerop (length trimmed))
                    :handled
                    (%handle-slash trimmed model-box messages-box session-box))
            (:exit (return))
            (:handled)
            (t
             (handler-case
                 (multiple-value-bind (text new-messages)
                     (fx.agent:run-turn :session (car session-box)
                                        :model (car model-box)
                                        :api-key key
                                        :user-input trimmed
                                        :messages (car messages-box))
                   (declare (ignore text))
                   (setf (car messages-box) new-messages))
               (fx.gateway:gateway-error (e) (say "~a" e))
               (sb-sys:interactive-interrupt () (say "(interrupted)"))))))))
    (say "bye")))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  ;; sbcl leaves the "--" end-of-options marker in *posix-argv*.
  (let ((argv (remove "--" argv :test #'string= :count 1)))
    (%dispatch argv)))

(defun %dispatch (argv)
  (let ((command (first argv)))
    (cond
      ((null command) (repl))
      ((string= command "ask")
       (if (second argv)
           (ask (format nil "~{~a~^ ~}" (rest argv)))
           (say "usage: fx ask \"prompt\"")))
      ((string= command "setup") (setup))
      ((string= command "resume")
       (let ((id (or (second argv) (fx.session:latest-session-id))))
         (if id
             (repl :session-id id)
             (say "(no stored sessions)"))))
      ((string= command "models") (models))
      ((string= command "sessions") (sessions))
      ((member command '("version" "--version" "-v") :test #'string=)
       (say "fx ~a" +version+))
      ((member command '("help" "--help" "-h") :test #'string=) (%print-help))
      (t (say "unknown command: ~a" command) (%print-help)))))

(in-package #:fx)

(defun main ()
  (fx.cli:main))
