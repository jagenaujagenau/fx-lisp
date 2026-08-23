;;;; skills.lisp — skills, port of core/skills + builtins/skills.zig.
;;;;
;;;; Skills are directories containing a SKILL.md with YAML frontmatter
;;;; (name, description). Discovery scans fx's root list in precedence
;;;; order — workspace roots first, then the managed install root, then
;;;; global compatibility roots — and the first occurrence of a name wins.
;;;; The catalog is advertised in the static context; the `skill` tool
;;;; reads SKILL.md or a relative resource in bounded chunks.
;;;;
;;;; Limits are fx's context_limits defaults: description 1 KB, catalog
;;;; 16 KB, chunk 20 KB, file 1 MB. install_skill is ported below (local
;;;; directories, git clone, skills.sh URLs, owner/repo@skill specs, and
;;;; pasted npx/bunx skills add commands); the $ search menu is not.

(in-package #:fx.skills)

(defparameter +max-description-bytes+ 1024)
(defparameter +max-catalog-bytes+ 16384)
(defparameter +max-chunk-bytes+ 20480)
(defparameter +max-file-bytes+ (* 1024 1024))
(defparameter +max-name-bytes+ 256)

;; Same tables as builtins/skills.zig.
(defparameter +workspace-roots+
  '(".fx/skills" "skills" ".opencode/skills" ".codex/skills"
    ".claude/skills" ".agents/skills" ".claw/skills"))
(defparameter +global-roots+
  '(".fx/skills" ".config/opencode/skills" ".codex/skills"
    ".claude/skills" ".agents/skills" ".claw/skills"))

(defstruct skill name description location source)

;;; ------------------------------------------------------------ frontmatter

(defun %frontmatter-block (content)
  "Return (values frontmatter-lines body-start) or NIL without frontmatter."
  (when (fx.util:string-prefix-p "---" content)
    (let ((lines (fx.util:split-lines content)))
      (when (string= (string-trim " " (first lines)) "---")
        (loop for line in (rest lines)
              for index from 1
              when (string= (string-trim " " line) "---")
                return (values (subseq lines 1 index) (1+ index))
              finally (return nil))))))

(defun %indent (line)
  (loop for ch across line
        while (char= ch #\Space)
        count 1))

(defun %parse-scalar (value rest-lines)
  "Parse a YAML scalar VALUE; block scalars consume REST-LINES.
Returns (values text consumed-count)."
  (let ((trimmed (string-trim " " value)))
    (cond
      ((member trimmed '("|" "|-" ">" ">-") :test #'string=)
       (let ((block-lines '()) (consumed 0))
         (dolist (line rest-lines)
           (if (or (zerop (length (string-trim " " line)))
                   (plusp (%indent line)))
               (progn (push (string-trim " " line) block-lines)
                      (incf consumed))
               (return)))
         (values (format nil (if (fx.util:string-prefix-p "|" trimmed)
                                 "~{~a~^~%~}" "~{~a~^ ~}")
                         (nreverse block-lines))
                 consumed)))
      ((and (>= (length trimmed) 2)
            (member (char trimmed 0) '(#\" #\'))
            (char= (char trimmed 0) (char trimmed (1- (length trimmed)))))
       (values (subseq trimmed 1 (1- (length trimmed))) 0))
      (t (values trimmed 0)))))

(defun parse-skill-file (content)
  "Parse SKILL.md CONTENT. Returns (values name description) or NIL."
  (multiple-value-bind (lines) (%frontmatter-block content)
    (unless lines (return-from parse-skill-file nil))
    (let ((name nil) (description nil))
      (loop with remaining = lines
            while remaining
            do (let* ((line (pop remaining))
                      (colon (position #\: line)))
                 (when (and colon (zerop (%indent line)))
                   (let ((key (string-trim " " (subseq line 0 colon))))
                     (when (member key '("name" "description") :test #'string=)
                       (multiple-value-bind (value consumed)
                           (%parse-scalar (subseq line (1+ colon)) remaining)
                         (setf remaining (nthcdr consumed remaining))
                         (cond ((string= key "name") (setf name value))
                               (t (setf description value)))))))))
      (when (and name (plusp (length name))
                 (<= (length name) +max-name-bytes+)
                 (every (lambda (ch) (and (graphic-char-p ch)
                                          (not (char= ch #\/))))
                       name))
        (values name
                (let ((description (or description "")))
                  (if (> (length description) +max-description-bytes+)
                      (subseq description 0 +max-description-bytes+)
                      description)))))))

(defun validate-managed-name (name)
  "Port of validateManagedSkillName."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) +max-name-bytes+)
       (not (member name '("." "..") :test #'string=))
       (not (char= (char name 0) #\/))
       (not (find #\/ name))
       (not (find #\\ name))))

(defun resolve-metadata (content fallback-name)
  "Port of resolveMetadata: frontmatter wins; a file without frontmatter is
valid under FALLBACK-NAME; invalid frontmatter is rejected.
Returns (values name description) or NIL."
  (cond
    ((not (fx.util:string-prefix-p "---" content))
     (when (validate-managed-name fallback-name)
       (values fallback-name "")))
    (t (parse-skill-file content))))

;;; -------------------------------------------------------------- discovery

(defun %scan-root (root source found order)
  "Add skills under ROOT (a directory of skill directories) to FOUND."
  (let ((root-dir (probe-file (concatenate 'string root "/"))))
    (when root-dir
      (dolist (entry (ignore-errors (directory (merge-pathnames "*/" root-dir)
                                               :resolve-symlinks nil)))
        (let* ((skill-md (merge-pathnames "SKILL.md" entry))
               (content (and (probe-file skill-md)
                             (ignore-errors
                              (fx.util:read-file-string skill-md)))))
          (when content
            (multiple-value-bind (name description) (resolve-metadata
                                                     content
                                                     (car (last (pathname-directory entry))))
              (when (and name (not (gethash name found)))
                (setf (gethash name found)
                      (make-skill :name name
                                  :description description
                                  :location (namestring entry)
                                  :source source))
                (vector-push-extend name order)))))))))

(defun discover-skills (&key (workspace (namestring *default-pathname-defaults*))
                             (include-global t))
  "Scan all roots; earlier roots win on name conflicts."
  (let ((found (make-hash-table :test #'equal))
        (order (make-array 0 :adjustable t :fill-pointer t)))
    (dolist (root +workspace-roots+)
      (%scan-root (fx.util:expand-path root workspace) "workspace" found order))
    (when include-global
      (dolist (root +global-roots+)
        (%scan-root (fx.util:expand-path (concatenate 'string "~/" root))
                    "global" found order)))
    (loop for name across order collect (gethash name found))))

(defvar *catalog-cache* nil)

(defun skills (&key reload)
  (when (or reload (null *catalog-cache*))
    (setf *catalog-cache* (discover-skills)))
  *catalog-cache*)

(defun find-skill (name)
  (find name (skills) :key #'skill-name :test #'string=))

;;; ---------------------------------------------------------------- catalog

(defun static-context ()
  "The skills section appended to the static context, or NIL when empty.
Bounded by the skill_catalog_bytes limit; omitted entries are counted."
  (let ((entries (skills)))
    (when entries
      (let ((out (make-string-output-stream))
            (bytes 0)
            (omitted 0))
        (format out "# Skills~%~%")
        (format out "Installed skills are listed below. Load one with the skill tool only when the user explicitly invokes it or the task clearly matches it.~%")
        (dolist (skill entries)
          (let ((line (format nil "- ~a — ~a (location: ~a)"
                              (skill-name skill)
                              (fx.compaction:compact-line
                               (skill-description skill) 200)
                              (skill-location skill))))
            (if (> (+ bytes (length line) 1) +max-catalog-bytes+)
                (incf omitted)
                (progn (write-line line out)
                       (incf bytes (1+ (length line)))))))
        (when (plusp omitted)
          (format out "(skill catalog omitted ~d entr~:@p)~%" omitted))
        (string-right-trim '(#\Newline) (get-output-stream-string out))))))

;;; --------------------------------------------------------------- the tool

(defun %resource-path (skill resource)
  (let ((resource (or resource "SKILL.md")))
    (when (or (fx.util:string-prefix-p "/" resource)
              (fx.util:string-prefix-p "~" resource)
              (search ".." resource))
      (error 'fx.tools:tool-error
             :message "resource must be a relative path inside the skill"))
    (fx.util:expand-path resource (skill-location skill))))

(defun read-skill-chunk (skill &key resource (offset 0))
  "Bounded chunked read of a skill resource, with a next_offset trailer."
  (let ((path (%resource-path skill resource)))
    (unless (probe-file path)
      (error 'fx.tools:tool-error
             :message (format nil "skill resource not found: ~a"
                              (or resource "SKILL.md"))))
    (let* ((content (fx.util:read-file-string path))
           (content (if (> (length content) +max-file-bytes+)
                        (subseq content 0 +max-file-bytes+)
                        content))
           (offset (min (max 0 offset) (length content)))
           (end (min (length content) (+ offset +max-chunk-bytes+)))
           (chunk (subseq content offset end)))
      (if (< end (length content))
          (format nil "~a~%~%(truncated; next_offset: ~d of ~d bytes)"
                  chunk end (length content))
          chunk))))

(fx.tools::define-tool "skill"
    (:description "Read an installed skill or one of its relative text resources in bounded chunks. Pass the exact advertised location when one is listed, then use next_offset to continue. When to use: the user explicitly invokes a listed skill or the task clearly matches one. When NOT to use: generic exploration, ordinary file edits, guessing from vague words, or installing a missing skill."
     :schema (fx.tools::schema
              :properties
              `(("name" "type" "string" "description"
                 "The name of the skill from the available skills list.")
                ("location" "type" "string" "description"
                 "The exact advertised location of the selected skill.")
                ("resource" "type" "string" "description"
                 "Optional relative text resource within the selected skill. Defaults to SKILL.md.")
                ("offset" "type" "integer" "description"
                 "Optional UTF-8 byte offset. Use the returned next_offset to continue."))
              :required '("name"))
     :requires-approval nil
     :action-label "Loading skill")
    (args)
  (let* ((name (or (fx.json:jget args "name")
                   (error 'fx.tools:tool-error :message "skill requires a name")))
         (skill (or (find-skill name)
                    (error 'fx.tools:tool-error
                           :message (format nil "unknown skill: ~a (reload with /skills)" name))))
         (location (fx.json:jget args "location")))
    (when (and location
               (not (equal (string-right-trim "/" location)
                           (string-right-trim "/" (skill-location skill)))))
      (error 'fx.tools:tool-error
             :message (format nil "stale location for skill ~a; advertised location is ~a"
                              name (skill-location skill))))
    (read-skill-chunk skill
                      :resource (fx.json:jget args "resource")
                      :offset (or (fx.json:jget args "offset") 0))))

;;; ---------------------------------------------------------------- install
;;; Port of builtins/skills.zig installFromSource and friends. Sources:
;;; a GitHub owner/repo, a full git URL, a local path, a skills.sh URL,
;;; an owner/repo@skill spec, or a pasted "npx skills add ..." command.

(defun managed-skills-dir ()
  (namestring (merge-pathnames "skills/" (fx.util:fx-dir))))

(defun %tokens (input)
  (remove "" (fx.util:split-lines
              (substitute #\Newline #\Space
                          (substitute #\Newline #\Tab input)))
          :test #'string=))

(defun looks-like-install-command-p (input)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) input)))
    (and (or (fx.util:string-prefix-p "npx " trimmed)
             (fx.util:string-prefix-p "bunx " trimmed))
         (search "skills add" trimmed))))

(defun %parse-npx-source (input)
  "Extract (values source filter) from a pasted npx/bunx skills add command."
  (when (looks-like-install-command-p input)
    (let ((source nil) (filter nil) (tokens (%tokens input)))
      (loop while tokens
            do (let ((token (pop tokens)))
                 (cond
                   ((member token '("npx" "bunx" "skills" "add" "-g" "-y" "--yes")
                            :test #'string=))
                   ((string= token "--skill")
                    (setf filter (pop tokens)))
                   ((fx.util:string-prefix-p "--skill=" token)
                    (setf filter (subseq token 8)))
                   ((fx.util:string-prefix-p "-" token))
                   ((null source) (setf source token)))))
      (when source (values source filter)))))

(defun %parse-skills-sh (input)
  "skills.sh/<owner>/<repo>[/<skill>] -> (values \"owner/repo\" skill)."
  (let ((marker (search "skills.sh/" input)))
    (when marker
      (let* ((rest (subseq input (+ marker (length "skills.sh/"))))
             (parts (remove "" (fx.util:split-lines
                                (substitute #\Newline #\/ rest))
                            :test #'string=)))
        (when (>= (length parts) 2)
          (values (format nil "~a/~a" (first parts) (second parts))
                  (third parts)))))))

(defun %parse-repo-skill (input)
  "owner/repo@skill -> (values \"owner/repo\" \"skill\")."
  (unless (or (fx.util:string-prefix-p "http://" input)
              (fx.util:string-prefix-p "https://" input)
              (fx.util:string-prefix-p "git@" input)
              (fx.util:string-prefix-p "/" input)
              (fx.util:string-prefix-p "./" input)
              (fx.util:string-prefix-p "../" input)
              (fx.util:string-prefix-p "~/" input))
    (let ((at (position #\@ input :from-end t)))
      (when (and at (< (1+ at) (length input)))
        (let ((slash (position #\/ input :from-end t :end at)))
          (when (and slash (plusp slash))
            (values (subseq input 0 at) (subseq input (1+ at)))))))))

(defun normalize-install-source (raw &optional explicit-filter)
  "Port of normalizeInstallRequest. Returns (values source filter)."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) raw)))
    (when (zerop (length trimmed))
      (error 'fx.tools:tool-error :message "install source must not be empty"))
    (multiple-value-bind (npx-source npx-filter) (%parse-npx-source trimmed)
      (let ((source (or npx-source trimmed)))
        (multiple-value-bind (sh-source sh-filter) (%parse-skills-sh source)
          (multiple-value-bind (repo-source repo-filter)
              (unless sh-source (%parse-repo-skill source))
            (values (or sh-source repo-source source)
                    (find-if (lambda (f) (and f (plusp (length f))))
                             (list npx-filter sh-filter repo-filter
                                   explicit-filter)))))))))

(defun %clone-url (source)
  "Port of cloneUrlForSource."
  (if (or (fx.util:string-prefix-p "http" source)
          (fx.util:string-prefix-p "file://" source)
          (fx.util:string-prefix-p "git@" source))
      source
      (format nil "https://github.com/~a.git" source)))

(defun %repo-name (url)
  (let* ((name (let ((slash (position #\/ url :from-end t)))
                 (if slash (subseq url (1+ slash)) url))))
    (if (fx.util:string-suffix-p ".git" name)
        (subseq name 0 (- (length name) 4))
        name)))

(defun %copy-skill-dir (source-dir skills-dir name)
  (let ((destination (fx.util:expand-path name skills-dir)))
    (ensure-directories-exist (concatenate 'string destination "/"))
    (let* ((out (make-string-output-stream))
           (process (sb-ext:run-program
                     "cp" (list "-R"
                                (concatenate 'string
                                             (string-right-trim "/" source-dir)
                                             "/.")
                                destination)
                     :search t :output out :error out :wait t)))
      (unless (zerop (sb-ext:process-exit-code process))
        (error 'fx.tools:tool-error
               :message (format nil "could not copy skill into ~a: ~a"
                                destination
                                (get-output-stream-string out)))))))

(defun %skill-md-content (dir)
  (let ((path (fx.util:expand-path "SKILL.md" dir)))
    (when (probe-file path)
      (ignore-errors (fx.util:read-file-string path)))))

(defun %filter-matches-p (filter metadata-name dir-name)
  (or (null filter)
      (equal filter metadata-name)
      (equal filter dir-name)))

(defun install-from-directory (skills-dir source-dir fallback-name filter)
  "Port of installFromDirectory: root SKILL.md installs the whole tree
under FALLBACK-NAME; nested SKILL.md directories install individually.
Returns the installed metadata names."
  (ensure-directories-exist skills-dir)
  (let ((installed '()))
    (let ((root-content (%skill-md-content source-dir)))
      (when root-content
        (multiple-value-bind (name)
            (resolve-metadata root-content fallback-name)
          (when (and name
                     (validate-managed-name fallback-name)
                     (%filter-matches-p filter name fallback-name))
            (%copy-skill-dir source-dir skills-dir fallback-name)
            (push name installed)))))
    (fx.tools::walk-files
     source-dir
     (lambda (rel)
       (let ((segments (remove "" (fx.util:split-lines
                                   (substitute #\Newline #\/ rel))
                               :test #'string=)))
         (when (and (>= (length segments) 2)
                    (equal (car (last segments)) "SKILL.md")
                    (notany (lambda (segment)
                              (fx.util:string-prefix-p "." segment))
                            segments))
           (let* ((dir-name (nth (- (length segments) 2) segments))
                  (parent-rel (format nil "~{~a~^/~}" (butlast segments)))
                  (parent-dir (fx.util:expand-path parent-rel source-dir))
                  (content (%skill-md-content parent-dir)))
             (when (and content (validate-managed-name dir-name))
               (multiple-value-bind (name) (resolve-metadata content dir-name)
                 (when (and name (%filter-matches-p filter name dir-name))
                   (%copy-skill-dir parent-dir skills-dir dir-name)
                   (push name installed)))))))))
    (nreverse installed)))

(defun install-from-source (raw-source &key filter
                                            (skills-dir (managed-skills-dir)))
  "Port of installFromSource: local directory first, then git clone."
  (multiple-value-bind (source merged-filter)
      (normalize-install-source raw-source filter)
    (let ((local (let ((expanded (fx.util:expand-path source)))
                   (and (probe-file (concatenate 'string
                                                 (string-right-trim "/" expanded)
                                                 "/"))
                        expanded))))
      (if local
          (prog1 (install-from-directory
                  skills-dir local
                  (car (last (remove "" (fx.util:split-lines
                                         (substitute #\Newline #\/
                                                     (string-right-trim "/" local)))
                                     :test #'string=)))
                  merged-filter)
            (skills :reload t))
          (let ((tmp (format nil "/tmp/fx-skill-install-~a" (fx.util:random-id 6))))
            (unwind-protect
                 (let* ((out (make-string-output-stream))
                        (process (sb-ext:run-program
                                  "git" (list "clone" "--depth" "1"
                                              (%clone-url source) tmp)
                                  :search t :output out :error out :wait t)))
                   (unless (zerop (sb-ext:process-exit-code process))
                     (error 'fx.tools:tool-error
                            :message (format nil "git clone failed for ~a: ~a"
                                             (%clone-url source)
                                             (string-trim '(#\Space #\Newline)
                                                          (get-output-stream-string out)))))
                   (prog1 (install-from-directory skills-dir tmp
                                                  (%repo-name source)
                                                  merged-filter)
                     (skills :reload t)))
              (sb-ext:run-program "rm" (list "-rf" tmp) :search t :wait t)))))))

(defun remove-managed-skill (name)
  "Port of removeSkill: only names inside the managed install root."
  (unless (validate-managed-name name)
    (error 'fx.tools:tool-error :message "Invalid skill name. Use a single directory name without '/' or '\\\\'."))
  (let ((path (fx.util:expand-path name (managed-skills-dir))))
    (unless (probe-file (concatenate 'string path "/"))
      (error 'fx.tools:tool-error
             :message (format nil "Skill '~a' not found in ~a" name
                              (managed-skills-dir))))
    (sb-ext:run-program "rm" (list "-rf" path) :search t :wait t)
    (skills :reload t)
    name))

(defun create-skill-template (name)
  "Port of createSkillTemplate; returns the created SKILL.md path."
  (unless (validate-managed-name name)
    (error 'fx.tools:tool-error :message "Invalid skill name. Use a single directory name without '/' or '\\\\'."))
  (let ((path (fx.util:expand-path (format nil "~a/SKILL.md" name)
                                   (managed-skills-dir))))
    (fx.util:write-file-string
     path
     (format nil "---~%name: ~a~%description: Describe when this skill should activate~%---~%~%# ~a~%~%Instructions for this skill...~%"
             name name))
    (skills :reload t)
    path))

(fx.tools::define-tool "install_skill"
    (:description "Install a reusable skill from a supported source into fx managed skill storage. When to use: the user asks to install a skill or pastes a skills install command. When NOT to use: no installation is required, install packages, fetch unrelated repos, or modify project code."
     :schema (fx.tools::schema
              :properties
              `(("source" "type" "string" "description"
                 "GitHub repo, local path, skills.sh URL, owner/repo@skill spec, or a pasted npx skills add ... command.")
                ("skill" "type" "string" "description"
                 "Optional skill name filter for multi-skill repos."))
              :required '("source"))
     :requires-approval t
     :action-label "Installing skill")
    (args)
  (let ((installed (install-from-source
                    (or (fx.json:jget args "source")
                        (error 'fx.tools:tool-error
                               :message "install_skill requires a source"))
                    :filter (fx.json:jget args "skill"))))
    (if installed
        (format nil "installed: ~{~a~^, ~}" installed)
        (if (fx.json:jget args "skill")
            (format nil "Skill '~a' not found in the repository."
                    (fx.json:jget args "skill"))
            "No skills found (no SKILL.md files)."))))
