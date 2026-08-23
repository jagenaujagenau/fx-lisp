;;;; run-tests.lisp — offline tests for the fx Common Lisp port.

(defpackage #:fx.tests
  (:use #:cl)
  (:export #:run))

(in-package #:fx.tests)

(defvar *failures* 0)
(defvar *checks* 0)

(defmacro check (name form)
  `(progn
     (incf *checks*)
     (handler-case
         (if ,form
             (format t "ok   ~a~%" ,name)
             (progn (incf *failures*)
                    (format t "FAIL ~a~%" ,name)))
       (error (e)
         (incf *failures*)
         (format t "FAIL ~a (error: ~a)~%" ,name e)))))

(defun temp-dir ()
  (let ((dir (format nil "/tmp/fx-lisp-tests-~a/" (fx.util:random-id 4))))
    (ensure-directories-exist dir)
    dir))

(defun run ()
  (let ((*failures* 0) (*checks* 0))
    ;; ---- JSON codec
    (check "json roundtrip object"
           (let* ((decoded (fx.json:decode "{\"a\":[1,2.5,\"x\"],\"b\":true,\"c\":null,\"d\":false}")))
             (and (equal (fx.json:jget decoded "a") (list 1 2.5d0 "x"))
                  (eq (gethash "b" decoded) t)
                  (eq (gethash "c" decoded) :null)
                  (eq (gethash "d" decoded) :false))))
    (check "json string escapes"
           (string= (fx.json:decode "\"a\\n\\\"b\\u00e9\\ud83d\\ude00\"")
                    (format nil "a~%\"bé😀")))
    (check "json encode escapes"
           (string= (fx.json:encode-to-string (format nil "x~%y\"z"))
                    "\"x\\ny\\\"z\""))
    (check "json encode reparse"
           (let* ((obj (fx.json:make-jobject "k" (list 1 "two" t :null)))
                  (again (fx.json:decode (fx.json:encode-to-string obj))))
             (equal (fx.json:jget again "k") (list 1 "two" t :null))))
    ;; ---- glob matching
    (check "glob star" (fx.tools::glob-match-p "*.md" "README.md"))
    (check "glob nested basename" (fx.tools::glob-match-p "*.md" "docs/a.md"))
    (check "glob doublestar" (fx.tools::glob-match-p "src/**/*.lisp" "src/a/b/c.lisp"))
    (check "glob doublestar zero" (fx.tools::glob-match-p "src/**/*.lisp" "src/x.lisp"))
    (check "glob no match" (not (fx.tools::glob-match-p "src/*.zig" "src/a/b.zig")))
    (check "glob question" (fx.tools::glob-match-p "a?.txt" "ab.txt"))
    ;; ---- filesystem tools
    (let ((dir (temp-dir)))
      (check "write_file"
             (fx.util:string-prefix-p "wrote"
              (fx.tools:execute-tool "write_file"
               (fx.json:make-jobject "path" (format nil "~aa.txt" dir)
                                     "content" (format nil "one~%two~%three~%")))))
      (check "read_file range"
             (search "two"
                     (fx.tools:execute-tool "read_file"
                      (fx.json:make-jobject "path" (format nil "~aa.txt" dir)
                                            "start_line" 2 "line_count" 1))))
      (check "edit_file"
             (progn
               (fx.tools:execute-tool "edit_file"
                (fx.json:make-jobject "path" (format nil "~aa.txt" dir)
                                      "old_string" "two" "new_string" "TWO"))
               (search "TWO" (fx.util:read-file-string (format nil "~aa.txt" dir)))))
      (check "edit_file rejects ambiguous"
             (handler-case
                 (progn (fx.tools:execute-tool "edit_file"
                         (fx.json:make-jobject "path" (format nil "~aa.txt" dir)
                                               "old_string" "e" "new_string" "E"))
                        nil)
               (fx.tools:tool-error () t)))
      (check "list_files"
             (search "a.txt"
                     (fx.tools:execute-tool "list_files"
                      (fx.json:make-jobject "path" dir))))
      (check "glob_files count"
             (string= "1 matching path"
                      (fx.tools:execute-tool "glob_files"
                       (fx.json:make-jobject "pattern" "*.txt" "path" dir
                                             "mode" "count"))))
      (check "grep_files matches"
             (search "a.txt:1:one"
                     (fx.tools:execute-tool "grep_files"
                      (fx.json:make-jobject "pattern" "one" "path" dir))))
      (check "grep_files case_insensitive"
             (search "a.txt"
                     (fx.tools:execute-tool "grep_files"
                      (fx.json:make-jobject "pattern" "TWO" "path" dir
                                            "case_insensitive" t
                                            "mode" "files_with_matches"))))
      (check "file_info"
             (search "file," (fx.tools:execute-tool "file_info"
                              (fx.json:make-jobject "path" (format nil "~aa.txt" dir)))))
      (check "copy+rename+delete"
             (progn
               (fx.tools:execute-tool "copy_file"
                (fx.json:make-jobject "path" (format nil "~aa.txt" dir)
                                      "new_path" (format nil "~ab.txt" dir)))
               (fx.tools:execute-tool "rename_file"
                (fx.json:make-jobject "path" (format nil "~ab.txt" dir)
                                      "new_path" (format nil "~ac.txt" dir)))
               (fx.tools:execute-tool "delete_file"
                (fx.json:make-jobject "path" (format nil "~ac.txt" dir)))
               (not (probe-file (format nil "~ac.txt" dir)))))
      (check "create_folder"
             (progn
               (fx.tools:execute-tool "create_folder"
                (fx.json:make-jobject "path" (format nil "~asub/deep" dir)))
               (and (probe-file (format nil "~asub/deep/" dir)) t))))
    ;; ---- terminal tool
    (check "terminal exec"
           (let ((result (fx.tools:execute-tool "terminal"
                          (fx.json:make-jobject "action" "exec"
                                                "command" "echo hello; exit 3"))))
             (and (fx.util:string-prefix-p "exit code: 3" result)
                  (search "hello" result))))
    ;; ---- prompt
    (check "system prompt matches zig sections"
           (let ((p (fx.prompt:gateway-system-prompt)))
             (and (search "You are fx, a local coding CLI assistant with tool access." p)
                  (search "# Workspace behavior" p)
                  (search "# Source routing" p)
                  (search "# Interaction" p)
                  (search "# Safety" p)
                  (search "# Tools and verification" p))))
    ;; ---- gateway stream parsing (offline, via internal accumulator)
    (check "tool call delta merge"
           (let ((accs (make-hash-table)))
             (fx.gateway::%merge-tool-call-delta
              accs (fx.json:decode "{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"pa\"}}"))
             (fx.gateway::%merge-tool-call-delta
              accs (fx.json:decode "{\"index\":0,\"function\":{\"arguments\":\"th\\\":\\\"x\\\"}\"}}"))
             (let ((calls (fx.gateway::%finish-tool-calls accs)))
               (and (= 1 (length calls))
                    (equal (getf (first calls) :id) "c1")
                    (equal (getf (first calls) :name) "read_file")
                    (equal (getf (first calls) :arguments) "{\"path\":\"x\"}")))))
    ;; ---- tool definitions shape
    (check "gateway tool definitions"
           (let* ((defs (fx.tools:gateway-tool-definitions))
                  (names (mapcar (lambda (d) (fx.json:jget* d "function" "name")) defs)))
             (and (member "read_file" names :test #'string=)
                  (member "terminal" names :test #'string=)
                  (member "grep_files" names :test #'string=)
                  (let ((json (fx.json:encode-to-string (first defs))))
                    (search "\"parameters\"" json)))))
    ;; ---- permissions
    (let ((rules (fx.permissions:parse-rules
                  (fx.json:decode "{\"bash\":{\"git status\":\"allow\",\"git push*\":\"deny\",\"ls *\":\"allow\"},\"edit\":{\"*\":\"ask\"},\"read\":\"allow\"}"))))
      (check "rule allow exact bash"
             (eq :allow (fx.permissions:evaluate-rules rules "bash" "git status")))
      (check "rule deny wildcard bash"
             (eq :deny (fx.permissions:evaluate-rules rules "bash" "git push origin main")))
      (check "bash allow wildcard requires static command"
             (and (eq :allow (fx.permissions:evaluate-rules rules "bash" "ls -la"))
                  (null (fx.permissions:evaluate-rules rules "bash" "ls ; rm -rf /"))))
      (check "rule ask edit"
             (eq :ask (fx.permissions:evaluate-rules rules "edit" "src/main.zig")))
      (check "rule none unmatched"
             (null (fx.permissions:evaluate-rules rules "glob" "x")))
      (check "read allow-all"
             (eq :allow (fx.permissions:evaluate-rules rules "read" "/anything")))
      (let ((engine (fx.permissions:make-engine :mode :ask :rules rules
                                                :workspace "/ws/proj/")))
        (check "deny rule beats yolo"
               (progn (setf (fx.permissions:engine-mode engine) :yolo)
                      (prog1 (eq :deny (fx.permissions:decide engine "terminal"
                                                              "git push origin" t))
                        (setf (fx.permissions:engine-mode engine) :ask))))
        (check "ask mode asks for unmatched mutation"
               (eq :ask (fx.permissions:decide engine "write_file" "/ws/proj/a" t)))
        (check "explicit ask rule outranks auto mode"
               (progn (setf (fx.permissions:engine-mode engine) :auto)
                      (eq :ask (fx.permissions:decide engine "write_file"
                                                      "/ws/proj/a.txt" t))))
        (check "auto mode allows unruled workspace mutation"
               (eq :allow (fx.permissions:decide engine "create_folder"
                                                 "/ws/proj/newdir" t)))
        (check "auto mode still asks for external path"
               (eq :ask (fx.permissions:decide engine "create_folder" "/etc/newdir" t)))
        (check "auto mode still asks for bash"
               (eq :ask (fx.permissions:decide engine "terminal" "make install" t)))
        (check "always grant covers workspace tree"
               (progn (fx.permissions:add-grant engine "write_file" "/ws/proj/a.txt")
                      (eq :allow (fx.permissions:decide engine "edit_file"
                                                        "/ws/proj/deep/b.txt" t))))
        (check "bash grant is exact"
               (progn (fx.permissions:add-grant engine "terminal" "make install")
                      (and (eq :allow (fx.permissions:decide engine "terminal"
                                                             "make install" t))
                           (eq :ask (fx.permissions:decide engine "terminal"
                                                           "make installer" t)))))
        (check "web_fetch grant is per-domain"
               (progn (fx.permissions:add-grant engine "web_fetch" "example.com")
                      (and (eq :allow (fx.permissions:decide engine "web_fetch"
                                                             "example.com" t))
                           (eq :ask (fx.permissions:decide engine "web_fetch"
                                                           "evil.com" t)))))))
    (check "permission names collapse"
           (and (equal "edit" (fx.permissions:permission-name-for-tool "write_file"))
                (equal "bash" (fx.permissions:permission-name-for-tool "terminal"))
                (equal "read" (fx.permissions:permission-name-for-tool "read_file"))))
    (check "url domain extraction"
           (and (equal "example.com"
                       (fx.permissions:url-domain "https://Example.com:8080/x?y=1"))
                (null (fx.permissions:url-domain "https://user@example.com/"))))
    ;; ---- web_fetch policy + html reduction (offline)
    (check "web_fetch rejects bad urls"
           (flet ((rejected-p (url)
                    (handler-case
                        (progn (fx.tools::%validate-url url) nil)
                      (fx.tools:tool-error () t))))
             (and (rejected-p "ftp://example.com/x")
                  (rejected-p "https://user:pw@example.com/")
                  (rejected-p "http://localhost/admin")
                  (rejected-p "http://127.0.0.1:8080/")
                  (rejected-p "http://192.168.1.5/")
                  (rejected-p "http://10.0.0.1/")
                  (not (rejected-p "https://example.com/page")))))
    (check "html to markdown"
           (let ((md (fx.tools::html-to-markdown
                      "<html><head><style>x{}</style><script>bad()</script></head><body><h1>Title</h1><p>Hello &amp; welcome.</p><ul><li>one</li><li>two</li></ul><a href=\"https://x.y\">link</a></body></html>")))
             (and (search "# Title" md)
                  (search "Hello & welcome." md)
                  (search "- one" md)
                  (search "[link](https://x.y)" md)
                  (not (search "bad()" md))
                  (not (search "<p>" md)))))
    ;; ---- compaction
    (flet ((user (text) (fx.json:make-jobject "role" "user" "content" text))
           (assistant (text) (fx.json:make-jobject "role" "assistant" "content" text))
           (tool (text) (fx.json:make-jobject "role" "tool" "content" text
                                              "tool_call_id" "c")))
      (let ((messages
              (loop for i from 1 to 10
                    append (list (user (format nil "request ~d" i))
                                 (tool (format nil "exit code: 0~%result ~d" i))
                                 (assistant (format nil "outcome ~d" i))))))
        (check "turn grouping"
               (multiple-value-bind (summary turns)
                   (fx.compaction:group-turns messages)
                 (and (null summary) (= 10 (length turns))
                      (= 3 (length (first turns))))))
        (check "no compaction under limit"
               (multiple-value-bind (result compacted-p)
                   (fx.compaction:compact-messages (subseq messages 0 12)
                                                   :max-turns 8)
                 (declare (ignore result))
                 (not compacted-p)))
        (multiple-value-bind (compacted compacted-p removed)
            (fx.compaction:compact-messages messages :max-turns 8)
          (check "compaction over limit"
                 (and compacted-p (= removed 6)))
          (check "summary head carries fx continuation format"
                 (let ((head (fx.json:jget (first compacted) "content")))
                   (and (fx.compaction:summary-message-p (first compacted))
                        (search "Conversation summary:" head)
                        (search "- Earlier turns compacted: 6" head)
                        (search "- Recent user requests:" head)
                        (search "request 1" head)
                        (search "- Assistant outcomes:" head)
                        (search "- Tool execution evidence:" head)
                        (search "Recent conversation turns are preserved verbatim." head)
                        (search "Resume directly." head))))
          (check "recent turns preserved verbatim"
                 (multiple-value-bind (summary turns)
                     (fx.compaction:group-turns compacted)
                   (and summary (= 4 (length turns))
                        (equal "request 7" (fx.json:jget (first (first turns))
                                                         "content"))
                        (equal "outcome 10"
                               (fx.json:jget (third (first (last turns)))
                                             "content")))))
          (check "recompaction accumulates counts"
                 (multiple-value-bind (again again-p)
                     (fx.compaction:compact-messages
                      (append compacted
                              (loop for i from 11 to 20
                                    append (list (user (format nil "request ~d" i))
                                                 (assistant (format nil "outcome ~d" i)))))
                      :max-turns 8)
                   (and again-p
                        (search "- Earlier turns compacted: 16"
                                (fx.json:jget (first again) "content"))
                        (search "- Previously compacted context:"
                                (fx.json:jget (first again) "content")))))
          (check "forced compaction keeps one turn"
                 (multiple-value-bind (forced forced-p)
                     (fx.compaction:compact-messages compacted :force t)
                   (and forced-p
                        (multiple-value-bind (summary turns)
                            (fx.compaction:group-turns forced)
                          (and summary (= 1 (length turns))))))))
        (check "summary respects size caps"
               (let* ((big (loop for i from 1 to 60
                                 append (list (user (make-string 500
                                                                 :initial-element #\x))
                                              (assistant "ok"))))
                      (head (fx.json:jget
                             (first (fx.compaction:compact-messages big :max-turns 5))
                             "content"))
                      (summary-start (search "Conversation summary:" head)))
                 (and summary-start
                      (< (- (length head) summary-start) 2000)
                      (<= (length (fx.util:split-lines head)) 40))))
        (check "compact-line collapses whitespace"
               (equal "a b c" (fx.compaction:compact-line
                               (concatenate 'string "  a" (string #\Newline)
                                            (string #\Newline) "b"
                                            (string #\Tab) "c  ")
                               50)))))
    ;; ---- subagent (offline validation paths)
    (flet ((command (json) (fx.json:jget (fx.json:decode json) "command"))
           (rejected-p (thunk)
             (handler-case (progn (funcall thunk) nil)
               (fx.tools:tool-error () t))))
      (fx.subagent:reset-children)
      (check "subagent tool registered"
             (and (fx.tools:find-tool "subagent")
                  (not (fx.tools:tool-spec-requires-approval
                        (fx.tools:find-tool "subagent")))))
      (check "subagent requires one branch"
             (and (rejected-p (lambda () (fx.subagent:handle-command
                                          (command "{\"command\":{}}"))))
                  (rejected-p (lambda () (fx.subagent:handle-command
                                          (command "{\"command\":{\"create\":{\"name\":\"a\",\"mode\":\"one_off\",\"prompt\":\"x\"},\"lifecycle\":{\"id\":\"z\",\"action\":\"cancel\"}}}"))))))
      (check "one_off requires prompt"
             (rejected-p (lambda () (fx.subagent:handle-command
                                     (command "{\"command\":{\"create\":{\"name\":\"a\",\"mode\":\"one_off\"}}}")))))
      (check "unported branches rejected"
             (and (rejected-p (lambda () (fx.subagent:handle-command
                                          (command "{\"command\":{\"relationship\":{\"action\":\"attach\",\"id\":\"x\"}}}"))))
                  (rejected-p (lambda () (fx.subagent:handle-command
                                          (command "{\"command\":{\"configure\":{\"id\":\"x\"}}}"))))))
      (check "inspect unknown id rejected"
             (rejected-p (lambda () (fx.subagent:handle-command
                                     (command "{\"command\":{\"inspect\":{\"id\":\"nope\",\"sections\":[\"status\"]}}}")))))
      (check "create without credential rejected"
             (let ((fx.agent:*api-key* nil))
               (rejected-p (lambda () (fx.subagent:handle-command
                                       (command "{\"command\":{\"create\":{\"name\":\"a\",\"mode\":\"one_off\",\"prompt\":\"x\"}}}")))))))
    (check "child permission mode capped at caller"
           (let ((parent (fx.permissions:make-engine :mode :auto)))
             (let ((fx.agent:*engine* parent))
               (and (eq :auto (fx.subagent::%capped-permission-mode "yolo" :auto))
                    (eq :ask (fx.subagent::%capped-permission-mode "ask" :auto))
                    (eq :auto (fx.subagent::%capped-permission-mode nil :auto))))))
    ;; ---- skills
    (check "frontmatter inline + quoted"
           (multiple-value-bind (name description)
               (fx.skills:parse-skill-file
                (format nil "---~%name: review~%description: \"Reviews code, carefully.\"~%---~%body here"))
             (and (equal name "review")
                  (equal description "Reviews code, carefully."))))
    (check "frontmatter folded block scalar"
           (multiple-value-bind (name description)
               (fx.skills:parse-skill-file
                (format nil "---~%name: deploy~%description: >-~%  line one~%  line two~%---~%body"))
             (and (equal name "deploy")
                  (equal description "line one line two"))))
    (check "frontmatter literal block scalar"
           (multiple-value-bind (name description)
               (fx.skills:parse-skill-file
                (format nil "---~%name: n~%description: |~%  a~%  b~%---~%"))
             (and (equal name "n") (equal description (format nil "a~%b")))))
    (check "missing frontmatter rejected"
           (null (fx.skills:parse-skill-file "just a plain file")))
    (check "missing name rejected"
           (null (fx.skills:parse-skill-file
                  (format nil "---~%description: x~%---~%"))))
    (let ((workspace (temp-dir)))
      (flet ((add-skill (root name description)
               (fx.util:write-file-string
                (format nil "~a~a/~a/SKILL.md" workspace root name)
                (format nil "---~%name: ~a~%description: ~a~%---~%~%# ~a instructions~%~a"
                        name description name
                        (make-string 30000 :initial-element #\x)))))
        (add-skill ".fx/skills" "review" "workspace fx wins")
        (add-skill "skills" "review" "shared root loses")
        (add-skill "skills" "deploy" "deploys things")
        (let ((skills (fx.skills:discover-skills :workspace workspace
                                                 :include-global nil)))
          (check "discovery finds both skills" (= 2 (length skills)))
          (check "workspace .fx root wins name conflicts"
                 (let ((review (find "review" skills :key #'fx.skills:skill-name
                                                    :test #'string=)))
                   (and review
                        (equal "workspace fx wins"
                               (fx.skills:skill-description review))
                        (search ".fx/skills" (fx.skills:skill-location review)))))
          (let ((fx.skills::*catalog-cache* skills))
            (check "catalog advertises skills"
                   (let ((catalog (fx.skills:static-context)))
                     (and (search "# Skills" catalog)
                          (search "review — workspace fx wins" catalog)
                          (search "deploy" catalog)
                          (search "location:" catalog))))
            (check "skill tool chunks with next_offset"
                   (let ((first-chunk (fx.tools:execute-tool "skill"
                                       (fx.json:make-jobject "name" "deploy"))))
                     (and (search "deploy instructions" first-chunk)
                          (search "next_offset: 20480" first-chunk)
                          (let ((rest-chunk (fx.tools:execute-tool "skill"
                                             (fx.json:make-jobject
                                              "name" "deploy"
                                              "offset" 20480))))
                            (and (plusp (length rest-chunk))
                                 (not (search "next_offset" rest-chunk)))))))
            (check "skill tool rejects traversal"
                   (handler-case
                       (progn (fx.tools:execute-tool "skill"
                               (fx.json:make-jobject "name" "deploy"
                                                     "resource" "../../etc/passwd"))
                              nil)
                     (fx.tools:tool-error () t)))
            (check "skill tool rejects stale location"
                   (handler-case
                       (progn (fx.tools:execute-tool "skill"
                               (fx.json:make-jobject "name" "deploy"
                                                     "location" "/somewhere/else"))
                              nil)
                     (fx.tools:tool-error () t)))
            (check "skill tool rejects unknown skill"
                   (handler-case
                       (progn (fx.tools:execute-tool "skill"
                               (fx.json:make-jobject "name" "nope"))
                              nil)
                     (fx.tools:tool-error () t)))))))
    (check "empty catalog omitted from context"
           (null (fx.skills:discover-skills :workspace (temp-dir)
                                            :include-global nil)))
    ;; ---- web_search (offline)
    (check "web_search markdown escaping"
           (and (equal "a\\[b\\] c" (fx.websearch:escape-markdown-title
                                     (format nil "a[b]~%c")))
                (equal "https://x/%28y%29" (fx.websearch:escape-markdown-url
                                            "https://x/(y)"))))
    (check "web_search output format"
           (let ((output (fx.websearch:format-output
                          "zig release" "Commentary."
                          '(("Zig News" "https://ziglang.org/news"))
                          "perplexity/sonar")))
             (and (fx.util:string-prefix-p "Web search results for query: zig release"
                                           output)
                  (search "untrusted reference material" output)
                  (search "Commentary." output)
                  (search "Search results from perplexity/sonar:" output)
                  (search "- [Zig News](https://ziglang.org/news)" output)
                  (fx.util:string-suffix-p "markdown hyperlinks." output))))
    (check "web_search rejects conflicting domain filters"
           (handler-case
               (progn (fx.tools:execute-tool "web_search"
                       (fx.json:decode "{\"query\":\"x y\",\"allowed_domains\":[\"a.com\"],\"blocked_domains\":[\"b.com\"]}"))
                      nil)
             (fx.tools:tool-error (e)
               (search "only one non-empty domain filter"
                       (princ-to-string e)))))
    (check "web_search requires query"
           (handler-case
               (progn (fx.tools:execute-tool "web_search"
                       (fx.json:decode "{\"query\":\"x\"}"))
                      nil)
             (fx.tools:tool-error () t)))
    (check "web_search domain filter mapping"
           (and (equal '("a.com") (fx.websearch::%domain-filter '("a.com") nil))
                (equal '("-b.com" "-c.com")
                       (fx.websearch::%domain-filter nil '("b.com" "c.com")))))
    ;; ---- mcp (offline)
    (check "mcp config parsing"
           (let ((configs (fx.mcp:parse-config
                           (fx.json:decode "{\"mcp\":{\"alpha\":{\"type\":\"local\",\"command\":[\"node\",\"s.js\"]},\"beta\":{\"type\":\"stdio\",\"command\":\"python3\",\"args\":[\"m.py\"],\"enabled\":false},\"remote\":{\"type\":\"sse\",\"url\":\"https://x\"}}}"))))
             (and (= 2 (length configs))
                  (let ((alpha (find "alpha" configs
                                     :key #'fx.mcp:server-config-name
                                     :test #'string=))
                        (beta (find "beta" configs
                                    :key #'fx.mcp:server-config-name
                                    :test #'string=)))
                    (and (equal "node" (fx.mcp:server-config-command alpha))
                         (equal '("s.js") (fx.mcp:server-config-args alpha))
                         (fx.mcp:server-config-enabled alpha)
                         (equal "python3" (fx.mcp:server-config-command beta))
                         (equal '("m.py") (fx.mcp:server-config-args beta))
                         (not (fx.mcp:server-config-enabled beta)))))))
    (check "mcp tools registered"
           (and (fx.tools:find-tool "mcp_search_tools")
                (fx.tools:find-tool "mcp_select_tool")
                (fx.tools:find-tool "mcp_features")))
    (check "mcp dynamic name formatting"
           (equal "mcp_alpha_echo" (fx.mcp:dynamic-tool-name "alpha" "echo")))
    ;; ---- modes
    (check "mode registry matches fx builtins"
           (and (fx.modes:lookup "code") (fx.modes:lookup "ask")
                (equal "ask" fx.modes:+default-mode-id+)
                (eq :auto (fx.modes:mode-spec-permission-mode
                           (fx.modes:lookup "code")))
                (eq :ask (fx.modes:mode-spec-permission-mode
                          (fx.modes:lookup "ask")))
                (eq :full (fx.modes:mode-spec-tool-policy
                           (fx.modes:lookup "code")))
                (null (fx.modes:lookup "unknown"))))
    (check "read-only projection filters mutating tools"
           (let* ((all (fx.tools:gateway-tool-definitions))
                  (plan (fx.modes:project-tools all "plan"))
                  (names (mapcar (lambda (d) (fx.json:jget* d "function" "name"))
                                 plan)))
             (and (< (length plan) (length all))
                  (member "read_file" names :test #'string=)
                  (member "grep_files" names :test #'string=)
                  (not (member "write_file" names :test #'string=))
                  (not (member "terminal" names :test #'string=))
                  (equal all (fx.modes:project-tools all "code")))))
    (check "mode tool gate and denial"
           (and (fx.modes:tool-allowed-p "code" "terminal")
                (fx.modes:tool-allowed-p "plan" "read_file")
                (not (fx.modes:tool-allowed-p "plan" "terminal"))
                (fx.modes:tool-allowed-p "unknown-mode" "terminal")
                (search "not available in Plan mode"
                        (fx.modes:denial-message "plan" "terminal"))))
    ;; ---- hooks
    (fx.hooks:clear-hooks)
    (check "pre-tool-use rewrite chains then block short-circuits"
           (progn
             (fx.hooks:register-pre-tool-use
              "rewrite" (lambda (input)
                          (let ((args (getf input :arguments)))
                            (list :rewrite-arguments
                                  (fx.json:jput args "path" "rewritten.txt")))))
             (fx.hooks:register-pre-tool-use
              "block-secrets" (lambda (input)
                                (if (search "secret"
                                            (or (fx.json:jget
                                                 (getf input :arguments) "path")
                                                ""))
                                    (list :block "secrets are off limits")
                                    :continue)))
             (multiple-value-bind (outcome payload)
                 (fx.hooks:run-pre-tool-use
                  "read_file" (fx.json:make-jobject "path" "a.txt") 1)
               (and (eq outcome :rewritten)
                    (equal "rewritten.txt" (fx.json:jget payload "path"))))))
    (check "pre-tool-use block wins"
           (progn
             (fx.hooks:clear-hooks)
             (fx.hooks:register-pre-tool-use
              "block" (lambda (input) (declare (ignore input))
                        (list :block "nope")))
             (multiple-value-bind (outcome payload)
                 (fx.hooks:run-pre-tool-use
                  "read_file" (fx.json:make-jobject) 1)
               (and (eq outcome :blocked) (equal "nope" payload)))))
    (check "handler errors are treated as continue"
           (progn
             (fx.hooks:clear-hooks)
             (fx.hooks:register-pre-tool-use
              "crashy" (lambda (input) (declare (ignore input))
                         (error "boom")))
             (eq :unchanged (fx.hooks:run-pre-tool-use
                             "read_file" (fx.json:make-jobject) 1))))
    (check "stop hook continue-once honors can-continue"
           (progn
             (fx.hooks:clear-hooks)
             (fx.hooks:register-stop
              "verify" (lambda (input) (declare (ignore input))
                         (list :continue-once "run the tests before finishing")))
             (multiple-value-bind (action context)
                 (fx.hooks:run-stop "done" 1 t)
               (and (eq action :continue-once)
                    (equal "run the tests before finishing" context)
                    (eq :allow (fx.hooks:run-stop "done" 2 nil))))))
    (check "no stop hooks allows completion"
           (progn (fx.hooks:clear-hooks)
                  (eq :allow (fx.hooks:run-stop "done" 1 t))))
    ;; ---- install_skill
    (check "npx command source parsing"
           (multiple-value-bind (source filter)
               (fx.skills:normalize-install-source
                "npx -y skills add vercel-labs/agent-skills --skill frontend-design")
             (and (equal source "vercel-labs/agent-skills")
                  (equal filter "frontend-design"))))
    (check "bunx --skill= parsing"
           (multiple-value-bind (source filter)
               (fx.skills:normalize-install-source
                "bunx skills add owner/repo --skill=deploy -g")
             (and (equal source "owner/repo") (equal filter "deploy"))))
    (check "skills.sh url parsing"
           (multiple-value-bind (source filter)
               (fx.skills:normalize-install-source
                "https://skills.sh/vercel-labs/agent-skills/code-review")
             (and (equal source "vercel-labs/agent-skills")
                  (equal filter "code-review"))))
    (check "owner/repo@skill parsing"
           (multiple-value-bind (source filter)
               (fx.skills:normalize-install-source "acme/toolbox@linter")
             (and (equal source "acme/toolbox") (equal filter "linter"))))
    (check "plain sources pass through"
           (and (equal "acme/toolbox" (fx.skills:normalize-install-source
                                       "acme/toolbox"))
                (equal "/tmp/x" (fx.skills:normalize-install-source "/tmp/x"))
                (equal "git@github.com:a/b.git"
                       (fx.skills:normalize-install-source
                        "git@github.com:a/b.git"))))
    (check "managed name validation"
           (and (fx.skills:validate-managed-name "review")
                (not (fx.skills:validate-managed-name "../evil"))
                (not (fx.skills:validate-managed-name "a/b"))
                (not (fx.skills:validate-managed-name "."))
                (not (fx.skills:validate-managed-name ""))))
    (let ((repo (temp-dir))
          (managed (concatenate 'string (temp-dir) "managed/")))
      (fx.util:write-file-string
       (format nil "~askills/alpha/SKILL.md" repo)
       (format nil "---~%name: alpha~%description: first~%---~%body a"))
      (fx.util:write-file-string
       (format nil "~askills/alpha/extra.txt" repo) "asset")
      (fx.util:write-file-string
       (format nil "~askills/beta/SKILL.md" repo)
       (format nil "---~%name: beta~%description: second~%---~%body b"))
      (fx.util:write-file-string
       (format nil "~askills/.hidden/SKILL.md" repo)
       (format nil "---~%name: hidden~%---~%x"))
      (check "local multi-skill install"
             (let ((installed (fx.skills:install-from-directory
                               managed repo "repo" nil)))
               (and (equal '("alpha" "beta") (sort installed #'string<))
                    (probe-file (format nil "~aalpha/SKILL.md" managed))
                    (probe-file (format nil "~aalpha/extra.txt" managed))
                    (probe-file (format nil "~abeta/SKILL.md" managed))
                    (not (probe-file (format nil "~a.hidden/" managed))))))
      (check "install filter narrows"
             (let* ((managed2 (concatenate 'string (temp-dir) "m2/"))
                    (installed (fx.skills:install-from-directory
                                managed2 repo "repo" "beta")))
               (and (equal '("beta") installed)
                    (not (probe-file (format nil "~aalpha/" managed2))))))
      (check "root SKILL.md installs whole tree under repo name"
             (let ((root-repo (temp-dir))
                   (managed3 (concatenate 'string (temp-dir) "m3/")))
               (fx.util:write-file-string
                (format nil "~aSKILL.md" root-repo)
                (format nil "---~%name: solo~%description: root~%---~%x"))
               (let ((installed (fx.skills:install-from-directory
                                 managed3 root-repo "toolbox" nil)))
                 (and (equal '("solo") installed)
                      (probe-file (format nil "~atoolbox/SKILL.md" managed3)))))))
    (check "no-frontmatter SKILL.md resolves under directory name"
           (multiple-value-bind (name description)
               (fx.skills:resolve-metadata "# Just a body" "fallback-dir")
             (and (equal name "fallback-dir") (equal description ""))))
    ;; ---- semantic_search
    (check "keyword splitting drops stop words and short words"
           (equal '("retry" "backoff" "policy" "work" "HTTP")
                  (fx.tools::split-search-keywords
                   "how does the retry backoff policy work in HTTP? a b")))
    (let ((dir (temp-dir)))
      (fx.util:write-file-string
       (format nil "~asrc/retry_policy.txt" dir)
       (format nil "the retry loop applies backoff~%unrelated line~%retry retry backoff again~%"))
      (fx.util:write-file-string
       (format nil "~asrc/other.txt" dir)
       (format nil "nothing relevant here~%one retry mention~%"))
      (fx.util:write-file-string
       (format nil "~anode_modules/dep.txt" dir)
       (format nil "retry backoff retry backoff~%"))
      (let ((output (fx.tools:execute-tool "semantic_search"
                     (fx.json:make-jobject "query" "retry backoff strategy"
                                           "path" dir))))
        (check "semantic search ranks and formats"
               (and (fx.util:string-prefix-p "[search] 2 results for: retry backoff strategy"
                                             output)
                    (let ((first-hit (position #\Newline output)))
                      (fx.util:string-prefix-p "src/retry_policy.txt:"
                                               (subseq output (1+ first-hit))))
                    (search "src/other.txt:2: one retry mention" output)))
        (check "semantic search skips ignored dirs"
               (not (search "node_modules" output))))
      (check "semantic search single file and sample line"
             (let ((output (fx.tools:execute-tool "semantic_search"
                            (fx.json:make-jobject
                             "query" "retry backoff"
                             "path" (format nil "~asrc/retry_policy.txt" dir)))))
               (and (search "[search] 1 result" output)
                    (search ":1: the retry loop applies backoff" output))))
      (check "semantic search no results"
             (fx.util:string-prefix-p "[search] no results for: zzqx"
              (fx.tools:execute-tool "semantic_search"
               (fx.json:make-jobject "query" "zzqx qqzz" "path" dir)))))
    (check "semantic_search is read-only for plan mode"
           (fx.modes:tool-allowed-p "plan" "semantic_search"))
    ;; ---- usage ledger
    (check "usage ledger accumulates per model"
           (progn
             (fx.gateway:reset-usage)
             (fx.gateway:record-usage "m/a" (fx.json:decode "{\"prompt_tokens\":100,\"completion_tokens\":20}"))
             (fx.gateway:record-usage "m/a" (fx.json:decode "{\"prompt_tokens\":50,\"completion_tokens\":5}"))
             (fx.gateway:record-usage "m/b" (fx.json:decode "{\"prompt_tokens\":7,\"completion_tokens\":3}"))
             (equal '(("m/a" 2 150 25) ("m/b" 1 7 3))
                    (fx.gateway:usage-summary))))
    (check "usage ledger ignores missing usage"
           (progn (fx.gateway:reset-usage)
                  (fx.gateway:record-usage "m/a" nil)
                  (null (fx.gateway:usage-summary))))
    ;; ---- ask_user_question
    (check "ask_user_question fails without interactive hook"
           (let ((fx.tools:*ask-user-hook* nil))
             (handler-case
                 (progn (fx.tools:execute-tool "ask_user_question"
                         (fx.json:decode "{\"questions\":[{\"question\":\"Which db?\",\"options\":[{\"label\":\"sqlite\"},{\"label\":\"postgres\"}]}]}"))
                        nil)
               (fx.tools:tool-error (e)
                 (search "no interactive user" (princ-to-string e))))))
    (check "ask_user_question returns chosen answers"
           (let ((fx.tools:*ask-user-hook*
                   (lambda (questions)
                     (mapcar (lambda (q) (first (first (second q))))
                             questions))))
             (let ((result (fx.json:decode
                            (fx.tools:execute-tool "ask_user_question"
                             (fx.json:decode "{\"questions\":[{\"question\":\"Which db?\",\"options\":[{\"label\":\"sqlite\",\"description\":\"file-based\"},{\"label\":\"postgres\"}]}]}")))))
               (let ((answer (first (fx.json:jget result "answers"))))
                 (and (equal "Which db?" (fx.json:jget answer "question"))
                      (equal "sqlite" (fx.json:jget answer "answer")))))))
    (check "ask_user_question validates options"
           (let ((fx.tools:*ask-user-hook* (lambda (q) (declare (ignore q)) '("x"))))
             (handler-case
                 (progn (fx.tools:execute-tool "ask_user_question"
                         (fx.json:decode "{\"questions\":[{\"question\":\"?\",\"options\":[{\"label\":\"only-one\"}]}]}"))
                        nil)
               (fx.tools:tool-error () t))))
    ;; ---- session resume
    (check "session resume preserves tool_calls"
           (let ((session (fx.session:make-new-session)))
             (fx.session:append-event
              session (fx.json:make-jobject
                       "kind" "message"
                       "message" (fx.json:decode "{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{}\"}}]}")))
             (fx.session:append-event
              session (fx.json:make-jobject
                       "kind" "usage" "model" "m" "prompt_tokens" 5))
             (let ((messages (fx.session:session-messages
                              (fx.session:load-session
                               (fx.session:session-id session)))))
               (and (= 1 (length messages))
                    (equal "read_file"
                           (fx.json:jget*
                            (first (fx.json:jget (first messages) "tool_calls"))
                            "function" "name"))))))
    ;; ---- session store
    (check "session roundtrip"
           (let* ((session (fx.session:make-new-session)))
             (fx.session:append-event
              session (fx.json:make-jobject
                       "kind" "message"
                       "message" (fx.json:make-jobject "role" "user" "content" "hi")))
             (let ((messages (fx.session:session-messages
                              (fx.session:load-session (fx.session:session-id session)))))
               (and (= 1 (length messages))
                    (equal (fx.json:jget (first messages) "content") "hi")
                    (equal (fx.session:latest-session-id)
                           (fx.session:session-id session))))))
    (format t "~%~d check~:p, ~d failure~:p~%" *checks* *failures*)
    (when (plusp *failures*) (sb-ext:exit :code 1))
    t))
