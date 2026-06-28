
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lantool.scm
;; DESCRIPTION : interface with language tool
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc lantool)
  (version version-compare)
  (version spell-edit))

(tm-define lantool-server "http://localhost:8081/v2/check")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check LanguageTool support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lantool-support :uninit)

(tm-define (supports-lantool?)
  (if (!= lantool-support :uninit) lantool-support
      (let* ((cmd (string-append "curl -X POST " lantool-server
                                 " -d \"language=en-US\""
                                 " -d \"text=Some text with a error.\""))
             (val (eval-system cmd)))
        (set! lantool-support (string-occurs? "{\"software\":{" val))
        lantool-support)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking a chunk of text with LanguageTool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lantool-reply s)
  (with pos (string-search-forwards "{\"software\":" 0 s)
    (and (>= pos 0)
         (substring s pos (string-length s)))))

(define ((lantool-return html tmp return) output)
  (system-remove tmp)
  (return html (lantool-reply output)))

(tm-define (lantool-process* t return)
  (let* ((html (compress-html t 1))
         (tmp (url-glue (url-temp) ".html"))
         (dummy (string-save html tmp))
         (lan (string-replace (language-to-locale spell-language) "_" "-"))
         ;;(cmd (string-append "languagetool"
         ;;                    " --json "
         ;;                    " -d WHITESPACE_RULE"
         ;;                    " " (url->string tmp)))
         (cmd (string-append "curl -X POST " lantool-server
                             " -d \"language=" lan "\""
                             " -d \"contentType=text/html\""
                             " -d \"disabledRules=UPPERCASE_SENTENCE_START\""
                             " --data-urlencode \"text=$(cat "
                             (url->string tmp)
                             ")\" 2> /dev/null"))
         )
    ;;(display* "cmd] " cmd "\n")
    (async-eval-system cmd (lantool-return html tmp return))))

(tm-define (lantool-process t return)
  (spell-initiate (get-env "language"))
  (lantool-process* t return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lantool-test html out)
  (when out
    (display* "--- input ---\n")
    (display* html "\n")
    (display* "--- lantool ---\n")
    (display* out "\n")
    (with html* (lantool-correct html out)
      (display* "--- output ---\n")
      (display* html* "\n")
      (display* "--- result ---\n")
      (display* (decompress-html html* 1) "\n"))))

(tm-define (lantool-finalize html out)
  (when out
    (with html* (lantool-correct html out)
      (tree-set (buffer-tree) (decompress-html html* 1)))
    (spell-go-to-first*)))

(tm-define (lantool-finalize* html out)
  (when out
    (with html* (lantool-correct html out)
      (clipboard-cut "dummy")
      (insert (decompress-html html* 1)))
    (spell-go-to-first*)))

(tm-define ((lantool-rechecked old-t* old-p) html out)
  (when out
    (with-innermost t spell-context?
      (and-let* ((new-p (tree->path t))
                 (new-t* (tm->stree t)))
        (when (and (== new-p old-p) (== new-t* old-t*))
          (let* ((html* (lantool-correct html out))
                 (repl (decompress-html html* 1))
                 (ok "The new version contains no errors."))
            (when (not (tm-func? repl 'spell-error))
              (set! repl `(spell-error ,repl ,(translate ok))))
            (tree-remove! t 1 (- (tm-arity t) 1))
            (tree-insert! t 1 (cdr (tm-children repl)))
            (refresh-tooltips)))))))

(tm-define (lantool-recheck)
  (with-innermost t spell-context?
    (and-let* ((w (tm-ref t 0))
               (p (tree->path t))
               (t* (tm->stree t)))
      (lantool-process* w (lantool-rechecked t* p)))))

(kbd-map
  ("C-F13" (lantool-process (buffer-tree) lantool-test))
  ("C-F14" (lantool-process (selection-tree) lantool-finalize*))
  ("C-F15" (lantool-process (buffer-tree) lantool-finalize)))
