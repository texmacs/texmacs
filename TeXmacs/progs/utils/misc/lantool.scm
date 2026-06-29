
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lantool.scm
;; DESCRIPTION : interface with LanguageTool
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

(define lantool-cache (make-ahash-table))

(define (lantool-reply s)
  (with pos (string-search-forwards "{\"software\":" 0 s)
    (and (>= pos 0)
         (substring s pos (string-length s)))))

(define ((lantool-return html tmp return) output)
  (with r (lantool-reply output)
    (system-remove tmp)
    (ahash-set! lantool-cache (list spell-language html) r)
    (return html r)))

(tm-define (lantool-process* t return)
  (let* ((html (compress-html t 1))
         (r (ahash-ref lantool-cache (list spell-language html))))
    ;;(display* "cached? " (if r "yes" "no") "\n")
    (when (not spell-language) (set! r html))
    (if r (return html r)
        (let* ((html (compress-html t 1))
               (tmp (url-glue (url-temp) ".html"))
               (dummy (string-save html tmp))
               (loc (language-to-locale spell-language))
               (lan (string-replace loc "_" "-"))
               (disable "UPPERCASE_SENTENCE_START")
               ;;(cmd (string-append "languagetool"
               ;;                    " --json "
               ;;                    " -d WHITESPACE_RULE"
               ;;                    " " (url->string tmp)))
               (cmd (string-append "curl -X POST " lantool-server
                                   " -d \"language=" lan "\""
                                   " -d \"contentType=text/html\""
                                   " -d \"disabledRules=" disable "\""
                                   " --data-urlencode \"text=$(cat "
                                   (url->string tmp)
                                   ")\" 2> /dev/null"))
               )
          ;;(display* "cmd] " cmd "\n")
          (async-eval-system cmd (lantool-return html tmp return))))))

(tm-define (lantool-process t return)
  (spell-initiate (get-env "language"))
  (lantool-process* t return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LanguageTool processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ((lantool-processed-one serial tp st next) html out)
  (cond ((not (process-active? serial)) (noop))
        ((tm-equal? html out) (noop))
        ((tm? out)
         (let* ((t (tree-pointer->tree tp))
                (st* (tm->stree t))
                (html* (lantool-correct html out))
                (new-t (decompress-html html* 1)))
           (when (== st st*)
             (if (or (tm-func? t 'document) (tm-func? new-t 'document))
                 (begin
                   (tree-select t)
                   (clipboard-cut "dummy")
                   (insert new-t))
                 (tree-set! t new-t))
             (when (not (tree-innermost spell-context?))
               (with l (tree-search t spell-context?)
                 (when (nnull? l)
                   (tree-go-to (car l) 0 :start)
                   (delayed
                     (:idle 100)
                     (recenter-window)
                     (refresh-tooltips))))))))
        (else (set-message "No running LanguageTool server" "Spell check")))
  (tree-pointer-detach tp)
  (next))

(tm-define (lantool-process-one serial tp st next)
  (lantool-process* (tree-pointer->tree tp)
                    (lantool-processed-one serial tp st next)))

(tm-define (lantool-process-document t)
  (spell-initiate (get-env "language"))
  (with end (make-process-end 'lantool 'spell)
    ((make-process-document process-serial lantool-process-one t end))))

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
            (when (null? (tm-search repl spell-context?))
              (set! repl `(spell-error ,repl ,(translate ok))))
            (when (tm-func? repl 'spell-error)
              (tree-remove! t 1 (- (tm-arity t) 1))
              (tree-insert! t 1 (cdr (tm-children repl)))
              (refresh-tooltips))))))))

(tm-define (lantool-recheck)
  (with-innermost t spell-context?
    (and-let* ((w (tm-ref t 0))
               (p (tree->path t))
               (t* (tm->stree t)))
      (lantool-process* w (lantool-rechecked t* p)))))

(kbd-map
  ("C-F13" (lantool-process (buffer-tree) lantool-test))
  ("C-F14" (lantool-process (selection-tree) lantool-finalize*))
  ("C-F15" (lantool-process (buffer-tree) lantool-finalize))
  ("C-F16" (lantool-process-document (buffer-tree))))
