
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : spell-lantool.scm
;; DESCRIPTION : interface with LanguageTool
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tools spell spell-lantool)
  (version version-compare)
  (tools spell spell-edit))

(tm-define lantool-server "")

(define (notify-languagetool-server-preferences var val)
  (set! lantool-server (string-append val "/v2/check")))

(define-preferences
  ("languagetool server" "http://localhost:8081"
   notify-languagetool-server-preferences))

(set! lantool-server
      (string-append (get-preference "languagetool server") "/v2/check"))

(tm-define lantool-api-key
   (get-preference "languagetool API key"))

(define (notify-languagetool-api-key-preferences var val)
  (set! lantool-api-key val))

(define-preferences
  ("languagetool API key" "" notify-languagetool-server-preferences))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check LanguageTool support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lantool-support :uninit)

(tm-define (supports-lantool?)
  (and (get-boolean-preference "grammar checking")  
       (if (!= lantool-support :uninit) lantool-support
           (with val
	       (http-post-query lantool-server
		'("Content-Type" "application/x-www-form-urlencoded")
		'("language" "en-US"
		  "text" "Some text with a error."))
             (set! lantool-support (string-occurs? "{\"software\":{" val))
             lantool-support))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking a chunk of text with LanguageTool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lantool-cache (make-ahash-table))

(define (lantool-reply s)
  (with pos (string-search-forwards "{\"software\":" 0 s)
    (and (>= pos 0)
         (substring s pos (string-length s)))))

(define ((lantool-return lan html return) output)
  ;;(display* "---------------------------------------\n")
  ;;(display* output "\n")
  (with r (lantool-reply output)
    (ahash-set! lantool-cache (list lan html) r)
    (return html r)))

(tm-define (lantool-process* t return)
  (let* ((html (compress-html t 1))
         (lan (tree-get-env t "language"))
         (r (ahash-ref lantool-cache (list lan html))))
    ;;(display* "cached? " (if r "yes" "no") "\n")
    (when (not (process-running? 'spell)) (set! r html))
    (if r (return html r)
        (let* ((html (compress-html t 1))
               (loc (language-to-locale lan))
               (loc* (string-replace loc "_" "-"))
               (disable "UPPERCASE_SENTENCE_START"))
          (async-http-post-query lantool-server
	    '("Content-Type" "application/x-www-form-urlencoded")
	    (list "language" loc*
		  "contentType" "text/html"
		  "disabledRules" disable
		  "text" html) ;; TODO << add API key
	    (lantool-return lan html return))))))

(tm-define (lantool-process-old t return)
  (spell-initiate)
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
           (when (and (== st st*)
                      (nnull? (tm-search new-t spell-context?)))
             (tree-set! t new-t)
             (tree-correct-upwards t)
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

(tm-define (make-lantool-process)
  (spell-initiate)
  (make-process lantool-process-one 'lantool 'spell))

(tm-define (lantool-check)
  ((make-lantool-process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous rechecking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(kbd-map
  ("C-F15" (lantool-process-old (buffer-tree) lantool-test))
  ("C-F16" (lantool-check)))
