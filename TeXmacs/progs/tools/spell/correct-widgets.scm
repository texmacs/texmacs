
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : correct-widgets.scm
;; DESCRIPTION : widgets for grammar correction
;; COPYRIGHT   : (C) 2026  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tools spell correct-widgets)
  (:use (tools spell spell-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer for the manual correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (correct-buffer)
  (string->url "tmfs://aux/correct"))

(tm-define (inside-correct-buffer?)
  (== (current-buffer) (correct-buffer)))

(define (focus-on-master-buffer)
  (buffer-focus* (buffer-get-master (correct-buffer))))

(tm-define (focus-on-correct-editor)
  ;(display* "focus-on-correct-editor\n")
  (if toolbar-correct-active?
      (begin
	(focus-on-master-buffer)
	(when (qt-gui?)
	  (set! toolbar-correct-active? #f)
	  (update-bottom-tools)
	  (set! toolbar-correct-active? #t)
	  (update-bottom-tools))
	(update-menus)
	(delayed (:idle 50) (keyboard-focus-on "spell-correct")))
      (buffer-focus* (correct-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explanation and suggestions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define correct-explanation "")

(define correct-suggestions (list))

(define correct-source "")

(define (clear-widget)
  (set! correct-source "")
  (buffer-set-body (correct-buffer) `(document ,correct-source))
  (set! correct-explanation "")
  (set! correct-suggestions (list))
  (if toolbar-correct-active?
      (begin
	(update-bottom-tools)
	(update-menus))
      (begin
	(refresh-now "correct-explanation")
	(refresh-now "correct-suggestions"))))

(tm-define (update-widget)
  ;(display* "update-widget\n")
  (clear-widget)
  (when (inside-spell?)
    (with t (tree-innermost 'spell-error)
      (when (tree-is? t 'spell-error)
	  (with l (tree-children t)
	    (set! correct-source (tree->stree (car l)))
	    (buffer-set-body (correct-buffer)
			     `(document ,correct-source))
	    (set! correct-explanation (tree->string (cadr l)))
	    (set! correct-suggestions (map tree->string (cddr l)))
	    (if toolbar-correct-active?
		(update-menus)
		(begin
		  (refresh-now "correct-explanation")
		  (refresh-now "correct-suggestions"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main widget for corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (explain-document)
  `(with "bg-color" "#eee" 
     (tabular
      (tformat (cwith "1" "1" "1" "1" "cell-width" "390px")
	       (cwith "1" "1" "1" "1" "cell-hmode" "exact")
	       (cwith "1" "1" "1" "1" "cell-hyphen" "t")
	       (table (row (cell (document ,correct-explanation))))))))

(define (prefix-suggestions i l)
  (if (null? l) l
      (cons (string-append (number->string i) ": " (car l))
            (prefix-suggestions (+ i 1) (cdr l)))))

(define (corrected-source)
  (with doc (tree->stree (buffer->tree (correct-buffer)))
    (if (and (tm-func? doc 'document) (pair? (cdr doc)))
      (cadr doc) doc)))

(tm-define (correct-begin)
  (let* ((u (current-buffer))
	 (aux (correct-buffer)))
    (buffer-set-master aux u)
    (when (not (buffer-exists? aux))
      (buffer-set-body aux `(document "")))
    (spell-register-hook update-widget)))

(tm-define (correct-end)
  (focus-on-master-buffer)
  (spell-cancel-hook update-widget))

(define (correct-replace)
  (focus-on-master-buffer)
  (spell-replace (corrected-source) focus-on-correct-editor))

(define (correct-retain)
  (focus-on-master-buffer)
  (if (== (corrected-source) correct-source) 
      (spell-retain-permanent focus-on-correct-editor)
      (spell-replace (corrected-source) focus-on-correct-editor)))

(tm-define (correct-document)
  (if (buffer-exists? (correct-buffer))
      (buffer->tree (correct-buffer))
      `(document "")))

(tm-define (correct-follow-suggestion i mode)
  (focus-on-master-buffer)
  (cond ((== i "") (noop))
        ((string? i)
         (with nr (string->number (substring i 0 1))
           (correct-follow-suggestion nr mode)))
        (else
          (when (and (>= i 0) (<= i (length correct-suggestions)))
            (spell-retain i mode focus-on-correct-editor)))))

(tm-widget ((correct-widget u style init aux) quit)
  (padded
    (hlist
      (vlist
        (refreshable "correct-explanation"
          (resize "400px" "45px"
            (texmacs-output (explain-document) `(style "generic"))))
        (glue #t #t 0 10)
        (resize "400px" "75px"
          (texmacs-input `(with ,@init ,(correct-document))
                         `(style (tuple ,@style)) aux))
        (glue #t #t 0 0)
        (explicit-buttons
          (aligned
            (meti (hlist // (text "Accept during this pass"))
              ("Tab" (correct-replace)))
            (meti (hlist // (text "Permanently insert into dictionary"))
              (" + " (correct-retain)))))
        (glue #t #t 0 0)
        ===
        (hlist
          >>>
          ((balloon (icon "tm_search_first.xpm") "First error")
           (focus-on-master-buffer)
	   (spell-go-to-first* focus-on-correct-editor))
          ((balloon (icon "tm_search_previous.xpm") "Previous error")
           (focus-on-master-buffer)
	   (spell-go-to-previous)
	   (focus-on-correct-editor))
          ((balloon (icon "tm_search_next.xpm") "Next error")
           (focus-on-master-buffer)
	   (spell-go-to-next* focus-on-correct-editor))
          ((balloon (icon "tm_search_last.xpm") "Last error")
           (focus-on-master-buffer)
	   (spell-go-to-last)
	   (focus-on-correct-editor))
          /// ///
          ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
           (set-boolean-preference "toolbar correct" #t)
	   (focus-on-master-buffer)
	   (open-correct-toolbar)
	   (quit))
          ((balloon (icon "tm_close_tool.xpm") "Close spell tool")
	   (correct-end)
	   (spell-terminate)
           (quit))))
      /// ///
      (resize "200px" "225px"
        (refreshable "correct-suggestions"
          (choice (begin (focus-on-master-buffer)
			 (correct-follow-suggestion answer #f))
                  (prefix-suggestions 1 correct-suggestions)
                  ""))))))

(define (get-main-attrs getter)
  (list "mode" (getter "mode")
        "language" (getter "language")
        "math-language" (getter "math-language")
        "prog-language" (getter "prog-language")
        "par-first" "0tab"))

(tm-define (open-correct-widget)
  (when (not (inside-correct-buffer?))
    (let* ((u (current-buffer))
           (st (embedded-style-list))
           (init (get-main-attrs get-env))
           (aux (correct-buffer)))
      (correct-begin)
      (focus-on-master-buffer)
      (spell-go-to-first)
      (dialogue-window (correct-widget u st init aux)
		       noop "Grammar" aux))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized keyboard shortcuts in correct mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (keyboard-press key time)
  (:require (inside-correct-buffer?))
  (cond ((and (string>=? key "1") (string<=? key "9"))
         (correct-follow-suggestion key #f))
        ((== key "tab") (correct-replace))
        ((== key "C-tab") (correct-replace))
        ((== key "A-tab") (correct-replace))
        ((== key "M-tab") (correct-replace))
        ((== key "+") (correct-retain))
        (else (former key time))))

(tm-define (kbd-enter t shift?)
  (:require (inside-correct-buffer?))
  (with doc (tree->stree (buffer-tree))
    (when (and (tm-func? doc 'document) (pair? (cdr doc)))
      (set! doc (cadr doc)))
      (focus-on-master-buffer)
      (spell-replace doc)))
 
(tm-define (kbd-incremental t forwards?)
  (:require (inside-correct-buffer?))
  (if forwards?
      (spell-go-to-next)
      (spell-go-to-previous)))

(tm-define (traverse-incremental t forwards?)
  (:require (or (inside-spell?) (inside-correct-buffer?)))
  (if forwards?
      (spell-go-to-next)
      (spell-go-to-previous)))

(tm-define (traverse-extremal t forwards?)
  (:require (or (inside-spell?) (inside-correct-buffer?)))
  (if forwards?
      (spell-go-to-last)
      (spell-go-to-first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toolbar widget for corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-toolbar-keypress what)
  (with key (and (pair? what) (cadr what))
    (if (pair? what) (set! what (car what)))
    (buffer-set-body (correct-buffer) `(document ,what))
    (cond ((== key "home")
	   (focus-on-master-buffer)
	   (spell-go-to-first* focus-on-correct-editor)
	   (focus-on-correct-editor))
          ((== key "end")
	   (focus-on-master-buffer)
	   (spell-go-to-last)
	   (focus-on-correct-editor))
          ((or (== key "up") (== key "pageup"))
	   (focus-on-master-buffer)
	   (spell-go-to-previous)
	   (focus-on-correct-editor))
          ((or (== key "down") (== key "pagedown"))
	   (focus-on-master-buffer)
	   (spell-go-to-next* focus-on-correct-editor)
	   (focus-on-correct-editor))
          ((== key "escape") (correct-end) (exit-toolbar))
          ((== key "tab") (correct-replace))
          ((== key "C-tab") (correct-replace))
          ((== key "A-tab") (correct-replace))
          ((== key "M-tab") (correct-replace))
          ((== key "+") (correct-retain))
          ((== key "return") (correct-replace))
          ((in? key (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (correct-follow-suggestion i #f)))))

(define (exit-toolbar)
  (set! toolbar-correct-active? #f)
  (update-bottom-tools)
  (when toolbar-db-active?
    (db-show-toolbar))
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)))

(tm-widget (correct-toolbar)
  (glue #f #f 0 1)
  (vlist
    (text (string-append "  " correct-explanation))
    (glue #t #t 0 0)
    (hlist
      //
      ((balloon (icon "tm_right.xpm") "Accept during this pass")
       (correct-replace))
      ((balloon (icon "tm_add.xpm") "Permanently add to dictionary")
       (correct-retain))
      ///
      (text "Correct: ") //
      (input (spell-toolbar-keypress answer) "spell-correct"
	     (list (convert correct-source "texmacs-stree" "verbatim-snippet"))
	     "15em")
      (assuming (nnull? correct-suggestions)
	(minibar
	  (for (i (.. 0 (length correct-suggestions)))
	    ///
	    (with text (string-append (number->string (+ i 1)) ": "
				      (list-ref correct-suggestions i))
	      ((eval text) (correct-follow-suggestion (+ i 1) #f))))))
      >>> >>> >>>
      ((balloon (icon "tm_search_first.xpm") "First error")
       (focus-on-master-buffer)
       (spell-go-to-first* focus-on-correct-editor)
       (focus-on-correct-editor))
      ((balloon (icon "tm_search_previous.xpm") "Previous error")
       (focus-on-master-buffer)
       (spell-go-to-previous)
       (focus-on-correct-editor))
      ((balloon (icon "tm_search_next.xpm") "Next error")
       (focus-on-master-buffer)
       (spell-go-to-next* focus-on-correct-editor))
      ((balloon (icon "tm_search_last.xpm") "Last error")
       (focus-on-master-buffer)
       (spell-go-to-last)
       (focus-on-correct-editor))
      ///
      ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
       (set-boolean-preference "toolbar correct" #f)
       (exit-toolbar)
       (open-correct-widget))
      ((balloon (icon "tm_close_tool.xpm") "Close spell tool")
       (correct-end)
       (exit-toolbar))
      //)
    (glue #f #f 0 1)))

(tm-define (open-correct-toolbar)
  (set-boolean-preference "toolbar correct" #t)
  (correct-begin)
  (set! toolbar-correct-active? #t)
  (update-bottom-tools)
  (focus-on-master-buffer)
  (spell-go-to-first*)
  (update-menus)
  (focus-on-correct-editor))

(tm-define (open-correct)
  (:interactive #t)
  (if (get-boolean-preference "toolbar correct")
      (open-correct-toolbar)
      (open-correct-widget)))
