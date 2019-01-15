
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : spell-widgets.scm
;; DESCRIPTION : widgets for general purpose editing
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic spell-widgets)
  (:use (generic generic-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic spell buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-window #f)

(tm-define (spell-buffer)
  (string->url "tmfs://aux/spell"))

(tm-define (spell-master-buffer)
  (and (buffer-exists? (spell-buffer))
       (with mas (buffer-get-master (spell-buffer))
         (cond ((nnull? (buffer->windows mas))
                mas)
               ((in? spell-window (window-list))
                (buffer-set-master (spell-buffer)
                                   (window->buffer spell-window))
                (with-buffer (buffer-get-master (spell-buffer))
                  (set-spell-reference (cursor-path)))
                (spell-master-buffer))
               ((nnull? (window-list))
                (set! spell-window (car (window-list)))
                (spell-master-buffer))
               (else #f)))))

(tm-define (inside-spell-buffer?)
  (== (current-buffer) (spell-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting the spell results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-serial 0)
(define spell-buffer-cache #f)
(define spell-language "english")

(define (spell-buffer-tree)
  (let* ((t (buffer-tree))
         (p (tree->path t))
         (cp (cDr (cursor-path)))
         (pos (if (list-starts? cp p) (list-tail cp (length p)) (list))))
    (tree-spell-at spell-language t p pos 1000)))

(define (cached-spell-buffer-tree)
  (with sels spell-buffer-cache
    (set! spell-buffer-cache #f)
    (or sels (spell-buffer-tree))))

(define (go-to* p)
  (go-to p)
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)
    (delayed
      (:pause 25)
      (set! spell-serial (+ spell-serial 1))
      (perform-spell-sub 100 #f))))

(define (perform-spell-sub limit top?)
  (with-buffer (spell-master-buffer)
    (with sels (cached-spell-buffer-tree)
      ;;(display* "sels= " sels "\n")
      (if (null? sels)
          (with go-to** (if top? go-to* go-to)
            (go-to** (get-spell-reference #t))
            (if toolbar-spell-active?
                (toolbar-spell-end)
                (if spell-quit (spell-quit))))
          (begin
            (set-alt-selection "alternate" sels)
            (or (next-spell-result #t #f)
                (next-spell-result #f #f)))))))

(define (perform-spell)
  (set! spell-serial (+ spell-serial 1))
  (perform-spell-sub 100 #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Current spell focus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-correct-string "")
(define spell-suggestions (list))
(define spell-focus-hack? #t)

(define (selection->string sel)
  (and (list-2? sel)
       (== (cDr (car sel)) (cDr (cadr sel)))
       (with t (path->tree (cDr (car sel)))
         (and (tree-atomic? t)
              (let* ((s (tree->string t))
                     (n (string-length s))
                     (i1 (cAr (car sel)))
                     (i2 (cAr (cadr sel))))
                (and (>= i1 0) (> i2 i1) (>= n i2)
                     (substring s i1 i2)))))))

(define (spell-get-language sel)
  (with-buffer (spell-master-buffer)
    (let* ((bt (buffer-tree))
           (rp (tree->path bt))
           (sp (car sel))
           (p (and (list-starts? sp rp)(sublist sp (length rp) (length sp))))
           (lan spell-language))
      (if (not p) lan
          (tm->stree (tree-descendant-env bt (cDr p) "language" lan))))))

(define (spell-focus-on sel)
  (selection-set-range-set sel)
  (and-with ss (selection->string sel)
    (let* ((lan (spell-get-language sel))
           (st (tm->stree (spell-check lan ss)))
           (l0 (if (tm-func? st 'tuple) (cdr st) (list)))
           (l1 (if (null? l0) l0 (cdr l0)))
           (l (if (<= (length l1) 9) l1 (sublist l1 0 9)))
           (aux (spell-buffer)))
      (buffer-set-body aux `(document ,ss))
      (set! spell-correct-string ss)
      (set! spell-suggestions l)
      (refresh-now "spell-suggestions")
      (when toolbar-spell-active?
        (update-menus)
        (delayed
          (:idle 1)
          (when toolbar-spell-active?
            (keyboard-focus-on "spell")
            (when spell-focus-hack?
              (set! spell-focus-hack? #f)
              (delayed
                (:idle 100)
                (when toolbar-spell-active?
                  (keyboard-focus-on "spell")
                  (set! spell-focus-hack? #t))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting a particular next or previous spell result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-spell-reference cur)
  (set-alt-selection "spell-reference" (list cur cur)))

(define (get-spell-reference forward?)
  (with sel (get-alt-selection "spell-reference")
    (if (nnull? sel) (car sel)
        (if forward? (cursor-path) (cursor-path*)))))

(define (spell-next sels cur strict?)
  (with sel (next-search-hit sels cur strict?)
    (and (nnull? sel) sel)))

(define (spell-previous sels cur strict?)
  (with sel (previous-search-hit sels cur strict?)
    (and (nnull? sel) sel)))

(define (next-spell-result forward? strict?)
  (let* ((cur (get-spell-reference forward?))
         (sel (navigate-search-hit cur forward? #f strict?)))
    (and (nnull? sel)
         (begin
           (go-to* (car sel))
           (when strict? (set-spell-reference (car sel)))
           (spell-focus-on sel)
           #t))))

(define (extreme-spell-result last?)
  (let* ((cur (get-spell-reference last?))
         (sel (navigate-search-hit cur last? #t #f)))
    (and (nnull? sel)
         (begin
           (go-to* (car sel))
           (set-spell-reference (car sel))
           (spell-focus-on sel)))))

(tm-define (spell-next-match forward?)
  (with-buffer (spell-master-buffer)
    (next-spell-result forward? #t)))

(tm-define (spell-extreme-match last?)
  (with-buffer (spell-master-buffer)
    (extreme-spell-result last?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correct occurrences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-corrected 0)
(define spell-accepted 0)
(define spell-inserted 0)

(define (spell-current-selection)
  (with-buffer (spell-master-buffer)
    (with sels (get-alt-selection "alternate")
      (and (nnull? sels)
           (or (with cur (get-spell-reference #t)
                 (spell-next sels cur #f))
               (with cur (get-spell-reference #f)
                 (spell-previous sels cur #f)))))))

(define (spell-replace-one by)
  (and-with sel (spell-current-selection)
    (go-to* (car sel))
    (selection-set-range-set sel)
    (clipboard-cut "dummy")
    (insert-go-to by (list (string-length by)))
    #t))

(tm-define (spell-replace-by by)
  (with-buffer (spell-master-buffer)
    (start-editing)
    (set! spell-corrected (+ spell-corrected 1))
    (spell-replace-one by)
    (end-editing))
  (perform-spell))

(tm-define (spell-follow-suggestion i)
  (cond ((== i "") (noop))
        ((string? i)
         (with nr (- (string->number (substring i 0 1)) 1)
           (spell-follow-suggestion nr)))
        (else
          (when (and (>= i 0) (< i (length spell-suggestions)))
            (spell-replace-by (list-ref spell-suggestions i))))))

(tm-define (spell-accept-word)
  (and-with sel (spell-current-selection)
    (and-with ss (selection->string sel)
      (with lan (spell-get-language sel)
        (set! spell-accepted (+ spell-accepted 1))
        (spell-accept lan ss)
        (perform-spell)))))

(tm-define (spell-insert-word)
  (and-with sel (spell-current-selection)
    (and-with ss (selection->string sel)
      (with lan (spell-get-language sel)
        (set! spell-inserted (+ spell-inserted 1))
        (spell-insert lan ss)
        (perform-spell)))))

(tm-define (spell-statistics)
  (delayed
    (:idle 100)
    (with r "spell check"
      (cond ((> spell-inserted 0)
             (set-message "Your personal dictionary has been modified" r))
            ((== spell-corrected 1)
             (set-message "One error has been corrected" r))
            ((> spell-corrected 1)
             (with n (number->string spell-corrected)
               (set-message (string-append n " errors have been corrected") r)))
            (else (set-message "No errors have been corrected" r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized keyboard shortcuts in spell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (keyboard-press key time)
  (:require (inside-spell-buffer?))
  (cond ((and (string>=? key "1") (string<=? key "9"))
         (spell-follow-suggestion key))
        ((== key "tab")
         (spell-accept-word))
        ((== key "+")
         (spell-insert-word))
        (else (former key time))))

(tm-define (kbd-enter t shift?)
  (:require (inside-spell-buffer?))
  (with doc (tree->stree (buffer-tree))
    (when (and (tm-func? doc 'document) (pair? (cdr doc)))
      (set! doc (cadr doc)))
    (when (string? doc)
      (spell-replace-by doc))))
  
(tm-define (kbd-incremental t forwards?)
  (:require (inside-spell-buffer?))
  (spell-next-match forwards?))

(tm-define (traverse-incremental t forwards?)
  (:require (inside-spell-buffer?))
  (spell-next-match forwards?))

(tm-define (traverse-extremal t forwards?)
  (:require (inside-spell-buffer?))
  (spell-extreme-match forwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-quit #f)

(define (prefix-suggestions i l)
  (if (null? l) l
      (cons (string-append (number->string i) ": " (car l))
            (prefix-suggestions (+ i 1) (cdr l)))))

(tm-widget ((spell-widget u style init aux) quit)
  (padded
    (hlist
      (vlist
        (with dummy (set! spell-quit quit)
          (resize "400px" "75px"
            (texmacs-input `(with ,@init (document ""))
                           `(style (tuple ,@style)) aux)))
        (glue #t #t 0 0)
        (explicit-buttons
          (aligned
            (meti (hlist // (text "Accept during this pass"))
              ("Tab" (spell-accept-word)))
            (meti (hlist // (text "Permanently insert into dictionary"))
              (" + " (spell-insert-word)))))
        (glue #t #t 0 0)
        ===
        (hlist
          >>>
          ((balloon (icon "tm_search_first.xpm") "First error")
           (spell-extreme-match #f))
          ((balloon (icon "tm_search_previous.xpm") "Previous error")
           (spell-next-match #f))
          ((balloon (icon "tm_search_next.xpm") "Next error")
           (spell-next-match #t))
          ((balloon (icon "tm_search_last.xpm") "Last error")
           (spell-extreme-match #t))
          /// ///
          ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
           (set-boolean-preference "toolbar spell" #t)
           (quit)
           (toolbar-spell-start))
          ((balloon (icon "tm_close_tool.xpm") "Close spell tool")
           (quit))))
      /// ///
      (resize "200px" "225px"
        (refreshable "spell-suggestions"
          (choice (spell-follow-suggestion answer)
                  (prefix-suggestions 1 spell-suggestions)
                  ""))))))

(define (get-main-attrs getter)
  (list "mode" (getter "mode")
        "language" (getter "language")
        "math-language" (getter "math-language")
        "prog-language" (getter "prog-language")))

(tm-define (spell-cancel . args)
  (set! spell-quit #f)
  (set! spell-serial (+ spell-serial 1))
  (with-buffer (spell-master-buffer)
    (cancel-alt-selection "alternate"))
  (multi-spell-done)
  (spell-statistics))

(tm-define (open-spell)
  (:interactive #t)
  (when (not (inside-spell-buffer?))
    (multi-spell-start)
    (let* ((u (current-buffer))
           (st (get-style-list))
           (init (get-main-attrs get-env))
           (aux (spell-buffer)))
      (buffer-set-master aux u)
      (set! spell-window (current-window))
      (set-spell-reference (cursor-path))
      (set! spell-correct-string "")
      (set! spell-suggestions (list))
      (set! spell-corrected 0)
      (set! spell-accepted 0)
      (set! spell-inserted 0)
      (delayed
        (:idle 100)
        (perform-spell))
      (dialogue-window (spell-widget u st init aux)
                       spell-cancel
                       "Spell" aux))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-toolbar-keypress what)
  (with key (and (pair? what) (cadr what))
    (if (pair? what) (set! what (car what)))
    (cond ((== key "home") (spell-extreme-match #f))
          ((== key "end") (spell-extreme-match #t))
          ((== key "up") (spell-next-match #f))
          ((== key "down") (spell-next-match #t))
          ((== key "pageup") (spell-next-match #f))
          ((== key "pagedown") (spell-next-match #t))
          ((== key "escape") (toolbar-spell-end))
          ((== key "tab") (spell-accept-word))
          ((== key "+") (spell-insert-word))
          ((== key "return") (spell-replace-by what))
          ((in? key (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (spell-follow-suggestion (- (string->number key) 1))))))

(tm-widget (spell-toolbar)
  (hlist
    ((balloon (icon "tm_right.xpm") "Accept during this pass")
     (spell-accept-word))
    ((balloon (icon "tm_add.xpm") "Permanently add to dictionary")
     (spell-insert-word))
    ///
    (text "Correct: ")
    (input (spell-toolbar-keypress answer) "spell"
           (list spell-correct-string) "15em")
    (assuming (nnull? spell-suggestions)
      (minibar
        (for (i (.. 0 (length spell-suggestions)))
          ///
          (with text (string-append (number->string (+ i 1)) ": "
                                    (list-ref spell-suggestions i))
            ((eval text) (spell-follow-suggestion i))))))
    >>> >>> >>>
    ((balloon (icon "tm_search_first.xpm") "First error")
     (spell-extreme-match #f))
    ((balloon (icon "tm_search_previous.xpm") "Previous error")
     (spell-next-match #f))
    ((balloon (icon "tm_search_next.xpm") "Next error")
     (spell-next-match #t))
    ((balloon (icon "tm_search_last.xpm") "Last error")
     (spell-extreme-match #t))
    ///
    ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
     (set-boolean-preference "toolbar spell" #f)
     (toolbar-spell-end)
     (open-spell))
    ((balloon (icon "tm_close_tool.xpm") "Close spell tool")
      (toolbar-spell-end))))

(tm-define (toolbar-spell-start)
  (:interactive #t)
  (multi-spell-start)
  (set! toolbar-spell-active? #t)
  (set! spell-focus-hack? #t)
  (set! spell-correct-string "")
  (set! spell-suggestions (list))
  (show-bottom-tools 0 #t)
  (set! spell-corrected 0)
  (set! spell-accepted 0)
  (set! spell-inserted 0)
  (let* ((u (current-buffer))
         (aux (spell-buffer)))
    (set-spell-reference (cursor-path))
    (buffer-set-body aux `(document ,spell-correct-string))
    (buffer-set-master aux u)
    (set! spell-window (current-window))
    (perform-spell)
    (delayed
      (:idle 250)
      (when toolbar-spell-active?
        (keyboard-focus-on "spell")))))

(tm-define (toolbar-spell-end)
  (multi-spell-done)
  (cancel-alt-selection "alternate")
  (set! toolbar-spell-active? #f)
  (set! spell-focus-hack? #t)
  (set! spell-correct-string "")
  (set! spell-suggestions (list))
  (show-bottom-tools 0 #f)
  (set! spell-serial (+ spell-serial 1))
  (when toolbar-db-active?
    (db-show-toolbar))
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden))
  (spell-statistics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("toolbar spell" "on" noop))

(tm-define (interactive-spell)
  (:interactive #t)
  (set! spell-language (get-init "language"))
  (with sels (spell-buffer-tree)
    (if (null? sels)
        (delayed
          (:idle 100)
          (set-message "No spelling errors" "spell check"))
        (begin
          (set! spell-buffer-cache sels)
          (if (and (get-boolean-preference "toolbar spell")
                   (not (buffer-aux? (current-buffer))))
              (toolbar-spell-start)
              (open-spell))))))
