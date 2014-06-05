
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-widgets.scm
;; DESCRIPTION : widgets for general purpose editing
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-widgets)
  (:use (generic generic-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic search and replace buffers management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-window #f)

(tm-define (search-buffer)
  (string->url "tmfs://aux/search"))

(tm-define (replace-buffer)
  (string->url "tmfs://aux/replace"))

(tm-define (master-buffer)
  (and (buffer-exists? (search-buffer))
       (with mas (buffer-get-master (search-buffer))
         (cond ((nnull? (buffer->windows mas))
                mas)
               ((in? search-window (window-list))
                (buffer-set-master (search-buffer)
                                   (window->buffer search-window))
                (with-buffer (buffer-get-master (search-buffer))
                  (set-search-reference (cursor-path))
                  (set-search-filter))
                (master-buffer))
               ((nnull? (window-list))
                (set! search-window (car (window-list)))
                (master-buffer))
               (else #f)))))

(tm-define (inside-search-buffer?)
  (== (current-buffer) (search-buffer)))

(tm-define (inside-replace-buffer?)
  (== (current-buffer) (replace-buffer)))

(tm-define (inside-search-or-replace-buffer?)
  (in? (current-buffer) (list (search-buffer) (replace-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtered searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-filter-table (make-ahash-table))

(define (mode-language mode)
  (cond ((== mode "text") "language")
        ((== mode "math") "math-language")
        ((== mode "prog") "prog-language")
        (else "language")))

(define (get-main-attrs getter)
  (list "mode" (getter "mode")
        "language" (getter "language")
        "math-language" (getter "math-language")
        "prog-language" (getter "prog-language")))

(define (set-search-filter)
  (let* ((vars (list "mode" (mode-language (get-env "mode"))))
         (vals (map get-env-tree vars))
         (attrs (append-map list vars vals))
         (env `(attr ,@attrs)))
    (ahash-set! search-filter-table (current-buffer) env)))

(define (get-search-filter)
  (ahash-ref search-filter-table (current-buffer)))

(define (check-same-sub? env var val)
  (cond ((or (null? env) (null? (cdr env))) #f)
        ((tm-equal? (car env) var) (tm-equal? (cadr env) val))
        (else (check-same-sub? (cddr env) var val))))

(define (check-same? new-env old-env)
  ;;(display* "Check " new-env ", " old-env "\n")
  (if (null? old-env) #t
      (and (check-same-sub? new-env (car old-env) (cadr old-env))
           (check-same? new-env (cddr old-env)))))

(define (accept-search-result? p)
  (let* ((buf (buffer-tree))
         (rel (path-strip (cDr p) (tree->path buf)))
         (initial (cons 'attr (get-main-attrs get-init)))
         (old-env (get-search-filter))
         (new-env (tree-descendant-env buf rel initial)))
    ;;(display* p " ~> " new-env "\n")
    (check-same? (tm-children new-env) (tm-children old-env))))

(define (filter-search-results sels)
  (if (or (null? sels) (null? (cdr sels))) (list)
      (with r (filter-search-results (cddr sels))
        (if (accept-search-result? (car sels))
            (cons* (car sels) (cadr sels) r)
            r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (perform-search)
  (let* ((what (buffer-get-body (search-buffer)))
         (ok? #t))
    (when (tm-func? what 'document 1)
      (set! what (tm-ref what 0)))
    (when (tm-func? what 'inactive 1)
      (set! what (tm-ref what 0)))
    (when (tm-func? what 'inactive* 1)
      (set! what (tm-ref what 0)))
    (with-buffer (master-buffer)
      (if (tree-empty? what)
          (begin
            (selection-cancel)
            (cancel-alt-selection "alternate")
            (go-to (get-search-reference #t)))
          (let* ((t (buffer-tree))
                 (sels* (tree-search-tree t what (tree->path t)))
                 (sels (filter-search-results sels*)))
            (if (null? sels)
                (begin
                  (selection-cancel)
                  (cancel-alt-selection "alternate")
                  (go-to (get-search-reference #t))
                  (set! ok? #f))
                (begin
                  (set-alt-selection "alternate" sels)
                  (with after? (next-search-result #t #f)
                    (when (not after?)
                      (selection-cancel))))))))
    (with-buffer (search-buffer)
      (if ok?
          (init-default "bg-color")
          (init-env "bg-color" "#fff0f0")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting a particular next or previous search result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-search-reference cur)
  (set-alt-selection "search-reference" (list cur cur)))

(define (get-search-reference forward?)
  (with sel (get-alt-selection "search-reference")
    (if (nnull? sel) (car sel)
        (if forward? (cursor-path) (cursor-path*)))))

(define (search-next sels cur strict?)
  (while (and (nnull? sels)
              (nnull? (cdr sels))
              (not (path-less-eq? cur (cadr sels))))
    (set! sels (cddr sels)))
  (if (and (>= (length sels) 4)
           (== (cadr sels) (caddr sels))
           (== (cadr sels) cur))
      (set! sels (cddr sels)))
  (if (and strict? (>= (length sels) 2))
      (set! sels (cddr sels)))
  (and (>= (length sels) 2)
       (list (car sels) (cadr sels))))

(define (search-previous sels cur strict?)
  (set! sels (reverse sels))
  (while (and (nnull? sels)
              (nnull? (cdr sels))
              (not (path-less-eq? (cadr sels) cur)))
    (set! sels (cddr sels)))
  (if (and strict? (>= (length sels) 2))
      (set! sels (cddr sels)))
  (and (>= (length sels) 2)
       (list (cadr sels) (car sels))))

(define (next-search-result forward? strict?)
  (let* ((sels (get-alt-selection "alternate"))
         (cur (get-search-reference forward?)))
    (and (nnull? sels)
         (and-with sel (if forward?
                           (search-next sels cur strict?)
                           (search-previous sels cur strict?))
           (selection-set-range-set sel)
           (go-to (car sel))
           (when strict? (set-search-reference (car sel)))
           #t))))

(define (extreme-search-result last?)
  (with sels (get-alt-selection "alternate")
    (and (nnull? sels)
         (and-with sel (if last?
                           (list (cAr (cDr sels)) (cAr sels))
                           (list (car sels) (cadr sels)))
           (selection-set-range-set sel)
           (go-to (car sel))
           (set-search-reference (car sel))))))

(tm-define (search-next-match forward?)
  (with-buffer (master-buffer)
    (next-search-result forward? #t)))

(tm-define (search-extreme-match last?)
  (with-buffer (master-buffer)
    (extreme-search-result last?)))

(tm-define (search-rotate-match)
  (with ok? (search-next-match #t)
    (when (not ok?)
      (search-extreme-match #f))))

(tm-define ((search-cancel u) . args)
  (with-buffer (master-buffer)
    (cancel-alt-selection "alternate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace occurrences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (by-tree)
  (and (buffer-exists? (replace-buffer))
       (with by (buffer-get-body (replace-buffer))
         (when (tm-func? by 'document 1)
           (set! by (tm-ref by 0)))
         by)))

(define (replace-next by)
  (let* ((sels (get-alt-selection "alternate"))
         (cur (get-search-reference #t)))
    (and (nnull? sels)
         (and-with sel (search-next sels cur #f)
           (go-to (car sel))
           (selection-set-range-set sel)
           (clipboard-cut "dummy")
           (insert-go-to (tree-copy by) (path-end by '()))
           #t))))

(tm-define (replace-one)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (start-editing)
      (replace-next by)
      (end-editing))
    (perform-search)))

(tm-define (replace-all)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (start-editing)
      (while (replace-next by)
        (perform-search))
      (end-editing))
    (perform-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized keyboard shortcuts in search and replace modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-kbd-intercepted? #f)

(tm-define (keyboard-press key time)
  (:require (inside-search-buffer?))
  (set! search-kbd-intercepted? #f)
  (former key time)
  (when (not search-kbd-intercepted?)
    (perform-search)))

(tm-define (kbd-enter t shift?)
  (:require (inside-search-buffer?))
  (if (or shift? (inside? 'inactive) (inside? 'inactive*))
      (former t shift?)
      (begin
        (set! search-kbd-intercepted? #t)
        (search-rotate-match))))

(tm-define (kbd-enter t shift?)
  (:require (inside-replace-buffer?))
  (if (or shift? (inside? 'inactive) (inside? 'inactive*))
      (former t shift?)
      (replace-one)))

(tm-define (kbd-incremental t forwards?)
  (:require (inside-search-or-replace-buffer?))
  (set! search-kbd-intercepted? #t)
  (search-next-match forwards?))

(tm-define (traverse-incremental t forwards?)
  (:require (inside-search-or-replace-buffer?))
  (set! search-kbd-intercepted? #t)
  (search-next-match forwards?))

(tm-define (traverse-extremal t forwards?)
  (:require (inside-search-or-replace-buffer?))
  (set! search-kbd-intercepted? #t)
  (search-extreme-match forwards?))

(kbd-map
  (:require (inside-search-or-replace-buffer?))
  ("std ?" (make 'select-region))
  ("std 1" (insert '(wildcard "x")))
  ("std 2" (insert '(wildcard "y")))
  ("std 3" (insert '(wildcard "z"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((search-widget u style init aux) quit)
  (padded
    (resize "600px" "100px"
      (texmacs-input `(with ,@init (document ""))
                     `(style (tuple ,@style)) aux))
    ===
    (hlist
      ((balloon (icon "tm_search_first.xpm") "First occurrence")
       (search-extreme-match #f))
      ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
       (search-next-match #f))
      ((balloon (icon "tm_search_next.xpm") "Next occurrence")
       (search-next-match #t))
      ((balloon (icon "tm_search_last.xpm") "Last occurrence")
       (search-extreme-match #t))
      >>>
      ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
       (set-boolean-preference "toolbar search" #t)
       (quit)
       (toolbar-search-start))
      ((balloon (icon "tm_close_tool.xpm") "Close search tool")
       (quit)))))

(tm-define (open-search)
  (:interactive #t)
  (let* ((u (current-buffer))
         (st (list-remove-duplicates (rcons (get-style-list) "macro-editor")))
         (init (get-main-attrs get-env))
         (aux (search-buffer)))
    (buffer-set-master aux u)
    (set! search-window (current-window))
    (set-search-reference (cursor-path))
    (set-search-filter)
    (dialogue-window (search-widget u st init aux)
                     (search-cancel u)
                     "Search")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((replace-widget u style init saux raux) quit)
  (padded
    (resize "600px" "100px"
      (texmacs-input `(with ,@init (document ""))
                     `(style (tuple ,@style)) saux))
    === ===
    (resize "600px" "100px"
      (texmacs-input `(with ,@init (document ""))
                     `(style (tuple ,@style)) raux))
    === ===
    (hlist
      ((balloon (icon "tm_search_first.xpm") "First occurrence")
       (search-extreme-match #f))
      ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
       (search-next-match #f))
      ((balloon (icon "tm_search_next.xpm") "Next occurrence")
       (search-next-match #t))
      ((balloon (icon "tm_search_last.xpm") "Last occurrence")
       (search-extreme-match #t))
      // // //
      ((balloon (icon "tm_replace_one.xpm") "Replace one occurrence")
       (replace-one))
      ((balloon (icon "tm_replace_all.xpm") "Replace all further occurrences")
       (replace-all))
      >>>
      ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
       (set-boolean-preference "toolbar replace" #t)
       (quit)
       (toolbar-replace-start))
      ((balloon (icon "tm_close_tool.xpm") "Close replace tool")
       (quit)))))

(tm-define (open-replace)
  (:interactive #t)
  (let* ((u (current-buffer))
         (st (list-remove-duplicates (rcons (get-style-list) "macro-editor")))
         (init (get-main-attrs get-env))
         (saux (search-buffer))
         (raux (replace-buffer)))
    (buffer-set-master saux u)
    (buffer-set-master raux u)
    (set! search-window (current-window))
    (set-search-reference (cursor-path))
    (set-search-filter)
    (dialogue-window (replace-widget u st init saux raux)
                     (search-cancel u)
                     "Search and replace")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-toolbar-search what)
  (let* ((u (current-buffer))
         (aux (search-buffer)))
    (set-search-reference (cursor-path))
    (set-search-filter)
    (buffer-set-body aux `(document ,what))
    (buffer-set-master aux u)
    (set! search-window (current-window))
    (perform-search)))

(tm-define (search-toolbar-keypress what r?)
  (with key (and (pair? what) (cadr what))
    (if (pair? what) (set! what (car what)))
    (cond ((== key "home") (search-extreme-match #f))
          ((== key "end") (search-extreme-match #t))
          ((== key "up") (search-next-match #f))
          ((== key "down") (search-next-match #t))
          ((== key "pageup") (search-next-match #f))
          ((== key "pagedown") (search-next-match #t))
          ((and r? (in? key (list "tab" "S-tab" "return")))
           (search-toolbar-search what)
           (keyboard-focus-on "replace-by"))
          ((== key "return") (search-rotate-match))
          ((== key "escape") (toolbar-search-end))
          ((string? what) (search-toolbar-search what))
          (else (cancel-alt-selection "alternate")))))

(tm-widget (search-toolbar)
  (text "Search: ")
  ;;(resize "0.5w" "24px"
  ;;  (texmacs-input `(document "")
  ;;                 `(style (tuple "generic"))
  ;;                 (search-buffer)))
  (input (search-toolbar-keypress answer #f) "search" (list "") "25em")
  //
  ((balloon (icon "tm_search_first.xpm") "First occurrence")
   (search-extreme-match #f))
  ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
   (search-next-match #f))
  ((balloon (icon "tm_search_next.xpm") "Next occurrence")
   (search-next-match #t))
  ((balloon (icon "tm_search_last.xpm") "Last occurrence")
   (search-extreme-match #t))
  // // // // // // // // // // // // // // // // >>>
  ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
   (set-boolean-preference "toolbar search" #f)
   (toolbar-search-end)
   (open-search))
  ((balloon (icon "tm_close_tool.xpm") "Close search tool")
   (toolbar-search-end)))

(tm-define (toolbar-search-start)
  (:interactive #t)
  (set! toolbar-search-active? #t)
  (set! toolbar-replace-active? #f)
  (show-bottom-tools 0 #t)
  (search-toolbar-search "")
  (delayed
    (:idle 250)
    (keyboard-focus-on "search")
    (perform-search)
    (notify-change 68)))

(tm-define (toolbar-search-end)
  (cancel-alt-selection "alternate")
  (set! toolbar-search-active? #f)
  (set! toolbar-replace-active? #f)
  (show-bottom-tools 0 #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-toolbar-replace by)
  (let* ((u (current-buffer))
         (aux (replace-buffer)))
    (buffer-set-body aux `(document ,by))
    (buffer-set-master aux u)
    (set! search-window (current-window))
    (replace-one)))

(tm-define (replace-toolbar-keypress by)
  (with key (and (pair? by) (cadr by))
    (if (pair? by) (set! by (car by)))
    (cond ((not (string? by)) (cancel-alt-selection "alternate"))
          ((== key "home") (search-extreme-match #f))
          ((== key "end") (search-extreme-match #t))
          ((== key "up") (search-next-match #f))
          ((== key "down") (search-next-match #t))
          ((== key "pageup") (search-next-match #f))
          ((== key "pagedown") (search-next-match #t))
          ((== key "tab") (keyboard-focus-on "replace-what"))
          ((== key "S-tab") (keyboard-focus-on "replace-what"))
          ((== key "return") (replace-toolbar-replace by))
          ((== key "S-return") (undo 0) (perform-search))
          ((== key "escape") (toolbar-search-end))
          (else (perform-search)))))

(tm-widget (replace-toolbar)
  (text "Replace: ")
  (input (search-toolbar-keypress answer #t) "replace-what" (list "") "15em")
  //
  (text "by: ")
  (input (replace-toolbar-keypress answer) "replace-by" (list "") "15em")
  //
  ;;(if (nnull? (get-alt-selection "alternate"))
  ((balloon (icon "tm_search_first.xpm") "First occurrence")
   (search-extreme-match #f))
  ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
   (search-next-match #f))
  ((balloon (icon "tm_search_next.xpm") "Next occurrence")
   (search-next-match #t))
  ((balloon (icon "tm_search_last.xpm") "Last occurrence")
   (search-extreme-match #t))
  //
  ((balloon (icon "tm_replace_one.xpm") "Replace one occurrence")
   (replace-one))
  ((balloon (icon "tm_replace_all.xpm") "Replace all further occurrences")
   (replace-all))
  // // // // // // // // // >>>
  ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
   (set-boolean-preference "toolbar replace" #f)
   (toolbar-search-end)
   (open-replace))
  ((balloon (icon "tm_close_tool.xpm") "Close replace tool")
   (toolbar-search-end)))

(tm-define (toolbar-replace-start)
  (:interactive #t)
  (set! toolbar-search-active? #f)
  (set! toolbar-replace-active? #t)
  (show-bottom-tools 0 #t)
  (search-toolbar-search "")
  (delayed
    (:idle 250)
    (keyboard-focus-on "replace-what")
    (perform-search)
    (notify-change 68)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("toolbar search" "on" noop)
  ("toolbar replace" "on" noop))

(tm-define (interactive-search)
  (:interactive #t)
  (if (get-boolean-preference "toolbar search")
      (toolbar-search-start)
      (open-search)))

(tm-define (interactive-replace)
  (:interactive #t)
  (if (get-boolean-preference "toolbar replace")
      (toolbar-replace-start)
      (open-replace)))
