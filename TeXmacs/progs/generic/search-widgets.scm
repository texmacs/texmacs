
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : search-widgets.scm
;; DESCRIPTION : widgets for general purpose editing
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic search-widgets)
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
  (or (== (get-init "mode") "src")
      (let* ((buf (buffer-tree))
             (rel (path-strip (cDr p) (tree->path buf)))
             (initial (cons 'attr (get-main-attrs get-init)))
             (old-env (get-search-filter))
             (new-env (tree-descendant-env* buf rel initial)))
        ;;(display* p " ~> " new-env "\n")
        (check-same? (tm-children new-env) (tm-children old-env)))))

(define (filter-search-results sels)
  (if (or (null? sels) (null? (cdr sels))) (list)
      (with r (filter-search-results (cddr sels))
        (if (accept-search-result? (car sels))
            (cons* (car sels) (cadr sels) r)
            r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-serial 0)
(define search-filter-out? #f)

(define (tree-perform-search t what p limit)
  (let* ((source-mode 2)
         (old-mode (get-access-mode))
         (new-mode (if (== (get-init "mode") "src") source-mode old-mode)))
    (set-access-mode new-mode)
    (let* ((cp (cDr (cursor-path)))
           (pos (if (list-starts? cp p) (list-tail cp (length p)) (list)))
           (r (tree-search-tree-at t what p pos limit)))
      (set-access-mode old-mode)
      r)))

(define (go-to* p)
  (go-to p)
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)
    (delayed
      (:pause 25)
      (set! search-serial (+ search-serial 1))
      (perform-search-sub 100 #f))))

(define (perform-search-sub limit top?)
  (let* ((what (buffer-get-body (search-buffer)))
         (ok? #t)
	 (too-many-matches? #f)
	 (serial search-serial)
         (go-to** (if top? go-to* go-to)))
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
            (go-to** (get-search-reference #t)))
          (let* ((t (buffer-tree))
                 (sels* (tree-perform-search t what (tree->path t) limit))
                 (sels (filter-search-results sels*)))
	    ;;(display* "sels= " sels "\n")
	    (cond ((>= (length sels*) limit)
		   (set-alt-selection "alternate" sels)
		   (set! too-many-matches? #t)
		   (next-search-result #t #f))
		  ((null? sels)
		   (selection-cancel)
		   (cancel-alt-selection "alternate")
		   (go-to** (get-search-reference #t))
		   (set! ok? #f))
		  (else
		   (set-alt-selection "alternate" sels)
		   (with after? (next-search-result #t #f)
		     (when (not after?)
		       (selection-cancel))))))))
    (with-buffer (search-buffer)
      (if ok?
          (init-default "bg-color")
          (init-env "bg-color" "#fff0f0")))
    (when too-many-matches?
      ;;(display* "Extend limit to " (* 2 limit) "\n")
      (delayed
	(:pause limit)
	(when (== serial search-serial)
	  (perform-search-sub (* 2 limit) top?))))))

(define (perform-search*)
  (set! search-serial (+ search-serial 1))
  (perform-search-sub 100 #t))

(define (perform-search)
  (search-show-only)
  (perform-search*))

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
  (with sel (next-search-hit sels cur strict?)
    (and (nnull? sel) sel)))

(define (search-previous sels cur strict?)
  (with sel (previous-search-hit sels cur strict?)
    (and (nnull? sel) sel)))

(define (next-search-result forward? strict?)
  (let* ((cur (get-search-reference forward?))
         (sel (navigate-search-hit cur forward? #f strict?)))
    (and (nnull? sel)
         (begin
           (selection-set-range-set sel)
           (go-to* (car sel))
           (when strict? (set-search-reference (car sel)))
           #t))))

(define (extreme-search-result last?)
  (let* ((cur (get-search-reference last?))
         (sel (navigate-search-hit cur last? #t #f)))
    (and (nnull? sel)
         (begin
           (selection-set-range-set sel)
           (go-to* (car sel))
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
  (search-show-all)
  (set! search-serial (+ search-serial 1))
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
           (go-to* (car sel))
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
    (perform-search*)))

(tm-define (replace-all)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (start-editing)
      (while (replace-next by)
        (perform-search*))
      (end-editing))
    (perform-search*)))

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
;; Hacks for keyboard events between the moments that
;; a search was triggered and that the search bar actually shows up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define waiting-for-toolbar? #f)
(define pending-key-strokes "")
(define last-search "")

(define (wait-for-toolbar)
  (set! waiting-for-toolbar? #t)
  (set! pending-key-strokes "")
  (delayed
    (:pause 3000)
    (when waiting-for-toolbar?
      (stop-waiting-for-toolbar))))

(define (stop-waiting-for-toolbar)
  (set! waiting-for-toolbar? #f))

(tm-define (keyboard-press key time)
  (:require waiting-for-toolbar?)
  (when (== (tmstring-length key) 1)
    (set! pending-key-strokes (string-append pending-key-strokes key)))
  (when (in? key '("F3" "C-f" "A-f" "M-f" "C-g" "A-g" "M-g" "C-s" "A-s" "M-s"))
    (set! pending-key-strokes last-search)))

(define (notify-bar-change)
  ;; FIXME: not clear what is the most appriate setting here
  ;; The value 127 is safest, but causes the whole document to be re-typeset 
  (if (style-has? "beamer-style")
      (notify-change 127)
      (notify-change 68)))

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
      ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
              "v" (search-filter-enabled?))
       (search-toggle-filter))
      ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
       (set-boolean-preference "toolbar search" #t)
       (quit)
       (toolbar-search-start))
      ((balloon (icon "tm_close_tool.xpm") "Close search tool")
       (quit)))))

(tm-define (open-search)
  (:interactive #t)
  (when (not (inside-search-buffer?))
    (let* ((u (current-buffer))
           (st (list-remove-duplicates
                (rcons (get-style-list) "macro-editor")))
           (init (get-main-attrs get-env))
           (aux (search-buffer)))
      (buffer-set-master aux u)
      (set! search-window (current-window))
      (set-search-reference (cursor-path))
      (set-search-filter)
      (set! search-filter-out? #f)
      (dialogue-window (search-widget u st init aux)
                       (search-cancel u)
                       "Search" aux))))

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
      ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
              "v" (search-filter-enabled?))
       (search-toggle-filter))
      ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
       (set-boolean-preference "toolbar replace" #t)
       (quit)
       (toolbar-replace-start))
      ((balloon (icon "tm_close_tool.xpm") "Close replace tool")
       (quit)))))

(tm-define (open-replace)
  (:interactive #t)
  (when (not (inside-search-buffer?))
    (let* ((u (current-buffer))
           (st (list-remove-duplicates
                (rcons (get-style-list) "macro-editor")))
           (init (get-main-attrs get-env))
           (saux (search-buffer))
           (raux (replace-buffer)))
      (buffer-set-master saux u)
      (buffer-set-master raux u)
      (set! search-window (current-window))
      (set-search-reference (cursor-path))
      (set-search-filter)
      (set! search-filter-out? #f)
      (dialogue-window (replace-widget u st init saux raux)
                       (search-cancel u)
                       "Search and replace" saux raux))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-toolbar-search what)
  (let* ((u (current-buffer))
         (aux (search-buffer))
         (what-case (if (get-boolean-preference "case-insensitive-match")
                        (string-downcase what) what)))

    (set-search-reference (cursor-path))
    (set-search-filter)
    (buffer-set-body aux `(document ,what-case))
    (buffer-set-master aux u)
    (set! search-window (current-window))
    (perform-search)))

(tm-define (search-toolbar-keypress what r?)
  (with key (and (pair? what) (cadr what))
    (if (pair? what) (set! what (car what)))
    (set! last-search what)
    (cond ((== key "home") (search-extreme-match #f))
          ((== key "end") (search-extreme-match #t))
          ((== key "up") (search-next-match #f))
          ((== key "down") (search-next-match #t))
          ((== key "pageup") (search-next-match #f))
          ((== key "pagedown") (search-next-match #t))
          ((== key "S-F3") (search-next-match #f))
          ((== key "F3") (search-next-match #t))
          ((in? key '("C-F" "A-F" "M-F" "C-G" "A-G" "M-G" "C-r" "A-r" "M-r"))
           (search-next-match #f))
          ((in? key '("C-f" "A-f" "M-f" "C-g" "A-g" "M-g" "C-s" "A-s" "M-s"))
           (search-next-match #t))
          ((and r? (in? key (list "tab" "S-tab" "return")))
           (search-toolbar-search what)
           (keyboard-focus-on "replace-by"))
          ((== key "return") (search-rotate-match))
          ((== key "escape") (toolbar-search-end))
          ((string? what) (search-toolbar-search what))
          (else (cancel-alt-selection "alternate")))))

(tm-widget (search-toolbar)
  (hlist
    (text "Search: ")
    ;;(resize "0.5w" "24px"
    ;;  (texmacs-input `(document "")
    ;;                 `(style (tuple "generic"))
    ;;                 (search-buffer)))
    (input (search-toolbar-keypress answer #f) "search"
           (list pending-key-strokes) "25em")
    //
    ((balloon (icon "tm_search_first.xpm") "First occurrence")
     (search-extreme-match #f))
    ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
     (search-next-match #f))
    ((balloon (icon "tm_search_next.xpm") "Next occurrence")
     (search-next-match #t))
    ((balloon (icon "tm_search_last.xpm") "Last occurrence")
     (search-extreme-match #t))
    >>>
    ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
            "v" (search-filter-enabled?))
     (search-toggle-filter))
    ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
     (set-boolean-preference "toolbar search" #f)
     (toolbar-search-end)
     (open-search))
    ((balloon (icon "tm_close_tool.xpm") "Close search tool")
      (toolbar-search-end))))

(tm-define (toolbar-search-start)
  (:interactive #t)
  (search-show-all)
  (set! search-filter-out? #f)
  (set! toolbar-search-active? #t)
  (set! toolbar-replace-active? #f)
  (show-bottom-tools 0 #t)
  (search-toolbar-search "")
  (wait-for-toolbar)
  (notify-bar-change)
  (delayed
    (:idle 250)
    (keyboard-focus-on "search")
    (search-toolbar-search pending-key-strokes)
    (notify-bar-change)
    (stop-waiting-for-toolbar)))

(tm-define (toolbar-search-end)
  (cancel-alt-selection "alternate")
  (search-show-all)
  (set! search-filter-out? #f)
  (set! toolbar-search-active? #f)
  (set! toolbar-replace-active? #f)
  (show-bottom-tools 0 #f)
  (set! search-serial (+ search-serial 1))
  (set! pending-key-strokes "")
  (when toolbar-db-active?
    (db-show-toolbar))
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)))

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
          ((== key "S-return") (undo 0) (perform-search*))
          ((== key "C-return") (replace-all))
          ((== key "escape") (toolbar-search-end))
          (else (perform-search*)))))

(tm-widget (replace-toolbar)
  (hlist
    (text "Replace: ")
    (input (search-toolbar-keypress answer #t) "replace-what"
           (list pending-key-strokes) "15em")
    //
    (text " by: ")
    (input (replace-toolbar-keypress answer) "replace-by"
           (list "") "15em")
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
    >>>
    ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
            "v" (search-filter-enabled?))
     (search-toggle-filter))
    ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
     (set-boolean-preference "toolbar replace" #f)
     (toolbar-search-end)
     (open-replace))
    ((balloon (icon "tm_close_tool.xpm") "Close replace tool")
      (toolbar-search-end))))

(tm-define (toolbar-replace-start)
  (:interactive #t)
  (search-show-all)
  (set! search-filter-out? #f)
  (set! toolbar-search-active? #f)
  (set! toolbar-replace-active? #t)
  (show-bottom-tools 0 #t)
  (search-toolbar-search "")
  (wait-for-toolbar)
  (notify-bar-change)
  (delayed
    (:idle 250)
    (keyboard-focus-on "replace-what")
    (search-toolbar-search pending-key-strokes)
    (notify-bar-change)
    (stop-waiting-for-toolbar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hiding paragraphs that do not match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-only-sub u what)
  (with t (if (tree-is? u 'hide-para) (tree-ref u 0) u)
    (let* ((sels* (tree-search-tree t what (tree->path t) 100))
           (sels (filter-search-results sels*)))
      (if (null? sels)
          (when (not (tree-is? u 'hide-para))
            (tree-insert-node u 0 '(hide-para)))
          (begin
            (when (tree-is? u 'hide-para)
              (tree-remove-node! u 0))
            (show-only u what))))))

(define (show-only-document t what)
  (if (tree-is? t 'document) (show-only t what)))

(define (show-only t what)
  (cond ((== what (string->tree ""))
         (show-all t))
        ((tree-is? t 'document)
         (for-each (cut show-only-sub <> what) (tree-children t)))
        ((tree-is? t 'hide-para)
         (show-only-sub t what))
        ((tree-compound? t)
         (for-each (cut show-only-document <> what)
                   (tree-accessible-children t)))
        (else (noop))))

(define (show-all-document t)
  (if (tree-is? t 'document) (show-all t)))

(define (show-all t)
  (cond ((tree-is? t 'document)
         (for-each show-all (tree-children t)))
        ((tree-is? t 'hide-para)
         (tree-remove-node t 0))
        ((tree-compound? t)
         (for-each show-all-document (tree-accessible-children t)))
        (else (noop))))

(define (search-show-only)
  (when search-filter-out?
    (with-buffer (master-buffer)
      (let* ((doc (buffer-get-body (master-buffer)))
             (what (buffer-get-body (search-buffer))))
        (when (tree-func? what 'document 1)
          (set! what (tree-ref what 0)))
        (when (and (tree-func? doc 'document)
                   (tree-func? (tree-ref doc :last) 'blank-line))
          (tree-remove! doc (- (tree-arity doc) 1) 1))
        (show-only doc what)
        (when (and (tree-func? doc 'document)
                   (list-and (map (lambda (x) (tree-is? x 'hide-para))
                                  (tree-children doc)))
                   (not (tree-func? (tree-ref doc :last) 'blank-line)))
          (tree-insert! doc (tree-arity doc) '((blank-line))))))))

(define (search-show-all)
  (when search-filter-out?
    (with-buffer (master-buffer)
      (with doc (buffer-get-body (master-buffer))
        (show-all doc)
        (when (and (tree-func? doc 'document)
                   (tree-func? (tree-ref doc :last) 'blank-line))
          (tree-remove! doc (- (tree-arity doc) 1) 1))))))

(define (search-filter-enabled?)
  search-filter-out?)

(define (search-toggle-filter)
  (search-show-all)
  (set! search-filter-out? (not search-filter-out?))
  (perform-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("toolbar search" "on" noop)
  ("toolbar replace" "on" noop))

(tm-define (interactive-search)
  (:interactive #t)
  (if (and (get-boolean-preference "toolbar search")
	   (not (buffer-aux? (current-buffer))))
      (toolbar-search-start)
      (open-search)))

(tm-define (interactive-replace)
  (:interactive #t)
  (if (and (get-boolean-preference "toolbar replace")
	   (not (buffer-aux? (current-buffer))))
      (toolbar-replace-start)
      (open-replace)))
