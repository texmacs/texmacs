
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
;; Highlighting the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (inside-search-widget?)
  (== (current-buffer) (string->url "tmfs://aux/search")))

(tm-define (keyboard-press key time)
  (:require (inside-search-widget?))
  (former key time)
  (with what (buffer-tree)
    (when (tm-func? what 'document 1)
      (set! what (tm-ref what 0)))
    (with-buffer (buffer-get-master (current-buffer))
      (if (tree-empty? what)
          (selection-cancel)
          (let* ((t (buffer-tree))
                 (sels (tree-search-tree t what (tree->path t))))
            (if (null? sels)
                (selection-cancel)
                (begin
                  (set-alt-selection "alternate" sels)
                  (next-search-result #t #f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting a particular next or previous search result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-search-cursor)
  (cursor-path))

(define (set-search-cursor cur)
  (go-to cur))

(define (search-next sels cur strict?)
  (while (and (nnull? sels)
              (nnull? (cdr sels))
              (not (path-inf-eq? cur (cadr sels))))
    (set! sels (cddr sels)))
  (if (and strict? (>= (length sels) 2))
      (set! sels (cddr sels)))
  (and (>= (length sels) 2)
       (list (car sels) (cadr sels))))

(define (search-previous sels cur strict?)
  (set! sels (reverse sels))
  (while (and (nnull? sels)
              (nnull? (cdr sels))
              (not (path-inf-eq? (cadr sels) cur)))
    (set! sels (cddr sels)))
  (if (and strict? (>= (length sels) 2))
      (set! sels (cddr sels)))
  (and (>= (length sels) 2)
       (list (cadr sels) (car sels))))

(define (next-search-result forward? strict?)
  (let* ((sels (get-alt-selection "alternate"))
         (cur (get-search-cursor)))
    (and (nnull? sels)
         (and-with sel (if forward?
                           (search-next sels cur strict?)
                           (search-previous sels cur strict?))
           (selection-set-range-set sel)
           (when strict? (set-search-cursor (car sel)))))))

(tm-define (search-next-match forward?)
  (when (inside-search-widget?)
    (with-buffer (buffer-get-master (current-buffer))
      (next-search-result forward? #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((search-widget u style aux) quit)
  (padded
    (resize "600px" "60px"
      (texmacs-input `(document "") `(style (tuple ,@style)) aux))
    ======
    (explicit-buttons
      (hlist
        ("Previous" (search-next-match #f)) // //
        ("Next" (search-next-match #t)) >>>
        ("Done" (quit))))))

(tm-define (open-search)
  (:interactive #t)
  (let* ((u  (current-buffer))
         (st (list-remove-duplicates (rcons (get-style-list) "macro-editor")))
         (aux "tmfs://aux/search"))
    (buffer-set-master aux u)
    (dialogue-window (search-widget u st aux) noop "Search")))
