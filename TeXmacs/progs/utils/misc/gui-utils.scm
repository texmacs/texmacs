
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-utils.scm
;; DESCRIPTION : support functions for gui markup in style packages
;; COPYRIGHT   : (C) 2023  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc gui-utils)
  (:use (utils misc tooltip)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-system-font)
  (:secure #t)
  (cond ((os-macos?) "Lucida Grande")
        (else "Linux Biolinum")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (focus-on-canvas)
  (with mas (buffer-get-master (current-buffer))
    (if (not mas) (keyboard-focus-on "canvas")
        (begin
          (buffer-focus* mas)
          (keyboard-focus-on "canvas")))))

(tm-define (gui-on-select type x y cmd)
  (:secure #t)
  ;;(display* "gui-on-select " type ", " x ", " y ", " cmd "\n")
  (set! type (tm->stree type))
  (set! cmd (tm->stree cmd))
  (or (and (in? type (list "click" "drag")) "done")
      (and (== type "select")
           (begin
             (focus-on-canvas)
             (delayed
               (:idle 1)
               (when (string? cmd)
                 ;;(display* "gui-on-select " cmd "\n")
                 (secure-eval (string->object cmd)))
               (close-tooltip))
             (update-menus)
             "done"))))

(tm-define (gui-on-toggle type x y cmd)
  (:secure #t)
  ;;(display* "gui-on-toggle " type ", " x ", " y ", " cmd "\n")  
  (let* ((val (tm-ref cmd :up 0))
         (new (object->string (and val (tm-equal? val "false")))))
    (when (and (== (tm->stree type) "select") val)
      (cond ((tm-equal? val "true") (tree-assign! val "false"))
            ((tm-equal? val "false") (tree-assign! val "true"))))
    (and-let* ((cmd* (tm->stree cmd))
               (ncmd (string-append "(with answer " new " " cmd* ")")))
      (gui-on-select type x y ncmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard emulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define emu-modifier-table (make-ahash-table))

(tm-define (emu-toggle-modifier t)
  (:secure #t)
  (ahash-set! emu-modifier-table t
              (not (ahash-ref emu-modifier-table t)))
  (refresh-now "custom-keyboard"))

(tm-define (emu-active-modifier? t)
  (ahash-ref emu-modifier-table t))

(tm-define (emu-key t)
  (:secure #t)
  (with s (tm->stree t)
    (when (string? s)
      (if (emu-active-modifier? "Control")
          (set! s (string-append "C-" s)))
      (if (emu-active-modifier? "Alt")
          (set! s (string-append "A-" s)))
      (if (emu-active-modifier? "Meta")
          (set! s (string-append "M-" s)))
      (key-press s)
      (if (not (emu-active-modifier? "Lock"))
          (set! emu-modifier-table (make-ahash-table))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists via tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (as-symbol x fall-back)
  (if (tm-atomic? x) (string->symbol (tm->string x)) fall-back))
(define (as-string x fall-back)
  (if (tm-atomic? x) (tm->string x) fall-back))

(define (subtable-format*)
  (with cw  (list 'cwith "1" "-1" "1" "-1")
    `((cwith "1" "-1" "1" "-1" "cell-hpart" "0.00000001")
      (cwith "1" "-1" "-1" "-1" "cell-hpart" "0.0001")
      (cwith "1" "-1" "1" "-1" "cell-vpart" "0.00000001")
      (cwith "-1" "-1" "1" "-1" "cell-vpart" "0.0001"))))

(define subtable-format (subtable-format*))

(define (gui-hlist-pair t pos)
  (cond ((tm-func? t 'glue 4)
         (let* ((col  (number->string (+ pos 1)))
                (cw   (list 'cwith "1" "1" col col))
                (ext? (tm-equal? (tm-ref t 0) "true"))
                (w    (tm->stree (tm-ref t 2)))
                (h    (tm->stree (tm-ref t 3)))
                (mode (if ext? "auto" "exact")))
           (cons `((,@cw "cell-valign" "t")
                   (,@cw "cell-width" ,w)
                   (,@cw "cell-height" ,h)
                   (,@cw "cell-hmode" ,mode)
                   (,@cw "cell-vmode" ,mode)
                   ,@(if ext? (list `(,@cw "cell-hpart" "1")) (list)))
                 "")))
        ((tm-func? t 'vlist)
         (with rew (gui-vlist-table* "raw-table" t)
           (cons (list) `(cell (subtable (tformat ,@subtable-format
                                                  ,@(cdr rew)))))))
        (else (cons (list) `(cell (document ,t))))))

(define (gui-hlist-pairs l pos)
  (if (null? l) l
      (let* ((head (gui-hlist-pair  (car l) pos))
             (tail (gui-hlist-pairs (cdr l) (+ pos 1))))
        (cons head tail))))

(tm-define (gui-hlist-table* tag* t)
  (let* ((tag   (as-symbol tag* 'stack))
         (pairs (gui-hlist-pairs (tree-children t) 0)))
    ;;(display* "gui-hlist-table " (tm->stree tag*) ", " (tm->stree t) "\n")
    ;;(for (p pairs)
    ;;  (display* "---> " p "\n"))
    `(,tag (tformat ,@(append-map car pairs) (table (row ,@(map cdr pairs)))))))

(tm-define (gui-hlist-table tag* t)
  (:secure #t)
  (let* ((stretch? (lambda (cw)
                     (and (tm-func? cw 'cwith 6)
                          (tm-equal? (tm-ref cw 4) "cell-hpart"))))
         (r (gui-hlist-table* tag* t)))
    (if (list-or (map stretch? (cDr (tm-children (tm-ref r 0)))))
        `(,(tm-label r) (tformat (twith "table-width" "1par")
                                 (twith "table-hmode" "exact")
                                 ,@(tm-children (tm-ref r 0))))
        r)))

(define (gui-vlist-pair t pos)
  (cond ((tm-func? t 'glue 4)
         (let* ((row  (number->string (+ pos 1)))
                (cw   (list 'cwith row row "1" "1"))
                (ext? (tm-equal? (tm-ref t 1) "true"))
                (w    (tm->stree (tm-ref t 2)))
                (h    (tm->stree (tm-ref t 3)))
                (mode (if ext? "auto" "exact")))
           (cons `((,@cw "cell-valign" "t")
                   (,@cw "cell-width" ,w)
                   (,@cw "cell-height" ,h)
                   (,@cw "cell-hmode" ,mode)
                   (,@cw "cell-vmode" ,mode)
                   ,@(if ext? (list `(,@cw "cell-vpart" "1")) (list)))
                 `(row ""))))
        ((tm-func? t 'hlist)
         (with rew (gui-hlist-table* "raw-table" t)
           (cons (list) `(row (cell (subtable (tformat ,@subtable-format
                                                       ,@(cdr rew))))))))
        (else (cons (list) `(row (cell (document ,t)))))))

(define (gui-vlist-pairs l pos)
  (if (null? l) l
      (let* ((head (gui-vlist-pair  (car l) pos))
             (tail (gui-vlist-pairs (cdr l) (+ pos 1))))
        (cons head tail))))

(tm-define (gui-vlist-table* tag* t)
  (:secure #t)
  (let* ((tag   (as-symbol tag* 'stack))
         (pairs (gui-vlist-pairs (tree-children t) 0)))
    ;;(display* "gui-vlist-table " (tm->stree tag*) ", " (tm->stree t) "\n")
    ;;(for (p pairs)
    ;;  (display* "===> " p "\n"))
    `(,tag (tformat ,@(append-map car pairs) (table ,@(map cdr pairs))))))


(tm-define (gui-vlist-table tag* t)
  (:secure #t)
  (let* ((r (gui-vlist-table* tag* t)))
    ;;(display* "r= " r "\n")
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gui-tile-item t)
  `(cell (document ,t)))

(define (gui-tiled-row l cols)
  (let* ((r (map (lambda (x) "") (.. (length l) cols)))
         (a (append l r)))
    `(row ,@(map gui-tile-item a))))

(define (gui-tiled-rows l cols)
  (if (> (length l) cols)
      (cons (gui-tiled-row (sublist l 0 cols) cols)
            (gui-tiled-rows (sublist l cols (length l)) cols))
      (list (gui-tiled-row l cols))))

(tm-define (gui-tiled tag* t)
  (:secure #t)
  (let* ((tag (as-symbol tag* 'stack))
         (c (tree-children t))
         (cols (or (and (nnull? c) (string->number (tree->string (car c)))) 8))
         (args (if (null? c) c (cdr c))))
    `(,tag (table ,@(gui-tiled-rows args cols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choice lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gui-choice-item x f on? off-tag on-tag)
  (let* ((p (and (tree? x) (tree->path x)))
         (s (string-append "(" f " '" (object->string p) ")"))
         (cmd (if p s "(noop)")))
    `(,(if (on? x) on-tag off-tag) ,x ,cmd)))

(define (check-toggle old val all)
  (if (func? old 'tuple) (set! old (cdr old)))
  (if (nlist? old) (set! old (list)))
  (let* ((rem (list-remove old val))
         (tog (if (in? val old) rem (cons val old)))
         (new (list-filter all (cut in? <> tog))))
    (cons 'tuple (list-remove-duplicates new))))

(tm-define (gui-on-choice tag p)
  (:secure #t)
  (and-with t (path->tree p)
    (and-with u (tm-ref t :up)
      (when (tree-is? u tag)
        (let* ((val (tree->stree t))
               (old (tree->stree (tree-ref u 1)))
               (all (cddr (tree->stree u)))
               (new (if (== tag 'choice-list) val
                        (check-toggle old val all)))
               (sel (if (func? new 'tuple) (cdr new) new))
               (c   (as-string (tree-ref u 0) "(noop)"))
               (cmd (string-append "(with answer '" (object->string sel)
                                   " " c ")")))
          (focus-on-canvas)
          (close-tooltip)
          (tree-set (tree-ref u 1) new)
          (when (tree-in? u :up '(input-popup input-list))
            (tree-set (tree-ref u :up 3) new))
          (delayed
            (:idle 1)
            ;;(display* "gui-on-choice: " cmd "\n")
            (secure-eval (string->object cmd)))
          (update-menus))))))

(tm-define (gui-choice-list list-tag* off-tag* on-tag* t)
  (:secure #t)
  (let* ((list-tag (as-symbol list-tag* 'vlist))
         (off-tag (as-symbol off-tag* 'menu-button))
         (on-tag (as-symbol on-tag* 'menu-button-pressed))
         (c (tree-children t))
         (n (length c))
         (cur (if (>= n 2) (tm->stree (cadr c)) '(uninit)))
         (args (if (>= n 2) (cddr c) (list)))
         (on? (lambda (x) (== (tm->stree x) cur)))
         (f "gui-on-choice 'choice-list"))
    `(,list-tag ,@(map (cut gui-choice-item <> f on? off-tag on-tag) args))))

(tm-define (gui-check-list list-tag* off-tag* on-tag* t)
  (:secure #t)
  (let* ((list-tag (as-symbol list-tag* 'vlist))
         (off-tag (as-symbol off-tag* 'menu-button))
         (on-tag (as-symbol on-tag* 'menu-button-pressed))
         (c (tree-children t))
         (n (length c))
         (cur (if (>= n 2) (tm->stree (cadr c)) '(uninit)))
         (sel (if (func? cur 'tuple) (cdr cur) (list)))
         (args (if (>= n 2) (cddr c) (list)))
         (on? (lambda (x) (in? (tm->stree x) sel)))
         (f "gui-on-choice 'check-list"))
    `(,list-tag ,@(map (cut gui-choice-item <> f on? off-tag on-tag) args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-input-context? t)
  (tree-in? t '(input-field input-popup)))

(tm-define (gui-input-relay type fun key time)
  ;;(display* "Relay " type ", " key ", " time "\n")
  (when (== key "return")
    (fun)))

(tm-define (keyboard-press key time)
  (:require (tree-innermost gui-input-context?))
  (with t (tree-innermost gui-input-context?)
    (when (!= key "return")
      (former key time))
    (and-with t* (tree-innermost gui-input-context?)
      (when (and (tree? t) (== (tree->path t) (tree->path t*)))
        (let* ((type (tree->stree (tree-ref t 0)))
               (cmd  (as-string (tree->stree (tree-ref t 1)) "(noop)"))
               (val  (object->string (tree->stree (tree-ref t 3))))
               (cmd* (string-append "(with answer '" val " " cmd ")"))
               (fun  (lambda ()
                       (delayed
                         (:idle 1)
                         ;;(display* "keyboard-press: " cmd* "\n")
                         (secure-eval (string->object cmd*))
                         (delayed
                           (:pause 25)
                           (close-tooltip)
                           (update-menus))))))
          (gui-input-relay type fun key time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tab-select name)
  (:secure #t)
  (when (tree->path name)
    (and-with passive (tm-ref name :up)
      (and-with nr (tree-index passive)
        (and-with names (tm-ref passive :up)
          (and-with bodies (tm-ref names :up 1)
            (when (tm-func? bodies 'document 1)
              (set! bodies (tree-ref bodies 0)))
            (and-with activate (tm-ref bodies nr)
              (when (and (tm-is? passive 'passive-tab)
                         (tm-in? bodies '(tabs-body switch)))
                (for (t (tm-children names))
                  (tree-assign-node t 'passive-tab))
                (tree-assign-node (tm-ref names nr) 'active-tab)
                (for (t (tm-children bodies))
                  (tree-assign-node t 'hidden))
                (tree-assign-node (tm-ref bodies nr) 'shown)))))))))
