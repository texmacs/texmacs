
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : spell-edit.scm
;; DESCRIPTION : editing routines for spell checking
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version spell-edit)
  (:use (utils library process)
        (version version-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language for a given tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-get-env t var)
  (let* ((bt (buffer-tree))
         (bp (tree->path bt))
         (tp (tree->path t))
         (val (get-init var))
         (rp (and tp (list-starts? tp bp) (list-drop tp (length bp)))))
    (if (not rp) val
        (tm->stree (tree-descendant-env bt rp var val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-context? t)
  (spell-tag? (tree-label t)))

(tm-define (inside-spell?)
  (not (not (tree-innermost spell-context?))))

(texmacs-modes
  (in-spell% (inside-spell?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving across the differences between both versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-go-to-first)
  (go-start)
  (spell-go-to-next))

(tm-define (spell-go-to-previous)
  (go-to-previous-tag-argument (group-resolve 'spell-tag) 0)
  (recenter-window))

(tm-define (spell-go-to-next)
  (go-to-next-tag-argument (group-resolve 'spell-tag) 0)
  (recenter-window))

(tm-define (spell-go-to-last)
  (go-end)
  (spell-go-to-previous))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retaining particular versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-replace-cache (make-ahash-table))

(tm-define (spell-go-to-first*)
  (go-start)
  (delayed
    (:idle 10)
    (spell-go-to-next*)))

(tm-define (spell-go-to-next*)
  (go-to-next-tag-argument (group-resolve 'spell-tag) 0)
  (when (not (tree-innermost spell-context?))
    (go-to-previous-tag-argument (group-resolve 'spell-tag) 0))
  (let* ((t (tree-innermost spell-context?))
         (i (and t (ahash-ref spell-replace-cache (tm->stree t)))))
    (if i
        (spell-retain i :recurse)
        (recenter-window))))

(tm-define (spell-retain i mode)
  (with-innermost t spell-context?
    (and-with r (tm-ref t (if (== i 0) i (+ i 1)))
      (when (== mode #t)
        (ahash-set! spell-replace-cache (tm->stree t) i))
      (tree-cut t)
      (insert r)
      (when (!= mode :recurse)
        (refresh-tooltips))
      (delayed
        (:idle 10)
        (spell-go-to-next*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-dictionaries (make-ahash-table))
(define spell-accepted-words (make-ahash-table))

(define (spell-dictionary lan)
  (url-append "$TEXMACS_HOME_PATH/langs/natural/spell"
              (string-append lan ".scm")))

(define (spell-load-dictionary lan)
  (when (not (ahash-ref spell-dictionaries lan))
    (with dic (spell-dictionary lan)
      (if (url-exists? dic)
          (with l (load-object dic)
            ;;(display* "\nDictionary " lan " -> " l "\n\n")
            (ahash-set! spell-dictionaries lan l)
            (for (w l)
              (ahash-set! spell-accepted-words (list lan w) #t)))
          (ahash-set! spell-dictionaries lan '())))))

(define (spell-save-dictionary lan)
  (and-with dic (spell-dictionary lan)
    (and-with l (ahash-ref spell-dictionaries lan)
      (save-object dic l))))

(tm-define (spell-retain-permanent)
  (with-innermost t spell-context?
    (with lan (tree-get-env t "language")
      (spell-load-dictionary lan)
      (and-with l (ahash-ref spell-dictionaries lan)
        (with w (tm->stree (tm-ref t 0))
          (ahash-set! spell-dictionaries lan (cons w l))
          (ahash-set! spell-accepted-words (list lan w) #t)
          (spell-save-dictionary lan)))
      (spell-retain 0 #t))))

(tm-define (spell-replace-cached t)
  (let* ((i (ahash-ref spell-replace-cache (tm->stree t)))
         (lan (tree-get-env t "language"))
         (key (list lan (tm->stree (tm-ref t 0)))))
    (spell-load-dictionary lan)
    (cond (i (tree-ref t (if (== i 0) i (+ i 1))))
          ((ahash-ref spell-accepted-words key) (tree-ref t 0))
          (else t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous spell check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-user-words lan)
  (spell-load-dictionary lan)
  (list-filter (ahash-ref spell-dictionaries lan) string?))

(tm-define (continuous-spell-check)
  (delayed
    (:idle 100)
    (if (get-boolean-preference "continuous spell checking")
        (let* ((t (buffer-tree))
               (lan (get-init "language"))
               (errs (tree-spell* lan t (tree->path t) 1000000))
               (old-errs (get-alt-selection "spell-errors")))
          (when (!= errs old-errs)
            (set-alt-selection "spell-errors" errs)
            (notify-change 2048)))
        (when (nnull? (get-alt-selection "spell-errors"))
          (set-alt-selection "spell-errors" (list))
          (notify-change 2048)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manually initiate and terminate spell checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spell-terminate* t)
  (cond ((tree-atomic? t) (noop))
        ((tm-func? t 'spell-error)
         (tree-remove-node! t 0)
         (tree-correct-upwards t))
        (else (for-each spell-terminate* (tree-children t)))))

(tm-define (spell-initiate)
  (process-deactivate 'spell)
  (spell-terminate* (buffer-tree)))

(tm-define (spell-terminate . opt-t)
  (process-deactivate 'spell)
  (with-innermost t spell-context?
    (tree-remove-node! t 0)
    (tree-correct-upwards t)
    (close-tooltips))
  (cond ((null? opt-t)
         (spell-terminate* (buffer-tree)))
        ((tree? (car opt-t))
         (spell-terminate* (car opt-t))))
  (delayed
    (:idle 100)
    (recenter-window)
    (refresh-tooltips)))
