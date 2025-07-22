
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ia.scm
;; DESCRIPTION : IA tools
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc ia))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ia-correct)
  (when (selection-active-any?)
    (with lan (get-env "language")
      (with t (selection-tree)
        (clipboard-cut "primary")
        (with r (llama-correct t lan)
          (insert r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ia-translate into)
  (when (selection-active-any?)
    (with from (get-env "language")
      (with t (selection-tree)
        (clipboard-cut "primary")
        (with r (llama-translate t from into)
          (insert r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste while compressing non natural language text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ia-copy)
  (when (selection-active-any?)
    (clipboard-set "primary" (compress-html (selection-tree) 1))))

(tm-define (ia-cut)
  (when (selection-active-any?)
    (ia-copy)
    (clipboard-cut "dummy")))

(define (clipboard-get* key)
  (with t (clipboard-get key)
    (cond ((not (tm-func? t 'tuple)) t)
          ((< (tm-arity t) 2) t)
          ((tm-equal? (tm-ref t 0) "texmacs")
           (tm->string (tm-ref t 1)))
          ((tm-equal? (tm-ref t 0) "extern")
           (tm->string (tm-ref t 1)))
          (else t))))

(tm-define (ia-paste)
  (with t (decompress-html (clipboard-get* "extern") 1)
    (insert t)))
