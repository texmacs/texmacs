
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : shortcut-edit.scm
;; DESCRIPTION : editing keyboard shortcuts
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source shortcut-edit)
  (:use (source macro-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Management of the list of user keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define user-shortcuts-file "$TEXMACS_HOME_PATH/system/shortcuts.scm")
(define current-user-shortcuts (list))

(define (apply-user-shortcut sh cmd)
  (and-with val (string->object cmd)
    (eval `(kbd-map (,sh ,val)))))

(define (unapply-user-shortcut sh)
  (eval `(kbd-unmap ,sh)))

(define (load-user-shortcuts)
  (when (url-exists? user-shortcuts-file)
    (set! current-user-shortcuts (load-object user-shortcuts-file))
    (for (entry current-user-shortcuts)
      (with (sh cmd) entry
        (apply-user-shortcut sh cmd)))))

(define (save-user-shortcuts)
  (if (null? current-user-shortcuts)
      (url-remove user-shortcuts-file)
      (save-object user-shortcuts-file current-user-shortcuts)))

(tm-define (init-user-shortcuts)
  (load-user-shortcuts))

(define (shortcut-rewrite s1)
  (let* ((s2 (string-replace s1 "A-" "~A"))
         (s3 (string-replace s2 "C-" "~C"))
         (s4 (string-replace s3 "M-" "~M"))
         (s5 (string-replace s4 "S-" "~S")))
    s5))

(define (shortcut<=? s1 s2)
  (string<=? (shortcut-rewrite s1) (shortcut-rewrite s2)))

(tm-define (user-shortcuts-list)
  (sort (map car current-user-shortcuts) shortcut<=?))

(tm-define (set-user-shortcut sh cmd)
  (set! current-user-shortcuts
        (assoc-set! current-user-shortcuts sh (list cmd)))
  (save-user-shortcuts)
  (apply-user-shortcut sh cmd))

(tm-define (get-user-shortcut sh)
  (and-with val (assoc-ref current-user-shortcuts sh)
    (car val)))

(tm-define (remove-user-shortcut sh)
  (set! current-user-shortcuts
        (assoc-remove! current-user-shortcuts sh))
  (save-user-shortcuts)
  (unapply-user-shortcut sh))

(tm-define (has-user-shortcut? cmd)
  (in? cmd (map cadr current-user-shortcuts)))

(tm-define (encode-shortcut sh)
  (translate (kbd-system-rewrite sh)))

(tm-define (decode-shortcut sh)
  (with all (map (lambda (x) (cons (encode-shortcut x) x))
                 (map car current-user-shortcuts))
    (or (assoc-ref all sh) sh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (keyboard-press key time)
  (if (not (tree-func? (cursor-tree) 'preview-shortcut 1))
      (former key time)
      (and-let* ((t (cursor-tree))
                 (sh (tm-ref t 0))
                 (old (tm->string sh)))
        (if (or (== (cAr (cursor-path)) 0) (== old ""))
            (tree-set! sh key)
            (tree-set! sh (string-append old " " key)))
        (tree-go-to t :end))))
