
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : shortcut-widgets.scm
;; DESCRIPTION : widgets for editing keyboard shortcuts
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source shortcut-widgets)
  (:use (source shortcut-edit)
	(source macro-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcut editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-shortcut u)
  (and-with t (buffer-get-body u)
    (if (tm-is? t 'document) (set! t (tm-ref t :last)))
    (and (tm-func? t 'preview-shortcut 1)
         (tm-atomic? (tm-ref t 0))
         (!= (tm->string (tm-ref t 0)) "")
         (tm->string (tm-ref t 0)))))

(define (set-shortcut u sh)
  (and-with t (buffer-get-body u)
    (when (and (tm-func? t 'document 1)
               (tm-func? (tm-ref t 0) 'preview-shortcut 1))
      (tree-set (tm-ref t 0 0) sh))))

(tm-widget ((shortcuts-editor u cur-sh cur-cmd) quit)
  (padded
    (horizontal
      (resize "125px" "200px"
        (refreshable "shortcuts-list"
          (choice (and-let* ((sh (decode-shortcut answer))
                             (cmd (get-user-shortcut sh)))
                    (set! cur-sh sh)
                    (set! cur-cmd cmd)
                    (set-shortcut u sh)
                    (refresh-now "current-shortcut"))
                  (map encode-shortcut (user-shortcuts-list))
                  (encode-shortcut cur-sh))))
      // //
      (vertical
        (aligned
          (item (text "Shortcut")
            (resize "350px" "30px"
              (texmacs-input `(document (preview-shortcut ,cur-sh))
                             `(style (tuple "generic" "shortcut-editor")) u)))
          (item (text "Command")
            (refreshable "current-shortcut"
              (input (set! cur-cmd answer) "string"
                     (list cur-cmd "") "350px"))))
        (glue #f #t 0 0)
        (hlist
          >>
          (explicit-buttons
            ("Remove" (and-with sh (get-shortcut u)
                        (set! cur-sh "")
                        (remove-user-shortcut sh)
                        (refresh-now "shortcuts-list")))
            // //
            ("Apply" (and-with sh (get-shortcut u)
                       (set! cur-sh sh)
                       (set-user-shortcut sh cur-cmd)
                       (refresh-now "shortcuts-list")))
            // //
            ("Ok" (begin
                    (and-with sh (get-shortcut u)
                      (set-user-shortcut sh cur-cmd))
                    (quit)))))))))

(tm-define (open-shortcuts-editor . opt)
  (:interactive #t)
  (let* ((b (current-buffer))
         (u (string-append "tmfs://aux/edit-shortcuts"))
         (sh (if (null? opt) "" (car opt)))
         (cmd (if (or (null? opt) (null? (cdr opt))) "" (cadr opt))))
    (dialogue-window (shortcuts-editor u sh cmd)
                     (lambda x (noop))
                     "Shortcuts editor" u)
    (buffer-set-master u b)))
