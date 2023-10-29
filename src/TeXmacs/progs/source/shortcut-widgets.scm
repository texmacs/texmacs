
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
	(source macro-widgets)
        (utils library cursor)))

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

(tm-widget ((shortcuts-editor u) quit)
  (padded
    (horizontal
      (resize "125px" "200px"
        (refreshable "shortcuts-list"
          (choice (and-let* ((sh (decode-shortcut answer))
                             (cmd (get-user-shortcut sh)))
                    (global-set u :sh sh)
                    (global-set u :cmd cmd)
                    (set-shortcut u sh)
                    (refresh-now "current-shortcut"))
                  (map encode-shortcut (user-shortcuts-list))
                  (encode-shortcut (global-ref u :sh)))))
      // //
      (vertical
        (aligned
          (item (text "Shortcut")
            (resize "350px" "30px"
              (texmacs-input `(document (preview-shortcut ,(global-ref u :sh)))
                             `(style (tuple "generic" "shortcut-editor")) u)))
          (item (text "Command")
            (refreshable "current-shortcut"
              (input (global-set u :cmd answer) "string"
                     (list (global-ref u :cmd) "") "350px"))))
        (glue #f #t 0 0)
        (hlist
          >>
          (explicit-buttons
            ("Remove" (and-with sh (get-shortcut u)
                        (global-set u :sh "")
                        (remove-user-shortcut sh)
                        (refresh-now "shortcuts-list")))
            // //
            ("Clear" (set-shortcut u ""))
            // //
            ("Apply" (and-with sh (get-shortcut u)
                       (global-set u :sh sh)
                       (set-user-shortcut sh (global-ref u :cmd))
                       (refresh-now "shortcuts-list")))
            // //
            ("Ok" (begin
                    (and-with sh (get-shortcut u)
                      (set-user-shortcut sh (global-ref u :cmd)))
                    (quit)))))))))

(tm-tool* (shortcuts-tool win u)
  (:name "Edit keyboard shortcut")
  (padded
    (vertical
      (aligned
        (item (text "Shortcut")
          (resize "250px" "30px"
            (texmacs-input `(document (preview-shortcut ,(global-ref u :sh)))
                           `(style (tuple "generic" "shortcut-editor")) u)))
        (item (text "Command")
          (refreshable "current-shortcut"
            (input (global-set u :cmd answer) "string"
                   (list (global-ref u :cmd) "") "250px"))))
      ======
      (division "plain"
        (hlist >>
          ("Remove" (and-with sh (get-shortcut u)
                      (global-set u :sh "")
                      (remove-user-shortcut sh)
                      (refresh-now* win "shortcuts-list")))
          // //
          ("Clear" (set-shortcut u ""))
          // //
          ("Apply" (and-with sh (get-shortcut u)
                     (global-set u :sh sh)
                     (set-user-shortcut sh (global-ref u :cmd))
                     (refresh-now* win "shortcuts-list")))))))
  ===
  (division "plain"
    (division "title"
      (text "List of keyboard shortcuts")))
  (centered
    (resize "200px" "200px"
      (refreshable "shortcuts-list"
        (scrollable
          (choice (and-let* ((sh (decode-shortcut answer))
                             (cmd (get-user-shortcut sh)))
                    (global-set u :sh sh)
                    (global-set u :cmd cmd)
                    (set-shortcut u sh)
                    (refresh-now* win "current-shortcut"))
                  (map encode-shortcut (user-shortcuts-list))
                  (encode-shortcut (global-ref u :sh))))))))

(tm-define (open-shortcuts-editor . opt)
  (:interactive #t)
  (let* ((b (current-buffer))
         (u (string-append "tmfs://aux/edit-shortcuts"))
         (sh (if (null? opt) "" (car opt)))
         (cmd (if (or (null? opt) (null? (cdr opt))) "" (cadr opt)))
         (tool (list 'shortcuts-tool u)))
    (buffer-set-master u b)
    (global-set u :sh sh)
    (global-set u :cmd cmd)
    (if (side-tools?)
        (tool-focus :right tool u)
        (dialogue-window (shortcuts-editor u)
                         (lambda x (noop))
                         "Shortcuts editor" u))))
