
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : poster-menu.scm
;; DESCRIPTION : menus for posters
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various poster-menu)
  (:use (various poster-edit)
        (generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Poster themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (theme-name theme)
  (let* ((s1 (string-replace theme "-" " "))
         (s2 (string-replace s1 "poster title" "title")))
    (upcase-first s2)))

(menu-bind poster-theme-menu
  (for (theme (poster-themes))
    ((check (eval (theme-name theme)) "v" (has-style-package? theme))
     (add-style-package theme)))
  ---
  ((check "Alternative colors" "v" (has-style-package? "alt-colors"))
   (toggle-style-package "alt-colors"))
  ((check "Framed theorems" "v" (has-style-package? "framed-theorems"))
   (toggle-style-package "framed-theorems")))

(menu-bind poster-title-style-menu
  (for (theme (poster-title-styles))
    ((check (eval (theme-name theme)) "v" (has-style-package? theme))
     (add-style-package theme))))

(menu-bind document-style-extra-menu
  (:require (in-poster?))
  (-> "Poster theme" (link poster-theme-menu))
  (-> "Title style"  (link poster-title-style-menu)))

(tm-menu (focus-style-extra-menu t)
  (:require (in-poster?))
  (-> "Poster theme" (link poster-theme-menu))
  (-> "Title style"  (link poster-title-style-menu))
  (-> "Background color" (link document-background-color-menu)))

(tm-menu (focus-style-extra-icons t)
  (:require (in-poster?))
  (=> (balloon (eval (theme-name (current-poster-theme))) "Poster theme")
      (link poster-theme-menu))
  (=> (balloon (eval (theme-name (current-poster-title-style))) "Title style")
      (link poster-title-style-menu))
  (link focus-background-color-icons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page sizes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-poster-page-size-menu)
  ("A0" (init-page-type "a0"))
  ("A1" (init-page-type "a1"))
  ("A2" (init-page-type "a2"))
  ("A3" (init-page-type "a3"))
  ("A4" (init-page-type "a4")))

(tm-menu (document-page-size-menu)
  (:mode in-poster?)
  ("Default" (default-page-type))
  ---
  (group "Poster formats")
  (link document-poster-page-size-menu)
  ---
  (group "Standard formats")
  (link document-standard-page-formats)
  ---
  ("Other" (interactive init-page-size)))

(tm-menu (document-columns-menu)
  (:mode in-poster?)
  ("One column" (init-env "par-columns" "1"))
  ("Two columns" (init-env "par-columns" "2"))
  ("Three columns" (init-env "par-columns" "3"))
  (if (== (get-init "page-orientation") "landscape")
      ("Four columns" (init-env "par-columns" "4"))
      ("Five columns" (init-env "par-columns" "5"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Poster-blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-document-extra-menu t)
  (:mode in-poster?)
  (:require (document-propose-title?))
  ("Title" (make-poster-title)))

(tm-menu (focus-document-extra-icons t)
  (:mode in-poster?)
  (:require (document-propose-title?))
  (minibar
    ((balloon "Title" "Insert title") (make-poster-title))))

(tm-menu (poster-block-menu)
  (group "Titled block")
  ("Plain" (make-section 'plain-titled-block))
  ("Framed" (make-section 'framed-titled-block))
  ("Alternate" (make-section 'alternate-titled-block))
  ---
  (group "Untitled block")
  ("Plain" (make-section 'plain-block))
  ("Framed" (make-section 'framed-block))
  ("Alternate" (make-section 'alternate-block)))

(tm-define (focus-can-insert-remove? t)
  (:require (poster-block-context? t))
  #t)

(tm-menu (focus-insert-menu t)
  (:require (poster-block-context? t))
  ("Insert similar above" (structured-insert-up))
  ("Insert similar below" (structured-insert-down)))

(tm-menu (focus-insert-icons t)
  (:require (poster-block-context? t))
  ((balloon (icon "tm_insert_up.xpm") "Insert similar block above")
   (structured-insert-up))
  ((balloon (icon "tm_insert_down.xpm") "Insert similar block below")
   (structured-insert-down)))

(tm-menu (focus-toggle-menu t)
  (:require (poster-block-context? t))
  ("Wide block" (float-toggle-wide (focus-tree)))
  ("Titled block" (block-toggle-titled (focus-tree))))

(tm-menu (focus-toggle-icons t)
  (:require (poster-block-context? t))
  ((check (balloon (icon "tm_small_textual.xpm") "Toggle titled") "v"
          (titled-block-context? (focus-tree)))
   (block-toggle-titled (focus-tree)))
  ((check (balloon (icon "tm_wide_float.xpm") "Make block wide") "v"
          (block-wide? (focus-tree)))
   (block-toggle-wide (focus-tree))))
  
