
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-menu.scm
;; DESCRIPTION : menus for educational purposes
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-menu)
  (:use (education edu-edit)
        (generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Education menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind edu-menu
  ("Problem mode" (edu-set-mode :problem))
  ("Solution mode" (edu-set-mode :solution))
  ;;("Training mode" (edu-set-mode :training))
  )

(menu-bind course-menu
  (link edu-menu))

(menu-bind exam-menu
  (link edu-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document style options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-edu-options)
  (if (has-style-package? "edu-compact") "Compact" "Regular"))

(menu-bind edu-style-options-menu
  ("Regular" (remove-style-package* "edu-compact"))
  ("Compact" (add-style-package "edu-compact"))
  ---
  (link page-layout-menu))

(tm-menu (focus-style-extra-menu t)
  (:require (and (style-has? "std-edu-dtd")
                 (not (style-has? "beamer-style"))))
  (=> (eval (current-edu-options))
      (link edu-style-options-menu)))

(tm-menu (focus-style-extra-icons t)
  (:require (and (style-has? "std-edu-dtd")
                 (not (style-has? "beamer-style"))))
  (=> (balloon (eval (current-edu-options)) "Style options")
      (link edu-style-options-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-title-menu)
  (:require (style-has? "exam-style"))
  ("Class" (make-doc-data-element 'doc-exam-class))
  ("Date" (make-doc-data-element 'doc-exam-date))
  ("Miscellanous" (make-doc-data-element 'doc-misc))
  ("Note" (make-doc-data-element 'doc-note)))

(tm-menu (focus-title-hidden-menu)
  (:require (style-has? "exam-style"))
  ("Running title" (make-doc-data-element 'doc-running-title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting multiple choice lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind mc-menu
  ("Default" (make-tmlist 'mc))
  ---
  ("Monospaced" (make-mc 'mc-monospaced))
  ("Horizontal" (make-mc 'mc-horizontal))
  ("Vertical" (make-mc 'mc-vertical)))

(menu-bind list-menu
  (:require (style-has? "std-edu-dtd"))
  (former)
  ---
  ("Multiple choice" (make-mc 'mc))
  ---
  ("Monospaced" (make-mc 'mc-monospaced))
  ("Horizontal" (make-mc 'mc-horizontal))
  ("Vertical" (make-mc 'mc-vertical)))

(menu-bind lists-menu
  (:require (style-has? "std-edu-dtd"))
  (former)
  (-> "Multiple choice" (link mc-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizing multiple choice lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind mc-select-menu
  ("Exclusive" (mc-select #f))
  ("Plural" (mc-select #t)))

(menu-bind mc-button-theme-menu
  ("Default" (mc-set-button-theme #f))
  ---
  ("Plain boxes" (mc-set-button-theme 'with-button-box))
  ("Crossed boxes" (mc-set-button-theme 'with-button-box*))
  ("Plain circles" (mc-set-button-theme 'with-button-circle))
  ("Crossed circles" (mc-set-button-theme 'with-button-circle*))
  ---
  ("1, 2, 3" (mc-set-button-theme 'with-button-arabic))
  ("a, b, c" (mc-set-button-theme 'with-button-alpha))
  ("A, B, C" (mc-set-button-theme 'with-button-Alpha))
  ("i, ii, iii" (mc-set-button-theme 'with-button-roman))
  ("I, II, III" (mc-set-button-theme 'with-button-Roman))
  ---
  ("Wide colored"  (mc-set-button-theme 'with-button-ornament)))

(tm-menu (focus-toggle-menu t)
  (:require (mc-context? t)))

(tm-menu (focus-extra-menu t)
  (:require (mc-context? t))
  ---
  (if (mc-exclusive-context? t)
      (-> "Exclusive" (link mc-select-menu)))
  (if (mc-plural-context? t)
      (-> "Plural" (link mc-select-menu)))
  (-> (eval (mc-get-pretty-button-theme))
      (link mc-button-theme-menu)))

(tm-menu (focus-toggle-icons t)
  (:require (mc-context? t)))

(tm-menu (focus-extra-icons t)
  (:require (mc-context? t))
  //
  (mini #t
    (if (mc-exclusive-context? t)
	(=> "Exclusive" (link mc-select-menu)))
    (if (mc-plural-context? t)
	(=> "Plural" (link mc-select-menu))))
  //
  (mini #t
    (=> (eval (mc-get-pretty-button-theme))
        (link mc-button-theme-menu))))
