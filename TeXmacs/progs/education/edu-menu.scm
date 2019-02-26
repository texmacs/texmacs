
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
        (generic document-menu)
        (text text-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus for inserting and manipulating educational content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind edu-view-mode-menu
  ("Problem mode" (edu-set-mode :problem))
  ("Solution mode" (edu-set-mode :solution)))

(menu-bind question-menu
  ("Question" (make 'question))
  ("Exercise" (make 'exercise))
  ("Problem" (make 'problem))
  ---
  ("1, 2, 3" (make 'question-arabic))
  ("a, b, c" (make 'question-alpha))
  ("A, B, C" (make 'question-Alpha))
  ("i, ii, iii" (make 'question-roman))
  ("I, II, III" (make 'question-Roman)))

(menu-bind answer-menu
  ("Answer" (make 'answer*))
  ("Solution" (make 'solution*))
  ---
  ("<blacktriangleright>" (make 'answer-item))
  ("1, 2, 3" (make 'answer-arabic))
  ("a, b, c" (make 'answer-alpha))
  ("A, B, C" (make 'answer-Alpha))
  ("i, ii, iii" (make 'answer-roman))
  ("I, II, III" (make 'answer-Roman)))

(menu-bind gap-menu
  ("Inline" (make 'gap))
  ("Wide" (make 'gap-wide))
  ("Multiline" (make 'gap-long))
  ---
  ("Dots" (make 'gap-dots))
  ("Underlined" (make 'gap-underlined))
  ("Box" (make 'gap-box)))

(menu-bind mc-menu
  ("Default" (make-mc 'mc))
  ---
  ("Monospaced" (make-mc 'mc-monospaced))
  ("Horizontal" (make-mc 'mc-horizontal))
  ("Vertical" (make-mc 'mc-vertical)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main menus for inserting and manipulating educational content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind edu-insert-menu
  (-> "Question" (link question-menu))
  (-> "Answer" (link answer-menu))
  (-> "Gap" (link gap-menu))
  (-> "Multiple choice" (link mc-menu)))

;;(menu-bind view-menu
;;  (:require (style-has? "std-edu-dtd"))
;;  (former)
;;  ---
;;  (link edu-view-mode-menu))

(menu-bind text-extra-menu
  (:require (style-has? "std-edu-dtd"))
  (former)
  ---
  (link edu-insert-menu))

(menu-bind edu-view-icons
  (=> (balloon (icon "tm_view.xpm") "Select view mode")
      (link edu-view-mode-menu)))

(menu-bind edu-insert-icons
  (=> (balloon (icon "tm_question.xpm") "Insert a question")
      (link question-menu))
  (=> (balloon (icon "tm_answer.xpm") "Insert an answer")
      (link answer-menu))
  (=> (balloon (icon "tm_gap.xpm") "Insert a gap")
      (link gap-menu))
  (=> (balloon (icon "tm_mc.xpm") "Insert a multiple choice")
      (link mc-menu)))

(menu-bind text-extra-icons
  (:require (style-has? "std-edu-dtd"))
  (former)
  /
  ;;(link edu-view-icons)
  (link edu-insert-icons))

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
