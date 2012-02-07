
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preferences-menu.scm
;; DESCRIPTION : the preferences menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus preferences-menu)
  (:use
    (utils edit auto-close)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-view)
    (texmacs texmacs tm-print)
    (texmacs keyboard config-kbd)
    (convert latex init-latex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferred scripting language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (scripts-preferences-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (list-sort supported-scripts-list string<=?)))
    (for (name l)
      (with menu-name (ahash-ref supported-scripts-table name)
        ((eval menu-name) (set-preference "scripting language" name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Preferences menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define page-setup-tree
  '((enum ("Preview command" "preview command")
	  "default" "ggv" "ghostview" "gv" "kghostview" "open"
	  *)
    (enum ("Printing command" "printing command")
	  "lpr" "lp" "pdq"
	  *)
    (enum ("Paper type" "paper type")
	  "A3" "A4" "A5" "B4" "B5" "B6"
	  "Letter" "Legal" "Executive"
	  *)
    (enum ("Printer dpi" "printer dpi")
	  "150" "200" "300" "400" "600" "800" "1200"
	  *)
    (enum ("Font type" "font type")
	  ("Metafont bitmaps only" "Metafont only")
	  ("Metafont and available type 1" "Metafont + Type 1")
	  ("Type 1 with metafont fallback" "Type 1 + Metafont")
	  ("Type 1 only" "Type 1 only"))))

(tm-define preferences-tree
  `((enum ("Look and feel" "look and feel")
	  ("Default" "default")
	  ---
	  ("Emacs" "emacs")
	  ("Gnome" "gnome")
	  ("KDE" "kde")
	  ("Mac OS" "macos")
	  ("Windows" "windows"))
;   (enum ("Profile" "profile")
;	  ("Beginner" "beginner")
;	  ("Normal" "normal")
;	  ("Advanced" "advanced"))
    (enum ("Interactive questions" "interactive questions")
	  ("On the footer" "footer")
	  ("In popup windows" "popup"))
    (enum ("Details in menus" "detailed menus")
	  ("Simplified menus" "simple")
	  ("Detailed menus" "detailed"))
    (-> "View"
	(toggle ("Header" "header"))
	(toggle ("Main icon bar" "main icon bar"))
	(toggle ("Mode dependent icons" "mode dependent icons"))
	(toggle ("Focus dependent icons" "focus dependent icons"))
	(toggle ("User provided icons" "user provided icons"))
	(toggle ("Status bar" "status bar"))
	---
	(enum ("Shrinking factor" "shrinking factor")
	      "1" "2" "3" "4" "5" "7" "10" *))
    ---
    (enum ("Language" "language")
	  ("British" "british")
	  ("Bulgarian" "bulgarian")
	  ("Chinese" "chinese")
	  ("Czech" "czech")
	  ("Dutch" "dutch")
	  ("Danish" "danish")
	  ("English" "english")
	  ("Finnish" "finnish")
	  ("French" "french")
	  ("German" "german")
	  ("Hungarian" "hungarian")
	  ("Italian" "italian")
	  ("Japanese" "japanese")
	  ("Korean" "korean")
	  ("Polish" "polish")
	  ("Portuguese" "portuguese")
	  ("Romanian" "romanian")
	  ("Russian" "russian")
	  ("Slovene" "slovene")
	  ("Spanish" "spanish")
	  ("Swedish" "swedish")
	  ("Taiwanese" "taiwanese")
	  ("Ukrainian" "ukrainian"))
    (-> "Keyboard"
	(-> "Remote control"
	    (enum ("Left" "ir-left") "pageup" *)
	    (enum ("Right" "ir-right") "pagedown" *)
	    (enum ("Up" "ir-up") "home" *)
	    (enum ("Down" "ir-down") "end" *)
	    (enum ("Center" "ir-center") "return" "S-return" *)
	    (enum ("Play" "ir-play") "F5" *)
	    (enum ("Pause" "ir-pause") "escape" *)
	    (enum ("Menu" "ir-menu") "." *))
	(enum ("Cyrillic input method" "cyrillic input method")
	      ("Translit" "translit")
	      ("Jcuken" "jcuken")
	      ("Yawerty" "yawerty")
	      ("Koi8-r" "koi8-r")
	      ("Cp1251" "cp1251"))
	(enum ("Automatic quotes" "automatic quotes")
	      ("Default" "default")
	      ---
	      ("None" "none")
	      ("Dutch" "dutch")
	      ("English" "english")
	      ("French" "french")
	      ("German" "german")
	      ("Spanish" "spanish")
	      ("Swiss" "swiss"))
	(enum ("Automatic brackets" "automatic brackets")
	      ("Disable" "off")
	      ("Inside mathematics" "mathematics")
	      ("Enable" "on")))
    (-> "Printer" . ,page-setup-tree)
    (enum ("Security" "security")
	  ("Accept no scripts" "accept no scripts")
	  ("Prompt on scripts" "prompt on scripts")
	  ("Accept all scripts" "accept all scripts"))
    (-> "Converters"
	(-> "TeXmacs -> Html"
;	    (toggle ("Use CSS" "texmacs->html:css"))
	    (toggle ("Use MathML" "texmacs->html:mathml"))
	    (toggle ("Export formulas as images" "texmacs->html:images")))
	(-> "TeXmacs -> LaTeX"
	    (toggle ("Replace unrecognized styles"
		     "texmacs->latex:replace-style"))
	    (toggle ("Expand unrecognized macros"
		     "texmacs->latex:expand-macros"))
	    (toggle ("Expand user-defined macros"
		     "texmacs->latex:expand-user-macros"))
	    (toggle ("Export bibliographies as links"
		     "texmacs->latex:indirect-bib"))
	    (toggle ("Use catcode definitions in preamble"
		     "texmacs->latex:use-catcodes"))
	    (toggle ("Allow for macro definitions in preamble"
		     "texmacs->latex:use-macros")))
	(-> "TeXmacs -> Verbatim"
	    (toggle ("Wrap lines"
		     "texmacs->verbatim:wrap"))
	    (enum ("Encoding" "texmacs->verbatim:encoding")
		  ("Cork" "cork")
		  ("Iso-8859-1" "iso-8859-1")
		  ("Utf-8" "utf-8")))
	(-> "Verbatim -> TeXmacs"
	    (toggle ("Wrap lines"
		     "verbatim->texmacs:wrap"))
	    (enum ("Encoding" "verbatim->texmacs:encoding")
		  ("Cork" "cork")
		  ("Iso-8859-1" "iso-8859-1")
		  ("Utf-8" "utf-8"))))
    (-> "Mathematics"
        (-> "Keyboard"
            (item ("Enforce brackets to match" (toggle-matching-brackets)))
            (toggle ("Use extensible brackets" "use large brackets")))
        (-> "Context aids"
            (link context-preferences-menu))
        (-> "Semantics"
            (link semantic-math-preferences-menu))
        (-> "Automatic correction"
            (toggle ("Remove superfluous invisible operators"
                     "remove superfluous invisible"))
            (toggle ("Insert missing invisible operators"
                     "insert missing invisible"))
            (toggle ("Homoglyph substitutions"
                     "homoglyph correct")))
        (-> "Manual correction"
            (toggle ("Remove superfluous invisible operators"
                     "manual remove superfluous invisible"))
            (toggle ("Insert missing invisible operators"
                     "manual insert missing invisible"))
            (toggle ("Homoglyph substitutions"
                     "manual homoglyph correct"))))
    (-> "Scripts"
	("None" (set-preference "scripting language" "none"))
	---
	(link scripts-preferences-menu))
    (-> "Tools"
	(toggle ("Debugging tool" "debugging tool"))
	(toggle ("Linking tool" "linking tool"))
	(toggle ("Remote connections" "remote connections"))
	(toggle ("Source macros tool" "source tool"))
	(toggle ("Versioning tool" "versioning tool")))
    ---
    (enum ("Autosave" "autosave")
	  ("5 s" "5")
	  ("30 s" "30")
	  ("120 s" "120")
	  ("300 s" "300")
	  ---
	  ("Disable" "0"))
    (enum ("Bibtex command" "bibtex command")
	  "bibtex" "rubibtex" *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computation of the preference menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id-or-car x)
  (if (string? x) x (car x)))

(define (id-or-cadr x)
  (if (string? x) x (cadr x)))

(define (compute-preferences-entry s)
  `(interactive (lambda (val) (set-preference ,s val))
     ,(upcase-first s)))

(define (compute-preferences-enum s l)
  (cond ((null? l) l)
	((== (car l) '*)
	 (list '--- (list "Other" (compute-preferences-entry s))))
	((== (car l) '---)
	 (cons '--- (compute-preferences-enum s (cdr l))))
	(else (cons (list (id-or-car (car l))
			  `(set-preference ,s ,(id-or-cadr (car l))))
		    (compute-preferences-enum s (cdr l))))))

(tm-define (compute-preferences-menu-sub l)
  (cond ((or (nlist? l) (null? l)) l)
	((== (car l) 'string)
	 (let* ((x (cadr l))
		(s (id-or-car x))
		(v (id-or-cadr x)))
	   (list s (compute-preferences-entry v))))
	((== (car l) 'enum)
	 (let* ((x (cadr l))
		(s (id-or-car x))
		(v (id-or-cadr x)))
	   (cons* '-> s (compute-preferences-enum v (cddr l)))))
	((== (car l) 'toggle)
	 (let* ((x (cadr l))
		(s (id-or-car x))
		(v (id-or-cadr x)))
	   (list s (list 'toggle-preference v))))
	((== (car l) 'item) (cadr l))
	(else (map-in-order compute-preferences-menu-sub l))))

(tm-menu (compute-preferences-menu l)
  (with r (eval (cons* 'menu-dynamic (compute-preferences-menu-sub l)))
    (dynamic r)))

(tm-menu (page-setup-menu)
  (dynamic (compute-preferences-menu page-setup-tree)))

(tm-menu (preferences-menu)
  (dynamic (compute-preferences-menu preferences-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "look and feel"
  ("emacs" "Emacs")
  ("gnome" "Gnome")
  ("kde" "KDE")
  ("macos" "Mac OS")
  ("windows" "Windows"))

(define-preference-names "language"
  ("british" "British")
  ("bulgarian" "Bulgarian")
  ("chinese" "Chinese")
  ("czech" "Czech")
  ("dutch" "Dutch")
  ("danish" "Danish")
  ("english" "English")
  ("finnish" "Finnish")
  ("french" "French")
  ("german" "German")
  ("hungarian" "Hungarian")
  ("italian" "Italian")
  ("japanese" "Japanese")
  ("korean" "Korean")
  ("polish" "Polish")
  ("portuguese" "Portuguese")
  ("romanian" "Romanian")
  ("russian" "Russian")
  ("slovene" "Slovene")
  ("spanish" "Spanish")
  ("swedish" "Swedish")
  ("taiwanese" "Taiwanese")
  ("ukrainian" "Ukrainian"))

(define-preference-names "interactive questions"
  ("footer" "On the footer")
  ("popup" "In popup windows"))

(define-preference-names "detailed menus"
  ("simple ""Simplified menus")
  ("detailed" "Detailed menus"))

(tm-widget (appearance-preferences-widget)
  (aligned
    (text "Look and feel:")
    (enum (set-pretty-preference "look and feel" answer)
          '("Emacs" "Gnome" "KDE" "Mac OS" "Windows")
          (get-pretty-preference "look and feel")
          "10em")
    (text "User interface language:")
    (enum (set-pretty-preference "language" answer)
          '("British" "Bulgarian" "Chinese" "Czech" "Dutch" "Danish"
            "English" "Finnish" "French" "German" "Hungarian" "Italian"
            "Japanese" "Korean" "Polish" "Portuguese" "Romanian" "Russian"
            "Slovene" "Spanish" "Swedish" "Taiwanese" "Ukrainian")
          (get-pretty-preference "language")
          "10em")
    (text "Interactive questions:")
    (enum (set-pretty-preference "interactive questions" answer)
          '("On the footer" "In popup windows")
          (get-pretty-preference "interactive questions")
          "10em")
    (text "Details in menus:")
    (enum (set-pretty-preference "detailed menus" answer)
          '("Simplified menus" "Detailed menus")
          (get-pretty-preference "detailed menus")
          "10em")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "automatic quotes"
  ("default" "Default")
  ("none" "Disabled")
  ("dutch" "Dutch")
  ("english" "English")
  ("french" "French")
  ("german" "German")
  ("spanish" "Spanish")
  ("swiss" "Swiss"))

(define-preference-names "automatic brackets"
  ("off" "Disabled")
  ("mathematics" "Inside mathematics" "mathematics")
  ("on" "Enabled"))

(define-preference-names "cyrillic input method"
  ("translit" "Translit")
  ("jcuken" "Jcuken")
  ("yawerty" "Yawerty")
  ("koi8-r" "Koi8-r")
  ("cp1251" "Cp1251"))

(tm-widget (keyboard-preferences-widget)
  (aligned
    (text "Automatic quotes:")
    (enum (set-pretty-preference "automatic quotes" answer)
          '("Off" "Dutch" "English" "French" "German" "Spanish" "Swiss")
          (get-pretty-preference "automatic quotes")
          "10em")
    (text "Automatic brackets:")
    (enum (set-pretty-preference "automatic brackets" answer)
          '("Disabled" "Enabled" "Inside mathematics")
          (get-pretty-preference "automatic brackets")
          "10em")
    (text "Cyrillic input method:")
    (enum (set-pretty-preference "cyrillic input method" answer)
          '("Translit" "Jcuken" "Yawerty" "Koi8-r" "Cp1251")
          (get-pretty-preference "cyrillic input method")
          "10em")))

;(-> "Remote control"
;    (enum ("Left" "ir-left") "pageup" *)
;    (enum ("Right" "ir-right") "pagedown" *)
;    (enum ("Up" "ir-up") "home" *)
;    (enum ("Down" "ir-down") "end" *)
;    (enum ("Center" "ir-center") "return" "S-return" *)
;    (enum ("Play" "ir-play") "F5" *)
;    (enum ("Pause" "ir-pause") "escape" *)
;    (enum ("Menu" "ir-menu") "." *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (preferences-widget)
  (tabs
    (tab (text "Appearance")
      (centered
        (dynamic (appearance-preferences-widget))))
    (tab (text "Keyboard")
      (centered
        (dynamic (keyboard-preferences-widget))))))

(tm-define (open-preferences)
  (top-window preferences-widget "User preferences"))
