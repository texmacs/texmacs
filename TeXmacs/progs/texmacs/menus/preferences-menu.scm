
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
;; The Preferences menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define page-setup-tree
  '((enum ("Preview command" "preview command")
	  "default" "ggv" "ghostview" "gv" "kghostview"
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
	  ("Emacs" "emacs")
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
	(toggle ("Context dependent icons" "context dependent icons"))
	(toggle ("User provided icons" "user provided icons"))
	(toggle ("Status bar" "status bar"))
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
	(enum ("A modifier" "A")
	      ("Default" "default")
	      ("Equivalent for#Mod1" "Mod1")
	      ("Equivalent for#Mod2" "Mod2")
	      ("Equivalent for#Mod3" "Mod3")
	      ("Equivalent for#Mod4" "Mod4")
	      ("Equivalent for#Mod5" "Mod5"))
	(enum ("M modifier" "M")
	      ("Default" "default")
	      ("Equivalent for#Mod1" "Mod1")
	      ("Equivalent for#Mod2" "Mod2")
	      ("Equivalent for#Mod3" "Mod3")
	      ("Equivalent for#Mod4" "Mod4")
	      ("Equivalent for#Mod5" "Mod5"))
	(enum ("H modifier" "H")
	      ("Default" "default")
	      ("Equivalent for#Mod1" "Mod1")
	      ("Equivalent for#Mod2" "Mod2")
	      ("Equivalent for#Mod3" "Mod3")
	      ("Equivalent for#Mod4" "Mod4")
	      ("Equivalent for#Mod5" "Mod5"))
	---
	(enum ("Alt key" "alt")
	      ("Do not remap" "default")
	      ("Map to#A modifier" "A")
	      ("Map to#M modifier" "M")
	      ("Map to#H modifier" "H"))
	(enum ("Meta key" "meta")
	      ("Do not remap" "default")
	      ("Map to#A modifier" "A")
	      ("Map to#M modifier" "M")
	      ("Map to#H modifier" "H"))
	(enum ("Windows key" "windows")
	      ("Do not remap" "default")
	      ("Map to#A modifier" "A")
	      ("Map to#M modifier" "M")
	      ("Map to#H modifier" "H"))
	(enum ("Caps-lock key" "caps-lock")
	      ("Do not remap" "default")
	      ("Map to#A modifier" "A")
	      ("Map to#M modifier" "M")
	      ("Map to#H modifier" "H"))
	---
	(enum ("Cyrillic input method" "cyrillic input method")
	      ("Translit" "translit")
	      ("Jcuken" "jcuken")
	      ("Yawerty" "yawerty")
	      ("Koi8-r" "koi8-r")
	      ("Cp1251" "cp1251"))
	(enum ("Automatic quotes" "automatic quotes")
	      ("Default" "default")
	      ("None" "none")
	      ("Dutch" "dutch")
	      ("English" "english")
	      ("French" "french")
	      ("German" "german")
	      ("Spanish" "spanish")
	      ("Swiss" "swiss"))
	(toggle ("Automatically close brackets"
		 "automatically close brackets")))
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
		  ("Iso-8859-1" "iso-8859-1")
		  ("Utf-8" "utf-8")))
	(-> "Verbatim -> TeXmacs"
	    (toggle ("Wrap lines"
		     "verbatim->texmacs:wrap"))
	    (enum ("Encoding" "verbatim->texmacs:encoding")
		  ("Iso-8859-1" "iso-8859-1")
		  ("Utf-8" "utf-8"))))
    (-> "Utilities"
	(-> "Scripts"
	    ("None" (set-preference "scripting language" "none"))
	    ---
	    (link scripts-preferences-menu))
	(toggle ("Remote connections" "remote connections"))
	(toggle ("Linking tool" "linking tool"))
	(toggle ("Versioning tool" "versioning tool")))
    ---
    (enum ("Autosave" "autosave")
	  ("5 s" "5")
	  ("30 s" "30")
	  ("120 s" "120")
	  ("300 s" "300")
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
	(else (cons (list (id-or-car (car l))
			  `(set-preference ,s ,(id-or-cadr (car l))))
		    (compute-preferences-enum s (cdr l))))))

(define (compute-preferences-menu-sub l)
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
	(else (map-in-order compute-preferences-menu-sub l))))

(tm-define (compute-preferences-menu l)
  (eval `(menu-dynamic ,@(compute-preferences-menu-sub l))))

(menu-bind page-setup-menu ,@(compute-preferences-menu page-setup-tree))
(menu-bind preferences-menu ,@(compute-preferences-menu preferences-tree))
