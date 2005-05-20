
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preferences-menu.scm
;; DESCRIPTION : the preferences menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
	  "lpr" "lp"
	  *)
    (enum ("Paper type" "paper type")
	  "A3" "A4" "A5" "B4" "B5" "B6"
	  "Letter" "Legal" "Executive"
	  *)
    (enum ("Printer dpi" "printer dpi")
	  "150" "200" "300" "400" "600" "800" "1200"
	  *)
    (enum ("Font type" "font type")
	  "EC fonts" "CM fonts" "True Type")))

(tm-define preferences-tree
  `((enum ("Look and feel" "look and feel")
	  ("Emacs" "emacs")
	  ("Windows" "windows"))
;   (enum ("Profile" "profile")
;	  ("Beginner" "beginner")
;	  ("Normal" "normal")
;	  ("Advanced" "advanced"))
    (enum ("Language" "language")
	  ("British" "british")
	  ("Czech" "czech")
	  ("Dutch" "dutch")
	  ("Danish" "danish")
	  ("English" "english")
	  ("Finnish" "finnish")
	  ("French" "french")
	  ("German" "german")
	  ("Hungarian" "hungarian")
	  ("Italian" "italian")
	  ("Polish" "polish")
	  ("Portuguese" "portuguese")
	  ("Romanian" "romanian")
	  ("Russian" "russian")
	  ("Slovene" "slovene")
	  ("Spanish" "spanish")
	  ("Swedish" "swedish")
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
    (-> "View"
	(toggle ("Header" "header"))
	(toggle ("Main icon bar" "main icon bar"))
	(toggle ("Context dependent icons" "context dependent icons"))
	(toggle ("User provided icons" "user provided icons"))
	(toggle ("Status bar" "status bar"))
	(enum ("Shrinking factor" "shrinking factor")
	      "1" "2" "3" "4" "5" "7" "10" *))
    (-> "Converters"
;	(-> "Html"
;	    (group "TeXmacs -> Html")
;	    (toggle ("Use CSS" "texmacs->html:css"))
;	    (toggle ("Use MathML" "texmacs->html:mathml")))
	(-> "LaTeX"
	    (group "TeXmacs -> LaTeX")
	    (toggle ("Keep unrecognized styles and packages"
		     "texmacs->latex:faithful-style"))
	    (toggle ("Export bibliographies as links"
		     "texmacs->latex:indirect-bib"))))
    (enum ("Security" "security")
	  ("Accept no scripts" "accept no scripts")
	  ("Prompt on scripts" "prompt on scripts")
	  ("Accept all scripts" "accept all scripts"))
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
