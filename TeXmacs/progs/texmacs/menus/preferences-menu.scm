
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
    (convert latex init-latex)
    (language natural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferred scripting language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (scripts-preferences-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (scripts-list)))
    (for (name l)
      (with menu-name (scripts-name name)
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
          *)))

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
;         ("Beginner" "beginner")
;         ("Normal" "normal")
;         ("Advanced" "advanced"))
    (enum ("Complex actions" "complex actions")
          ("Through the menus" "menus")
          ("Through popup windows" "popups"))
    (enum ("Interactive questions" "interactive questions")
          ("On the footer" "footer")
          ("In popup windows" "popup"))
    (enum ("Details in menus" "detailed menus")
          ("Simplified menus" "simple")
          ("Detailed menus" "detailed"))
    (enum ("Buffer management" "buffer management")
          ("Documents in separate windows" "separate")
          ("Multiple documents share window" "shared"))
    ---
    (enum ("Language" "language")
          ,@(map (lambda (lan) (list (upcase-first lan) lan))
                 supported-languages))
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
        (enum ("Spacebar in text mode" "text spacebar")
              ("Default" "default")
              ---
              ("Allow multiple spaces" "allow multiple spaces")
              ("Glue multiple spaces" "glue multiple spaces")
              ("No multiple spaces" "no multiple spaces"))
        (enum ("Spacebar in math mode" "math spacebar")
              ("Default" "default")
              ---
              ("Allow spurious spaces" "allow spurious spaces")
              ("Avoid spurious spaces" "avoid spurious spaces")
              ("No spurious spaces" "no spurious spaces"))
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
;           (toggle ("Use CSS" "texmacs->html:css"))
            (toggle ("Use MathML" "texmacs->html:mathml"))
            (toggle ("Export formulas as images" "texmacs->html:images")))
        (-> "LaTeX -> TeXmacs"
            (toggle ("Import sophisticated objects as pictures"
                     "latex->texmacs:fallback-on-pictures"))
            ---
            (toggle ("Keep track of source code"
                     "latex->texmacs:source-tracking"))
            (toggle ("Only convert changes with respect to tracked version"
                     "latex->texmacs:conservative"))
            (when (== (get-preference "latex->texmacs:source-tracking") "on")
              (toggle ("Guarantee transparent source tracking"
                       "latex->texmacs:transparent-source-tracking"))))
        (-> "TeXmacs -> LaTeX"
            (toggle ("Replace unrecognized styles"
                     "texmacs->latex:replace-style"))
            (toggle ("Expand unrecognized macros"
                     "texmacs->latex:expand-macros"))
            (toggle ("Expand user-defined macros"
                     "texmacs->latex:expand-user-macros"))
            (toggle ("Export bibliographies as links"
                     "texmacs->latex:indirect-bib"))
            (toggle ("Allow for macro definitions in preamble"
                     "texmacs->latex:use-macros"))
            (enum ("Encoding" "texmacs->latex:encoding")
                  ("Strict Ascii" "ascii")
                  ("Cork charset with TeX catcode definition in preamble" "cork")
                  ("Utf-8 with inputenc LaTeX package" "utf-8"))
            ---
            (toggle ("Keep track of source code"
                     "texmacs->latex:source-tracking"))
            (toggle ("Only convert changes with respect to tracked version"
                     "texmacs->latex:conservative"))
            (when (== (get-preference "texmacs->latex:source-tracking") "on")
              (toggle ("Guarantee transparent source tracking"
                       "texmacs->latex:transparent-source-tracking"))
              (toggle ("Store tracking information in LaTeX files"
                       "texmacs->latex:attach-tracking-info"))))
        (-> "TeXmacs -> Verbatim"
            (toggle ("Wrap lines"
                     "texmacs->verbatim:wrap"))
            (enum ("Encoding" "texmacs->verbatim:encoding")
                  ("Automatic" "auto")
                  ("Cork" "cork")
                  ("Iso-8859-1" "iso-8859-1")
                  ("Utf-8" "utf-8")))
        (-> "Verbatim -> TeXmacs"
            (toggle ("Wrap lines"
                     "verbatim->texmacs:wrap"))
            (enum ("Encoding" "verbatim->texmacs:encoding")
                  ("Automated detection" "auto")
                  ("Cork" "cork")
                  ("Iso-8859-1" "iso-8859-1")
                  ("Utf-8" "utf-8")))
        (-> "TeXmacs -> Image"
            (enum ("Format" "texmacs->graphics:format")
                  ("Svg" "svg")
                  ("Eps" "eps")
                  ("Png" "png")))
        (when (and (supports-native-pdf?) (supports-ghostscript?))
          (-> "TeXmacs -> Pdf/Postscript"
              (toggle ("Produce native Pdf" "native pdf"))
              (toggle ("Produce native Postscript" "native postscript"))
              (toggle ("Expand beamer slides" "texmacs->pdf:expand slides"))
	      (toggle ("Distill encapsulated Pdf files" "texmacs->pdf:distill inclusion"))
	      (toggle ("Check exported files" "texmacs->pdf:check"))
	      (enum ("Pdf version" "texmacs->pdf:version")
		    ("Default" "default")
		    ("1.4" "1.4")
		    ("1.5" "1.5")
		    ("1.6" "1.6")
		    ("1.7" "1.7")))))
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
        (toggle ("Database tool" "database tool"))
        (toggle ("Debugging tool" "debugging tool"))
        (toggle ("Linking tool" "linking tool"))
        (toggle ("Remote tool" "remote tool"))
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
