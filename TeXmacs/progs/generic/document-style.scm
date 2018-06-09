
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-style.scm
;; DESCRIPTION : management of global document style
;; COPYRIGHT   : (C) 2001--2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relations between style files and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-category p) p)
(tm-define (style-category-overrides? p q) (== p q))
(tm-define (style-category-precedes? p q) #f)

(tm-define (style-includes? p q) #f)

(tm-define (style-overrides? p q)
  (style-category-overrides? (style-category p) (style-category q)))

(tm-define (style-precedes? p q)
  (style-category-precedes? (style-category p) (style-category q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu names of style files and packages, and balloon help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table style-synopsis)
(define-table style-menu-name)

(tm-define (style-get-documentation style)
  (with doc (ahash-ref style-synopsis style)
    (and doc (nnull? doc) (car doc))))

(tm-define (style-get-menu-name style)
  (with doc (ahash-ref style-menu-name style)
    (if (and doc (nnull? doc)) (car doc)
        (upcase-first style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting and setting the list of style packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-style-list)
  (with t (tree->stree (get-style-tree))
    (cond ((string? t) (list t))
          ((and (pair? t) (== (car t) 'tuple)) (cdr t))
          (else (texmacs-error "get-style-list ""invalid style ~S" t)))))

(define (normalize-style-list* l)
  (cond ((null? l) l)
        ((list-find (cdr l) (cut style-overrides? <> (car l)))
         (normalize-style-list* (cdr l)))
        ((list-find (cdr l) (cut style-precedes? <> (car l)))
         (normalize-style-list* (cons (cadr l) (cons (car l) (cddr l)))))
        (else (cons (car l) (normalize-style-list* (cdr l))))))

(define (normalize-style-list** l before)
  (cond ((null? l) l)
        ((list-find before (cut style-includes? <> (car l)))
         (normalize-style-list** (cdr l) (cons (car l) before)))
        (else (cons (car l) (normalize-style-list** (cdr l)
                                                    (cons (car l) before))))))

(define (normalize-style-list l2)
  (with l (list-remove-duplicates l2)
    (if (null? l) l
        (cons (car l)
              (normalize-style-list** (normalize-style-list* (cdr l))
                                      (list (car l)))))))

(tm-define (set-style-list l)
  (set-style-tree (tm->tree `(tuple ,@(normalize-style-list l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level routines for style and style package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-no-style?)
  (null? (get-style-list)))

(tm-define (set-no-style)
  (:check-mark "v" has-no-style?)
  (set-style-list '()))

(tm-define (has-main-style? style)
  (with l (get-style-list)
    (and (nnull? l) (== (car l) style))))

(tm-define (set-main-style style)
  (:argument style "Main document style")
  (:default  style "generic")
  (:check-mark "v" has-main-style?)
  (:balloon style-get-documentation)
  (let* ((old (get-style-list))
         (new (if (null? old) (list style) (cons style (cdr old)))))
    (set-style-list new)))

(tm-define (has-style-package? pack)
  (or (in? pack (get-style-list))
      (and (list-find (get-style-list) (cut style-includes? <> pack))
           (not (list-find (get-style-list) (cut style-overrides? <> pack))))))

(tm-define (add-style-package pack)
  (:argument pack "Add package")
  (:check-mark "v" has-style-package?)
  (:balloon style-get-documentation)
  (set-style-list (append (get-style-list) (list pack))))

(tm-define (remove-style-package pack)
  (:argument pack "Remove package")
  (:proposals pack (with l (get-style-list) (if (null? l) l (cdr l))))
  (:balloon style-get-documentation)
  (set-style-list (list-difference (get-style-list) (list pack))))

(tm-define (toggle-style-package pack)
  (:argument pack "Toggle package")
  (:check-mark "v" has-style-package?)
  (:balloon style-get-documentation)
  (if (has-style-package? pack)
      (remove-style-package pack)
      (add-style-package pack)))

(tm-define (edit-package-source name)
  (let* ((style-name (string-append name ".ts"))
         (style-url (url-append "$TEXMACS_STYLE_PATH" style-name))
         (file-name (url-resolve style-url "r")))
    (cursor-history-add (cursor-path))
    (load-buffer file-name)
    (cursor-history-add (cursor-path))))

(tm-define (edit-style-source)
  (with l (get-style-list)
    (when (and (nnull? l) (string? (car l)))
      (edit-package-source (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table with menu names for style packages which are used as style options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table style-menu-name
  ("framed-title"         "Framed titles")
  ("title-bar"            "Title bars")
  ("math-ss"              "Sans serif formulas")

  ("framed-session"       "Framed input fields")
  ("ring-session"         "Ring binder notebook style")
  ("large-formulas"       "Do not break up large formulas")

  ("centered-program"     "Centered programs")
  ("framed-program"       "Framed programs")
  ("compact-list"         "Compact lists")
  ("math-brackets"        "Color according to nesting level")
  ("math-check"           "Highlight errors")
  ("framed-theorems"      "Framed theorems")
  ("hanging-theorems"     "Hanging theorems")
  ("number-europe"        "European numbering style")
  ("number-us"            "US numbering style")
  ("number-long-article"  "Prefix by section number")
  ("captions-above"       "Captions above")

  ("normal-spacing"       "Default spacing")
  ("old-spacing"          "Old style spacing")
  ("wide-spacing"         "Wide spacing")
  ("invisible-multiply"   "Invisible multiplications")
  ("narrow-multiply"      "Narrow multiplications")
  ("regular-multiply"     "Regular multiplications")
  ("invisible-apply"      "Invisible function applications")
  ("narrow-apply"         "Narrow function applications")
  ("regular-apply"        "Regular function applications"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table with brief descriptions for common styles and style packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table style-synopsis
  ("article"        "Default style for writing articles")
  ("beamer"         "Style for laptop presentations")
  ("book"           "Default style for writing books")
  ("browser"        "Style for using TeXmacs as a web browser")
  ("generic"        "Default document style")
  ("letter"         "Default style for writing letters")
  ("seminar"        "Style for presentations using an overhead projector")
  ("source"         "Style for editing style files and packages")

  ("acmconf"        "ACM conference style")
  ("acmsmall"       "Small ACM journal style")
  ("acmlarge"       "Large ACM journal style")
  ("acmtog"         "Two column ACM journal style")
  ("sig-alternate"  "Alternate ACM conference style")
  ("sigconf"        "ACM SIGSAM conference style")
  ("amsart"         "AMS article style")
  ("elsart"         "Elsevier article style")
  ("elsarticle"     "Elsevier article style")
  ("ifac"           "IFAC article style")
  ("jsc"            "Style for Journal of Symbolic Computation")
  ("ieeeconf"       "IEEE conference style")
  ("ieeetran"       "Style for transactions by the IEEE")
  ("aip"            "REVTeX meta-style (American Institute of Physics)")
  ("aps"            "REVTeX meta-style (American Physical Society)")
  ("llncs"          "Style for Springer Lecture Notes in Computer Science")
  ("svjour"         "Article style for Springer journals")
  ("tmarticle"      "TeXmacs alternative article style")

  ("svmono"         "Style for Springer monographs")
  ("tmbook"         "TeXmacs alternative book style")

  ("manual"         "Style for writing technical manuals")
  ("mmxdoc"         "Style for writing Mathemagix documentation")
  ("mmxmanual"      "Style for writing Mathemagix manuals")
  ("tmdoc"          "Style for writing TeXmacs documentation")
  ("tmmanual"       "Style for writing TeXmacs manuals")
  ("tmweb"          "Style for writing pages for the TeXmacs website")

  ("exam"           "Style for exams")

  ("old-beamer"     "Former default style for laptop presentations")
  ("old-book"       "Former default style for writing books")
  ("old-generic"    "Former default document style")
  ("old-letter"     "Former default style for writing letters")
  ("old-seminar"    "Former style for overhead projector presentations")

  ("bibliography"   "Style for editing bibliographic data files")
  ("email"          "Style for writing and reading emails")
  ("mailbox"        "Style for displaying mailboxes"))

(define-table style-synopsis
  ("alt-colors"         "Color formulas and several other basic tags")
  ("framed-envs"        "Display various environments inside wide frames")
  ("ornaments"          "Tags for various fancy ornaments")
  ("presentation"       "Base package for laptop presentations")
  ("bluish"             "Bluish beamer theme")
  ("ice"                "Ice beamer theme")
  ("metal"              "Metallic beamer theme")
  ("reddish"            "Reddish beamer theme")
  ("ridged-paper"       "Ridged paper beamer theme")
  ("framed-title"       "Put titles of slides in wide frames")
  ("title-bar"          "Put titles of slides in bar at extreme top of screen")
  ("math-ss"            "Use sans serif font for mathematical formulas")

  ("framed-session"     "Render session inputs in frames")
  ("ring-session"       "Ring binder notebook style")
  ("large-formulas"     "Do not break up large fractions and matrices")

  ("centered-program"   "Use a centered rendering style for algorithms")
  ("framed-program"     "Display algorithms inside frames and center")
  ("cite-author-year"   "Mimick 'natbib' package from LaTeX")
  ("cite-sort"          "Package for sorting lists of citations")
  ("two-columns"        "Markup and adjustments for two column documents")
  ("compact-list"       "Less indentation and vertical spacing for lists")
  ("math-brackets"      "Indicate bracket nesting level using colors")
  ("math-check"         "Highlight mathematical formulas with syntax errors")
  ("framed-theorems"    "Display enunciations inside wide frames")
  ("hanging-theorems"   "Use hanging frames for enunciation titles")
  ("number-europe"      "Individual counters for theorems, propositions, etc.")
  ("number-long-article" "Prefix numbered environments by section number")
  ("number-us"          "Shared counter for theorems, propositions, etc.")
  ("captions-above"     "Place captions above figures and tables")

  ("doc"                "Rich collection of markup for writing documentation")
  ("doxygen"            "Package for mixing TeXmacs and doxygen documentation")
  ("poorman-doxygen"    "Replacement for 'doxygen' when lacking support")

  ("allouche"           "Example macro package by David Allouche")
  ("bpr"                "Example macro package for Basu/Pollack/Roy book")
  ("vdh"                "Example macro package by Joris van der Hoeven")

  ("graphical-macros"   "Collection of extra primitives for graphical mode")
  ("structured-list"    "Making item bodies part of item tags")
  ("structured-section" "Making section bodies part of section tags")

  ("normal-spacing"     "Default spacing")
  ("old-spacing"        "Old style spacing")
  ("wide-spacing"       "Wide spacing")
  ("invisible-multiply" "Use invisible space for multiplications")
  ("narrow-multiply"    "Use narrow space for multiplications")
  ("regular-multiply"   "Use regular space for multiplications")
  ("invisible-apply"    "Use invisible space for function applications")
  ("narrow-apply"       "Use narrow space for function applications")
  ("regular-apply"      "Use regular space for function applications"))
