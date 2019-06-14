
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : help-menu.scm
;; DESCRIPTION : the help menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc help-menu)
  (:use (doc help-funcs))); (doc apidoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plugin-documented? name)
  (and (url-exists-in-help? (string-append name ".en.tm"))
       (url-exists-in-help? (string-append name "-abstract.en.tm"))))

(tm-menu (help-plugins-menu)
  (for (name (list-filter (map symbol->string (plugin-list))
                          plugin-documented?))
    (with menu-name (session-name name)
      ((eval menu-name)
       (load-help-article (string-append name))))))

(menu-bind help-menu
  (when (url-exists-in-help? "about/welcome/welcome.en.tm")
	("Welcome" (load-help-buffer "about/welcome/welcome"))
	---)
  (if (detailed-menus?)
      (when (url-exists-in-help? "main/config/man-configuration.en.tm")
	(-> "Configuration"
	    ("Browse" (load-help-buffer "main/config/man-configuration"))
	    ---
	    ("User preferences"
	     (load-help-article "main/config/man-preferences"))
	    ("Keyboard configuration"
	     (load-help-article "main/config/man-config-keyboard"))
	    ("Users of Cyrillic languages"
	     (load-help-article "main/config/man-russian"))
	    ("Users of oriental languages"
	     (load-help-article "main/config/man-oriental")))))
  (when (url-exists-in-help? "main/man-manual.en.tm")
	(-> "Manual"
	    ("Browse" (load-help-buffer "main/man-manual"))
	    ---
	    ("Getting started"
	     (load-help-article "main/start/man-getting-started"))
	    ("Typing simple texts"
	     (load-help-article "main/text/man-text"))
	    ("Mathematical formulas"
	     (load-help-article "main/math/man-math"))
	    ("Tabular material"
	     (load-help-article "main/table/man-table"))
	    ("Automatic content generation"
	     (load-help-article "main/links/man-links"))
	    ("Creating technical pictures"
	     (load-help-article "main/graphics/man-graphics"))
	    ("Advanced layout features"
	     (load-help-article "main/layout/man-layout"))
            ---
	    ("Editing tools"
	     (load-help-article "main/editing/man-editing-tools"))
	    ("Laptop presentations"
	     (load-help-article "main/beamer/man-beamer"))
	    ("TeXmacs as an interface"
	     (load-help-article "main/interface/man-itf"))
            ---
	    ("Writing your own style files"
	     (load-help-article "devel/style/style"))
	    ("Customizing TeXmacs"
	     (load-help-article "main/scheme/man-scheme"))
	    ("The TeXmacs plug-in system"
	     (load-help-article "devel/plugin/plugins"))))
  (when (url-exists-in-help? "main/man-manual.en.tm")
	(-> "Reference guide"
	    ("Browse" (load-help-buffer "main/man-reference"))
	    ---
	    ("The TeXmacs format"
	     (load-help-article "devel/format/basics/basics"))
	    ("Standard environment variables"
	     (load-help-article "devel/format/environment/environment"))
	    ("TeXmacs primitives"
	     (load-help-article "devel/format/regular/regular"))
	    ("Stylesheet language"
	     (load-help-article "devel/format/stylesheet/stylesheet"))
	    ("Standard TeXmacs styles"
	     (load-help-article "main/styles/styles"))
	    ("Compatibility with other formats"
	     (load-help-article "main/convert/man-convert"))))
  (-> "Plug-ins"
      (link help-plugins-menu))
  (when (url-exists-in-help? "about/about.en.tm")
	(-> "About"
	    ("Browse" (load-help-buffer "about/about"))
	    ---
	    ("Summary"
	     (load-help-article "about/about-summary"))
	    ("License"
	     (load-help-buffer "$TEXMACS_PATH/LICENSE"))
	    ("Philosophy"
	     (load-help-article "about/philosophy/philosophy"))
	    ("The TeXmacs authors"
	     (load-help-article "about/authors/authors"))
	    ---
	    (when (and (updater-supported?) (not (updater-running?)))
		  ("Check for update"
		   (updater-check-foreground)))
	    (when (url-exists-in-help? "about/changes/changes-recent")
		  ("What is new"
		   (load-help-article "about/changes/changes-recent")))
	    ("Major changes"
	     (load-help-article "about/changes/changes-main"))
	    ("Change log"
	     (load-help-article "about/changes/change-log"))
	    ---
	    ("Original welcome message"
	     (load-help-article "about/welcome/first"))))
  (if (detailed-menus?)
      ---
      (when (url-exists-in-help? "about/contribute/contribute.en.tm")
	(-> "Help us"
	    ("Browse" (load-help-buffer "about/contribute/contribute"))
	    ---
	    ("Use TeXmacs"
	     (load-help-article "about/contribute/using/using"))
	    ("Making donations"
	     (load-help-article "about/contribute/material/donations"))
	    ("Documentation"
	     (load-help-article
	      "about/contribute/documentation/documentation"))
	    ("Internationalization"
	     (load-help-article "about/contribute/translate/translate"))
	    ("Writing data converters"
	     (load-help-article "about/contribute/converters/converters"))
	    ("Porting TeXmacs to other platforms"
	     (load-help-article "about/contribute/porting/porting"))
	    ("Interfacing TeXmacs with other systems"
	     (load-help-article "about/contribute/interfaces/interfaces"))
	    ("Become a TeXmacs developer"
	     (load-help-article "about/contribute/develop/develop"))))
      (when (url-exists-in-help? "about/projects/projects.en.tm")
	(-> "Projects"
	    ("Browse" (load-help-buffer "about/projects/projects"))
	    ---
	    ("Improving the current implementation"
	     (load-help-buffer "about/projects/improvements"))
	    ("Plans for the future"
	     (load-help-buffer "about/projects/future"))))
;;       (when (url-exists-in-help? "devel/format/format.en.tm")
;; 	(-> "Document format"
;; 	    ("Browse" (load-help-buffer "devel/format/format"))
;; 	    ---
;; 	    ("Documents are trees"
;; 	     (load-help-article "devel/format/trees"))
;; 	    ("The leaves of TeXmacs trees"
;; 	     (load-help-article "devel/format/leaves"))
;; 	    ("The primitive TeXmacs constructs"
;; 	     (load-help-article "devel/format/primitives"))
;; 	    ("System environment variables"
;; 	     (load-help-article "devel/format/env-vars"))
;; 	    ("Planned changes"
;; 	     (load-help-article "devel/format/planned-changes"))))
      (when (url-exists-in-help? "devel/interface/interface.en.tm")
	(-> "Interfacing"
	    ("Browse" (load-help-buffer "devel/interface/interface"))
	    ---
	    ("Introduction"
	     (load-help-article "devel/interface/interface-intro"))
	    ("Basic communication using pipes"
	     (load-help-article "devel/interface/interface-pipes"))
	    ("Formatted and structured output"
	     (load-help-article "devel/interface/interface-nested"))
	    ("Prompts and default input"
	     (load-help-article "devel/interface/interface-channels"))
	    ("Sending commands to TeXmacs"
	     (load-help-article "devel/interface/interface-commands"))
	    ("Background evaluations"
	     (load-help-article "devel/interface/interface-background"))
	    ("Mathematical and customized input"
	     (load-help-article "devel/interface/interface-input"))
	    ("Tab-completion"
	     (load-help-article "devel/interface/interface-tab"))
	    ("Dynamic libraries"
	     (load-help-article "devel/interface/interface-dynlibs"))
	    ("Miscellaneous features"
	     (load-help-article "devel/interface/interface-misc"))
	    ("Writing documentation"
	     (load-help-article "devel/interface/interface-documentation"))
	    ("Plans for the future"
	     (load-help-article "devel/interface/interface-plans"))))
;;       (when (url-exists-in-help? "devel/source/source.en.tm")
;; 	(-> "Source code"
;; 	    ("Browse" (load-help-buffer "devel/source/source"))
;; 	    ---
;; 	    ("General architecture of TeXmacs"
;; 	     (load-help-article "devel/source/architecture"))
;; 	    ("Basic data types"
;; 	     (load-help-article "devel/source/types"))
;; 	    ("Converters to other data formats"
;; 	     (load-help-article "devel/source/conversions"))
;; 	    ("The graphical user interface"
;; 	     (load-help-article "devel/source/gui"))
;; 	    ("TeXmacs fonts"
;; 	     (load-help-article "devel/source/fonts"))
;; 	    ("Mathematical typesetting"
;; 	     (load-help-article "devel/source/maths"))
;; 	    ("The boxes produced by the typesetter"
;; 	     (load-help-article "devel/source/boxes"))))
      (when (url-exists-in-help? "devel/scheme/scheme.en.tm")
	(-> "Scheme extensions"
	    ("Browse" (load-help-buffer "devel/scheme/scheme"))
	    ---
	    ("Overview of the scheme extension language"
	     (load-help-article "devel/scheme/overview/scheme-overview"))
	    ("TeXmacs extensions to scheme and utilities"
	     (load-help-article "devel/scheme/utils/scheme-utils"))
	    ("Programming routines for editing documents"
	     (load-help-article "devel/scheme/edit/scheme-edit"))
	    ("Program interface for buffer management"
	     (load-help-article "devel/scheme/buffer/scheme-buffer"))
	    ("Scheme interface for the graphical mode"
	     (load-help-article "devel/scheme/graphics/scheme-graphics"))
            
            ("Customizing and extending the user interface"
             (load-help-article "devel/scheme/gui/scheme-gui"))
	    ("Writing TeXmacs bibliography styles"
	     (load-help-article "devel/scheme/bibliography/bibliography"))
            ---
            ("Browse modules documentation" (apidoc-all-modules)))))
            ;("List all symbols" (apidoc-all-symbols)))))
  ---
  (-> "Search"
      ("Documentation" (interactive docgrep-in-doc))
      (if (detailed-menus?)
          ("Source code" (interactive docgrep-in-src)))
      ;;("My documents" (interactive docgrep-in-texts))
      ("Recent documents" (interactive docgrep-in-recent)))
  (if (detailed-menus?)
      (-> "Full manuals"
          (when (url-exists-in-help? "main/man-user-manual.en.tm")
            ("User manual" (load-help-book "main/man-user-manual")))
          ;; (when (url-exists-in-help? "tutorial/tut-tutorial.en.tm")
          ;;   ("Tutorial" (load-help-book "tutorial/tut-tutorial")))
          (when (url-exists-in-help? "devel/source/source.en.tm")
            ("Developers guide" (load-help-book "devel/source/source")))
          (when (url-exists-in-help? "devel/scheme/scheme.en.tm")
            ("Scheme developers guide" (load-help-book "devel/scheme/scheme")))
          ---
          (when (style-has? "tmdoc-style")
            ("Compile article" (tmdoc-expand-this "article"))
            ("Compile book" (tmdoc-expand-this "book"))))))
