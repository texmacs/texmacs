
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-menu.scm
;; DESCRIPTION : menus for setting global document properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-menu)
  (:use (generic document-edit)
        (generic generic-menu)
        (texmacs menus file-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (include-list base t)
  (cond ((tree-is? t 'document)
         (apply append (map (cut include-list base <>) (tree-children t))))
        ((and (tree-is? t 'include) (tree-atomic? (tree-ref t 0)))
         (list (url-relative base (tree->string (tree-ref t 0)))))
        (else (list))))

(tm-define (project-file-list)
  (if (project-attached?)
      (let* ((prj (project-get))
             (t (buffer->tree prj)))
        (include-list prj t))
      (list)))

(tm-define (main-project-entry)
  (buffer-list-menu (list (project-get))))

(tm-define (project-list-menu)
  (buffer-list-menu (project-file-list)))

(menu-bind project-menu
  (if (== (project-get) (current-buffer))
      (link preamble-menu))
  (if (!= (project-get) (current-buffer))
      (link main-project-entry))
  ---    
  (link project-list-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-style-menu
  (link style-menu)
  ---
  ("No style" (set-no-style))
  ("Edit style" (edit-style-source))
  ("Other style" (interactive set-main-style))
  ---
  (group "Customizations")
  (with l (get-style-list)
    (for (pack (if (null? l) l (cdr l)))
      (-> (eval (upcase-first pack))
          ("Edit package" (edit-package-source pack))
          ("Remove package" (remove-style-package pack)))))
  (-> "Add package"
      (link toggle-package-menu)
      ---
      ("Add other package" (interactive add-style-package))))

(menu-bind document-style-extra-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Source submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-source-preferences-menu
  (-> "Style"
      ("Default" (init-default "src-style"))
      ---
      ("Angular" (init-env "src-style" "angular"))
      ("Scheme" (init-env "src-style" "scheme"))
      ("Functional" (init-env "src-style" "functional"))
      ("Latex" (init-env "src-style" "latex")))
  (-> "Special"
      ("Default" (init-default "src-special"))
      ---
      ("None" (init-env "src-special" "raw"))
      ("Formatting" (init-env "src-special" "format"))
      ("Normal" (init-env "src-special" "normal"))
      ("Maximal" (init-env "src-special" "maximal")))
  (-> "Compactification"
      ("Default" (init-default "src-compact"))
      ---
      ("Minimal" (init-env "src-compact" "none"))
      ("Only inline tags" (init-env "src-compact" "inline"))
      ("Normal" (init-env "src-compact" "normal"))
      ("Inline arguments" (init-env "src-compact" "inline args"))
      ("Maximal" (init-env "src-compact" "all")))
  (-> "Closing style"
      ("Default" (init-default "src-close"))
      ---
      ("Repeat" (init-env "src-close" "repeat"))
      ("Stretched" (init-env "src-close" "long"))
      ("Compact" (init-env "src-close" "compact"))
      ("Minimal" (init-env "src-close" "minimal"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Update menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-update-menu
  ("All" (update-document "all"))
  ---
  ("Buffer" (update-document "buffer"))
  ("Bibliography" (update-document "bibliography"))
  ("Table of contents" (update-document "table-of-contents"))
  ("Index" (update-document "index"))
  ("Glossary" (update-document "glossary"))
  (if (project-attached?)
      ---
      ("Clear local information" (clear-local-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Font menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-font-menu
  (-> "Text font"
      ("Default" (init-default "font" "math-font" "prg-font"))
      ---
      ("Concrete" (init-env "font" "concrete"))
      (if (url-exists-in-tex? "pnr10.mf")
          ("Pandora" (init-env "font" "pandora")))
      ("Roman" (init-env "font" "roman"))
      (if (font-exists-in-tt? "STIX-Regular")
          ("Stix" (init-env "font" "stix")))
      ---
      ("Avant Garde" (init-env "font" "avant-garde"))
      ("Bookman" (init-env "font" "bookman"))
      ("Courier" (init-env "font" "courier"))
      ("Helvetica" (init-env "font" "helvetica"))
      ("N.C. Schoolbook" (init-env "font" "new-century-schoolbook"))
      ("Palatino" (init-env "font" "palatino"))
      ("Times" (init-env "font" "times"))
      ---
      (if (font-exists-in-tt? "texgyrebonum-regular")
          ("Bonum" (init-env "font" "bonum")))
      (if (font-exists-in-tt? "DejaVuSerif")
          ("Dejavu" (init-env "font" "dejavu")))
      ("Lucida" (init-env "font" "x-lucida"))
      (if (font-exists-in-tt? "luxirr")
          ("Luxi" (init-env "font" "luxi")))
      (if (font-exists-in-tt? "texgyrepagella-regular")
          ("Pagella" (init-env "font" "pagella")))
      (if (font-exists-in-tt? "texgyreschola-regular")
          ("Schola" (init-env "font" "schola")))
      (if (font-exists-in-tt? "texgyretermes-regular")
          ("Termes" (init-env "font" "termes")))
      ("Utopia" (init-env "font" "x-utopia"))
      (if (or (supports-chinese?)
              (supports-japanese?)
              (supports-korean?))
          ---
          (if (font-exists-in-tt? "Batang")
              ("Batang" (init-env "font" "batang")))
          (if (font-exists-in-tt? "FandolFang-Regular")
              ("FandolFang" (init-env "font" "FandolFang")))
          (if (font-exists-in-tt? "FandolHei-Regular")
              ("FandolHei" (init-env "font" "FandolHei")))
          (if (font-exists-in-tt? "FandolKai-Regular")
              ("FandolKai" (init-env "font" "FandolKai")))
          (if (font-exists-in-tt? "FandolSong-Regular")
              ("FandolSong" (init-env "font" "FandolSong")))
          (if (font-exists-in-tt? "fireflysung")
              ("Fireflysung" (init-env "font" "fireflysung")))
          (if (font-exists-in-tt? "AppleGothic")
              ("Gothic" (init-env "font" "apple-gothic")))
          (if (font-exists-in-tt? "Gulim")
              ("Gulim" (init-env "font" "gulim")))
          (if (font-exists-in-tt? "华文细黑")
              ("HeiTi" (init-env "font" "heiti")))
          (if (font-exists-in-tt? "ヒラギノ明朝 ProN W6")
              ("Hiragino Kaku" (init-env "font" "kaku")))
          (if (font-exists-in-tt? "ipam")
              ("Ipa" (init-env "font" "ipa")))
          (if (font-exists-in-tt? "ttf-japanese-gothic")
              ("Japanese" (init-env "font" "ttf-japanese")))
          (if (font-exists-in-tt? "kochi-mincho")
              ("Kochi" (init-env "font" "kochi")))
          (if (font-exists-in-tt? "儷黑 Pro")
              ("LiHei" (init-env "font" "lihei")))
          (if (font-exists-in-tt? "wqy-microhei")
              ("MicroHei" (init-env "font" "wqy-microhei")))
          (if (font-exists-in-tt? "mingliu")
              ("MingLiU" (init-env "font" "mingliu")))
          (if (and (font-exists-in-tt? "PMingLiU")
                   (not (font-exists-in-tt? "mingliu")))
              ("MingLiU" (init-env "font" "pmingliu")))       
          (if (font-exists-in-tt? "MS Gothic")
              ("MS Gothic" (init-env "font" "ms-gothic")))
          (if (font-exists-in-tt? "MS Mincho")
              ("MS Mincho" (init-env "font" "ms-mincho")))
          (if (font-exists-in-tt? "sazanami-gothic")
              ("Sazanami" (init-env "font" "sazanami")))
          (if (font-exists-in-tt? "simfang")
              ("SimFang" (init-env "font" "simfang")))
          (if (font-exists-in-tt? "simhei")
              ("SimHei" (init-env "font" "simhei")))
          (if (font-exists-in-tt? "simkai")
              ("SimKai" (init-env "font" "simkai")))
          (if (font-exists-in-tt? "simli")
              ("SimLi" (init-env "font" "simli")))
          (if (font-exists-in-tt? "simsun")
              ("SimSun" (init-env "font" "simsun")))
          (if (and (font-exists-in-tt? "SimSun")
                   (not (font-exists-in-tt? "simsun")))
              ("SimSun" (init-env "font" "apple-simsun")))
          (if (font-exists-in-tt? "simyou")
              ("SimYou" (init-env "font" "simyou")))
          (if (font-exists-in-tt? "ukai")
              ("UKai" (init-env "font" "ukai")))
          (if (font-exists-in-tt? "unbatang")
              ("Unbatang" (init-env "font" "unbatang")))
          (if (font-exists-in-tt? "uming")
              ("UMing" (init-env "font" "uming")))
          (if (font-exists-in-tt? "wqy-zenhei")
              ("ZenHei" (init-env "font" "wqy-zenhei")))))
  (-> "Mathematical font"
      ("Default" (init-default "math-font"))
      ---
      ("Adobe" (init-env "math-font" "adobe"))
      (if (font-exists-in-tt? "Apple Symbols")
          ("Apple symbols" (init-env "math-font" "math-apple")))
      (if (font-exists-in-tt? "Asana-Math")
          ("Asana" (init-env "math-font" "math-asana")))
      (if (font-exists-in-tt? "texgyrebonum-math")
          ("Bonum" (init-env "math-font" "math-bonum")))
      ("Concrete" (init-env "math-font" "concrete"))
      (if (font-exists-in-tt? "DejaVuSerif")
          ("Dejavu" (init-env "math-font" "math-dejavu")))
      ("Euler new roman" (init-env "math-font" "ENR"))
      (if (font-exists-in-tt? "LucidaGrande")
          ("Lucida" (init-env "math-font" "math-lucida")))
      (if (font-exists-in-tt? "texgyrepagella-math")
          ("Pagella" (init-env "math-font" "math-pagella")))
      ("Roman" (init-env "math-font" "roman"))
      (if (font-exists-in-tt? "texgyreschola-math")
          ("Schola" (init-env "math-font" "math-schola")))
      (if (font-exists-in-tt? "STIX-Regular")
          ("Stix" (init-env "math-font" "math-stix")))
      (if (font-exists-in-tt? "texgyretermes-math")
          ("Termes" (init-env "math-font" "math-termes"))))
  (-> "Program font"
      ("Default" (init-default "prog-font"))
      ---
      ("Concrete" (init-env "prog-font" "concrete"))
      (if (url-exists-in-tex? "pnr10.mf")
          ("Pandora" (init-env "prog-font" "pandora")))
      ("Roman" (init-env "prog-font" "roman"))
      ("Times" (init-env "prog-font" "times"))))

(menu-bind document-font-base-size-menu
  ("Default" (init-default "font-base-size"))
  ---
  ("8" (init-env "font-base-size" "8"))
  ("9" (init-env "font-base-size" "9"))
  ("10" (init-env "font-base-size" "10"))
  ("11" (init-env "font-base-size" "11"))
  ("12" (init-env "font-base-size" "12"))
  ("14" (init-env "font-base-size" "14"))
  ---
  ("Other" (init-interactive-env "font-base-size")))

(menu-bind document-font-dpi-menu
  ("Default" (init-default "dpi"))
  ---
  ("150" (init-env "dpi" "150"))
  ("200" (init-env "dpi" "200"))
  ("300" (init-env "dpi" "300"))
  ("400" (init-env "dpi" "400"))
  ("600" (init-env "dpi" "600"))
  ("800" (init-env "dpi" "800"))
  ("1200" (init-env "dpi" "1200"))
  ---
  ("Other" (init-interactive-env "dpi")))

(menu-bind document-full-font-menu
  (link document-font-menu)
  ---
  (-> "Size" (link document-font-base-size-menu))
  (-> "Dpi" (link document-font-dpi-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Short document font menu for focus bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default-font?)
  (with l (get-style-list)
    (with f (list-filter l (lambda (p) (string-ends? p "-font")))
      (and (test-default? "font") (null? f)))))

(tm-define (init-default-font)
  (:check-mark "*" test-default-font?)
  (init-default "font")
  (init-default "math-font")
  (init-default "font-family")
  (remove-font-packages))

(menu-bind document-short-chinese-font-menu
  (if (font-exists-in-tt? "AppleGothic")
      ("Apple Gothic" (init-font "apple-gothic")))
  (if (font-exists-in-tt? "FandolSong-Regular")
      ("FandolSong" (init-font "FandolSong")))
  (if (font-exists-in-tt? "fireflysung")
      ("Fireflysung" (init-font "fireflysung")))
  (if (font-exists-in-tt? "wqy-microhei")
      ("MicroHei" (init-font "wqy-microhei")))
  (if (font-exists-in-tt? "MS Mincho")
      ("MS Mincho" (init-font "ms-mincho")))
  (if (font-exists-in-tt? "simsun")
      ("SimSun" (init-font "simsun")))
  (if (font-exists-in-tt? "uming")
      ("UMing" (init-font "uming")))
  (if (font-exists-in-tt? "wqy-zenhei")
      ("ZenHei" (init-font "wqy-zenhei"))))

(menu-bind document-short-japanese-font-menu
  (if (font-exists-in-tt? "AppleGothic")
      ("Apple Gothic" (init-font "apple-gothic")))
  (if (font-exists-in-tt? "华文细黑")
      ("HeiTi" (init-font "heiti")))
  (if (font-exists-in-tt? "ヒラギノ明朝 ProN W6")
      ("Hiragino Kaku" (init-font "kaku")))
  (if (font-exists-in-tt? "ipam")
      ("Ipa" (init-font "ipa")))
  (if (font-exists-in-tt? "ttf-japanese-gothic")
      ("Japanese" (init-font "ttf-japanese")))
  (if (font-exists-in-tt? "sazanami-gothic")
      ("Sazanami" (init-font "sazanami")))
  (if (font-exists-in-tt? "ukai")
      ("UKai" (init-font "ukai"))))

(menu-bind document-short-korean-font-menu
  (if (font-exists-in-tt? "AppleGothic")
      ("Apple Gothic" (init-font "apple-gothic")))
  (if (font-exists-in-tt? "Batang")
      ("Batang" (init-font "batang")))
  (if (font-exists-in-tt? "Gulim")
      ("Gulim" (init-font "gulim")))
  (if (font-exists-in-tt? "unbatang")
      ("Unbatang" (init-font "unbatang"))))

(menu-bind document-short-font-menu
  ("Default" (init-default-font))
  ---
  ("Roman" (init-font "roman" "roman"))
  ("Stix" (init-font "stix" "math-stix"))
  (if (or (font-exists-in-tt? "texgyrebonum-math")
          (font-exists-in-tt? "texgyrepagella-math")
          (font-exists-in-tt? "texgyreschola-math")
          (font-exists-in-tt? "texgyretermes-math"))
      ---
      (group "TeX Gyre")
      (if (font-exists-in-tt? "texgyrebonum-math")
          ("Bonum" (init-font "bonum" "math-bonum")))
      (if (font-exists-in-tt? "texgyrepagella-math")
          ("Pagella" (init-font "pagella" "math-pagella")))
      (if (font-exists-in-tt? "texgyreschola-math")
          ("Schola" (init-font "schola" "math-schola")))
      (if (font-exists-in-tt? "texgyretermes-math")
          ("Termes" (init-font "termes" "math-termes"))))
  (if (or (font-exists-in-tt? "DejaVuSerif")
          (font-exists-in-tt? "FiraSans-Regular")
          (font-exists-in-tt? "LinLibertine_R")
          (font-exists-in-tt? "Optima")
          (font-exists-in-tt? "Papyrus"))
      ---
      (if (font-exists-in-tt? "AmericanTypewriter")
          ("American Typewriter" (init-font "American Typewriter")))
      (if (font-exists-in-tt? "Baskerville")
          ("Baskerville" (init-font "Baskerville")))
      (if (font-exists-in-tt? "Chalkboard")
          ("Chalkboard" (init-font "Chalkboard")))
      (if (font-exists-in-tt? "Chalkduster")
          ("Chalkduster" (init-font "Chalkduster")))
      (if (font-exists-in-tt? "Cochin")
          ("Cochin" (init-font "Cochin")))
      (if (font-exists-in-tt? "Cuprum-Regular")
          ("Cuprum" (init-font "Cuprum")))
      (if (font-exists-in-tt? "DejaVuSerif")
          ("Dejavu" (init-font "dejavu" "math-dejavu")))
      (if (font-exists-in-tt? "Didot")
          ("Didot" (init-font "Didot")))
      (if (font-exists-in-tt? "Essays1743")
          ("Essays1743" (init-font "Essays1743")))
      (if (font-exists-in-tt? "FiraSans-Regular")
          ("Fira" (init-font "Fira")))
      (if (font-exists-in-tt? "MarkerFelt")
          ("Marker Felt" (init-font "Marker Felt")))
      (if (font-exists-in-tt? "meyne_textur")
          ("Meyne Textur" (init-font "Meyne Textur")))
      (if (font-exists-in-tt? "LinBiolinum_R")
          ("Linux Biolinum" (init-font "Linux Biolinum")))
      (if (font-exists-in-tt? "LinLibertine_R")
          ("Linux Libertine" (init-font "Linux Libertine")))
      (if (font-exists-in-tt? "Optima")
          ("Optima" (init-font "Optima")))
      (if (font-exists-in-tt? "Papyrus")
          ("Papyrus" (init-font "Papyrus"))))
  (if (and (supports-chinese?) (== (get-init "language") "chinese"))
      ---
      (link document-short-chinese-font-menu))
  (if (and (supports-japanese?) (== (get-init "language") "japanese"))
      ---
      (link document-short-japanese-font-menu))
  (if (and (supports-korean?) (== (get-init "language") "korean"))
      ---
      (link document-short-korean-font-menu))
  (if (and (supports-chinese?) (== (get-init "language") "taiwanese"))
      ---
      (link document-short-chinese-font-menu))
  ---
  (if (and (new-fonts?) (use-popups?))
      ("Other" (open-document-font-selector)))
  (if (not (and (new-fonts?) (use-popups?)))
      (-> "Other" (link document-font-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Paragraph menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-paragraph-menu
  (-> "Style"
      ("Default" (init-default "par-mode"))
      ---
      ("Justified" (init-env "par-mode" "justify"))
      ("Left aligned" (init-env "par-mode" "left"))
      ("Centered" (init-env "par-mode" "center"))
      ("Right aligned" (init-env "par-mode" "right")))
  (-> "Line breaking"
      ("Default" (init-default "par-hyphen"))
      ---
      ("Normal" (init-env "par-hyphen" "normal"))
      ("Professional" (init-env "par-hyphen" "professional")))
  (-> "Margins"
      ("Default" (init-default "par-first"))
      ---
      ("First indentation" (init-interactive-env "par-first")))
  (-> "Spacing"
      ("Default" (init-default "par-sep" "par-line-sep"
                               "interpargraph space"))
      ---
      ("Interline separation" (init-interactive-env "par-sep"))
      ("Interline space" (init-interactive-env "par-line-sep"))
      ("Interparagraph space" (init-interactive-env "par-par-sep")))
  (-> "Number of columns"
      ("Default" (init-default "par-columns"))
      ---
      (link document-columns-menu))
  (-> "Advanced"
      (-> "Space stretchability"
          ("Default" (init-default "par-flexibility"))
          ---
          ("Minimal (1)" (init-env "par-flexibility" "1"))
          ("Small (2)" (init-env "par-flexibility" "2"))
          ("Modest (4)" (init-env "par-flexibility" "4"))
          ("Large (1000)" (init-env "par-flexibility" "1000"))
          ---
          ("Other" (init-interactive-env "par-flexibility")))
      (-> "Intercharacter stretching"
          ("Default" (init-default "par-kerning-stretch"))
          ("Automatic" (init-env "par-kerning-stretch" "auto"))
          ("Tolerant" (init-env "par-kerning-stretch" "tolerant"))
          ---
          ("Off" (init-env "par-kerning-stretch" "0"))
          ("Tiny (0.02)" (init-env "par-kerning-stretch" "0.02"))
          ("Modest (0.05)" (init-env "par-kerning-stretch" "0.05"))
          ("Flexible (1.0)" (init-env "par-kerning-stretch" "1.0"))
          ---
          ("Other" (init-interactive-env "par-kerning-stretch")))
      (-> "CJK spacing"
          ("Default" (init-default "par-spacing"))
          ---
          ("Plain" (init-env "par-spacing" "plain"))
          ("Quanjiao" (init-env "par-spacing" "quanjiao"))
          ("Banjiao" (init-env "par-spacing" "banjiao"))
          ("Hangmobanjiao" (init-env "par-spacing" "hangmobanjiao"))
          ("Kaiming" (init-env "par-spacing" "kaiming")))
      ("Use protrusion" (toggle-init-env "par-kerning-margin"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page sizes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-common-page-size-menu)
  ("A3" (init-page-type "a3"))
  ("A4" (init-page-type "a4"))
  ("A5" (init-page-type "a5"))
  ("B4" (init-page-type "b4"))
  ("B5" (init-page-type "b5"))
  ("Letter" (init-page-type "letter"))
  ("Legal" (init-page-type "legal"))
  ("Executive" (init-page-type "executive")))

(tm-menu (document-beamer-page-size-menu)
  ("Widescreen 16:9" (init-page-type "16:9"))
  ("Widescreen 8:5" (init-page-type "8:5"))
  ;;("Widescreen 3:2" (init-page-type "3:2"))
  ("Standard 4:3" (init-page-type "4:3"))
  ("Standard 5:4" (init-page-type "5:4")))

(tm-menu (document-standard-page-formats)
  (-> "A series"
      ("A0" (init-page-type "a0"))
      ("A1" (init-page-type "a1"))
      ("A2" (init-page-type "a2"))
      ("A3" (init-page-type "a3"))
      ("A4" (init-page-type "a4"))
      ("A5" (init-page-type "a5"))
      ("A6" (init-page-type "a6"))
      ("A7" (init-page-type "a7"))
      ("A8" (init-page-type "a8"))
      ("A9" (init-page-type "a9")))
  (-> "B series"
      ("B0" (init-page-type "b0"))
      ("B1" (init-page-type "b1"))
      ("B2" (init-page-type "b2"))
      ("B3" (init-page-type "b3"))
      ("B4" (init-page-type "b4"))
      ("B5" (init-page-type "b5"))
      ("B6" (init-page-type "b6"))
      ("B7" (init-page-type "b7"))
      ("B8" (init-page-type "b8"))
      ("B9" (init-page-type "b9")))
  (-> "Arch series"
      ("ArchA" (init-page-type "archA"))
      ("ArchB" (init-page-type "archB"))
      ("ArchC" (init-page-type "archC"))
      ("ArchD" (init-page-type "archD"))
      ("ArchE" (init-page-type "archE")))
  (-> "American"
      ("10x14" (init-page-type "10x14"))
      ("11x17" (init-page-type "11x17"))
      ("C5" (init-page-type "C5"))
      ("Comm10" (init-page-type "Comm10"))
      ("DL" (init-page-type "DL"))
      ("Executive" (init-page-type "executive"))
      ("Half letter" (init-page-type "halfletter"))
      ("Half executive" (init-page-type "halfexecutive"))
      ("Ledger" (init-page-type "ledger"))
      ("Legal" (init-page-type "legal"))
      ("Letter" (init-page-type "letter"))
      ("Monarch" (init-page-type "Monarch")))
  (-> "Miscellaneous"
      ("C sheet" (init-page-type "csheet"))
      ("D sheet" (init-page-type "dsheet"))
      ("E sheet" (init-page-type "esheet"))
      ("Flsa" (init-page-type "flsa"))
      ("Flse" (init-page-type "flse"))
      ("Folio" (init-page-type "folio"))
      ("Lecture note" (init-page-type "lecture note"))
      ("Note" (init-page-type "note"))
      ("Quarto" (init-page-type "quarto"))
      ("Statement" (init-page-type "statement"))
      ("Tabloid" (init-page-type "tabloid"))))

(tm-menu (document-page-size-menu)
  ("Default" (default-page-type))
  ---
  (group "Common formats")
  (link document-common-page-size-menu)
  ---
  (group "Standard formats")
  (link document-standard-page-formats)
  (-> "Beamer"
      (link document-beamer-page-size-menu))
  ---
  ("Other" (interactive init-page-size)))

(tm-menu (document-page-size-menu)
  (:require (style-has? "beamer-style"))
  ("Default" (default-page-type))
  ---
  (group "Beamer formats")
  (link document-beamer-page-size-menu)
  ---
  (group "Standard formats")
  (-> "Common"
      (link document-common-page-size-menu))
  (link document-standard-page-formats)
  ---
  ("Other" (interactive init-page-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number of columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-columns-menu)
  ("One column" (init-env "par-columns" "1"))
  ("Two columns" (init-env "par-columns" "2")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Page menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind page-rendering-menu
  ("Paper" (init-page-rendering "paper"))
  ("Papyrus" (init-page-rendering "papyrus"))
  ("Screen" (init-page-rendering "automatic"))
  ("Beamer" (init-page-rendering "beamer"))
  ("Book" (init-page-rendering "book"))
  ("Panorama" (init-page-rendering "panorama")))

(menu-bind page-layout-menu
  ("Margins as on paper" (toggle-page-screen-margin))
  ("Reduced margins" (toggle-reduced-margins))
  ("Indent paragraphs" (toggle-indent-paragraphs))
  ("No page numbers" (toggle-no-page-numbers)))

(menu-bind document-page-menu
  (-> "Type"
      ("Default" (init-default-page-rendering))
      ---
      (link page-rendering-menu))
  (-> "Size"
      (link document-page-size-menu))
  (-> "Orientation"
      ("Default" (init-default-page-orientation))
      ---
      ("Portrait" (init-page-orientation "portrait"))
      ("Landscape" (init-page-orientation "landscape")))
  (-> "Crop marks"
      ("Default" (init-default "page-crop-marks"))
      ---
      ("None" (init-env "page-crop-marks" ""))
      ("A3" (init-env "page-crop-marks" "a3"))
      ("A4" (init-env "page-crop-marks" "a4"))
      ("Letter" (init-env "page-crop-marks" "letter")))
  (-> "Margins"
      ("Default" (init-default "page-width-margin" "page-height-margin"
                               "page-odd" "page-even" "page-right"
                               "par-width" "page-odd-shift"
                               "page-even-shift" "page-top" "page-bot"
                               "page-height-margin"))
      ---
      ("Explicit margins" (init-env "page-width-margin" "false"))
      (when (test-env? "page-width-margin" "false")
        ("Odd page left margin" (init-interactive-env "page-odd"))
        ("Odd page right margin" (init-interactive-env "page-right"))
        ("Even page left margin" (init-interactive-env "page-even"))
        (when (test-env? "page-height-margin" "false")
          ("Top margin" (init-interactive-env "page-top"))
          ("Bottom margin" (init-interactive-env "page-bot"))))
      ---
      ("Margins from width" (init-env "page-width-margin" "true"))
      (when (test-env? "page-width-margin" "true")
        ("Paragraph width" (init-interactive-env "par-width"))
        ("Odd page shift" (init-interactive-env "page-odd-shift"))
        ("Even page shift" (init-interactive-env "page-even-shift"))
        (when (test-env? "page-height-margin" "false")
          ("Top margin" (init-interactive-env "page-top"))
          ("Bottom margin" (init-interactive-env "page-bot")))))
  (-> "Screen margins"
      ("Default" (init-default "page-screen-margin"
                               "page-screen-left" "page-screen-right"
                               "page-screen-top" "page-screen-bot"))
      ("Margins as on paper" (toggle-page-screen-margin))
      ---
      (when (test-env? "page-screen-margin" "true")
        ("Left margin" (init-interactive-env "page-screen-left"))
        ("Right margin" (init-interactive-env "page-screen-right"))
        ("Top margin" (init-interactive-env "page-screen-top"))
        ("Bottom margin" (init-interactive-env "page-screen-bot"))))
  (if (detailed-menus?)
      ---
      (group "Breaking")
      (-> "Algorithm"
          ("Default" (init-default "page-breaking"))
          ---
          ("Sloppy" (init-env "page-breaking" "sloppy"))
          ("Medium" (init-env "page-breaking" "medium"))
          ("Professional" (init-env "page-breaking" "professional")))
      (-> "Limits"
          ("Allowed reduction" (init-interactive-env "page-shrink"))
          ("Allowed extension" (init-interactive-env "page-extend")))
      (-> "Flexibility"
          ("Default" (init-default "page-flexibility"))
          ---
          ("0" (init-env "page-flexibility" "0.0"))
          ("1/4" (init-env "page-flexibility" "0.25"))
          ("1/2" (init-env "page-flexibility" "0.5"))
          ("3/4" (init-env "page-flexibility" "0.75"))
          ("1" (init-env "page-flexibility" "1.0"))
          ---
          ("Other" (init-interactive-env "page-flexibility")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Metadata menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-metadata-menu
  ("Title" (init-interactive-env "global-title"))
  ("Author" (init-interactive-env "global-author"))
  ("Subject" (init-interactive-env "global-subject")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Magnification menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-magnification-menu
  ("Default" (init-default "magnification"))
  ---
  ("0.7" (init-env "magnification" "0.7"))
  ("0.8" (init-env "magnification" "0.8"))
  ("1" (init-env "magnification" "1"))
  ("1.2" (init-env "magnification" "1.2"))
  ("1.4" (init-env "magnification" "1.4"))
  ("1.7" (init-env "magnification" "1.7"))
  ("2" (init-env "magnification" "2"))
  ---
  ("Other" (init-interactive-env "magnification")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Color menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-background col)
  (init-env-tree "bg-color" col))

(define (set-background-pattern name)
  (when (pair? name) (set! name (car name)))
  (init-env-tree "bg-color" (tm-pattern name "" "")))

(define (set-background-picture name)
  (when (pair? name) (set! name (car name)))
  (init-env-tree "bg-color" (tm-pattern name "100%" "100@")))

(menu-bind document-foreground-color-menu
  ("Default" (init-default "color"))
  ---
  (pick-color (init-env "color" answer))
  ---
  ("Palette" (interactive-color
              (lambda (col) (init-env "color" col)) '()))
  ("Other" (init-interactive-env "color")))

(menu-bind document-background-color-menu
  ("Default" (init-default "bg-color"))
  ---
  (pick-background "" (init-env-tree "bg-color" answer))
  ---
  ("Palette" (interactive-background set-background '()))
  ("Pattern" (open-pattern-selector set-background "1cm"))
  ("Picture" (open-background-picture-selector set-background))
  ("Other" (init-interactive-env "bg-color")))

(menu-bind document-colors-menu
  (-> "Background" (link document-background-color-menu))
  (-> "Foreground" (link document-foreground-color-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global and document language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind global-language-menu
  (for (lan supported-languages)
    (when (supported-language? lan)
      ((check (eval (upcase-first lan)) "*"
              (and (test-document-language? lan)
                   (== lan (get-output-language))))
       (set-document-language lan)
       (set-output-language lan)))))

(menu-bind document-language-menu
  ("Default" (set-default-document-language))
  ---
  (for (lan supported-languages)
    (when (supported-language? lan)
      ((check (eval (upcase-first lan)) "*" (test-document-language? lan))
       (set-document-language lan)))))

(tm-define (current-language-icon)
  (with lan (get-env "language")
    (if (in? lan supported-languages)
        (string-append "tm_" lan ".xpm")
        "tm_stateless.xpm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document -> Supported scripts menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (supported-scripts-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (scripts-list)))
    (for (name l)
      ((check (eval (scripts-name name)) "v"
              (test-env? "prog-scripts" name))
       (noop) ;; NOTE: inhibit segfault due to property searching?
       (init-env "prog-scripts" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Informative flags menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-informative-flags-menu
  ("Default" (init-default "info-flag"))
  ---
  ("None" (init-env "info-flag" "none"))
  ("Minimal" (init-env "info-flag" "minimal"))
  ("Short" (init-env "info-flag" "short"))
  ("Detailed" (init-env "info-flag" "detailed"))
  ("Also on paper" (init-env "info-flag" "paper")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Document menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind full-document-menu
  (-> "Style" (link document-style-menu))
  (link document-style-extra-menu)
  ;;(-> "Add package"
  ;;(link add-package-menu)
  ;;---
  ;;("Other" (interactive add-style-package)))
  ;;(-> "Remove package"
  ;;(link remove-package-menu)
  ;;---
  ;;("Other" (interactive remove-style-package)))
  (if (and (not (project-attached?))
           (!= (get-init-tree "sectional-short-style") (tree 'macro "false")))
      (-> "Part" (link document-part-menu)))
  (-> "Source"
      ("Edit source tree" (toggle-source-mode))
      ---
      (group "Preferences")
      (link document-source-preferences-menu))
  (-> "Update" (link document-update-menu))
  ---
  (-> "Font" (link document-full-font-menu))
  (-> "Paragraph" (link document-paragraph-menu))
  (-> "Page" (link document-page-menu))
  (-> "Metadata" (link document-metadata-menu))
  (-> "Bibliography"
      (when (buffer-has-biblio? (current-buffer))
	("Local entries" (open-biblio))))
  ---
  (-> "Magnification" (link document-magnification-menu))
  (-> "Colors" (link document-colors-menu))
  (if (detailed-menus?)
      (-> "Language" (link document-language-menu)))
  (-> "Scripts"
      ("Default" (init-default "prog-scripts"))
      ---
      (link supported-scripts-menu))
  (-> "Informative flags" (link document-informative-flags-menu))
  (if (== (get-preference "experimental encryption") "on")
      (-> "Encryption" (link document-encryption-menu))))

(menu-bind compressed-document-menu
  (-> "Style" (link document-style-menu))
  (link document-style-extra-menu)
  (if (and (not (project-attached?))
           (!= (get-init-tree "sectional-short-style") (tree 'macro "false")))
      (-> "Part" (link document-part-menu)))
  (-> "Source"
      ("Edit source tree" (toggle-source-mode))
      ("Preferences" (open-source-tree-preferences)))
  (-> "Update" (link document-update-menu))
  ---
  (if (new-fonts?)
      ("Font" (interactive open-document-font-selector)))
  (if (not (new-fonts?))
      (-> "Font" (link document-full-font-menu)))
  ("Paragraph" (open-document-paragraph-format))
  ("Page" (open-document-page-format))
  ("Metadata" (open-document-metadata))
  (when (buffer-has-biblio? (current-buffer))
    ("Bibliography" (open-biblio)))
  ;;("Colors" (open-document-colors))
  ---
  (-> "Magnification" (link document-magnification-menu))
  (-> "Colors" (link document-colors-menu))
  (if (detailed-menus?)
      (-> "Language" (link document-language-menu)))
  (-> "Scripts"
      ("Default" (init-default "prog-scripts"))
      ---
      (link supported-scripts-menu))
  (-> "Informative flags" (link document-informative-flags-menu))
  (if (== (get-preference "experimental encryption") "on")
      (-> "Encryption" (link document-encryption-menu))))

(menu-bind document-menu
  (if (use-menus?)
      (link full-document-menu))
  (if (use-popups?)
      (link compressed-document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document focus menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-document-extra-menu t))
(tm-menu (focus-style-extra-menu t))

(menu-bind cite-texmacs-only-menu
  ("TeXmacs website" (cite-texmacs "TeXmacs:website"))
  ("TeXmacs manual" (cite-texmacs "TeXmacs:manual")))

(menu-bind cite-texmacs-related-menu
  (-> "Tutorials"
      ("GNU TeXmacs: a scientific editing platform"
       (cite-texmacs "TeXmacs:vdH2:2006"))
      ("TeXmacs in 60 minutes"
       (cite-texmacs "TeXmacs:Seidl:2003"))
      ("TeXmacs Quick-Start Guide"
       (cite-texmacs "TeXmacs:Ratier:2005")))
  (-> "Surveys"
      ("GNU TeXmacs (ASCM 2005)"
       (cite-texmacs "TeXmacs:vdH:2005"))
      ("GNU TeXmacs (Dagstuhl 2006)"
       (cite-texmacs "TeXmacs:vdH1:2006"))
      ("GNU TeXmacs: a scientific editing platform"
       (cite-texmacs "TeXmacs:HGGLPR:2012"))
      ("GNU TeXmacs: Towards a Scientific Office Suite"
       (cite-texmacs "TeXmacs:GHPR:2014")))      
  (-> "Research papers"
      ("GNU TeXmacs, a free, structured, wysiwyg and technical text editor"
       (cite-texmacs "TeXmacs:vdH:2001"))
      ("Conservative conversion between LaTeX and TeXmacs"
       (cite-texmacs "TeXmacs:HP:2014"))
      ("Towards semantic mathematical editing"
       (cite-texmacs "TeXmacs:vdH:2015"))
      ("Preserving syntactic correctness while editing mathematical formulas"
       (cite-texmacs "TeXmacs:HLR:2015"))
      ("Mathematical Font Art"
       (cite-texmacs "TeXmacs:vdH:2016")))
  (-> "Plug-ins"
      ("TeXmacs interfaces to Maxima, Mupad and Reduce"
       (cite-texmacs "TeXmacs:Grozin:2001"))
      ("TeXmacs-Maxima interface"
       (cite-texmacs "TeXmacs:Grozin:2005"))
      ("TeXmacs-Reduce interface"
       (cite-texmacs "TeXmacs:Grozin:2012"))
      ("TeXmacs as an authoring tool for formal developments"
       (cite-texmacs "TeXmacs:AR:2004"))
      ("A Document-Oriented Coq Plugin for TeXmacs"
       (cite-texmacs "TeXmacs:MG:2006"))))

(menu-bind cite-texmacs-menu
  (assuming (or (in-beamer?) (in-seminar?) (in-browser?))
    (group "Acknowledge")
    ("Written with TeXmacs" (acknowledge-texmacs))
    ---)
  (group "Cite")
  (link cite-texmacs-only-menu)
  ---
  (group "Cite related work")
  (link cite-texmacs-related-menu))

(menu-bind cite-texmacs-short-menu
  (link cite-texmacs-only-menu)
  ---
  (link cite-texmacs-related-menu))

(tm-menu (focus-style-menu t)
  (group "Style")
  (let* ((st* (get-style-list))
         (st (if (null? st*) (list "no style") st*)))
    ;;((eval (upcase-first (car st)))
    ;;(open-style-selector))
    (-> (eval (upcase-first (car st)))
        (link style-menu)
        ---
        ("Edit style" (edit-style-source))
        ("Other style" (interactive set-main-style)))
    (dynamic (focus-style-extra-menu t))
    (for (pack (list-filter (cdr st) (negate hidden-package?)))
      (-> (eval (upcase-first pack))
          ("Edit package" (edit-package-source pack))
          ("Remove package" (remove-style-package pack)))))
  (-> "Add style package"
      (link add-package-menu)
      ---
      ("Other package" (interactive add-style-package))))

(define (number-columns-text s)
  (cond ((not (string? s)) "One column")
        ((== s "1") "One column")
        ((== s "2") "Two columns")
        ((== s "3") "Three columns")
        ((== s "4") "Four columns")
        ((== s "5") "Five columns")
        (else (string-append s " columns"))))

(tm-menu (focus-document-menu t)
  (group "Document")
  (-> (eval (upcase-first (get-init "page-type")))
      (link document-page-size-menu))
  (-> (eval (string-append (get-init "font-base-size") " pt"))
      (link document-font-base-size-menu))
  (-> (eval (upcase-first (get-init-page-rendering)))
      (link page-rendering-menu))
  (-> (eval (upcase-first (get-init "page-orientation")))
      ("Portrait" (init-page-orientation "portrait"))
      ("Landscape" (init-page-orientation "landscape")))
  (-> (eval (number-columns-text (get-init "par-columns")))
      (link document-columns-menu))
  (-> "Layout"
      (link page-layout-menu))
  (if (and (== (get-preference "experimental encryption") "on")
	   (!= (get-init "encryption") ""))
      (-> "Encryption" (link document-encryption-menu)))
  (-> (eval (upcase-first (get-init "language")))
      (link document-language-menu))
  (-> (eval (upcase-first (font-family-main (get-init "font"))))
      (link document-short-font-menu)))

(tm-menu (standard-focus-menu t)
  (:require (tree-is-buffer? t))
  (dynamic (focus-style-menu t))
  ---
  (dynamic (focus-document-menu t))
  (-> "Cite TeXmacs" (link cite-texmacs-menu))
  (dynamic (focus-document-extra-menu t))
  ---
  ("Help" (focus-help)))

(tm-menu (focus-document-extra-icons t))
(tm-menu (focus-style-extra-icons t))

(tm-menu (focus-style-icons t)
  (minibar
    (let* ((st* (get-style-list))
           (st (if (null? st*) (list "no style") st*)))
      ;;((balloon (eval (upcase-first (car st))) "Document style")
      ;; (open-style-selector))
      (=> (balloon (eval (upcase-first (car st))) "Document style")
          (link style-menu)
          ---
          ("Edit style" (edit-style-source))
          ("Other style" (interactive set-main-style)))
      (dynamic (focus-style-extra-icons t))
      (for (pack (list-filter (cdr st) (negate hidden-package?)))
        (=> (eval pack)
            ("Edit package" (edit-package-source pack))
            ("Remove package" (remove-style-package pack)))))
    (=> (balloon (icon "tm_add.xpm") "Add style package")
        (link add-package-menu)
        ---
        ("Other package" (interactive add-style-package)))
    (assuming (tree-is-buffer? t)
      ((balloon (icon "tm_focus_help.xpm") "Describe tag")
       (focus-help)))
    (=> (balloon (icon "tm_like.xpm") "Cite TeXmacs")
        (link cite-texmacs-menu))))

(define (is-background-picture? bg*)
  (with bg (tm->stree bg*)
    (and (tm-is? bg 'pattern)
         (>= (tm-arity bg) 3)
         (in? (tm-ref bg 1) '("100%" "100@"))
         (in? (tm-ref bg 2) '("100%" "100@")))))

(tm-menu (focus-background-color-icons)
  (with setter (lambda (col) (init-env-tree "bg-color" col))
    (assuming (not (is-background-picture? (get-init-tree "bg-color")))
      (dynamic (focus-customizable-icons-item
                "bg-color" "Background color" :global)))
    (assuming (is-background-picture? (get-init-tree "bg-color"))
      ((balloon (icon "tm_camera.xpm") "Select background picture")
       (with bg (tree->stree (get-init-tree "bg-color"))
         (open-background-picture-selector setter bg))))))

(tm-define (current-page-icon)
  (cond ((test-init? "page-orientation" "landscape")
         (cond ((test-init? "par-columns" "1") "tm_landscape_1col.xpm")
               ((test-init? "par-columns" "2") "tm_landscape_2col.xpm")
               (else "tm_landscape.xpm")))
        (else
         (cond ((test-init? "par-columns" "1") "tm_portrait_1col.xpm")
               ((test-init? "par-columns" "2") "tm_portrait_2col.xpm")
               (else "tm_portrait.xpm")))))

(tm-menu (focus-document-icons t)
  (minibar
    (=> (balloon (eval (upcase-first (get-init "page-type")))
                 "Paper size")
        (link document-page-size-menu))
    (=> (balloon (eval `(verbatim ,(upcase-first
                                    (font-family-main (get-init "font")))))
                 "Main document font")
        (link document-short-font-menu))
    (=> (balloon (eval (string-append (get-init "font-base-size") "pt"))
                 "Font size")
        (link document-font-base-size-menu))
    (=> (balloon (icon (eval (current-page-icon))) "Page layout")
        ("Portrait" (init-page-orientation "portrait"))
        ("Landscape" (init-page-orientation "landscape"))
        ---
        (link document-columns-menu)
        ---
	(link page-rendering-menu)
        ---
        (link page-layout-menu))
    (if (and (== (get-preference "experimental encryption") "on")
	     (!= (get-init "encryption") ""))
	(=> (balloon (icon "tm_lock_open.xpm") "Encryption")
	 (link document-encryption-menu)))
    (=> (balloon (icon (eval (current-language-icon))) "Document language")
        (link document-language-menu))))

(tm-menu (standard-focus-icons t)
  (:require (tree-is-buffer? t))
  (dynamic (focus-style-icons t))  
  //
  (dynamic (focus-document-icons t))  
  //   
  (dynamic (focus-document-extra-icons t)))
