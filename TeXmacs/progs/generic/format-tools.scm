
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-tools.scm
;; DESCRIPTION : Widgets for text, paragraph and page properties
;; COPYRIGHT   : (C) 2013-2021  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-tools)
  (:use (generic format-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (window-get-env win l mode)
  ;; FIXME: should read environment from buffer in window win
  (cond ((== mode :local)
         (get-env l))
        ((== mode :global)
         (get-init l))
        (else "")))

(tm-define (window-set-env win l val mode)
  ;; FIXME: should modify buffer in window win
  ;; FIXME: maybe call 'window-set-line-env'
  (cond ((== mode :local)
         (make-multi-line-with (list l val)))
        ((== mode :global)
         (init-env l val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (paragraph-basic-tool win mode)
  (aligned
    (item (text "Alignment:")
      (enum (window-set-env win "par-mode" answer mode)
            '("left" "center" "right" "justify")
            (window-get-env win "par-mode" mode) "10em"))
    (assuming (== mode :local)
      (item ====== ======)
      (item (text "Left margin:")
        (enum (window-set-env win "par-left" answer mode)
              (cons-new (window-get-env win "par-left" mode)
                        '("0tab" "1tab" "2tab" ""))
              (window-get-env win "par-left" mode) "10em"))
      (item (text "Right margin:")
        (enum (window-set-env win "par-right" answer mode)
              (cons-new (window-get-env win "par-right" mode)
                        '("0tab" "1tab" "2tab" ""))
              (window-get-env win "par-right" mode) "10em")))
    (item (text "First indentation:")
      (enum (window-set-env win "par-first" answer mode)
            (cons-new (window-get-env win "par-first" mode)
                      '("0tab" "1tab" "-1tab" ""))
            (window-get-env win "par-first" mode) "10em"))
    (item ====== ======)
    (item (text "Interline space:")
      (enum (window-set-env win "par-sep" answer mode)
            (cons-new (window-get-env win "par-sep" mode)
                      '("0fn" "0.2fn" "0.5fn" "1fn" ""))
            (window-get-env win "par-sep" mode) "10em"))
    (item (text "Interparagraph space:")
      (enum (window-set-env win "par-par-sep" answer mode)
            (cons-new (window-get-env win "par-par-sep" mode)
                      '("0fn" "0.3333fn" "0.5fn" "0.6666fn" "1fn" "0.5fns" ""))
            (window-get-env win "par-par-sep" mode) "10em"))
    (item ====== ======)
    (item (text "Number of columns:")
      (enum (begin
              (window-set-env win "par-columns" answer mode)
              (refresh-now "paragraph-tool-columns"))
            '("1" "2" "3" "4" "5" "6")
            (window-get-env win "par-columns" mode) "10em"))
    (item (when (!= (window-get-env win "par-columns" mode) "1")
            (text "Column separation:"))
      (refreshable "paragraph-formatter-columns-sep"
        (when (!= (window-get-env win "par-columns" mode) "1")
          (enum (window-set-env win "par-columns-sep" answer mode)
                (cons-new (window-get-env win "par-columns-sep" mode)
                          '("1fn" "2fn" "3fn" ""))
                (window-get-env win "par-columns-sep" mode) "10em"))))))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "format paragraph"))
  (centered
    (centered
      (bold (text "This paragraph format")))
    ===
    (dynamic (paragraph-basic-tool win :local))))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "document paragraph"))
  (centered
    (centered
      (bold (text "Global paragraph format")))
    ===
    (dynamic (paragraph-basic-tool win :global))))
