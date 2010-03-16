
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-images.scm
;; DESCRIPTION : setup image converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert images init-images))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphical document and geometric image formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format postscript
  (:name "Postscript")
  (:suffix "ps" "eps"))

(define-format pdf
  (:name "Pdf")
  (:suffix "pdf"))

(converter pdf-file postscript-file
  (:shell "pdf2ps" from to))

(converter postscript-file pdf-file
  (:shell "ps2pdf" from to))

(define-format xfig
  (:name "Xfig")
  (:suffix "fig"))

(converter xfig-file postscript-file
  (:shell "fig2ps" from to))

(define-format xmgrace
  (:name "Xmgrace")
  (:suffix "agr" "xmgr"))

(converter xmgrace-file postscript-file
  (:require (url-exists-in-path? "xmgrace"))
  (:shell "xmgrace" "-noask -hardcopy -hdevice EPS -printfile" to from))

(define-format svg
   (:name "Svg")
   (:suffix "svg"))

(converter svg-file postscript-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-P" to))

(converter svg-file png-file
   (:require (url-exists-in-path? "rsvg"))
   (:shell "rsvg" "-f png" from to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitmap image formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format xpm
  (:name "Xpm")
  (:suffix "xpm"))

(converter xpm-file ppm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format jpeg
  (:name "Jpeg")
  (:suffix "jpg" "jpeg"))

(converter jpeg-file postscript-document
  (:function image->psdoc))

(converter jpeg-file pnm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format tif
  (:name "Tif")
  (:suffix "tif"))

(converter tif-file postscript-document
  (:function image->psdoc))

(define-format ppm
  (:name "Ppm")
  (:suffix "ppm"))

(converter ppm-file gif-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format gif
  (:name "Gif")
  (:suffix "gif"))

(converter gif-file postscript-document
  (:function image->psdoc))

(converter gif-file pnm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format png
  (:name "Png")
  (:suffix "png"))

(converter png-file postscript-document
  (:function image->psdoc))

(converter png-file pnm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format pnm
  (:name "Pnm")
  (:suffix "pnm"))

(converter pnm-file postscript-document
  (:function image->psdoc))

