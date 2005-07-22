
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-images.scm
;; DESCRIPTION : setup image converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  (:require (url-exists-in-path? "xpmtoppm"))
  (:shell "xpmtoppm" from ">" to))

(define-format jpeg
  (:name "Jpeg")
  (:suffix "jpg" "jpeg"))

(converter jpeg-file postscript-file
  (:shell "convert" from to))

(converter jpeg-file postscript-file
  (:shell "jpeg2ps" from ">" to))

(converter jpeg-file pnm-file
  (:shell "djpeg" "-pnm" from ">" to))

(define-format tif
  (:name "Tif")
  (:suffix "tif"))

(converter tif-file postscript-file
  (:shell "tiff2ps" from ">" to))

(define-format ppm
  (:name "Ppm")
  (:suffix "ppm"))

(converter ppm-file gif-file
  (:shell "ppmtogif" from ">" to))

(define-format gif
  (:name "Gif")
  (:suffix "gif"))

(converter gif-file pnm-file
  (:shell "giftopnm" from "| cat >" to))

(define-format png
  (:name "Png")
  (:suffix "png"))

(converter png-file pnm-file
  (:shell "pngtopnm" from ">" to))

(define-format pnm
  (:name "Pnm")
  (:suffix "pnm"))

(converter pnm-file postscript-document
  (:require (url-exists-in-path? "pnmtops"))
  (:function (lambda (file)
	       (eval-system (string-append "pnmtops -noturn "
					   (url->string file))))))
