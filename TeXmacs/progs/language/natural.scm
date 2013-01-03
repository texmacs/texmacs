
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : natural.scm
;; DESCRIPTION : Routines related to natural language manipulation/translation
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language natural))

(define (replace-arg str arg val)
  (let ((i (string-contains str arg))
        (r (string-length arg)))
    (if i (string-append (substring str 0 i) 
                         val 
                         (string-drop str (+ i r)))
        str)))

(tm-define (tr origstr . vals)
  (with str (string-translate origstr)
    (with n 0
      (list-fold
       (lambda (val s)
         (set! n (+ n 1))
         (with arg (string-append "%" (number->string n))
           (replace-arg s arg val)))
       str vals))))

