
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-scilab.scm
;; DESCRIPTION : Initialize scilab plugin
;; COPYRIGHT   : (C) 1999-2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scilab-commander s)
  (let* ((t (string->object s))
         (s (cadr t))
         (n (cAr t)))
    (string-append "scilab_complete (\"" s "\", " (number->string n) ")\n")))

(define (scilab-launcher)
  (with path "$TEXMACS_PATH/plugins/scilab/scilab/init-scilab.sce"
    (string-append
     (if (os-mingw?)
         "Scilex --texmacs -texmacs"
         "scilab --texmacs")
     " -f \"" (url-concretize (unix->url path)) "\"")))

(plugin-configure scilab
  (:macpath "scilab*" "Contents/MacOS/bin")
  (:winpath "scilab*" "bin")
  (:require (url-exists-in-path? (if(os-mingw?) "Scilex" "scilab")))
  (:launch ,(scilab-launcher))
  (:session "Scilab")
  (:commander ,scilab-commander)
  (:tab-completion #t))

(tm-define (scilab-verbatim->tree t)
  (:secure #t)
  (let* ((t (tree->stree t))
         (s (if (string? t) t ""))
         (s (string-replace s "\\\\\\\\" "\\"))
         (s (string-replace s "<gtr>" ">"))
         (s (string-replace s "<less>" "<"))
         (s (convert s "verbatim-snippet" "stm-snippet"))
         (s (string-replace s "\\\\" "\\")))
    (stree->tree (string->object s))))

(when (supports-scilab?)
  (import-from (scilab-menus))
  (with f (url-concretize
           "$TEXMACS_PATH/plugins/scilab/scilab/scilab-demo.sce")
    (if (url-exists? f)
        (scilab-add-to-demo-menu `("Basics" ,f))))
  (kbd-map
    (:mode in-scilab?)
    ("$" "$")))
