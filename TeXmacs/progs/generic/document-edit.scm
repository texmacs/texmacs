
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-edit.scm
;; DESCRIPTION : setting global document properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-edit)
  (:use (utils base environment)
        (utils library length)
        (utils library cursor)
        (generic generic-edit)
        (generic document-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (project-attach master)
  (:argument master "file" "Master file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preamble mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-source-mode?)
  (== (get-env "preamble") "true"))

(tm-define (toggle-source-mode)
  (:synopsis "Toggle source code editing mode.")
  (:check-mark "v" in-source-mode?)
  (let ((new (if (string=? (get-env "preamble") "true") "false" "true")))
    (init-env "preamble" new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default? . vals)
  (if (null? vals)
      #t
      (and (not (init-has? (car vals)))
           (apply test-default? (cdr vals)))))

(tm-define (init-default . args)
  (:check-mark "*" test-default?)
  (for-each init-default-one args))

(tm-define (get-init-env s)
  (with t (get-init-tree s)
    (cond ((tree-atomic? t) (tree->string t))
          ((and (tree-func? t 'macro 1) (tree-atomic? (tree-ref t 0)))
           (tree->string (tree-ref t 0)))
          (else #f))))

(tm-define (test-init? var val)
  (== (get-init-tree var) (string->tree val)))

(tm-property (init-env var val)
  (:check-mark "*" test-init?))

(tm-define (set-init-env s val)
  (with old (get-init-tree s)
    (if (and (tree-func? old 'macro 1) (not (tm-is? val 'macro)))
        (init-env-tree s `(macro ,val))
        (init-env-tree s val))))

(tm-define (init-interactive-env var)
  (:interactive #t)
  (interactive (lambda (s) (set-init-env var s))
    (list (or (logic-ref env-var-description% var) var) "string"
          (get-init-env var))))

(tm-define (test-init-true? var)
  (test-init? var "true"))

(tm-define (toggle-init-env var)
  (:check-mark "*" test-init-true?)
  (with new (if (== (get-init-env var) "true") "false" "true")
    (init-default var)
    (delayed
      (when (!= new (get-init-env var))
        (set-init-env var new)))))

(define (init-multi* l)
  (when (and (nnull? l) (nnull? (cdr l)))
    (init-env (car l) (cadr l))
    (init-multi* (cddr l))))

(tm-define (init-multi l)
  (if (and (list-2? l) (== (car l) "font"))
      (init-font (cadr l))
      (init-multi* l)))

(tm-define (test-init-font? val . opts)
  (== (font-family-main (get-init "font")) val))

(tm-define (remove-font-packages)
  (with l (get-style-list)
    (with f (list-filter l (lambda (p) (not (string-ends? p "-font"))))
      (set-style-list f))))

(define (font-package-name val)
  (cond ((== val "Fira") "fira-font")
        ((== val "Linux Biolinum") "biolinum-font")
        ((== val "Linux Libertine") "libertine-font")
        (else (string-append val "-font"))))

(tm-define (init-font val . opts)
  (:check-mark "*" test-init-font?)
  (cond ((== val "TeXmacs Computer Modern")
         (init-font "roman" "roman"))
        ((and (== val "roman") (!= opts (list "roman")))
         (init-font "roman" "roman"))
        ((string-starts? val "Stix")
         (init-font "stix" "math-stix"))
        ((string-starts? val "TeX Gyre Bonum")
         (init-font "bonum" "math-bonum"))
        ((string-starts? val "TeX Gyre Pagella")
         (init-font "pagella" "math-pagella"))
        ((string-starts? val "TeX Gyre Schola")
         (init-font "schola" "math-schola"))
        ((string-starts? val "TeX Gyre Termes")
         (init-font "termes" "math-termes"))
        (else
          (init-env "font" val)
          (when (nnull? opts)
            (init-env "math-font" (car opts)))
          (init-env "font-family" "rm")
          (remove-font-packages)
          (with pack (font-package-name val)
            (with dir "$TEXMACS_PATH/packages/customize/fonts"
              (when (url-exists? (url-append dir (string-append pack ".ts")))
                (init-default "font")
                (init-default "font-family")
                (add-style-package pack)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial environment management in specific buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (initial-set-tree u var val)
  (when (tm? val)
    (with-buffer u
      (init-env-tree var val))))

(tm-define (initial-set u var val)
  (when (string? val)
    (with-buffer u
      (init-env var val))))

(tm-define (initial-get-tree u var)
  (or (with-buffer u
        (get-init-tree var))
      (tree "")))

(tm-define (initial-get u var)
  (or (with-buffer u
        (get-init var))
      ""))

(tm-define (initial-defined? u var)
  (with-buffer u
    (style-has? var)))

(tm-define (initial-has? u var)
  (with-buffer u
    (init-has? var)))

(tm-define (initial-default u . vars)
  (with-buffer u
    (apply init-default vars)))


(tm-define (buffer-get-metadata u kind)
  (or (with-buffer u
        (get-metadata kind))
      ""))

(tm-define (buffer-has-biblio? u)
  (with-buffer u
    (with l (list-attachments)
      (nnull? (list-filter l (cut string-ends? <> "-bibliography"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default-document-language?)
  (null? (list-intersection (get-style-list) supported-languages)))

(tm-define (set-default-document-language)
  (:check-mark "*" test-default-document-language?)
  (let* ((old (get-style-list))
         (new (list-difference old supported-languages)))
    (when (!= new old)
      (set-style-list new))))

(tm-define (get-document-language)
  (with l (list-intersection (get-style-list) supported-languages)
    (if (null? l) (get-init "language") (car l))))

(tm-define (test-document-language? s)
  (== s (get-document-language)))

(tm-define (set-document-language lan)
  (:check-mark "*" test-document-language?)
  (let* ((old (get-style-list))
         (rem (list-difference old supported-languages))
         (new (append rem (if (== lan "english") (list) (list lan)))))
    (when (!= new old)
      (set-style-list new))))

(define (search-env-var t which)
  (cond ((nlist? t) #f)
        ((null? t) #f)
        ((match? t '(associate "language" :%1)) (caddr t))
        (else (let ((val (search-env-var (car t) which)))
                (if val val (search-env-var (cdr t) which))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main page layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-default-page-medium?) (test-default? "page-medium"))
(tm-define (init-default-page-medium)
  (:check-mark "*" test-default-page-medium?)
  (init-default "page-medium")
  (notify-page-change))

(define (test-page-medium? s) (== (get-init "page-medium") s))
(tm-define (init-page-medium s)
  (:check-mark "*" test-page-medium?)
  (init-env "page-medium" s)
  (notify-page-change))

(define (test-default-page-type?)
  (test-default? "page-type" "page-width" "page-height"))
(tm-define (default-page-type)
  (:check-mark "*" test-default-page-type?)
  (init-default "page-type" "page-width" "page-height")
  (notify-page-change))

(define (test-page-type? s) (== (get-init "page-type") s))
(tm-define (init-page-type s)
  (:check-mark "*" test-page-type?)
  (init-env "page-type" s)
  (init-env "page-width" "auto")
  (init-env "page-height" "auto")
  (notify-page-change))

(tm-define (init-page-size w h)
  (:argument w "Page width")
  (:argument h "Page height")
  (init-env "page-type" "user")
  (init-env "page-width" w)
  (init-env "page-height" h)
  (notify-page-change))

(define (test-default-page-orientation?) (test-default? "page-orientation"))
(tm-define (init-default-page-orientation)
  (:check-mark "*" test-default-page-orientation?)
  (init-default "page-orientation")
  (notify-page-change))

(define (test-page-orientation? s) (string=? (get-env "page-orientation") s))
(tm-define (init-page-orientation s)
  (:check-mark "*" test-page-orientation?)
  (init-env "page-orientation" s)
  (notify-page-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper for global page rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (panorama-packets)
  (let* ((nr (nr-pages))
         (ww (get-window-width))
         (wh (get-window-height))
         (pw (get-page-width #f))
         (ph (get-page-height #f))
         (best-n 0)
         (best-f 0))
    (for (n (.. 1 (+ nr 1)))
      (let* ((r (quotient (+ nr (- n 1)) n))
             (tw (* n pw))
             (th (* r ph))
             (aw (- ww (* n 5120)))
             (ah (- wh (* r 5120)))
             (fw (/ (* 1.0 aw) tw))
             (fh (/ (* 1.0 ah) th))
             (f (min fw fh)))
        (when (or (== n 1) (> f best-f))
          (set! best-n n)
          (set! best-f f))))
    (cond ((> best-n 0) best-n)
          ((> nr 10) 10)
          (else (inexact->exact (ceiling (sqrt (* 1.0 nr))))))))

(define (test-default-page-rendering?) (test-default? "page-medium"))
(tm-define (init-default-page-rendering)
  (:check-mark "*" test-default-page-rendering?)
  (init-default "page-medium")
  (init-default "page-border")
  (init-default "page-packet")
  (init-default "page-offset")
  (notify-page-change))

(tm-define (get-init-page-rendering)
  (cond ((== (get-init "page-border") "attached") "book")
        ((!= (get-init "page-packet") "1") "panorama")
        (else (get-init "page-medium"))))

(define (test-page-rendering? s) (== (get-init-page-rendering) s))
(tm-define (init-page-rendering s)
  (:check-mark "*" test-page-rendering?)
  (when (in? s (list "paper" "papyrus"))
    (set-preference "page medium" s))
  (save-zoom (get-init-page-rendering))
  (cond ((== s "book")
         (init-env "page-medium" "paper")
         (init-env "page-border" "attached")
         (init-env "page-packet" "2")
         (init-env "page-offset" "1")
	 (notify-page-change)
	 (delayed (:idle 25) (restore-zoom s)))
        ((== s "panorama")
         (init-env "page-medium" "paper")
         (init-env "page-packet" (number->string (panorama-packets)))
         (init-default "page-border")
         (init-default "page-offset")
	 (notify-page-change)
	 (delayed (:idle 25) (fit-all-to-screen)))
        (else
          (init-env "page-medium" s)
          (init-default "page-border")
          (init-default "page-packet")
          (init-default "page-offset")
	  (notify-page-change)
	  (delayed (:idle 25) (restore-zoom s)))))

(tm-define (initial-get-page-rendering u)
  (with-buffer u
    (get-init-page-rendering)))

(tm-define (initial-set-page-rendering u s)
  (with-buffer u
    (init-page-rendering s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further page layout settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visible-header-and-footer?)
  (== (get-env "page-show-hf") "true"))

(tm-define (toggle-visible-header-and-footer)
  (:synopsis "Toggle visibility of headers and footers in 'page' paper mode.")
  (:check-mark "v" visible-header-and-footer?)
  (init-env "page-show-hf"
            (if (== (get-env "page-show-hf") "true") "false" "true")))

(define (page-width-margin?)
  (== (get-env "page-width-margin") "true"))

(tm-define (toggle-page-width-margin)
  (:synopsis "Toggle mode for determining margins from paragraph width.")
  (:check-mark "v" page-width-margin?)
  (init-env "page-width-margin" (if (page-width-margin?) "false" "true")))

(define (not-page-screen-margin?)
  (== (get-env "page-screen-margin") "false"))

(tm-define (toggle-page-screen-margin)
  (:synopsis "Toggle mode for using special margins for screen editing.")
  (:check-mark "v" not-page-screen-margin?)
  (init-env "page-screen-margin"
            (if (not-page-screen-margin?) "true" "false")))

(define (reduced-margins?)
  (test-init? "page-odd" "1cm"))

(tm-define (toggle-reduced-margins)
  (:synopsis "Toggle mode for using reduced margins to save paper.")
  (:check-mark "v" reduced-margins?)
  (cond ((has-style-package? "reduced-margins")
         (remove-style-package "reduced-margins"))
        ((has-style-package? "normal-margins")
         (remove-style-package "normal-margins"))
        ((reduced-margins?)
         (add-style-package "normal-margins"))
        (else
         (add-style-package "reduced-margins"))))

(define (indent-paragraphs?)
  (nin? (get-init-env "par-first")
        (list "0fn" "0em" "0tab" "0cm" "0mm" "0in")))

(tm-define (toggle-indent-paragraphs)
  (:synopsis "Toggle mode for using a first indentation for each paragraph")
  (:check-mark "v" indent-paragraphs?)
  (cond ((has-style-package? "indent-paragraphs")
         (remove-style-package "indent-paragraphs"))
        ((has-style-package? "padded-paragraphs")
         (remove-style-package "padded-paragraphs"))
        ((indent-paragraphs?)
         (add-style-package "padded-paragraphs"))
        (else
         (add-style-package "indent-paragraphs"))))

(define (no-page-numbers?)
  (test-init? "no-page-numbers" "true"))

(tm-define (toggle-no-page-numbers)
  (:synopsis "Toggle mode for using standard page numbering")
  (:check-mark "v" no-page-numbers?)
  (cond ((has-style-package? "page-numbers")
         (remove-style-package "page-numbers"))
        ((has-style-package? "no-page-numbers")
         (remove-style-package "no-page-numbers"))
        ((no-page-numbers?)
         (add-style-package "page-numbers"))
        (else
         (add-style-package "no-page-numbers"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citing TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-search-first lab)
  (safe-car (tree-search (buffer-tree) (cut tree-is? <> lab))))

(define (acknowledged-texmacs? . refs)
  (if (null? refs)
      (document-search-first 'cite-website)
      (document-search-first 'cite-TeXmacs)))

(define (add-biblio)
  (with bib (document-search-first 'bibliography)
    (when (and (not bib) (tree-is? (buffer-tree) 'document))
      (tree-insert (buffer-tree) (tree-arity (buffer-tree))
                   `((bibliography "bib" "tm-plain" "" (document "")))))))

(define (update-biblio)
  (update-document "bibliography")
  (delayed (:idle 1) (update-document "bibliography")))

(tm-define (acknowledge-texmacs . refs)
  (:synopsis "Acknowledge that a document has been written using TeXmacs")
  (:check-mark "v" acknowledged-texmacs?)
  (let* ((tit  (document-search-first 'doc-data))
         (bib  (document-search-first 'bibliography))
         (aweb (document-search-first 'cite-website))
         (nweb (and aweb (tree-search-upwards aweb 'doc-note)))
         (acit (document-search-first 'cite-TeXmacs))
         (ncit (and acit (tree-search-upwards acit 'doc-note))))
    (cond ((and nweb (nnull? refs))
           (tree-set! aweb `(cite-TeXmacs ,@refs))
           (update-biblio))
          ((and ncit (null? refs))
           (tree-set! acit `(cite-website))
           (update-biblio))
          ((and nweb (null? refs))
           (tree-remove (tree-up nweb) (tree-index nweb) 1)
           (update-biblio))
          ((and aweb (null? refs))
           (tree-cut aweb))
          ((and ncit (nnull? refs))
           (tree-remove (tree-up ncit) (tree-index ncit) 1)
           (update-biblio))
          ((and tit (nnull? refs))
           (add-biblio)
           (tree-insert! tit (tree-arity tit)
                         `((doc-note (document (cite-TeXmacs ,@refs)))))
           (update-biblio))
          ((and tit (null? refs))
           (tree-insert! tit (tree-arity tit)
                         `((doc-note (document (cite-website))))))
          ((nnull? refs)
           (add-biblio)
           (insert `(cite-TeXmacs ,@refs))
           (update-biblio))
          (else (make 'cite-website)))))

(define (is-cite? t)
  (tree-in? t '(cite nocite cite-TeXmacs)))

(define (is-citation? t ref types)
  (and (tm-equal? t ref)
       (tree-up t)
       (tree-in? (tree-up t) types)))

(define (cited-texmacs? ref)
  (let* ((l '(cite nocite cite-TeXmacs))
         (pred? (cut is-citation? <> ref l))
         (alt-pred? (cut is-citation? <> ref '(cite-TeXmacs))))
    (cond ((is-cite? (cursor-tree))
           (safe-car (tree-search (cursor-tree) pred?)))
          ((is-cite? (tree-up (cursor-tree)))
           (safe-car (tree-search (tree-up (cursor-tree)) pred?)))
          ((document-search-first 'doc-data)
           (safe-car (tree-search (buffer-tree) alt-pred?)))
          (else (safe-car (tree-search (buffer-tree) pred?))))))

(tm-define (cite-texmacs ref)
  (:synopsis "Cite a paper or other work on TeXmacs")
  (:check-mark "v" cited-texmacs?)
  (cond ((cited-texmacs? ref)
         (with t (cited-texmacs? ref)
           (cond ((and (== (tree-arity (tree-up t)) 1)
                       (tree-search-upwards t 'doc-note))
                  (with note (tree-search-upwards t 'doc-note)
                    (tree-remove (tree-up note) (tree-index note) 1)
                    (update-biblio)))
                 ((> (tree-arity (tree-up t)) 1)
                  (tree-remove (tree-up t) (tree-index t) 1)))))
        ((tree-in? (cursor-tree) '(cite nocite cite-TeXmacs))
         (tree-insert (cursor-tree) (tree-arity (cursor-tree)) (list ref))
         (update-biblio))
        ((and (tree-in? (tree-up (cursor-tree)) '(cite nocite cite-TeXmacs))
              (tm-equal? (cursor-tree) ""))
         (tree-set (cursor-tree) ref))
        ((tree-in? (tree-up (cursor-tree)) '(cite nocite cite-TeXmacs))
         (with i (+ (tree-index (cursor-tree)) 1)
           (tree-insert (tree-up (cursor-tree)) i (list ref))
           (tree-go-to (tree-up (cursor-tree)) i :end)))
        ((document-search-first 'cite-TeXmacs)
         (with t (document-search-first 'cite-TeXmacs)
           (tree-insert t (tree-arity t) (list ref))
           (update-biblio)))
        ((document-search-first 'doc-data)
         (acknowledge-texmacs ref))
        (else
         (add-biblio)
         (insert `(cite ,ref))
         (update-biblio))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc-update-times 1)

(define (notify-doc-update-times var val)
  (with n (cond ((string? val) (or (string->number val) 1))
                ((number? val) val)
                (else 1))
    (set! doc-update-times (min (max 1 n) 5)))) ; Just in case

(define-preferences 
  ("document update times" "1" notify-doc-update-times))

(define (wait-update-current-buffer)
  (system-wait "Updating current buffer, " "please wait")
  (update-current-buffer))

(tm-define (update-document what)
  (for (.. 0 doc-update-times)       
    (delayed    ; allow typesetting/magic to happen before next update
      (:idle 1)
      (cursor-after
       (cond ((== what "all") 
              (generate-all-aux) (inclusions-gc) (wait-update-current-buffer))
             ((== what "bibliography")
              (generate-all-aux) (wait-update-current-buffer))
             ((== what "buffer") 
              (wait-update-current-buffer))
             (else (generate-aux what)))))))
