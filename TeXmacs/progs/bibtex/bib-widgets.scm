;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-widgets.scm
;; DESCRIPTION : Widgets for bibliography
;; COPYRIGHT   : (C) 2014 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO:
;;  - Handle several bibliographies in a document.
;;  - Handle external BibTeX.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex bib-widgets)
  (:use (bibtex bib-complete) (generic document-edit)))

(define bib-widget-url (string->url ""))
(define bib-widget-style "tm-plain")
(define bib-widget-use-relative? #t)
(define bib-widget-update-buffer? #t)

(define (bib-widget-url-chosen answer)
  (set! bib-widget-url answer)
  (refresh-now "bib-widget-file-input")
  (refresh-now "bib-widget-preview"))

(define (bib-widget-style-chosen answer)
  (set! bib-widget-style answer)
  (refresh-now "bib-widget-preview"))

(define (bib-widget-output-content t style)
  (if (not (tree-is? t 'string))
      `(with "bg-color" "#ffffff"
         (paragraph-box "480px" ,(bibstyle style (tree->stree t))))
      `(with "bg-color" "white"
         (paragraph-box "480px" 
           (document (concat "Please choose a " (BibTeX) " file"))))))

(define (bib-widget-output)
  (with style (if (== "tm-" (string-take bib-widget-style 3))
                  (string-drop bib-widget-style 3)
                  bib-widget-style)
    (eval `(use-modules (bibtex ,(string->symbol style))))
    (with t (parse-bib (string-load bib-widget-url))
      (stree->tree (bib-widget-output-content t style)))))

(define (bib-widget-insert doit?)
  (if doit?
      (with file (if bib-widget-use-relative? 
                     (url-delta (current-buffer) bib-widget-url)
                     bib-widget-url)
        (if (not (make-return-after))
            (insert 
             (list 'bibliography "bib" bib-widget-style (url->string file)
                   '(document ""))))
        (if bib-widget-update-buffer? (update-document "bibliography")))))

(define (bib-widget-modify doit?)
  (if doit?
      (with l (select (buffer-tree) '(:* bibliography))
        (if (== 1 (length l))
            (with t (car l)
              (tree-set! t 1 bib-widget-style)
              (tree-set! t 2 (url->string bib-widget-url))
              (if bib-widget-update-buffer? (update-document "bibliography"))
              #t)
            #f))))

(tm-widget (bib-widget-preview)
  (resize ("520px" "520px" "9999px") ("100px" "100px" "9999px")
    (scrollable 
      (refreshable "bib-widget-preview"
        (texmacs-output
         (bib-widget-output)
         '(style "generic"))))))

(tm-widget ((bibliography-widget modify?) cmd)
  (padded
    (hlist 
      (text "File:") // //
      (refreshable "bib-widget-file-input"
        (hlist
          (input (if answer
                     (begin (set! bib-widget-url (string->url answer))
                            (refresh-now "bib-widget-preview")))
                 "file" (list (url->string bib-widget-url)) "40em")
          // //
          (explicit-buttons 
            ("" 
             (choose-file 
              (lambda (answer)
                (set! bib-widget-url answer)
                (refresh-now "bib-widget-file-input")
                (refresh-now "bib-widget-preview"))
              "Choose" "bibtex"))))))
    ===
    (hlist
      ;(balloon "Use relative path:" "Select this to use a path relative to the current document. You can use this to be able to move around the folder containing your document and the bibliography.")
      (text "Use relative path:") //
      (toggle (set! bib-widget-use-relative? answer)
              bib-widget-use-relative?)
      // //
      (text "Update buffer:") // 
      (toggle (set! bib-widget-update-buffer? answer)
              bib-widget-update-buffer?)
      ///
      (text "Style:") // //
      (enum (bib-widget-style-chosen answer)
            '("tm-plain" "tm-alpha" "tm-acm" "tm-ieeetr" "tm-siam")
            bib-widget-style "10em"))
    === === ===
    (hlist // (dynamic (bib-widget-preview)) //)
    ===
    (bottom-buttons 
      >>> ("Cancel" (cmd #f)) // //
      (if modify? ("Modify current" (cmd #t)) // //)
      ("Insert" (cmd #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-bibliography-inserter)
  (set! bib-widget-url (string->url ""))
  (set! bib-widget-style "tm-plain")
  (let ((u (current-bib-file #f))
        (style (current-bib-style #f)))
  (if (and (not (url-none? u)) (!= style ""))
      (begin
        (set! bib-widget-url u)
        (set! bib-widget-style style)
        (dialogue-window (bibliography-widget #t) 
                         bib-widget-modify "Modify bibliography"))
      (dialogue-window (bibliography-widget #f)
                       bib-widget-insert "Insert bibliography"))))
