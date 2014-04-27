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
;;  - Handle external BibTeX.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex bib-widgets)
  (:use (bibtex bib-complete) (generic document-edit)))

(define bibwid-url (string->url ""))
(define bibwid-style "tm-plain")
(define bibwid-use-relative? #t)
(define bibwid-update-buffer? #t)
(define bibwid-cwd (string->url ""))

(define url-sep (if (os-win32?) "\\" "/"))
(define (url-delta* u1 u2)
  (with u (url-delta (url->string (url-glue u1 url-sep)) u2)
    (if (or (url-rooted? u) (equal? (url-tail u) u))
        u
        (url-append (url-parent) u))))

;;;;; HACK!!! url-delta seems bogus
(with u "/some/test/here.blah"
  (if (== (url-delta (url-head u) u) (url-tail u))
      (set! url-delta* url-delta)))

(define (bibwid-set-url u)
  (cond ((and (== bibwid-use-relative? #t) (url-rooted? u))
         (set! bibwid-url (url-delta* bibwid-cwd u)))
        ((and (== bibwid-use-relative? #f) (not (url-rooted? u)))
         (set! bibwid-url (url-append bibwid-cwd u)))
        (else (set! bibwid-url u))))

(define (bibwid-set-style answer)
  (set! bibwid-style answer)
  (refresh-now "bibwid-preview"))

(define (bibwid-output-content t style)
  (if (tree-is? t 'string) 
      '(with "bg-color" "white"
         (paragraph-box "480px" 
           (document (concat "Please choose a valid " (BibTeX) " file"))))
      `(with "bg-color" "#ffffff"
         (paragraph-box "480px" ,(bibstyle style (tree->stree t))))))

(define (bibwid-output)
  (with style (if (== "tm-" (string-take bibwid-style 3))
                  (string-drop bibwid-style 3)
                  bibwid-style)
    (eval `(use-modules (bibtex ,(string->symbol style))))
    (with u (if (and bibwid-use-relative? (not (url-rooted? bibwid-url)))
                (url-append bibwid-cwd bibwid-url)
                bibwid-url)
      (with t (if (url-exists? u) 
                  (parse-bib (string-load u)) 
                  (tree ""))
        (stree->tree (bibwid-output-content t style))))))

(define (bibwid-insert doit?)
  (when doit?
    (if (not (make-return-after))
        (insert 
         (list 'bibliography "bib" bibwid-style (url->string bibwid-url)
               '(document ""))))
    (if bibwid-update-buffer? (update-document "bibliography"))))

(define (bibwid-modify doit?)
  (when doit?
    (with l (select (buffer-tree) '(:* bibliography))
      (when (> (length l) 0)
        (with t (car l)
          (tree-set! t 1 bibwid-style)
          (tree-set! t 2 (url->string bibwid-url))
          (if bibwid-update-buffer? (update-document "bibliography")))))))

(define (bibwid-set-filename u)
  (bibwid-set-url u)
  (refresh-now "bibwid-file-input")
  (refresh-now "bibwid-preview"))

(define (bibwid-set-relative val)
  (set! bibwid-use-relative? val)
  (bibwid-set-filename bibwid-url))

(tm-widget (bibwid-preview)
  (resize ("520px" "520px" "9999px") ("100px" "100px" "9999px")
    (scrollable 
      (refreshable "bibwid-preview"
        (texmacs-output
         (bibwid-output)
         '(style "generic"))))))

(tm-widget ((bibliography-widget modify?) cmd)
  (padded
    (hlist 
      (text "File:") // //
      (refreshable "bibwid-file-input"
        (hlist
          (input (when (and answer (!= answer (url->string bibwid-url)))
                   (bibwid-set-url (string->url answer))
                   (refresh-now "bibwid-preview"))
                 "file" (list (url->string bibwid-url)) "40em")
          // //
          (explicit-buttons 
            ("" (choose-file bibwid-set-filename "Choose" "bibtex"))))))
    ===
    (hlist
      ;(balloon "Use relative path:" "Select this to use a path relative to the current document. You can use this to be able to move around the folder containing your document and the bibliography.")
      (text "Use relative path:") //
      (toggle (bibwid-set-relative answer) bibwid-use-relative?)
      // //
      (text "Update buffer:") // 
      (toggle (set! bibwid-update-buffer? answer)
              bibwid-update-buffer?)
      ///
      (text "Style:") // //
      (enum (bibwid-set-style answer)
            '("tm-plain" "tm-alpha" "tm-acm" "tm-ieeetr" "tm-siam")
            bibwid-style "10em"))
    === === ===
    (hlist // (dynamic (bibwid-preview)) //)
    ===
    (bottom-buttons >>>
      ("Cancel" (cmd #f)) // //
      (if modify? ("Modify" (cmd #t)))
      (if (not modify?) ("Insert" (cmd #t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-bibliography-inserter)
  (set! bibwid-url (string->url ""))
  (set! bibwid-style "tm-plain")
  (set! bibwid-cwd (url-head (current-buffer)))

  (let ((u (current-bib-file #f))
        (style (current-bib-style #f)))
  (if (and (not (url-none? u)) (!= style ""))
      (begin
        (bibwid-set-url u)
        (set! bibwid-style style)
        (dialogue-window (bibliography-widget #t) 
                         bibwid-modify "Modify bibliography"))
      (dialogue-window (bibliography-widget #f)
                       bibwid-insert "Insert bibliography"))))
