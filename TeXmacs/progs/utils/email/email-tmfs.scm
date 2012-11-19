
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : email-tmfs.scm
;; DESCRIPTION : support for reading email
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils email email-tmfs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building documents with email messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (email-escape id)
  (object->string (tmstring->string id)))

(define (email-mimetype id)
  (eval-system (string-append "mmail --mime " (email-escape id))))

(define (email-header id)
  (with h (eval-system (string-append "mmail --header " (email-escape id)))
    (string->object h)))

(define (extract-body doc)
  (with l (select doc '(body 0))
    (if (null? l) '(document "") (car l))))

(define (email-verbatim-body id)
  (with b (eval-system (string-append "mmail --body " (email-escape id)))
    ;;(display* "body= " b "\n")
    `(verbatim-message ,(convert b "verbatim-snippet" "texmacs-stree"))))

(define (email-html-body id)
  (with b (eval-system (string-append "mmail --body " (email-escape id)))
    ;;(display* "body= " b "\n")
    (convert b "html-snippet" "texmacs-stree")))

(define (email-body id)
  (with t (email-mimetype id)
    ;;(display* "mime type= " t "\n")
    (cond ((== t "text/html") (email-html-body id))
          (else (email-verbatim-body id)))))

(define (paragraphs doc)
  (cond ((== doc "") '())
        ((tm-is? doc 'document) (tm-children doc))
        (else (list doc))))

(define (email-message id)
  (let* ((h (email-header id))
         (b (email-body id))
         (d `(document ,@(paragraphs h) ,@(paragraphs b))))
    ;;(display* "document= " d "\n")
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "email"))
       (body ,d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-title-handler (email name doc)
  (if (== name "mailbox")
      "Mailbox"
      (with s (email-escape name)
        (with r (eval-system (string-append "mmail --title " s))
          (string-append "Email -- " r)))))

(tmfs-load-handler (email name)
  (if (== name "mailbox")
      (eval-system "mmail --list")
      (email-message name)))
