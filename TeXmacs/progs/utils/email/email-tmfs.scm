
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
  (escape-shell (tmstring->string id)))

(define (email-mime-type id)
  (eval-system (string-append "mmail --mime " (email-escape id))))

(define (email-arity id)
  (with n (eval-system (string-append "mmail --arity " (email-escape id)))
    (string->number n)))

(define (email-inline? id)
  (with i (eval-system (string-append "mmail --attached " (email-escape id)))
    (string-starts? i "no")))

(define (email-header id)
  (with h (eval-system (string-append "mmail --header " (email-escape id)))
    (string->object h)))

(define (extract-body doc)
  (with l (select doc '(body 0))
    (if (null? l) '(document "") (car l))))

(define (email-texmacs-body id)
  (with b (eval-system (string-append "mmail --body " (email-escape id)))
    ;;(display* "body= " b "\n")
    (convert b "texmacs-snippet" "texmacs-stree")))

(define (email-latex-body id)
  (with b (eval-system (string-append "mmail --body " (email-escape id)))
    ;;(display* "body= " b "\n")
    (convert b "latex-snippet" "texmacs-stree")))

(define (email-html-body id)
  (with b (eval-system (string-append "mmail --body " (email-escape id)))
    ;;(display* "body= " b "\n")
    (convert b "html-snippet" "texmacs-stree")))

(define (email-verbatim-body id)
  (with b (eval-system (string-append "mmail --body " (email-escape id)))
    ;;(display* "body= " b "\n")
    `(verbatim-message ,(convert b "verbatim-snippet" "texmacs-stree"))))

(define (email-best-alternative-sub ids mts l)
  (if (null? l) (car ids)
      (with i (list-find-index mts (lambda (x) (== x (car l))))
	(if i (list-ref ids i)
	    (email-best-alternative-sub ids mts (cdr l))))))

(define (email-best-alternative id)
  (let* ((n (email-arity id))
	 (ids (map (lambda (x) (string-append id "-" (number->string x)))
		   (... 1 n)))
	 (mts (map email-mime-type ids))
	 (l (list "text/x-texmacs" "application/x-texmacs"
                  "text/x-tex" "application/x-tex"
                  "text/html" "text/plain")))
    (email-best-alternative-sub ids mts l)))

(define (email-mixed-body id)
  (let* ((n (email-arity id))
	 (ids (map (lambda (x) (string-append id "-" (number->string x)))
		   (... 1 n)))
	 (oks (list-filter ids email-inline?))
	 (docs (map email-body oks))
	 (l (append-map paragraphs docs)))
    (if (null? l) "" `(document ,@l))))

(define (email-body id)
  (with t (email-mime-type id)
    ;;(display* "mime type= " t "\n")
    (cond ((== t "text/x-texmacs") (email-texmacs-body id))
	  ((== t "text/x-tex") (email-latex-body id))
	  ((== t "text/html") (email-html-body id))
	  ((== t "application/x-texmacs") (email-texmacs-body id))
	  ((== t "application/x-tex") (email-latex-body id))
	  ((== t "multipart/alternative")
	   (email-body (email-best-alternative id)))
	  ((or (== t "multipart/mixed") (== t "multipart/related"))
	   (email-mixed-body id))
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
  (cond ((== name "mailbox") "Mailbox")
        ((== name "inbox") "Inbox")
        (else
          (with s (email-escape name)
            (with r (eval-system (string-append "mmail --title " s))
              (string-append "Email -- " r))))))

(tmfs-load-handler (email name)
  (cond ((== name "mailbox") (eval-system "mmail --list"))
        ((== name "inbox") (eval-system "mmail --inbox"))
        (else (email-message name))))

(tm-define (email-open-mailbox)
  (revert-buffer-revert "tmfs://email/mailbox"))

(tm-define (email-open-inbox)
  (revert-buffer-revert "tmfs://email/inbox"))

(tm-define (email-pop)
  (system "mmail --pop-retrieve"))

(tm-define (email-settings server user pass)
  (:argument server "Pop server")
  (:argument user "User name")
  (:argument pass "Password")
  (:interactive #t)
  (with s (string-append server "\n" user "\n" pass "\n")
    (string-save s "~/MMail/inbox.info")))
