
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-elsevier.scm
;; DESCRIPTION : special conversions for elsevier styles
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-elsevier)
  (:use (convert latex tmtex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of elsevier style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define elsevier-counter 0)
(define elsevier-thanks '())
(define elsevier-abstract #f)

(tm-define (init-elsevier body)
  (:synopsis "Initialize Elsevier style")
  (set! elsevier-counter 0)
  (set! elsevier-thanks '())
  (with l (select body '(:* abstract))
    (set! elsevier-abstract (and (nnull? l) (list-2? (car l)) (cadar l)))))

(tm-define (tmtex-style-init body)
  (:mode elsevier-style?)
  (init-elsevier body))

(define (elsevier-label)
  (set! elsevier-counter (+ elsevier-counter 1))
  (number->string elsevier-counter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titles of documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (elsevier-produce start l)
  (if (null? l) '()
      (list (rcons start (tex-concat* l)))))

(define (elsevier-author tag)
  (let* ((name (tmtex-select-data tag 'author-name))
	 (address (tmtex-select-data tag 'author-address))
	 (note (tmtex-select-data tag 'author-note))
	 (email (tmtex-select-data tag 'author-email))
	 (homepage (tmtex-select-data tag 'author-homepage)))
    (when (nnull? note)
      (let* ((label (elsevier-label))
	     (ref `(thanksref ,label))
	     (thanks (elsevier-produce `(thanks (!option ,label)) note)))
	(set! name (rcons name ref))
	(set! elsevier-thanks (rcons elsevier-thanks (car thanks)))))
    `(,@(elsevier-produce '(author) name)
      ,@(elsevier-produce '(address) address)
      ,@(elsevier-produce '(ead) email)
      ,@(elsevier-produce '(ead (!option "url")) homepage))))

(tm-define (tmtex-doc-data s l)
  (:mode elsevier-style?)
  (let* ((tag (cons s l))
	 (title (tmtex-select-data tag 'doc-title))
	 (note (tmtex-select-data tag 'author-note))
	 (authors (map elsevier-author (select tag '(doc-author-data))))
	 (keywords (append-map cdr (select tag '(doc-keywords))))
	 (abstract (and elsevier-abstract (tmtex elsevier-abstract))))
    (when (nnull? note)
      (let* ((label (elsevier-label))
	     (ref `(thanksref ,label))
	     (thanks (elsevier-produce `(thanks (!option ,label)) note)))
	(set! title (rcons title ref))
	(set! elsevier-thanks (cons (car thanks) elsevier-thanks))))
    (when (nnull? keywords)
      (with l (list-intersperse (map tmtex keywords) '(!group (sep)))
	(set! keywords `(((!begin "keyword") ,(tex-concat* l))))))
    `((!begin "frontmatter")
       (!document
	 ,@(elsevier-produce '(title) title)
	 ,@(apply append authors)
	 ,@elsevier-thanks
	 ,@(if abstract `(((!begin "abstract") ,abstract)) '())
	 ,@keywords))))

(tm-define (tmtex-abstract s l)
  (:mode elsevier-style?)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Elsevier style is quite ugly.
;; Transform equations into eqnarray* for more uniform alignment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-equation s l)
  (:mode elsevier-style?)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex (car l))))
    (tmtex-env-reset "mode")
    (if (== s "equation")
	(list (list '!begin "eqnarray") r)  ;; FIXME: why do elsequation
	(list (list '!begin "eqnarray*") r) ;; and elsequation* not work?
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations for JSC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-style-init body)
  (:mode jsc-style?)
  (init-elsevier body)
  (set! tmtex-packages (cons "natbib" tmtex-packages))
  (latex-set-packages '("amsthm" "yjsco" "natbib")))
