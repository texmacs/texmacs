
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-ams.scm
;; DESCRIPTION : special conversions for AMS styles
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven, François Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-ams)
  (:use (convert latex tmtex)
        (convert latex latex-define)))

(tm-define (tmtex-transform-style x)
  (:mode ams-style?) x)

(tm-define (tmtex-provided-packages)
  (:mode ams-style?)
  '("amsmath"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS data preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-contains? t u)
  (cond ((== t u) #t)
        ((nlist? t) #f)
        ((null? t) #f)
        (else (or (stree-contains? (car t) u)
                  (in? #t (map (lambda (x) (stree-contains? x u)) (cdr t)))))))

(define (insert-maketitle-after t u)
  (cond ((nlist? t) t)
        ((== (car t) u) `(!document ,t (maketitle)))
        (else `(,(car t) ,@(map (lambda (x) (insert-maketitle-after x u))
                                (cdr t))))))

(tm-define (tmtex-style-preprocess doc)
  (:mode ams-style?)
  (cond ((stree-contains? doc 'abstract-data)
         (insert-maketitle-after doc 'abstract-data))
        ((stree-contains? doc 'doc-data)
         (insert-maketitle-after doc 'doc-data))
        (else doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode ams-style?)
  (let* ((names (map (lambda (x) `(author ,x))
                     (list-intersperse (map cadr names) '(tmSep))))
         (result `(,@names ,@affiliations ,@emails ,@urls ,@notes ,@miscs)))
    (if (null? result) '() `(!paragraph ,@result))))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes
                                subtits-l dates-l miscs-l notes-l tr ar)
  (:mode ams-style?)
  (let* ((title-opt  (if (null? tr) '() `((!option ,@(tmtex-concat-Sep tr)))))
         (titles     (tmtex-concat-Sep (map cadr titles)))
         (titles     (if (null? titles) '() `((title ,@title-opt ,@titles))))
         (title-data `(,@titles ,@subtitles ,@notes ,@miscs))
         (title-data (if (null? title-data) '() `((!paragraph ,@title-data))))
         (authors*   (filter pair? authors)))
    (if (and (null? title-data) (null? authors*) (null? dates)) '()
        `(!document ,@title-data ,@authors* ,@dates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-doc-running-title t)
  (:mode ams-style?)
  (tmtex (cadr t)))

(tm-define (tmtex-doc-subtitle t)
  (:mode ams-style?)
  `(tmsubtitle ,(tmtex (cadr t))))

(tm-define (tmtex-doc-note t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  (:mode ams-style?)
  `(date ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation t)
  (:mode ams-style?)
  `(address ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(email ,(tmtex (cadr t))))

(tm-define (tmtex-author-homepage t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(urladdr ,(tmtex (cadr t))))

(tm-define (tmtex-author-note t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS specific abstract markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-in-abstract what in)
  (if (null? in)
    (if (null? what) '() `(((!begin "abstract") (document ,@what))))
    `(((!begin "abstract") (!document ,@(map cadr in) ,@what)))))

(tm-define  (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)
  (:mode ams-style?)
  (with class `(,@acm ,@arxiv ,@pacs)
    (set! abstract (move-in-abstract class abstract)))
  (with result `(,@abstract ,@msc ,@keywords)
    (if (null? result) "" `(!document ,@result))))

(tm-define (tmtex-abstract-keywords t)
  (:mode ams-style?)
  (with args (tmtex-concat-sep (map tmtex (cdr t)))
    `(keywords ,@args)))

(tm-define (tmtex-abstract-msc t)
  (:mode ams-style?)
  (with args (tmtex-concat-Sep (map tmtex (cdr t)))
    `(subjclass ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMS specific macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (:mode ams-style?)
  (qed #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (ams-latex-texmacs-remark prim name)
  `(latex-texmacs-thmenv ,prim ,name
                         ("{" (!recurse (theoremstyle "remark"))) ("}")
                         ams-style?))

(ams-latex-texmacs-remark "remark" "Remark")
(ams-latex-texmacs-remark "note" "Note")
(ams-latex-texmacs-remark "example" "Example")
(ams-latex-texmacs-remark "convention" "Convention")
(ams-latex-texmacs-remark "warning" "Warning")
(ams-latex-texmacs-remark "acknowledgments" "Acknowledgments")
(ams-latex-texmacs-remark "answer" "Answer")
(ams-latex-texmacs-remark "question" "Question")
(ams-latex-texmacs-remark "exercise" "Exercise")
(ams-latex-texmacs-remark "problem" "Problem")
(ams-latex-texmacs-remark "solution" "Solution")
