
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-preamble.scm
;; DESCRIPTION : automatic generation of TeXmacs specific preamble
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: find a way to construct drd-tables for each style and
;;       each language using a suitable DRD mechanism
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-preamble)
  (:use (convert latex latex-drd)
	(convert latex texout)
	(convert latex latex-texmacs-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tmtex-preamble-language-def%
  ("bulgarian"
   "\\usepackage[cp1251]{inputenc}\n\\usepackage[bulgarian]{babel}")
  ("czech" "\\usepackage[czech]{babel}")
  ("danish" "\\usepackage[danish]{babel}")
  ("dutch" "\\usepackage[dutch]{babel}")
  ("finnish" "\\usepackage[finnish]{babel}")
  ("french" "\\usepackage[french]{babel}")
  ("german" "\\usepackage[german]{babel}")
  ("hungarian" "\\usepackage[hungarian]{babel}")
  ("italian" "\\usepackage[italian]{babel}")
  ("polish" "\\usepackage[polish]{babel}")
  ("portuguese" "\\usepackage[portuges]{babel}")
  ("romanian" "\\usepackage[romanian]{babel}")
  ("russian" "\\usepackage[cp1251]{inputenc}\n\\usepackage[russian]{babel}")
  ("slovene" "\\usepackage[slovene]{babel}")
  ("spanish" "\\usepackage[spanish]{babel}")
  ("swedish" "\\usepackage[swedish]{babel}")
  ("ukrainian"
   "\\usepackage[cp1251]{inputenc}\n\\usepackage[ukrainian]{babel}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page size settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tmpre-paper-type%
  ("a0" "a0paper")
  ("a1" "a1paper")
  ("a2" "a2paper")
  ("a3" "a3paper")
  ("a4" "a4paper")
  ("a5" "a5paper")
  ("a6" "a6paper")
  ("a7" "papersize={74mm,105mm}")
  ("a8" "papersize={52mm,74mm")
  ("a9" "papersize={37mm,52mm}")
  ("b0" "b0paper")
  ("b1" "b1paper")
  ("b2" "b2paper")
  ("b3" "b3paper")
  ("b4" "b4paper")
  ("b5" "b5paper")
  ("b6" "b6paper")
  ("b7" "papersize={88mm,125mm}")
  ("b8" "papersize={62mm,88mm}")
  ("b9" "papersize={44mm,62mm}")
  ("legal" "legalpaper")
  ("letter" "letterpaper")
  ("executive" "executivepaper")
  ("archA" "papersize={9in,12in}")
  ("archB" "papersize={12in,18in}")
  ("archC" "papersize={18in,24in}")
  ("archD" "papersize={24in,36in}")
  ("archE" "papersize={36in,48in}")
  ("10x14" "papersize={10in,14in}")
  ("11x17" "papersize={11in,17in}")
  ("C5" "papersize={162mm,229mm}")
  ("Comm10" "papersize={297pt,684pt}")
  ("DL" "papersize={110mm,220mm}")
  ("halfletter" "papersize={140mm,216mm}")
  ("halfexecutive" "papersize={133mm,184mm}")
  ("ledger" "papersize={432mm,279mm}")
  ("Monarch" "papersize={98mm,190mm}")
  ("csheet" "papersize={432mm,559mm}")
  ("dsheet" "papersize={559mm,864mm}")
  ("esheet" "papersize={864mm,1118mm}")
  ("flsa" "papersize={216mm,330mm}")
  ("flse" "papersize={216mm,330mm}")
  ("folio" "papersize={216mm,330mm}")
  ("lecture note" "papersize={15.5cm,23.5cm}")
  ("note" "papersize={216mm,279mm}")
  ("quarto" "papersize={215mm,275mm}")
  ("statement" "papersize={140mm,216mm}")
  ("tabloid" "papersize={279mm,432mm}"))

(define (tmtex-preamble-page-type init)
  (let* ((page-type (ahash-ref init "page-type"))
	 (page-size (drd-ref tmpre-paper-type% page-type)))
    (if page-size
	(begin
	  (ahash-set! tmtex-preamble-uses "geometry" #t)
	  (set! tmtex-preamble-init
		(string-append tmtex-preamble-init
			       "\\geometry{" page-size "}\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the preamble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmtex-preamble-uses (make-ahash-table))
(define tmtex-preamble-init "")
(define tmtex-preamble-result "")

(define (tmtex-preamble-test-insert s)
  (with packlist (drd-ref-list latex-needs% s)
    (if packlist
	(for-each 
	  (lambda (pack)
	    (if (not (ahash-ref tmtex-preamble-uses pack))
		(ahash-set! tmtex-preamble-uses pack #t)))
          packlist))))

(define (tmtex-preamble-build-sub l)
  (if (and (list? l) (nnull? l))
      (let ((x (car l)))
	(if (symbol? x) (tmtex-preamble-test-insert x))
	(if (and (list? x) (>= (length l) 2) (== (car x) '!begin))
	    (tmtex-preamble-test-insert (string->symbol (cadr x))))
	(if (and (in? x '(!sub !sup)) (texout-contains-table? (cadr l)))
	    (tmtex-preamble-test-insert 'tmscript))
	(if (match? x '(!begin "enumerate" (!option :1)))
	    (ahash-set! tmtex-preamble-uses "enumerate" #t))
	(for-each tmtex-preamble-build-sub (cdr l)))))

(define (tmtex-preamble-make-package-list l)
  (cond ((null? l) "")
        ((null? (cdr l)) (force-string (car l)))
        (else (string-append (force-string (car l)) ","
          (tmtex-preamble-make-package-list (cdr l))))))

(tm-define (tmtex-preamble-build text style lan init)
  (set! tmtex-preamble-uses (make-ahash-table))
  (set! tmtex-preamble-init "")
  (set! tmtex-preamble-result "")
  (tmtex-preamble-page-type init)
  (if (drd-ref tmtex-preamble-language-def% lan)
      (set! tmtex-preamble-result
	    (string-append (drd-ref tmtex-preamble-language-def% lan) "\n")))
  (tmtex-preamble-build-sub text)
  (set! tmtex-preamble-result
	(string-append tmtex-preamble-result (latex-macro-defs text)))
  (set! tmtex-preamble-result
	(string-append (latex-catcode-defs text) tmtex-preamble-result))
  (values
    (tmtex-preamble-make-package-list 
      (sort
	(map car (ahash-table->list tmtex-preamble-uses))
	(lambda (l r)
	  (let* ((tl (drd-ref latex-package-priority% l))
		 (tr (drd-ref latex-package-priority% r))
		 (vl (if tl tl 999999))
		 (vr (if tr tr 999999)))
		(< vl vr)))))
    tmtex-preamble-init
    tmtex-preamble-result))
