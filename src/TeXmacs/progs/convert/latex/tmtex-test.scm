
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-test.scm
;; DESCRIPTION : Test suite for tmtex.scm
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-test)
  (:use (convert latex tmtex))
  (:export regtest-tmtex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmtex-table)
  (regtest-table-library)
  ;; basic shorthands for latex table output
  (define (!row l) `(!row ,@l))
  (define (!table ll) `(!table ,@(map !row ll)))
  (define (tabular c ll) `((!begin "tabular" ,c) ,(!table ll)))
  (define (!row-hline l) `((!row ,@l) (hline)))
  (define (!table-hline ll) `(!table (hline) ,@(append-map !row-hline ll)))
  (define (tabular-hline c ll) `((!begin "tabular" ,c) ,(!table-hline ll)))
  ;; more shorthands
  (define (simple-table) (table '(("a"))))
  (define (simple-tformat) (tformat '() '(("a"))))
  (define (expect-simple c) (tabular c '(("a"))))
  (define (expect-hline c) (tabular-hline c '(("a"))))
  (tmtex-initialize)
  (regression-test-group
   "tmtex, tables" "table"
   tmtex :none
   (test "naked table" (simple-table) (expect-simple "l"))
   (test "naked tformat" (simple-tformat) (expect-simple "l"))
   (test "simple tabular" `(tabular ,(simple-tformat)) (expect-simple "l"))
   (test "simple tabular*" `(tabular* ,(simple-tformat)) (expect-simple "c"))
   (test "simple block" `(block ,(simple-tformat)) (expect-hline "|l|"))
   (test "simple block*" `(block* ,(simple-tformat)) (expect-hline "|c|"))
   ;; These conversions are only meaningful in math mode!
   ;; (test "simple matrix" `(matrix ,(simple-tformat))
   ;;	 `(!concat (#{left\(}#) ,(expect-simple "c") (#{right\)}#)))
   ;; (test "simple det" `(det ,(simple-tformat))
   ;;	 `(!concat (left|) ,(expect-simple "c")  (right|)))
   ;; (test "simple choice" `(choice ,(simple-tformat))
   ;;	 `(!concat (left\{) ,(expect-simple "c") (right.)))
   (test "tabular*, two cells" `(tabular* ,(tformat '() '(("a" "b"))))
	 (tabular "cc" '(("a" "b"))))
   (test "tabular*, four cells"
	 `(tabular* ,(tformat '() '(("a" "b") ("c" "d"))))
	 (tabular "cc" '(("a" "b") ("c" "d"))))
   (test "tabular*, first col aligned right"
	 `(tabular* ,(tformat (list (colwith "1" "cell halign" "r"))
			      '(("a" "b") ("c" "d"))))
	 (tabular "rc" '(("a" "b") ("c" "d"))))
   (test "tabular*, whole table aligned right"
	 `(tabular* ,(tformat (list (allwith "cell halign" "r"))
			      '(("a" "b") ("c" "d"))))
	 (tabular "rr" '(("a" "b") ("c" "d"))))
   (test "tabular*, one row border"
	 `(tabular* ,(tformat (list (rowwith "1" "cell bborder" "1ln"))
			      '(("a" "b") ("c" "d"))))
	 `((!begin "tabular" "cc") (!table ,@(!row-hline '("a" "b"))
					   ,(!row '("c" "d")))))
   (test "tabular*, one col border"
	 `(tabular* ,(tformat (list (colwith "1" "cell bborder" "1ln"))
			      '(("a" "b") ("c" "d"))))
	 (tabular "cc" '(("a" "b") ("c" "d"))))))

(define (regtest-tmtex)
  (let ((n (+ (regtest-tmtex-table))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tmtex: ok\n")))
