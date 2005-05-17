
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : xmltm-test.scm
;; DESCRIPTION : Test suite for XML import common tools.
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools xmltm-test)
  (:use (convert tools xmltm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Built-in parser

(define (regtest-htmltm-parse)
  (define (nl) (char->string #\newline))
  (define (highbit-chart)
    ((cut string-join <> (nl))
     (map
      string-concatenate      
      '(("      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F")
	("  Ax     ¡  ¢  £  ¤  ¥  ¦  §  ¨  ©  "
	 "ª  «  ¬  ­  ®  ¯")
	("  Bx  °  ±  ²  ³  ´  µ  ¶  ·  ¸  ¹  "
	 "º  »  ¼  ½  ¾  ¿")
	("  Cx  À  Á  Â  Ã  Ä  Å  Æ  Ç  È  É  "
	 "Ê  Ë  Ì  Í  Î  Ï")
	("  Dx  Ğ  Ñ  Ò  Ó  Ô  Õ  Ö  ×  Ø  Ù  "
	 "Ú  Û  Ü  İ  Ş  ß")
	("  Ex  à  á  â  ã  ä  å  æ  ç  è  é  "
	 "ê  ë  ì  í  î  ï")
	("  Fx  ğ  ñ  ò  ó  ô  õ  ö  ÷  ø  ù  "
	 "ú  û  ü  ı  ş  ÿ")))))
  (define (latin1-chart)
    ((cut string-join <> (nl))
     (map
      string-concatenate
     '(("      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F")
       ("  Ax  Â   Â¡  Â¢  Â£  Â¤  Â¥  "
	"Â¦  Â§  Â¨  Â©  Âª  Â«  "
	"Â¬  Â­  Â®  Â¯")
       ("  Bx  Â°  Â±  Â²  Â³  Â´  Âµ  "
	"Â¶  Â·  Â¸  Â¹  Âº  Â»  "
	"Â¼  Â½  Â¾  Â¿")
       ("  Cx  Ã€  Ã  Ã‚  Ãƒ  Ã„  Ã…  "
	"Ã†  Ã‡  Ãˆ  Ã‰  ÃŠ  Ã‹  "
	"ÃŒ  Ã  Ã  Ã")
       ("  Dx  Ã  Ã‘  Ã’  Ã“  Ã”  Ã•  "
	"Ã–  Ã—  Ã˜  Ã™  Ãš  Ã›  "
	"Ãœ  Ã  Ã  ÃŸ")
       ("  Ex  Ã   Ã¡  Ã¢  Ã£  Ã¤  Ã¥  "
	"Ã¦  Ã§  Ã¨  Ã©  Ãª  Ã«  "
	"Ã¬  Ã­  Ã®  Ã¯")
       ("  Fx  Ã°  Ã±  Ã²  Ã³  Ã´  Ãµ  "
	"Ã¶  Ã·  Ã¸  Ã¹  Ãº  Ã»  "
	"Ã¼  Ã½  Ã¾  Ã¿")))))

  (regression-test-group
   "internal html/xml parser" "parser"
   parse-html :none
   (test "null string" "" '(*TOP*))
   (test "text" "hello" '(*TOP* "hello"))
   (test "element" "<hello>" '(*TOP* (hello)))
   (test "attributes"
	 "<elem a=\"val\" b='vbl' c=cbl d>"
	 '(*TOP* (elem (@ (a "val") (b "vbl") (c "cbl") (d)))))
   (test "element, text" "<a>b</a>" '(*TOP* (a "b")))
   (test "element, padded text" "<a> b </a>" '(*TOP* (a " b ")))
   (test "element, element and text" "<a> b<br>d </a>"
	 '(*TOP* (a " b" (br) "d ")))
   (test "processing instruction"
	 "<?xml version='1.0'?>" '(*TOP* (*PI* xml "version='1.0'")))
   (test "empty PI" "<?empty?>" '(*TOP* (*PI* empty "")))
   (test "null PI" "<??>" `(*TOP* (*PI* ,(string->symbol "") "")))
   (test "doctype" "<!DOCTYPE some stuff>" '(*TOP* (*DOCTYPE* "some stuff")))
   (test "implicit /p" "<p>hello<p>b" '(*TOP* (p "hello") (p "b")))
   (test "implicit /li" "<ul><li>a<li>b</ul>" '(*TOP* (ul (li "a")  (li "b"))))
   (test "implici /dt /dd"
	 "<dl><dt>a<dt>b<dd>c<dd>d</dl>"
	 '(*TOP* (dl (dt "a") (dt "b") (dd "c") (dd "d"))))
   (test "implicit table tags"
	 "<table><thead><td>a<tbody><tr><th>b<td>c<tfoot><th>d<td>e</table>"
	 '(*TOP* (table (thead (td "a")) (tbody (tr (th "b") (td "c")))
			(tfoot (th "d") (td "e")))))
   (test "XML, UTF-8"
	 (string-append
	  "<?xml version='1.0' encoding='utf-8'?>" (nl) (latin1-chart))
	 `(*TOP* (*PI* xml "version='1.0' encoding='utf-8'")
		 ,(string-append (nl) (latin1-chart))))
   (test "XML, latin1"
	 (string-append
	  "<?xml version='1.0' encoding='iso-8859-1'?>" (nl) (highbit-chart))
	 `(*TOP* (*PI* xml "version='1.0' encoding='iso-8859-1'")
		 ,(string-append (nl) (latin1-chart))))
   (test "HTML, UTF-8" (latin1-chart) `(*TOP* ,(latin1-chart)))
   (test "HTML, latin1" (highbit-chart) `(*TOP* ,(latin1-chart)))))

;; Namespace-aware parser wrapper

(define (regtest-parse-xmlns)
  (define xhtml "'http://www.w3.org/1999/xhtml'")
  (define mathml "'http://www.w3.org/1998/Math/MathML'")
  (define concat string-append)
  (regression-test-group
   "namespace-aware parser wrapper" "xmlns"
   (cut xmltm-parse "" <>) :none
   (test "null string" "" '(*TOP*))
   (test "text" "hello" '(*TOP* "hello"))
   (test "element" "<hello>" '(*TOP* (hello)))
   (test "nested element" "<a><b/></a>" '(*TOP* (a (b))))
   (test "xmlns:foo" (concat "<foo:a xmlns:foo=" xhtml ">") '(*TOP* (h:a)))
   (test "xmlns 2"
	 (concat "<a xmlns=" mathml "><b xmlns=" xhtml "/></a>")
	 '(*TOP* (m:a (h:b))))
   (test "xmlns=''"
	 (concat "<a xmlns=" xhtml "><b xmlns=''/></a>")
	 '(*TOP* (h:a (b))))
   (test "xmlns:foo, xmlns:bar"
	 (concat "<foo:a xmlns:foo=" xhtml " xmlns:bar=" mathml "><bar:b>"
		 "</foo:a>")
	 '(*TOP* (h:a (m:b))))
   (test "attr, ns-less"
	 "<a x='hello'><b y='world'/></a>"
	 '(*TOP* (a (@ (x "hello")) (b (@ (y "world"))))))
   (test "attr, ns, prefixless"
	 (concat "<a xmlns=" xhtml " x='hello'><b y='world'/></a>")
	 '(*TOP* (h:a (@ (x "hello")) (h:b (@ (y "world"))))))
   (test "attr, ns, prefixed"
	 (concat "<a xmlns:xh=" xhtml " xh:x='hello'><b xh:y='world'/></a>")
	 '(*TOP* (a (@ (h:x "hello")) (b (@ (h:y "world"))))))))

;; Integration of the namespace layer with the HTML parser

;; NOTE: not sure this is really good design to mix support for HTML4 syntax
;; and XML namespaces. However, this cannot be changed unless HTML import is
;; separated from XML import. And it does not seem dangerous.

(define (regtest-parse-htmlns)
  (regression-test-group
   "integration of namespace layer with html parser" "htmlns"
   htmltm-parse :none
   (test "enum attribute"
	 "<frame noresize>" '(*TOP* (h:frame (@ (noresize)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fast serial constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-htmltm-serial)
  (define (serial l) (htmltm-serial #f l))
  (regression-test-group
   "htmltm, fast serial constructor" "serial-fast"
   serial :none
   (test "empty" '() "")
   (test "zero-concat" '((concat)) "")
   (test "string" '("hello") "hello")
   (test "two strings" '("aa" "bb") "aabb")
   (test "two concat strings" '((concat "a" "b") (concat "c" "d")) "abcd")
   (test "label" '((label "aa")) '(label "aa"))
   (test "null-string label null-string" '("" (label "aa") "") '(label "aa"))
   (test "string label string"
	 '("aa" (label "bb") "cc") '(concat "aa" (label "bb") "cc"))
   (test "label string label"
	 '((label "aa") "bb" (label "cc"))
	 '(concat (label "aa") "bb" (label "cc")))
   (test "zero-document" '((document)) '(document))
   (test "two documents" '((document "a") (document "b")) '(document "a" "b"))
   (test "text, doc" '("a" (document "b")) '(document "a" "b"))
   (test "doc, text" '((document "a") "b") '(document "a" "b"))
   (test "doc, zero-concat, doc"
	 '((document "a") (concat) (document "b"))
	 '(document "a" "b"))
   (test "text, zero-document, text"
	 '("a" (document) "b")
	 '(document "a" "b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-xmltm)
  (let ((n (+ (regtest-htmltm-parse)
	      (regtest-parse-xmlns)
	      (regtest-parse-htmlns)
	      (regtest-htmltm-serial))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of xmltm: ok\n")))
