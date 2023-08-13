
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : doxygen.scm
;; DESCRIPTION : 'doxygen' support for C++
;; COPYRIGHT   : (C) 2007--2012  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc doxygen)
  (:use (ice-9 rdelim)))

(display "TeXmacs] Loading doxygen C++ support\n")

;; Global variables

; Directory containing the current tag file
(define current-dir "")

(define current-source-file "")
(define current-namespace "")

; This table associate an url to a complete C++ expression
(define tag->url (make-ahash-table))

; This table stores a list of valid C++ expression for a key word.
(define key->tag (make-ahash-table))

; Time stamps are stored for tag files in order to
; refresh them when needed.
(define file->stamp (make-ahash-table))

; Web tag files are only loaded once
(define web->loaded? (make-ahash-table))

;; Parsing

(define (tagged? x l)
  ; #t iff l is a tag of type x, i.e. (x blabla)
  (and (list? l) (nnull? l) (eq? (car l) x)))

(define (parse-attributes x)
  (let ((attrs '()) ; assoc set of the attibutes to be returned
	(l (cdr x)))
    (while (nnull? l)
	   (let ((x (car l)))
	     (if (and (list? x) (= (length x) 2))
		 (set! attrs
		       (assoc-set! attrs (car x)  (cadr x)))))
	   (set! l (cdr l)))
    attrs))

(define (html->ascii s)
  ; Do a couple of string substitutions
  (let* ((x (string-replace s "&amp;" "&"))
	 (x (string-replace x "&lt;"  "<"))
	 (x (string-replace x "&gt;"  ">")))
    x))

(define (relative-path s)
  ; If s is a path to a header file blabla/include/mypackage...
  ; then return mypackage...
  (let ((i (string-search-forwards "include/" 0 s)))
    (if (>= i 0)
	(substring s (+ i 8))
	s)))

(define (cpp_basename s)
  ; If s is class name like blabla::name
  ; then return name
  (let ((i (string-contains? s "::"))
	(j (string-contains? s "<"))
	(k (string-contains? s "(")))
    (cond
      ((integer? i) (cpp_basename (substring s (+ i 2))))
      ((integer? j) (cpp_basename (substring s 0 j)))
      ((integer? k) (cpp_basename (substring s 0 k)))
      (else s))))

(define (parse-member argl)
  (let ((kind "")
	(type "")
	(name "")
	(arglist "")
	(anchorfile "")
	(anchor "")
	(l (cdr argl))
	(attrs '()))
    (while (nnull? l)
      (let ((x (car l)))
	(cond
	  ((string? x)) ; these are only spaces
	  ((tagged? '@ x)
	   (set! attrs (parse-attributes x))
	   (set! kind (assoc-ref attrs 'kind)))
	  ((tagged? 'type x)
	   (if (nnull? (cdr x))
	       (set! type (html->ascii (second x)))))
	  ((tagged? 'name x) (set! name (second x)))
	  ((tagged? 'arglist x)
	   (if (nnull? (cdr x))
	       (set! arglist (html->ascii (second x)))))
	  ((tagged? 'anchorfile x)
	   (set! anchorfile (second x)))
	  ((tagged? 'anchor x)
	   (set! anchor (second x)))
	  (else
	    (display "TeXmacs] doxygen warning, unknown tag type within a 'member:\n")
	    (display x)
	    (newline))))
      (set! l (cdr l)))
    (let ((u (string-append current-dir "/" anchorfile "#" anchor)))
      (cond
	((== kind "define")
	 (let ((tag (string-append current-source-file ":" name arglist))) 
	   (ahash-set! tag->url tag u)
	   (ahash-set! key->tag name
		       (cons tag (ahash-ref* key->tag name '())))))
	((== kind "function")
	 (let ((tag (string-append type " " current-namespace name arglist)))
	   (ahash-set! tag->url tag u)
	   (ahash-set! key->tag (cpp_basename name)
		       (cons tag (ahash-ref* key->tag name '())))))
	((or (== kind "variable")
	     (== kind "typedef")
	     (== kind "enumeration")
	     (== kind "enumvalue"))
	 (let ((tag (string-append current-namespace name)))
	   (ahash-set! tag->url tag u)
	   (ahash-set! key->tag (cpp_basename name)
		       (cons tag (ahash-ref* key->tag name '())))))
	((== kind "friend"))
	(else
	  (display "TeXmacs] doxygen warning, unknown kind of 'member:\n")
	  (display kind)
	  (newline))))))

(define (parse-compound_struct x)
  (let ((name "")
	(path "")
	(filename "")
	(templargs "")
	(templarg "")
	(previous-namespace current-namespace)
	(l (cdr x)))
    (while (nnull? l)
      (let ((x (car l)))
	(cond
	  ((string? x)) ; these are only spaces
	  ((tagged? '@ x))
	  ((tagged? 'path x))
	  ((tagged? 'base x))
	  ((tagged? 'class x))
	  ((tagged? 'struct x))
	  ((tagged? 'name x)
	   (set! name (html->ascii (second x)))
	   (set! current-namespace (string-append
				    current-namespace
				    name "::")))
	  ((tagged? 'filename x)
	   (set! filename (second x)))
	  ((and (tagged? 'templarg x) (= (length x) 2))
	   (set! templarg (html->ascii (second x)))
	   (if (string-null? templargs)
	       (set! templargs templarg)
	       (set! templargs (string-append templargs ", " templarg))))
	  ((tagged? 'templarg x))
	  ((tagged? 'member x) (parse-member x))
	  (else
	    (display "TeXmacs] doxygen warning, unknown tag type within a 'struct 'compound:\n")
	    (display x)
	    (newline))))
      (set! l (cdr l)))
    (if (not (string-null? templargs))
	(set! templargs (string-append "< " templargs " >")))
    (let ((tag (string-append previous-namespace name)))
      (ahash-set! tag->url tag (string-append current-dir "/" filename))
      (ahash-set! key->tag (cpp_basename name)
		  (cons tag (ahash-ref* key->tag (cpp_basename name) '()))))
    (set! current-namespace previous-namespace)))

(define (parse-compound_file x)
  (let ((name "")
	(path "")
	(filename "")
	(previous-namespace current-namespace)
	(l (cdr x)))
    (while (nnull? l)
      (let ((x (car l)))
	(cond
	  ((string? x)) ; these are only spaces
	  ((tagged? '@ x))
	  ((tagged? 'path x) (set! path (relative-path (second x))))
	  ((tagged? 'name x)
	   (set! name (second x))
	   (set! current-source-file name))
	  ((tagged? 'filename x)
	   (set! filename (string-append (second x) ".html")))
	  ((tagged? 'namespace x)
	   (set! current-namespace
		 (string-append previous-namespace (second x) "::")))
	  ((tagged? 'member x) (parse-member x))
	  ((tagged? 'class x))
	  ((tagged? 'docanchor x))
	  ((tagged? 'includes x))
	  (else
	    (display "TeXmacs] doxygen warning, unknown tag type within a 'file 'compound:\n")
	    (display x)
	    (newline))))
      (set! l (cdr l)))
    (let ((tag (string-append path name)))
      (ahash-set! tag->url tag (string-append current-dir "/" filename))
      (ahash-set! key->tag name (cons tag (ahash-ref* key->tag name '()))))
    (set! current-namespace previous-namespace)))

(define (parse-compound x)
  (let ((kind "")
	(l (cdr x))
	(attrs '()))
    (while (and (nnull? l) (== kind ""))
      (let ((x (car l)))
	(if (tagged? '@ x)
	    (begin
	      (set! attrs (parse-attributes x))
	      (set! kind (assoc-ref attrs 'kind))))
	(set! l (cdr l))))
    (cond
      ((== kind "file") (parse-compound_file x))
      ((== kind "namespace"))
      ((or (== kind "class")
	   (== kind "struct"))
       (parse-compound_struct x))
      (else
	(display "TeXmacs] doxygen warning, unknown kind of 'compound:\n")
	(display kind)
	(newline)))))

(define (parse-tagfile x)
  (let ((l (cdr x)))
    (while (nnull? l)
      (let ((x (car l)))
	(cond
	  ((string? x)) ; these are only spaces
	  ((tagged? 'compound x) (parse-compound x))
	  (else
	    (display "TeXmacs] doxygen warning, unknown tag within a 'tagfile:")
	    (display x)
	    (newline))))
      (set! l (cdr l)))))

(define (parse-main s)
  ;; s is a string that stores the content of a tag file
  (let ((l (parse-xml s)))
    (while (nnull? l)
      (let ((x (car l)))
	(cond
	  ((string? x)) ; these are essentially spaces
	  ((equal? x '*TOP*))
	  ((tagged? '*PI* x))
	  ((tagged? 'tagfile x) (parse-tagfile x))
	  (else
	    (display "TeXmacs] doxygen warning, unknown tag:")
	    (display x)
	    (newline))))
      (set! l (cdr l)))))

(define (load-tag-file relative_filename)
  (let ((filename (url->system
                   (url-append
                    (url-head (current-buffer)) relative_filename))))
    (if (url-test? filename "r")
	(let ((nst (url-last-modified filename))
	      (ost (ahash-ref* file->stamp filename '())))
	  (if (not (equal? nst ost))
	      (let* ((p (open-input-file filename))
		     (s (read-delimited "" p)))
		(close-input-port p)
		(ahash-set! file->stamp filename nst)
		(set! current-dir (url->system (url-head relative_filename)))
		(parse-main s))))
	(texmacs-error "Doxygen: file not found" filename))))

(define (load-tag-web relative_filename)
  (let* ((filename (url-append (url-head (current-buffer)) relative_filename))
	 (loaded? (ahash-ref* web->loaded? filename #f)))
    (if (not loaded?)
	(if (url-exists? filename)
	    (let ((s (string-load filename)))
	      (set! current-dir (url->system (url-head relative_filename)))
	      (parse-main s)
	      (ahash-set! web->loaded? filename #t))
	    (texmacs-error "Doxygen: file not found" filename)))))

;; TeXmacs exports

(tm-define (doxygen-load l)
  (:secure #t)
  (:synopsis "Load Doxygen tag file")
  (if (url-rooted-web? (current-buffer))
      (load-tag-web  (tree-as-string l))
      (load-tag-file (tree-as-string l)))
  "")

(tm-define (doxygen-ref x)
  (:secure #t)
  (:synopsis "Get the url for the given Doxygen tag")
  (let* ((xs (tree-as-string x))
	 (y (ahash-ref* tag->url xs "")))
    (if (not (string-null? y)) y
	(let ((proposal (ahash-ref* key->tag xs '())))
	  (cond
	    ((null? proposal)
	     (texmacs-error "doxygen"
			    (string-append "Invalid Doxygen tag: " xs)))
	    ((= 1 (length proposal))
	     (tree-set! x (first proposal))
	     (ahash-ref tag->url (first proposal)))
	    (else
	      (user-ask (cons* "Pick up a correct tag:" "Question" proposal)
		(lambda (y)
		  (tree-set! x y)
		  (ahash-ref tag->url y)))))))))
