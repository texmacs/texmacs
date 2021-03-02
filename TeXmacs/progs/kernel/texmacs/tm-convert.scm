
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-convert.scm
;; DESCRIPTION : Declaration of data formats and converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-convert)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-modes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-format-todo '())

(define-public-macro (lazy-format module . ignored)
  (set! lazy-format-todo (cons module lazy-format-todo))
  `(delayed (:idle 2000) (import-from ,module)))

(define (lazy-format-force)
  (if (nnull? lazy-format-todo)
      (eval (cons 'import-from lazy-format-todo)))
  (set! lazy-format-todo '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding new converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define converter-forward (make-ahash-table))
(define converter-backward (make-ahash-table))
(define converter-function (make-ahash-table))
(define converter-options (make-ahash-table))
(define converter-option-for (make-ahash-table))
(define converter-distance (make-ahash-table))
(define converter-path (make-ahash-table))

(define (converter-set-penalty from to penalty)
  (if (not (ahash-ref converter-forward from))
      (ahash-set! converter-forward from (make-ahash-table)))
  (ahash-set! (ahash-ref converter-forward from) to penalty)
  (if (not (ahash-ref converter-backward to))
      (ahash-set! converter-backward to (make-ahash-table)))
  (ahash-set! (ahash-ref converter-backward to) from penalty))

(define (converter-remove from to)
  (converter-set-penalty from to #f)
  (ahash-remove! (ahash-ref converter-forward from) to)
  (ahash-remove! (ahash-ref converter-backward to) from))

(define (converter-change-option from to option val)
  (with key (list from to)
    (if (not (ahash-ref converter-options key))
	(ahash-set! converter-options key '()))
    (ahash-set! converter-options key
		(assoc-set! (ahash-ref converter-options key) option val))))

(define (converter-set-option option val)
  (with key (ahash-ref converter-option-for option)
    (if key (converter-change-option (car key) (cadr key) option val))))

(define (converter-define-option from to option val)
  (with key (list from to)
    (ahash-set! converter-option-for option key)
    (converter-change-option from to option key)
    (define-preferences
      (option val converter-set-option))))

(define-public (converter-cmd from to cmd)
  "Helper routine for converter macro"
  (cond ((func? cmd :penalty 1)
	 (converter-set-penalty from to (second cmd)))
;;        ((func? cmd :require 1) ;; already handled earlier now 
;;	 (if (not ((second cmd))) (converter-remove from to)))
        ((func? cmd :option 2)
	 (converter-define-option from to (second cmd) (third cmd)))
        ((func? cmd :function 1)
	 (ahash-set! converter-function (list from to)
		     (lambda (x opts) ((second cmd) x))))
        ((func? cmd :function-with-options 1)
	 (ahash-set! converter-function (list from to) (second cmd)))
        ((func? cmd :shell)
	 (if (not (url-exists-in-path? (second cmd)))
	     (converter-remove from to))
	 (ahash-set! converter-function (list from to)
		     (lambda (what opts)
		       (converter-shell (cdr cmd) what to opts))))))

(define-public (converter-sub cmd)
  "Helper routine for converter macro"
  (cond ((and (list? cmd) (= (length cmd) 2)
	      (in? (car cmd) '(:function :function-with-options)))
	 (list (car cmd) (list 'unquote (cadr cmd))))
	((and (list? cmd) (= (length cmd) 2)
	      (in? (car cmd) '(:require)))
	 (list (car cmd) (list 'unquote `(lambda () ,(cadr cmd)))))
	(else cmd)))

(define-public-macro (converter from* to* . options)
  "Declare a converter between @from@ and @to* according to @options"
  (let* ((from (if (string? from*) from* (symbol->string from*)))
	 (to (if (string? to*) to* (symbol->string to*))))
    (set! converter-distance (make-ahash-table))
    (set! converter-path (make-ahash-table))
;; NEW if (:required) clause present but not fulfilled do nothing
;; this enables to define several possible implementations of a given converter
;; not presuming on the availability of external tools : the last valid one is retained
;; (previously the last defined -even if unavailable- erased whatever was already defined)
    (cond ((and (in? (car (first options)) '(:penalty)) 
            (in? (car (second options)) '(:require)) 
            (not (eval (second (second options))))) (noop))
          ((and (in? (car (first options)) '(:require)) 
            (not (eval (second (first options))))) (noop))
          (else (converter-set-penalty from to 1.0) 
             `(for-each (lambda (x) (converter-cmd ,from ,to x))
	         ,(list 'quasiquote (map converter-sub options)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (converter-shell-cmd l from to)
  (with x (car l)
    (string-append (if (or (os-win32?) (os-mingw?)) 
                     (escape-shell (url-concretize (url-resolve-in-path x))) x) " "
      (converter-shell-cmd-args (cdr l) from to))))

(define (converter-shell-cmd-args l from to)
  (if (null? l) ""
      (with x (car l)
	(string-append (cond ((== x 'from) (escape-shell (url-concretize from)))
			     ((== x 'to) (escape-shell (url-concretize to)))
			     (else x))
		       (cond ((and (string? x) (string-ends? x "=")) "")
                             (else " "))
		       (converter-shell-cmd-args (cdr l) from to)))))

(define (converter-shell l from to-format opts)
  ;;(display* "converter-shell " l ", " from ", " to-format ", " opts "\n")
  (let* ((last? (assoc-ref opts 'last?))
	 (dest (assoc-ref opts 'dest))
	 (dsuf (format-default-suffix to-format))
	 (suf (if (and dsuf (!= dsuf "")) (string-append "." dsuf) ""))
	 (to (if (and last? dest) dest (url-glue (url-temp) suf)))
	 (cmd (if (or (os-win32?) (os-mingw?)) (escape-shell (converter-shell-cmd l from to))
             (converter-shell-cmd l from to))))
    ;;(display* "shell: " cmd "\n")
    (system cmd)
    (if (url-exists? to) to #f)))

(define-public (converter-save s opts)
  "Helper routine for define-format macro"
  (let* ((last? (assoc-ref opts 'last?))
	 (dest (assoc-ref opts 'dest))
	 (to (if (and last? dest) dest (url-temp))))
    (string-save s to)
    (if (url-exists? to) to #f)))

(define-public (converter-load u opts)
  "Helper routine for define-format macro"
  (string-load u))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding converters from and to a format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (converters-sub l h p)
  (cond ((null? l) (map car (ahash-table->list h)))
	((ahash-ref h (car l)) (converters-sub (cdr l) h p))
	(else (let* ((hn (ahash-ref p (car l)))
		     (next (if hn (map car (ahash-table->list hn)) '())))
		(ahash-set! h (car l) #t)
		(converters-sub (append next (cdr l)) h p)))))

(define-public (converters-from . from)
  (lazy-format-force)
  (converters-sub from (make-ahash-table) converter-forward))

(define-public (converters-to . to)
  (lazy-format-force)
  (converters-sub to (make-ahash-table) converter-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the shortest path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (converter-insert from to penalty path)
  (if (ahash-ref converter-distance (list from to)) #f
      (begin
	(ahash-set! converter-distance (list from to) penalty)
	(ahash-set! converter-path (list from to) path)
	#t)))

(define (converter-walk from l*)
  ;;(display* "convert-walk " from ", " l* "\n")
  (if (nnull? l*)
      (let* ((l (list-sort l* (lambda (x y) (< (cadr x) (cadr y)))))
	     (aux (caar l))
	     (d (cadar l))
	     (path (caddar l)))
	(if (converter-insert from aux d (reverse path))
	    (let* ((hn (ahash-ref converter-forward aux))
		   (next (if hn (ahash-table->list hn) '()))
		   (r (map (lambda (x) (list (car x)
					     (+ d (cdr x))
					     (cons (car x) path)))
			   next)))
	      (converter-walk from (append (cdr l) r)))
	    (converter-walk from (cdr l))))))

(define-public (converter-search from to)
  (lazy-format-force)
  (converter-walk from (list (list from 0.0 (list from))))
  (ahash-ref converter-path (list from to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (std-converter-options from to)
  (or (ahash-ref converter-options (list from to)) '()))

(define (convert-via what from path options)
  ;;(display* "convert-via " what ", " from ", " path ", " options "\n")
  (if (null? path)
      (with dest (assoc-ref options 'dest)
        (when (and dest (url? what) (url? dest) (!= dest what))
          (system-copy what dest))
        what)
      (with fun (ahash-ref converter-function (list from (car path)))
	(if fun
	    (let* ((last? (null? (cdr path)))
		   (opts1 (acons 'last? last? options))
		   (opts2 (std-converter-options from (car path)))
		   (what* (fun what (append opts1 opts2)))
		   (result (convert-via what* (car path) (cdr path) options)))
	      (if (and (not last?) (string-ends? (car path) "-file"))
		  (system-remove what*))
	      result)
	    #f))))

(define-public (convert what from to . options)
  ;;(display* "convert " what ", " from ", " to ", " options "\n")
  (lazy-format-force)
  (with path (converter-search from to)
    (if path
	(convert-via what from (cdr path) options)
	#f)))

(define-public (convert-to-file what from to dest . options)
  (apply convert (cons* what from to (acons 'dest dest options))))

(define-public (image->postscript name)
  (let* ((suffix (locase-all (url-suffix name)))
	 (fm (string-append (format-from-suffix suffix) "-file"))
	 (s (convert name fm "postscript-document")))
    (if (string? s) s "")))

(define-public (texmacs->generic doc fm)
  (with r (convert doc "texmacs-tree" fm)
    (if r r "Error: bad format or data")))

(define-public (generic->texmacs s fm)
  (with r (convert s fm "texmacs-tree")
    (if r r (stree->tree '(error "bad format or data")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up conversion menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format<=? fm1 fm2)
  (string<=? (ahash-ref format-name fm1) (ahash-ref format-name fm2)))

(define-public (converters-from-special fm suf tm?)
  (let* ((l1 (converters-from fm))
	 (l2 (list-filter l1 (lambda (s) (string-ends? s suf))))
	 (l3 (map (lambda (s) (string-drop-right s (string-length suf))) l2))
         (l3 (list-filter l3 (lambda (s) (not (ahash-ref format-hidden s)))))
         (l4 (if tm? l3 (list-filter l3 (lambda (s) (!= s "texmacs"))))))
    (list-sort l4 format<=?)))

(define-public (converters-to-special fm suf tm?)
  (let* ((l1 (converters-to fm))
	 (l2 (list-filter l1 (lambda (s) (string-ends? s suf))))
	 (l3 (map (lambda (s) (string-drop-right s (string-length suf))) l2))
         (l3 (list-filter l3 (lambda (s) (not (ahash-ref format-hidden s)))))
         (l4 (if tm? l3 (list-filter l3 (lambda (s) (!= s "texmacs"))))))
    (list-sort l4 format<=?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfile-pair? t)
  (and (tm-compound? t)
       (nnull? (tm-children t))))

(define-public (tmfile-get doc what)
  (and (tm-func? doc 'document)
       (list-and (map tmfile-pair? (tm-children doc)))
       (with val (assoc-ref (map tm->list (tm-children doc)) what)
         (if (pair? val) (set! val (car val)))
         val)))

(define-public (tmfile-set doc what val)
  (and (tm-func? doc 'document)
       (list-and (map tmfile-pair? (tm-children doc)))
       (with l (reverse (map tm->list (tm-children doc)))
         (set! l (assoc-set! l what (list val)))
         (cons 'document (reverse l)))))

(define-public (tmfile? doc)
  (and (tmfile-get doc 'TeXmacs) (tmfile-get doc 'body)))

(define-public (tmfile-extract doc what)
  ;; FIXME: use tmfile-get instead whenever possible
  (if (tree? doc) (set! doc (tree->stree doc)))
  (and (func? doc 'document)
       (list-and (map tmfile-pair? (tm-children doc)))
       (with val (assoc-ref (cdr doc) what)
         (if (pair? val) (set! val (car val)))
         (if (tree? val) (set! val (tree->stree val)))
         val)))

(define-public (tmfile-assign doc what val)
  ;; FIXME: use tmfile-set instead whenever possible
  (if (tree? doc) (set! doc (tree->stree doc)))
  (and (func? doc 'document)
       (list-and (map tmfile-pair? (tm-children doc)))
       (with l (reverse (cdr doc))
         (set! l (assoc-set! l what (list (tm->tree val))))
         (cons 'document (reverse l)))))

(define (default-init var)
  ;; FIXME: should use C++ code
  (cond ((== var "mode") "text")
	((== var "language") "english")
	(else "")))

(tm-define (tmfile-init doc var . explicit?)
  (with init (tmfile-extract doc 'initial)
    (if (not init)
        (and (null? explicit?)
             (default-init var))
	(with item (list-find (cdr init) (lambda (x) (== (cadr x) var)))
	  (if item (caddr item)
              (and (null? explicit?)
                   (default-init var)))))))

(tm-define (tmfile-style-list doc)
  (with style (tmfile-extract doc 'style)
    (cond ((tm-func? style 'tuple) (cdr style))
          ((string? style) (list style))
          (else (list)))))

(tm-define (tmfile-language doc)
  (let* ((style (tmfile-style-list doc))
         (lans (list-intersection style supported-languages))
	 (lan (tmfile-init doc "language" #t)))
    (cond (lan lan)
          ((nnull? lans) (car lans))
          (else "english"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding new formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define format-name (make-ahash-table))
(define format-suffixes (make-ahash-table))
(define format-hidden (make-ahash-table))
(define format-mime (make-ahash-table))
(define format-recognize (make-ahash-table))
(define format-must-recognize (make-ahash-table))

(define-public (format-cmd name cmd)
  "Helper routine for define-format"
  (cond ((func? cmd :name 1)
	 (ahash-set! format-name name (second cmd)))
	((func? cmd :suffix)
	 (ahash-set! format-suffixes name (cdr cmd))
	 (for-each (lambda (s) (ahash-set! format-mime s name)) (cdr cmd)))
	((func? cmd :hidden)
         (ahash-set! format-hidden name #t))
	((func? cmd :recognize 1)
	 (ahash-set! format-recognize name (second cmd)))
	((func? cmd :must-recognize 1)
	 (ahash-set! format-recognize name (second cmd))
	 (ahash-set! format-must-recognize name #t))))

(define-public (format-sub cmd)
  "Helper routine for define-format"
  (if (and (list? cmd) (= (length cmd) 2)
	   (in? (car cmd) '(:recognize :must-recognize)))
      (list (car cmd) (list 'unquote (cadr cmd)))
      cmd))

(define-public-macro (define-format name* . options)
  "Declare data format @name* according to @options"
  (let* ((name (if (string? name*) name* (symbol->string name*)))
	 (name-document (string-append name "-document"))
	 (name-file (string-append name "-file")))
    `(begin
       (converter ,name-document ,name-file
	 (:function-with-options converter-save))
       (converter ,name-file ,name-document
	 (:function-with-options converter-load))
       (for-each (lambda (x) (format-cmd ,name x))
		 ,(list 'quasiquote (map format-sub options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful routines for format recognition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (format-skip-spaces s pos)
  (cond ((>= pos (string-length s)) pos)
	((tm-char-whitespace? (string-ref s pos))
	 (format-skip-spaces s (+ pos 1)))
	(else pos)))

(define-public (format-skip-line s pos)
  (cond ((>= pos (string-length s)) pos)
	((in? (string-ref s pos) '(#\newline #\cr)) (+ pos 1))
	(else (format-skip-line s (+ pos 1)))))

(define-public (format-test? s pos what)
  (with end (+ pos (string-length what))
    (and (>= (string-length s) end)
	 (== (string-downcase (substring s pos end)) what))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting suffix information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-get-suffixes-sub fm)
  (with l (ahash-ref format-suffixes fm)
    (if l l '())))

(define (format-image-suffixes)
  (with l (converters-to-special "postscript-file" "-file" #f)
    (apply append (map format-get-suffixes-sub l))))

(define (format-get-suffixes fm)
  (cond ((and (== fm "image") (os-win32?))
         '("ps" "eps" "bmp" "gif" "ico" "tga" "pcx" "wbmp" "wmf" "jpg"
	   "jpeg" "png" "tif" "jbig" "ras" "pnm" "jp2" "jpc" "pgx"
           "cut" "iff" "lbm" "jng" "koa" "mng" "pbm" "pcd" "pcx"
           "pgm" "ppm" "psd" "tga" "tiff" "xbm" "xpm"))
        ((== fm "image") (format-image-suffixes))
        ((== fm "sound")
	 '("au" "cdr" "cvs" "dat" "gsm" "ogg" "snd" "voc" "wav"))
        ((== fm "animation") '("gif"))
        (else (format-get-suffixes-sub fm))))

(define-public (format-get-suffixes* fm)
  (lazy-format-force)
  (cons 'tuple (format-get-suffixes fm)))

(define-public (format-default-suffix fm)
  (lazy-format-force)
  (with l (ahash-ref format-suffixes fm)
    (cond ((== fm "image") "png")
	  ((or (not l) (null? l))
	   (if (string-ends? fm "-file")
	       (format-default-suffix (string-drop-right fm 5))
	       ""))
	  (else (car l)))))

(define-public (image-formats)
  (let* ((suffixes (format-image-suffixes))
         (formats (map format-from-suffix suffixes)))
    (sort (list-remove-duplicates formats) string<=?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic determination of the format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (format? fm)
  (lazy-format-force)
  (not (not (ahash-ref format-name fm))))

(define-public (format-get-name fm)
  (lazy-format-force)
  (ahash-ref format-name fm))

(define-public (format-recognizes? doc fm)
  (lazy-format-force)
  (with pred? (ahash-ref format-recognize fm)
    (and pred? (pred? doc))))

(define-public (format-from-suffix suffix)
  (lazy-format-force)
  (with fm (ahash-ref format-mime (locase-all suffix))
    (if fm fm "generic")))

(define-public (format-determine body suffix)
  (lazy-format-force)
  (with p (list-find (ahash-table->list format-recognize)
		     (lambda (p) ((cdr p) body)))
    (if p (car p)
	(with fm (ahash-ref format-mime (locase-all suffix))
	  (cond ((not fm) "verbatim")
		((ahash-ref format-must-recognize fm) "verbatim")
		(else fm))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for file conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (file-of-format? u fm)
  (in? (url-suffix u) (format-get-suffixes fm)))

(define-public (file-format u)
  (string-append (format-from-suffix (url-suffix u)) "-file"))

(define-public (file-converter-exists? what dest)
  (nnot (converter-search (file-format what) (file-format dest))))

(define-public (file-convert what dest . options)
  (let* ((from (file-format what))
         (to   (file-format dest)))
    (apply convert (cons* what from to (acons 'dest dest options)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define viewer-table (make-ahash-table))

;; (define-public (suffix->viewer suf)
;;   (ahash-ref viewer-table suf))

;; (define-public (save-viewers)
;;   "Save viewers from disk"
;;   (with u "$TEXMACS_HOME_PATH/system/viewers.scm"
;;     (save-object u (ahash-table->list viewer-table))))

;; (define (retrieve-viewers)
;;   "Retrieve viewers from disk"
;;   (with u "$TEXMACS_HOME_PATH/system/viewers.scm"
;;     (when (url-exists? u)
;;       (set! viewer-table (list->ahash-table (load-object u))))))

;; (retrieve-viewers)
