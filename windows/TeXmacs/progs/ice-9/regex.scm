;;;; 	Copyright (C) 1997, 1999 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

;;;; POSIX regex support functions.

(define-module (ice-9 regex))

;;; FIXME:
;;;   It is not clear what should happen if a `match' function
;;;   is passed a `match number' which is out of bounds for the
;;;   regexp match: return #f, or throw an error?  These routines
;;;   throw an out-of-range error.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; These procedures are not defined in SCSH, but I found them useful.

(define-public (match:count match)
  (- (vector-length match) 1))

(define-public (match:string match)
  (vector-ref match 0))

(define-public (match:prefix match)
  (make-shared-substring (match:string match)
			 0
			 (match:start match 0)))

(define-public (match:suffix match)
  (make-shared-substring (match:string match)
			 (match:end match 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SCSH compatibility routines.

(define-public (regexp-match? match)
  (and (vector? match)
       (string? (vector-ref match 0))
       (let loop ((i 1))
	 (cond ((>= i (vector-length match)) #t)
	       ((and (pair? (vector-ref match i))
		     (integer? (car (vector-ref match i)))
		     (integer? (cdr (vector-ref match i))))
		(loop (+ 1 i)))
	       (else #f)))))

(define-public (regexp-quote regexp)
  (call-with-output-string
   (lambda (p)
     (let loop ((i 0))
       (and (< i (string-length regexp))
	    (begin
	      (case (string-ref regexp i)
		((#\* #\. #\( #\) #\+ #\? #\\ #\^ #\$ #\{ #\})
		 (write-char #\\ p)))
	      (write-char (string-ref regexp i) p)
	      (loop (1+ i))))))))

(define-public (match:start match . args)
  (let* ((matchnum (if (pair? args)
		       (+ 1 (car args))
		       1))
	 (start (car (vector-ref match matchnum))))
    (if (= start -1) #f start)))

(define-public (match:end match . args)
  (let* ((matchnum (if (pair? args)
		       (+ 1 (car args))
		       1))
	 (end (cdr (vector-ref match matchnum))))
    (if (= end -1) #f end)))

(define-public (match:substring match . args)
  (let* ((matchnum (if (pair? args)
		       (car args)
		       0))
	 (start (match:start match matchnum))
	 (end   (match:end match matchnum)))
    (and start end (make-shared-substring (match:string match)
					  start
					  end))))

(define-public (string-match pattern str . args)
  (let ((rx (make-regexp pattern))
	(start (if (pair? args) (car args) 0)))
    (regexp-exec rx str start)))

(define-public (regexp-substitute port match . items)
  ;; If `port' is #f, send output to a string.
  (if (not port)
      (call-with-output-string
       (lambda (p)
	 (apply regexp-substitute p match items)))

      ;; Otherwise, process each substitution argument in `items'.
      (for-each (lambda (obj)
		  (cond ((string? obj)   (display obj port))
			((integer? obj)  (display (match:substring match obj) port))
			((eq? 'pre obj)  (display (match:prefix match) port))
			((eq? 'post obj) (display (match:suffix match) port))
			(else (error 'wrong-type-arg obj))))
		items)))

;;; If we call fold-matches, below, with a regexp that can match the
;;; empty string, it's not obvious what "all the matches" means.  How
;;; many empty strings are there in the string "a"?  Our answer:
;;;
;;; 	This function applies PROC to every non-overlapping, maximal
;;;     match of REGEXP in STRING.
;;;
;;; "non-overlapping": There are two non-overlapping matches of "" in
;;; "a" --- one before the `a', and one after.  There are three
;;; non-overlapping matches of "q|x*" in "aqb": the empty strings
;;; before `a' and after `b', and `q'.  The two empty strings before
;;; and after `q' don't count, because they overlap with the match of
;;; "q".
;;;
;;; "maximal": There are three distinct maximal matches of "x*" in
;;; "axxxb": one before the `a', one covering `xxx', and one after the
;;; `b'.  Around or within `xxx', only the match covering all three
;;; x's counts, because the rest are not maximal.

(define-public (fold-matches regexp string init proc . flags)
  (let ((regexp (if (regexp? regexp) regexp (make-regexp regexp)))
	(flags (if (null? flags) 0 flags)))
    (let loop ((start 0)
	       (value init)
	       (abuts #f))		; True if start abuts a previous match.
      (let ((m (if (> start (string-length string)) #f
		   (regexp-exec regexp string start flags))))
	(cond
	 ((not m) value)
	 ((and (= (match:start m) (match:end m)) abuts)
	  ;; We matched an empty string, but that would overlap the
	  ;; match immediately before.  Try again at a position
	  ;; further to the right.
	  (loop (+ start 1) value #f))
	 (else
	  (loop (match:end m) (proc m value) #t)))))))

(define-public (list-matches regexp string . flags)
  (reverse! (apply fold-matches regexp string '() cons flags)))

(define-public (regexp-substitute/global port regexp string . items)

  ;; If `port' is #f, send output to a string.
  (if (not port)
      (call-with-output-string
       (lambda (p)
	 (apply regexp-substitute/global p regexp string items)))

      ;; Walk the set of non-overlapping, maximal matches.
      (let next-match ((matches (list-matches regexp string))
		       (start 0))
	(if (null? matches)
	    (display (make-shared-substring string start) port)
	    (let ((m (car matches)))

	      ;; Process all of the items for this match.  Don't use
	      ;; for-each, because we need to make sure 'post at the
	      ;; end of the item list is a tail call.
	      (let next-item ((items items))
	  
		(define (do-item item)
		  (cond
		   ((string? item)    (display item port))
		   ((integer? item)   (display (match:substring m item) port))
		   ((procedure? item) (display (item m) port))
		   ((eq? item 'pre)  
		    (display
		     (make-shared-substring string start (match:start m))
		     port))
		   ((eq? item 'post)
		    (next-match (cdr matches) (match:end m)))
		   (else (error 'wrong-type-arg item))))

		(if (pair? items)
		    (if (null? (cdr items))
			(do-item (car items)) ; This is a tail call.
			(begin
			  (do-item (car items)) ; This is not.
			  (next-item (cdr items)))))))))))
