
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-plugins.scm
;; DESCRIPTION : Configuration of plugins
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-plugins)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-modes))
  (:export
    plugin-old-data-table plugin-data-table
    connection-defined? connection-info connection-get-handlers
    plugin-configure-cmds plugin-configure-sub ; for plugin-configure macro
    plugin-configure plugin-initialize
    ;; lazy exports from other modules
    plugin-supports-math-input-ref plugin-math-input
    plugin-supports-completions?
    plugin-supports-input-done?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy exports from other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (texmacs plugin plugin-convert) plugin-supports-math-input-ref)
(lazy-define (texmacs plugin plugin-convert) plugin-math-input)
(lazy-define (texmacs plugin plugin-cmd) plugin-serializer-set!)
(lazy-define (texmacs plugin plugin-cmd) plugin-commander-set!)
(lazy-define (texmacs plugin plugin-cmd) plugin-supports-completions?)
(lazy-define (texmacs plugin plugin-cmd) plugin-supports-completions-set!)
(lazy-define (texmacs plugin plugin-cmd) plugin-supports-input-done?)
(lazy-define (texmacs plugin plugin-cmd) plugin-supports-input-done-set!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection types for plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-defined (make-ahash-table))
(define connection-default (make-ahash-table))
(define connection-variant (make-ahash-table))
(define connection-varlist (make-ahash-table))
(define connection-handler (make-ahash-table))

(define (connection-setup name val . opt)
  (ahash-set! connection-defined name #t)
  (if (null? opt)
      (ahash-set! connection-default name val)
      (with l (ahash-ref connection-varlist name)
	(if (not l) (set! l '("default")))
	(ahash-set! connection-variant (list name (car opt)) val)
	(ahash-set! connection-varlist name (rcons l (car opt))))))

(define (connection-defined? name)
  (ahash-ref connection-defined name))

(define (connection-info name session)
  (with pos (string-index session #\:)
    (if pos (connection-info name (substring session 0 pos))
	(with val (ahash-ref connection-variant (list name session))
	  (if val val (ahash-ref connection-default name))))))

(define (connection-insert-handler name channel routine)
  (if (not (ahash-ref connection-handler name))
      (ahash-set! connection-handler name '()))
  (ahash-set! connection-handler name
	      (cons (list 'tuple channel routine)
		    (ahash-ref connection-handler name))))

(define (connection-get-handlers name)
  (with r (ahash-ref connection-handler name)
    (if r (cons 'tuple r) '(tuple))))

(define (connection-menu-promise name menu-name)
  (define (menu-item variant)
    (list variant (lambda () (make-session name variant))))
  (menu-dynamic
    ,(with l (ahash-ref connection-varlist name)
       (if (not l)
	   (list menu-name (lambda () (make-session name "default")))
	   `(-> ,menu-name ,@(map menu-item l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-old-data-table (make-ahash-table))
(define plugin-data-table (make-ahash-table))

(define (plugin-configure-cmd name cmd)
  (cond ((or (func? cmd :require 1) (func? cmd :version 1))
	 (ahash-set! plugin-data-table name ((second cmd))))
        ((func? cmd :setup 1)
	 (if (not (== (ahash-ref plugin-data-table name)
		      (ahash-ref plugin-old-data-table name)))
	     ((second cmd))))
        ((func? cmd :initialize 1)
	 ((second cmd)))
	((func? cmd :launch 1)
	 (connection-setup name `(tuple "pipe" ,(second cmd))))
	((func? cmd :launch 2)
	 (connection-setup name `(tuple "pipe" ,(third cmd)) (cadr cmd)))
	((func? cmd :socket 2)
	 (connection-setup name `(tuple "socket" ,(second cmd) ,(third cmd))))
	((func? cmd :socket 3)
	 (connection-setup
	  name `(tuple "socket" ,(third cmd) ,(fourth cmd)) (cadr cmd)))
	((func? cmd :link 3)
	 (connection-setup
	  name `(tuple "dynlink" ,(second cmd) ,(third cmd) ,(fourth cmd))))
	((func? cmd :link 4)
	 (connection-setup
	  name `(tuple "dynlink" ,(third cmd) ,(fourth cmd) ,(fifth cmd))
	  (cadr cmd)))
	((func? cmd :handler 2)
	 (connection-insert-handler
	  name (second cmd) (symbol->string (third cmd))))
	((func? cmd :session 1)
	 (menu-extend session-menu
	   (promise (connection-menu-promise name (second cmd)))))
	((func? cmd :filter-in 1)
	 (noop))
	((func? cmd :serializer 1)
	 (plugin-serializer-set! name (second cmd)))
	((func? cmd :commander 1)
	 (plugin-commander-set! name (second cmd)))
	((func? cmd :tab-completion 1)
	 (if (second cmd) (plugin-supports-completions-set! name)))
	((func? cmd :test-input-done 1)
	 (if (second cmd) (plugin-supports-input-done-set! name)))))

(define (plugin-configure-cmds name cmds)
  (if (and (not (null? cmds)) (ahash-ref plugin-data-table name))
      (begin
        (plugin-configure-cmd name (car cmds))
	(plugin-configure-cmds name (cdr cmds)))))

(define (plugin-configure-sub cmd)
  (if (and (list? cmd) (= (length cmd) 2)
	   (in? (car cmd) '(:require :version :setup :initialize)))
      (list (car cmd) (list 'unquote `(lambda () ,(cadr cmd))))
      cmd))

(define-macro (plugin-configure name2 . options)
  (let* ((name (if (string? name2) name2 (symbol->string name2)))
	 (in-name (string->symbol (string-append "in-" name "?"))))
    `(begin
       (texmacs-modes (,in-name (== (get-env "prog-language") ,name)))
       (ahash-set! plugin-data-table ,name #t)
       (plugin-configure-cmds ,name
	 ,(list 'quasiquote (map plugin-configure-sub options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plugin-initialize name*)
  ;(display* "loading plugin " name* "\n")
  (let* ((name (symbol->string name*))
	 (file (string-append "plugins/" name "/progs/init-" name ".scm")))
    (with u (url "$TEXMACS_HOME_PATH:$TEXMACS_PATH" file)
      (if (url-exists? u)
	  (with fname (url-materialize u "r")
	    (load fname))))))
