
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-plugins.scm
;; DESCRIPTION : Configuration of plugins
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-plugins)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-modes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy exports from other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (utils plugins plugin-convert) plugin-supports-math-input-ref)
(lazy-define (utils plugins plugin-convert) plugin-math-input)
(lazy-define (utils plugins plugin-cmd) plugin-serializer-set!)
(lazy-define (utils plugins plugin-cmd) plugin-commander-set!)
(lazy-define (utils plugins plugin-cmd) plugin-supports-completions?)
(lazy-define (utils plugins plugin-cmd) plugin-supports-completions-set!)
(lazy-define (utils plugins plugin-cmd) plugin-supports-input-done?)
(lazy-define (utils plugins plugin-cmd) plugin-supports-input-done-set!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection types for plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-defined (make-ahash-table))
(define connection-default (make-ahash-table))
(define connection-variant (make-ahash-table))
(define-public connection-varlist (make-ahash-table))
(define connection-handler (make-ahash-table))

(define (connection-setup name val . opt)
  (ahash-set! connection-defined name #t)
  (if (null? opt) (set! opt (list "default")))
  (with l (or (ahash-ref connection-varlist name) (list))
    (ahash-set! connection-variant (list name (car opt)) val)
    (ahash-set! connection-varlist name (rcons l (car opt)))))

(define-public (connection-defined? name)
  (lazy-plugin-force)
  (ahash-ref connection-defined name))

(define-public (connection-info name session)
  (lazy-plugin-force)
  (with pos (string-index session #\:)
    (if pos (connection-info name (substring session 0 pos))
	(or (remote-connection-info name session)
            (ahash-ref connection-variant (list name session))
            (ahash-ref connection-variant (list name "default"))))))

(define (connection-insert-handler name channel routine)
  (if (not (ahash-ref connection-handler name))
      (ahash-set! connection-handler name '()))
  (ahash-set! connection-handler name
	      (cons (list 'tuple channel routine)
		    (ahash-ref connection-handler name))))

(define-public (connection-get-handlers name)
  (lazy-plugin-force)
  (with r (ahash-ref connection-handler name)
    (if r (cons 'tuple r) '(tuple))))

(define-public (sorted-supported-plugins)
  (lazy-plugin-force)
  (list-sort (map car (ahash-table->list connection-defined)) string<=?))

(define (launcher? x)
  (and (func? (cdr x) 'tuple 2)
       (== (caddr x) "pipe")))

(define (launcher-entry x)
  (rcons (car x) (cadddr x)))

(define (launcher<=? l1 l2)
  (or (string<=? (car l1) (car l2))
      (and (== (car l1) (car l2))
           (string<=? (cadr l1) (cadr l2)))))

(define-public (sorted-launchers)
  (lazy-plugin-force)
  (let* ((l1 (ahash-table->list connection-variant))
         (l2 (list-filter l1 launcher?))
         (l3 (map launcher-entry l2))
         (l4 (list-sort l3 launcher<=?)))
    l4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (write-local-launchers-info)
  (write (cons (url->system (string->url "$PATH"))
               (sorted-launchers)))
  (display "\n"))

(define-public (get-remote-launchers where)
  ;; NOTE: prepare environment in ~/.bashrc
  (let* ((tmp "$TEXMACS_HOME_PATH/system/remote-launchers")
         (rcmd "texmacs -s -x \"(write-local-launchers-info)\" -q")
         (qcmd (string-quote rcmd))
         (lcmd (string-append "ssh " where " " qcmd " > " tmp)))
    (system lcmd)
    (with res (string-load tmp)
      (system-remove tmp)
      (string->object res))))

(define remote-plugins "$TEXMACS_HOME_PATH/system/remote-plugins.scm")
(define remote-plugins-initialized? #f)
(define remote-plugins-table (make-ahash-table))

(define-public (load-remote-plugins)
  (when (not remote-plugins-initialized?)
    (set! remote-plugins-initialized? #t)
    (when (url-exists? remote-plugins)
      (with l (load-object remote-plugins)
        (set! remote-plugins-table (list->ahash-table l))))))

(define-public (save-remote-plugins)
  (with l (ahash-table->list remote-plugins-table)
    (save-object remote-plugins l)))

(define-public (detect-remote-plugins where)
  (load-remote-plugins)
  (ahash-set! remote-plugins-table where (get-remote-launchers where))
  (save-remote-plugins))

(define-public (remove-remote-plugins where)
  (load-remote-plugins)
  (ahash-remove! remote-plugins-table where)
  (save-remote-plugins))

(define-public (list-remote-plugins where)
  (load-remote-plugins)
  (ahash-ref remote-plugins-table where))

(define-public (remote-connection-info name session)
  (load-remote-plugins)
  (and-with pos (string-index session #\/)
    (let* ((where (substring session 0 pos))
           (rsession (substring session (+ pos 1) (string-length session)))
           (l (list-remote-plugins where))
           (path (car l))
           (bd (map (lambda (x) (cons (list (car x) (cadr x)) (caddr x)))
                    (cdr l)))
           (t (list->ahash-table bd))
           (val (or (ahash-ref t (list name rsession))
                    (ahash-ref t (list name "default")))))
      (and val
           (let* ((env (string-append "export PATH=" path))
                  (rem (string-quote (string-append env "; " val)))
                  (cmd (string-append "ssh " where " " rem)))
             `(tuple "pipe" ,cmd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Supported sessions and scripting languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public supported-sessions-list '())
(define-public supported-sessions-table (make-ahash-table))

(define (supported-sessions-add name menu-name)
  (if (symbol? name) (set! name (symbol->string name)))
  (set! supported-sessions-list (cons name supported-sessions-list))
  (ahash-set! supported-sessions-table name menu-name))

(tm-define (supports-sessions? name)
  (if (symbol? name) (set! name (symbol->string name)))
  (not (not (ahash-ref supported-sessions-table name))))

(tm-define (sorted-supported-sessions)
  (lazy-plugin-force)
  (list-sort supported-sessions-list string<=?))

(define-public supported-scripts-list '())
(define-public supported-scripts-table (make-ahash-table))

(define (supported-scripts-add name menu-name)
  (if (symbol? name) (set! name (symbol->string name)))
  (set! supported-scripts-list (cons name supported-scripts-list))
  (ahash-set! supported-scripts-table name menu-name))

(tm-define (supports-scripts? name)
  (if (symbol? name) (set! name (symbol->string name)))
  (not (not (ahash-ref supported-scripts-table name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache plugin settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public reconfigure-flag? #t)
(define plugin-loaded-setup? #f)
(define plugin-cache "$TEXMACS_HOME_PATH/system/cache/plugin_cache.scm")

(define check-dir-table (make-ahash-table))
(define-public plugin-data-table (make-ahash-table))

(define (plugin-load-setup)
  (when (not plugin-loaded-setup?)
    (set! plugin-loaded-setup? #t)
    (when (url-exists? plugin-cache)
      (with cached (load-object plugin-cache)
	(with (t1 t2) cached
	  (set! plugin-data-table (list->ahash-table t1))
	  (set! check-dir-table (list->ahash-table t2))
	  (when (path-up-to-date?)
	    (set! reconfigure-flag? #f)))))
    ;;(display* "Reconfigure " reconfigure-flag? "\n")
    (when reconfigure-flag?
      (set! check-dir-table (make-ahash-table))
      (set! plugin-data-table (make-ahash-table))
      (init-check-dir-table))))

(define (plugin-save-setup)
  (when reconfigure-flag?
    (save-object plugin-cache
		 (list (ahash-table->list plugin-data-table)
		       (ahash-table->list check-dir-table)))))

(define-public (plugin-versions name)
  (with versions (ahash-ref plugin-data-table name)
    (cond ((not versions) (list))
	  ((list? versions) versions)
	  ((string? versions) (list versions))
	  (else (list "default")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage directories where to search for plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define home-dir (string->url "~"))
(define texmacs-dir (string->url "$TEXMACS_PATH"))

(define (init-check-dir-table)
  (set! check-dir-table (make-ahash-table))
  (add-to-check-dir-table "$PATH"))

(define (add-to-check-dir-table u)
  (cond ((in? u (list (url-head u) home-dir texmacs-dir))
         (noop))
        ((url-or? u)
         (add-to-check-dir-table (url-ref u 1))
         (add-to-check-dir-table (url-ref u 2)))
        ((url-concat? u)
         (add-to-check-dir-table (url-head u))
         (for (v (url->list (url-expand (url-complete u "dr"))))
           (with s (url->system v)
             (when (not (ahash-ref check-dir-table s))
               (ahash-set! check-dir-table s (url-last-modified v))))))))

(define (add-to-path u)
  (add-to-check-dir-table u)
  (with p (url-expand (url-or "$PATH" (url-complete u "dr")))
    (setenv "PATH" (url->system p))))

(define (add-windows-program-path u)
  (add-to-path (url-append (url-or (system->url "C:\\.")
				   (system->url "C:\\Program File*")) u)))

(define (add-macos-program-path u)
  (add-to-path (url-append (system->url "/Applications") u)))

(define (path-up-to-date?)
  (with ok? #t
    (for (p (ahash-table->list check-dir-table))
      (with modified? (!= (url-last-modified (system->url (car p))) (cdr p))
        (if modified? (set! ok? #f))))
    ok?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plugin-configure-cmd name cmd)
  (cond ((func? cmd :require 1)
	 (when reconfigure-flag?
	   (ahash-set! plugin-data-table name ((second cmd)))))
        ((func? cmd :versions 1)
	 (when reconfigure-flag?
	   (ahash-set! plugin-data-table name ((second cmd)))))
        ((func? cmd :setup 1)
	 (if reconfigure-flag? ((second cmd))))
	((func? cmd :prioritary 1)
	 (ahash-set! plugin-data-table (list name :prioritary) (cadr cmd)))
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
	((func? cmd :winpath 2)
	 (when (os-mingw?)
           (add-windows-program-path (url-append (second cmd) (third cmd)))))
	((func? cmd :macpath 2)
	 (when (os-macos?)
           (add-macos-program-path (url-append (second cmd) (third cmd)))))
	((func? cmd :session 1)
	 (supported-sessions-add name (second cmd)))
	((func? cmd :scripts 1)
	 (supported-scripts-add name (second cmd)))
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

(define-public (plugin-configure-cmds name cmds)
  "Helper function for plugin-configure"
  (when (and (nnull? cmds) (ahash-ref plugin-data-table name))
    (plugin-configure-cmd name (car cmds))
    (plugin-configure-cmds name (cdr cmds))))

(define-public (plugin-configure-sub cmd)
  "Helper function for plugin-configure"
  (if (and (list? cmd) (= (length cmd) 2)
	   (in? (car cmd) '(:require :versions :setup :initialize)))
      (list (car cmd) (list 'unquote `(lambda () ,(cadr cmd))))
      cmd))

(define-public-macro (plugin-configure name2 . options)
  "Declare and configure plug-in with name @name2 according to @options"
  (let* ((name (if (string? name2) name2 (symbol->string name2)))
         (supports-name? (string->symbol (string-append "supports-" name "?")))
	 (in-name (string->symbol (string-append "in-" name "%")))
	 (name-scripts (string->symbol (string-append name "-scripts%"))))
    `(begin
       (texmacs-modes (,in-name (== (get-env "prog-language") ,name)))
       (texmacs-modes (,name-scripts (== (get-env "prog-scripts") ,name)))
       (define (,supports-name?) (ahash-ref plugin-data-table ,name))
       (if reconfigure-flag? (ahash-set! plugin-data-table ,name #t))
       (plugin-configure-cmds ,name
	 ,(list 'quasiquote (map plugin-configure-sub options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-initialize-todo (make-ahash-table))
(define plugin-initialize-done? #f)

(define (plugin-all-initialized?)
  (with l (ahash-table->list plugin-initialize-todo)
    (not (list-or (map cdr l)))))

(define-public (plugin-initialize name*)
  "Initialize plugin with name @name*"
  (plugin-load-setup)
  (if (ahash-ref plugin-initialize-todo name*)
      (let* ((name (symbol->string name*))
	     (file (string-append "plugins/" name "/progs/init-" name ".scm"))
	     (u (url-unix "$TEXMACS_HOME_PATH:$TEXMACS_PATH" file)))
	(ahash-set! plugin-initialize-todo name* #f)
	(if (url-exists? u)
	    (with fname (url-materialize u "r")
	      ;;(display* "loading plugin " name* "\n")
	      ;;(display* "loading plugin " fname "\n")
	      ;;(with start (texmacs-time)
	      ;;  (load fname)
	      ;;  (display* name " -> " (- (texmacs-time) start) " ms\n"))
	      (load fname)
	      ))
	(if (plugin-all-initialized?) (plugin-save-setup)))))

(define-public (lazy-plugin-initialize name)
  "Initialize the plug-in @name in a lazy way"
  (ahash-set! plugin-initialize-todo name #t)
  (if (eval (ahash-ref plugin-data-table (list name :prioritary)))
      (plugin-initialize name)
      (delayed
        (:idle 1000)
        (plugin-initialize name))))

(define-public (lazy-plugin-force)
  "Force all lazy plugin initializations to take place"
  (if plugin-initialize-done? #f
      (with l (ahash-table->list plugin-initialize-todo)
	(for-each plugin-initialize (map car l))
	(set! plugin-initialize-done? #t)
	#t)))
