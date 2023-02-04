
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-plugins.scm
;; DESCRIPTION : Configuration of plugins
;; COPYRIGHT   : (C) 1999-2013  Joris van der Hoeven
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

(define-public connection-defined (make-ahash-table))
(define-public connection-default (make-ahash-table))
(define-public connection-variant (make-ahash-table))
(define-public connection-varlist (make-ahash-table))
(define-public connection-handler (make-ahash-table))
(define-public connection-session (make-ahash-table))
(define-public connection-scripts (make-ahash-table))

(ahash-set! connection-defined "scheme" "Scheme")
(ahash-set! connection-session "scheme" "Scheme")
(ahash-set! connection-scripts "scheme" "Scheme")

(define (connection-setup name val . opt)
  (ahash-set! connection-defined name #t)
  (if (null? opt) (set! opt (list "default")))
  (with l (or (ahash-ref connection-varlist name) (list))
    (ahash-set! connection-variant (list name (car opt)) val)
    (if (nin? (car opt) l)
      (ahash-set! connection-varlist name (rcons l (car opt))))))

(define-public (connection-list)
  (list-sort (list-union (map car (ahash-table->list connection-defined))
                         (remote-connection-list))
             string<=?))

(define-public (local-connection-variants name)
  (lazy-plugin-force)
  (or (ahash-ref connection-varlist name) (list)))

(define-public (connection-variants name)
  (append (local-connection-variants name)
          (remote-connection-variants name)))

(define-public (connection-defined? name)
  (lazy-plugin-force)
  (or (ahash-ref connection-defined name)
      (remote-connection-defined? name)))

(define-public (connection-info-sub name session)
  (or (remote-connection-info name session)
      (ahash-ref connection-variant (list name session))
      (with l (connection-variants name)
        (and (nnull? l)
             (!= session (car l))
             (connection-info-sub name (car l))))))

(define-public (connection-info name session)
  (lazy-plugin-force)
  (with pos (string-index session #\:)
    (if pos (connection-info name (substring session 0 pos))
        (connection-info-sub name session))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plug-ins for console sessions and scripting languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (session-setup name menu-name)
  (if (symbol? name) (set! name (symbol->string name)))
  (ahash-set! connection-session name menu-name))

(define-public (session-list)
  (lazy-plugin-force)
  (with l (map car (ahash-table->list connection-session))
    (list-sort (list-union l (remote-session-list)) string<=?)))

(define (session-ref name)
  (or (ahash-ref connection-session name)
      (remote-session-ref name)))

(define-public (session-defined? name)
  (session-ref name))

(define-public (session-name name)
  (or (session-ref name) (upcase-first name)))

(define (scripts-setup name menu-name)
  (if (symbol? name) (set! name (symbol->string name)))
  (ahash-set! connection-scripts name menu-name))

(define-public (scripts-list)
  (lazy-plugin-force)
  (with l (map car (ahash-table->list connection-scripts))
    (list-sort (list-union l (remote-scripts-list)) string<=?)))

(define (scripts-ref name)
  (or (ahash-ref connection-scripts name)
      (remote-scripts-ref name)))

(define-public (scripts-defined? name)
  (scripts-ref name))

(define-public (scripts-name name)
  (or (scripts-ref name) (upcase-first name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (launcher? x)
  (and (func? (cdr x) 'tuple 2)
       (== (caddr x) "pipe")))

(define (launcher-entry x)
  (rcons (car x) (cadddr x)))

(define (launcher<=? l1 l2)
  (or (string<=? (car l1) (car l2))
      (and (== (car l1) (car l2))
           (string<=? (cadr l1) (cadr l2)))))

(define (launcher-list)
  (lazy-plugin-force)
  (let* ((l1 (ahash-table->list connection-variant))
         (l2 (list-filter l1 launcher?))
         (l3 (map launcher-entry l2))
         (l4 (list-sort l3 launcher<=?)))
    l4))

(define-public (write-local-plugin-info)
  (lazy-plugin-force)
  (write (list (url->system (string->url "$PATH"))
               (launcher-list)
               (ahash-table->list connection-session)
               (ahash-table->list connection-scripts)
               (url->system (string->url "$TEXMACS_PATH"))
               (url->system (string->url "$TEXMACS_HOME_PATH"))))
  (display "\n"))

(define-public (get-remote-plugin-info where)
  ;; NOTE: prepare environment in ~/.bashrc
  (let* ((tmp "$TEXMACS_HOME_PATH/system/remote-plugin-info")
         (rcmd "texmacs -s -x \"(write-local-plugin-info)\" -q")
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
        (set! remote-plugins-table (list->ahash-table l))
        (update-remote-tables)))))

(define-public (save-remote-plugins)
  (with l (ahash-table->list remote-plugins-table)
    (save-object remote-plugins l)))

(tm-define (detect-remote-plugins where)
  (:argument where "Remote server")
  (load-remote-plugins)
  (ahash-set! remote-plugins-table where (get-remote-plugin-info where))
  (update-remote-tables)
  (save-remote-plugins))

(tm-define (update-remote-plugins where)
  (:argument where "Remote server")
  (:proposals where (remote-connection-servers))
  (detect-remote-plugins where))

(tm-define (remove-remote-plugins where)
  (:argument where "Remote server")
  (:proposals where (remote-connection-servers))
  (load-remote-plugins)
  (ahash-remove! remote-plugins-table where)
  (update-remote-tables)
  (save-remote-plugins))

(define-public (list-remote-plugins where)
  (load-remote-plugins)
  (ahash-ref remote-plugins-table where))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve data about remote plug-ins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remote-servers-table (make-ahash-table))
(define remote-supported-table (make-ahash-table))
(define remote-variant-table (make-ahash-table))
(define remote-launch-table (make-ahash-table))
(define remote-session-table (make-ahash-table))
(define remote-scripts-table (make-ahash-table))

(define (opt-export env opts)
  (if (null? opts) env
      (string-append env
                     "; export TEXMACS_PATH=" (car opts)
                     "; export TEXMACS_HOME_PATH=" (cadr opts))))

(define-public (update-remote-tables)
  (set! remote-servers-table (make-ahash-table))
  (set! remote-supported-table (make-ahash-table))
  (set! remote-variant-table (make-ahash-table))
  (set! remote-launch-table (make-ahash-table))
  (set! remote-session-table (make-ahash-table))
  (set! remote-scripts-table (make-ahash-table))
  (for (entry (ahash-table->list remote-plugins-table))
    (with (where path launch session scripts . opts) entry
      (ahash-set! remote-servers-table where #t)
      (for (x launch)
        (with (p v c) x
          (ahash-set! remote-supported-table p #t)
          (with variant (string-append where "/" v)
            (with l (or (ahash-ref remote-variant-table p) (list))
              (ahash-set! remote-variant-table p (rcons l variant)))
            (let* ((env (opt-export (string-append "export PATH=" path) opts))
                   (rem (string-quote (string-append env "; " c)))
                   (cmd (string-append "ssh " where " " rem))
                   (val `(tuple "pipe" ,cmd)))
              (ahash-set! remote-launch-table (cons p variant) val)))))
      (for (x session)
        (ahash-set! remote-session-table (car x) (cdr x)))
      (for (x scripts)
        (ahash-set! remote-scripts-table (car x) (cdr x))))))

(define remote-initialized-data? #f)
(define (remote-initialize-data)
  (when (not remote-initialized-data?)
    (load-remote-plugins)
    (set! remote-initialized-data? #t)))

(define-public (remote-connection-servers)
  (remote-initialize-data)
  (sort (map car (ahash-table->list remote-servers-table)) string<=?))

(define (remote-connection-list)
  (remote-initialize-data)
  (sort (map car (ahash-table->list remote-supported-table)) string<=?))

(define (remote-connection-variants name)
  (remote-initialize-data)
  (or (ahash-ref remote-variant-table name) (list)))

(define-public (remote-connection-defined? name)
  (remote-initialize-data)
  (nnot (ahash-ref remote-variant-table name)))

(define (remote-connection-info name session)
  (remote-initialize-data)
  (ahash-ref remote-launch-table (cons name session)))

(define (remote-session-list)
  (remote-initialize-data)
  (map car (ahash-table->list remote-session-table)))

(define (remote-session-ref name)
  (remote-initialize-data)
  (ahash-ref remote-session-table name))

(define (remote-scripts-list)
  (remote-initialize-data)
  (map car (ahash-table->list remote-scripts-table)))

(define (remote-scripts-ref name)
  (remote-initialize-data)
  (ahash-ref remote-scripts-table name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache plugin reinit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reinit-connection)
  (set! connection-defined (make-ahash-table))
  (set! connection-default (make-ahash-table))
  (set! connection-variant (make-ahash-table))
  (set! connection-varlist (make-ahash-table))
  (set! connection-handler (make-ahash-table))
  (set! connection-session (make-ahash-table))
  (set! connection-scripts (make-ahash-table)))

(define-public (reinit-plugin-cache)
  (reinit-connection)
  (set! reconfigure-flag? #t)
  (with plugins (plugin-list)
    (for-each (cut ahash-set! plugin-initialize-todo <> #t) plugins)
    (for-each (cut plugin-initialize <>) plugins)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache plugin settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public reconfigure-flag? #t)
(define plugin-loaded-setup? #f)
(define plugin-cache "$TEXMACS_HOME_PATH/system/cache/plugin_cache.scm")

(define plugin-check-path "")
(define check-dir-table (make-ahash-table))
(define-public plugin-data-table (make-ahash-table))

(define (plugin-load-setup)
  (when (not plugin-loaded-setup?)
    (set! plugin-loaded-setup? #t)
    (when (url-exists? plugin-cache)
      (with cached (load-object plugin-cache)
        (if (== (length cached) 2) (set! cached (cons "" cached)))
        (with (p t1 t2) cached
          (set! plugin-check-path p)
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
                 (list (get-original-path)
                       (ahash-table->list plugin-data-table)
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

(define (add-to-path u after?)
  (add-to-check-dir-table u)
  (let* ((u1 (url-complete u "dr"))
         (u2 "$PATH")
         (u3 (if after? (url-or u2 u1) (url-or u1 u2)))
         (p  (url-expand u3)))
    (when (not (url-none? u1))
      (system-setenv "PATH" (url->system p)))))

(define (add-to-path* prefix u after?)
  (add-to-path (url-append (system->url prefix) u) after?))

(define (add-windows-program-path u after?)
  (add-to-path* "C:\\." u after?)
  (add-to-path* "C:\\Program File*" u after?)
  (add-to-path* "$HOME\\AppData\\Local" u after?)
  (add-to-path* "$HOME\\AppData\\Local\\Programs" u after?))

(define (add-macos-program-path u after?)
  (add-to-path* "/Applications" u after?)
  (add-to-path* "$HOME/Applications" u after?))

(define-public (plugin-add-windows-path rad rel after?)
  (when (os-mingw?)
    (add-windows-program-path (url-append rad rel) after?)))

(define-public (plugin-add-macos-path rad rel after?)
  (when (os-macos?)
    (add-macos-program-path (url-append rad rel) after?)))

(define (path-up-to-date?)
  (with ok? (== plugin-check-path (get-original-path))
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
           (add-windows-program-path (url-append (second cmd) (third cmd)) #t)))
        ((func? cmd :macpath 2)
         (when (os-macos?)
           (add-macos-program-path (url-append (second cmd) (third cmd)) #t)))
        ((func? cmd :session 1)
         (session-setup name (second cmd)))
        ((func? cmd :scripts 1)
         (scripts-setup name (second cmd)))
        ((func? cmd :filter-in 1)
         (noop))
        ((func? cmd :serializer 1)
         (plugin-serializer-set! name (second cmd)))
        ((func? cmd :commander 1)
         (plugin-commander-set! name (second cmd)))
        ((func? cmd :tab-completion 1)
         (if (second cmd) (plugin-supports-completions-set! name)))
        ((func? cmd :test-input-done 1)
         (if (second cmd) (plugin-supports-input-done-set! name))))

  (or (in? (car cmd) '(:macpath :winpath))
      (ahash-ref plugin-data-table name)))

(define-public (plugin-configure-cmds name cmds)
  "Helper function for plugin-configure"
  (when (and (nnull? cmds) (plugin-configure-cmd name (car cmds)))
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
       (define (,supports-name?)
         (or (ahash-ref plugin-data-table ,name)
             (remote-connection-defined? ,name)))
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
