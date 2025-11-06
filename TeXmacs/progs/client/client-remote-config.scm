
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-remote-config.scm
;; DESCRIPTION : remote server preferences, client side
;; COPYRIGHT   : (C) 2025 Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-remote-config)
  (:use (client client-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Admin preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-preferences-edit-form-path
  "$TEXMACS_PATH/progs/client/forms/server-preferences.tm")

(define server-preferences-edit-form-url
  (string->url server-preferences-edit-form-path))

(tm-define (client-get-server-preferences-edit-form)
  server-preferences-edit-form-url)

(tm-define (open-server-preferences-form server tree)
  (let* ((name (string-append "server-preferences-"
                              (number->string server)))
         (url (aux-name name)))
    (ahash-set! preference-buffers url server)
    (if (buffer-exists? url)
      (switch-to-buffer url)
      (open-auxiliary name tree url))))

(define remote-server-preferences (make-ahash-table))
(define preference-buffers (make-ahash-table))
(define current-server #f)

(tm-define (reset-preferences-table-cache server)
  (ahash-set! remote-server-preferences server (make-ahash-table)))

(tm-define (lset-preference key val)
  (:secure #t)
  ; (display* "setting pref: " server port key val "\n")
  (with current-server (ahash-ref preference-buffers (current-buffer-url))
    (when (not (hash-table? (ahash-ref remote-server-preferences
                                       current-server)))
      (ahash-set! remote-server-preferences current-server
                  (make-ahash-table)))
    (ahash-set! (ahash-ref remote-server-preferences current-server)
                key val)))

(tm-define (lget-preference key)
  (if (or (not current-server)
          (not (hash-table? (ahash-ref remote-server-preferences current-server))))
    #f
    (ahash-ref (ahash-ref remote-server-preferences current-server) key)))

(tm-define (lprint-preferences)
  (hash-for-each
    (lambda (server-name prefs)
      (begin (display* server-name "\n")
             (hash-for-each
               (lambda (key value) (display* "  \"" key "\": " value "\n"))
               prefs)))
   remote-server-preferences))

(tm-define (submit-preferences)
  (:secure #t)
  (with current-server (ahash-ref preference-buffers (current-buffer-url))
    (let* ((remote-prefs (ahash-ref remote-server-preferences current-server))
           (wcb (lambda (ret)
                  (display* "Preference submit result: " ret "\n"))))
      (if remote-prefs
        (client-remote-eval* current-server
                             `(remote-set-preferences
                                ,(ahash-table->list remote-prefs))
                             wcb)
        (client-open-error (format #f "No preferences for server ~A (~A:~A)"
                                   current-server
                                   (client-find-server-name current-server)
                                   (client-find-server-port current-server)))))
    (client-open-error "No server set")))

(tm-define (client-public-preferences-then server cb)
  (client-remote-then server `(remote-public-preferences) cb
                      "Cannot retrieve remote server preferences: "))

(tm-define (client-admin-preferences-then server cb)
  (client-remote-then server `(remote-admin-preferences) cb
                      "Cannot retrieve remote server admin preferences: "))

(tm-define (client-admin-preferences-form-then server cb)
  (client-remote-then server `(remote-admin-preferences-form) cb
    "Cannot retrieve remote server admin preferences form: "))

(tm-define (load-remote-config-form server)
  (client-admin-preferences-form-then server
    (lambda (form-tree)
      (reset-preferences-table-cache server)
      (open-server-preferences-form server form-tree))))
