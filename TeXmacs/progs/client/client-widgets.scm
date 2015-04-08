
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-widgets.scm
;; DESCRIPTION : widgets for remote clients
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-widgets)
  (:use (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (more-parents? dir)
  (with parent (url-head dir)
    (!= (url-head parent) parent)))

(define (get-ancestors dir)
  (let* ((parent (url-head dir))
         (this (url->string (url-tail dir)))
         (last (cons this dir)))
    (if (more-parents? parent)
        (rcons (get-ancestors parent) last)
        (list last))))

(define (string-strip s prefix)
  (if (string-starts? s prefix) (string-drop s (string-length prefix)) s))

(define (remote-relative dir name)
  (if (and (more-parents? dir) (more-parents? (url-head dir)))
      (remote-relative (url-head dir) name)
      (url-append dir name)))

(tm-widget ((remote-file-browser server orig-dir type) quit)
  (let* ((dir orig-dir)
         (entries (list))
         (select-dir
          (lambda (d)
            (with name (string-strip (url->string d) "tmfs://remote-dir/")
              ;;(display* "old dir= " dir "\n")
              ;;(display* `(remote-dir-load ,name) "\n")
              (client-remote-eval server `(remote-dir-load ,name)
                (lambda (new-entries)
                  ;;(display* "Got " new-entries "\n")
                  (set! dir d)
                  (set! entries new-entries)
                  ;;(display* "new dir= " dir "\n")
                  (refresh-now "remote-file-browser"))
                (lambda (err)
                  ;;(display* "Got " err "\n")
                  ;;(display* "new dir= " dir "\n")
                  (set-message err "remote directory"))))))
         (select-entry
          (lambda (e)
            (with (full-name dir? props) (assoc-ref entries e)
              (with name (remote-relative dir full-name)
                (cond (dir? (select-dir name))
                      (else (quit name)))))))
         (list-entry?
          (lambda (e)
            (with (short-name full-name dir? props) e
              (if (== type :directory) dir? #t))))
         (dummy (select-dir orig-dir)))
    (padded
      (refreshable "remote-file-browser"
        (hlist
          (explicit-buttons
            (for (a (get-ancestors dir))
              ((eval (car a)) (select-dir (cdr a))) //)
            >>))
        ===
        (resize "600px" "400px"
          (choice (select-entry answer)
                  (map car (list-filter entries list-entry?))
                  ""))
        (assuming (== type :directory)
          (bottom-buttons
            >>
            ("Ok" (quit dir))))))))

(tm-define (open-remote-file-browser server dir type name cmd)
  (:interactive #t)
  (dialogue-window (remote-file-browser server dir type) cmd name))

(tm-define (remote-browse server)
  (:interactive #t)
  (open-remote-file-browser server (current-buffer) :file
                            "File browser" ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-permission-all perms server id attr on?)
  (with old-vals (ahash-ref perms attr)
    (with new-vals (append (if on? (list "all") (list))
                           (list-difference old-vals (list "all")))
      (ahash-set! perms attr new-vals)
      (remote-set-field server id attr new-vals))))

(define (set-permissions perms enc server id attr vals*)
  (with vals (map (cut ahash-ref enc <>) vals*)
    (ahash-set! perms attr vals)
    (remote-set-field server id attr vals)))

(define (get-permissions perms dec server id attr)
  (with vals (list-union (ahash-ref perms "owner")
                         (ahash-ref perms attr))
    (with vals* (map (cut ahash-ref dec <>) vals)
      (sort vals* string<=?))))

(tm-widget ((entry-permissions-editor server id attrs users enc dec perms) quit)
  (padded
    (tabs
      (loop (attr attrs)
        (tab (text (upcase-first attr))
          (padded
            (hlist
              (toggle (begin
                        (set-permission-all perms server id attr answer)
                        (refresh-now "permission-checklist"))
                      (in? "all" (get-permissions perms dec server id attr)))
              // //
              (text "All users") >>)
            ===
            (refreshable "permission-checklist"
              (if (nin? "all" (get-permissions perms dec server id attr))
                  (choices (set-permissions perms enc server id attr answer)
                           (sort (map (cut ahash-ref dec <>) users) string<=?)
                           (get-permissions perms dec server id attr)))
              (if (in? "all" (get-permissions perms dec server id attr))
                  (resize "250px" "100px"
                    (text ""))))))))))

(tm-define (open-entry-permissions-editor server id attrs)
  (:interactive #t)
  (with-remote-search-user users server (list)
    (let* ((perms (make-ahash-table))
           (enc (make-ahash-table))
           (dec (make-ahash-table)))
      (with-remote-get-entry entry server id
        (for (attr attrs)
          (with vals (or (assoc-ref entry attr) (list))
            (ahash-set! perms attr vals)
            (set! users (list-union users vals))))
        (with-remote-get-user-pseudo pseudos server users
          (with-remote-get-user-name names server users
            (ahash-set! dec "all" "all")
            (ahash-set! enc "all" "all")
            (for-each (lambda (user pseudo name)
                        (when (and (string? pseudo) (string? name))
                          (with full (string-append pseudo " (" name ")")
                            (ahash-set! dec user full)
                            (ahash-set! enc full user))))
                      users pseudos names)
            (set! users (list-difference users (list "all")))
            (dialogue-window (entry-permissions-editor server id attrs
                                                       users enc dec perms)
                             noop "Change permissions")))))))

(tm-define (open-file-permissions-editor server u)
  (:interactive #t)
  (with-remote-get-file-identifier rid server u
    (when rid
      (with attrs (list "readable" "writable" "owner")
        (open-entry-permissions-editor server rid attrs)))))
