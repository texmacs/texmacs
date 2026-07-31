
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-markup.scm
;; DESCRIPTION : extra markup for remote documents such as the home page
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-markup)
  (:use (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve information from server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remote-info-table (make-ahash-table))

(define (get-remote-info kind)
  (and-with val (ahash-ref remote-info-table kind)
    (with (info stamp) val
      (with now (texmacs-time)
        (and (< now (+ stamp 60000))
             info)))))

(define (set-remote-info kind info ref)
  (with now (texmacs-time)
    (ahash-set! remote-info-table kind (list info now))
    (update-current-buffer)))

(define (server-from-file u)
  (and (url-rooted-protocol? u "tmfs")
       (with u* (url->string (url-unroot u))
         (and (tmfs-cdr u*)
              (tmfs-car (tmfs-cdr u*))))))

(define (retrieve-remote-user-info ref)
  (and-with server* (server-from-file ref)
    (and-with server (client-find-server server*)
      (client-get-account-then server (get-default-user)
        (lambda (info)
          (set-remote-info 'user-info info ref)))
      (list))))

(define (get-remote-user-info ref)
  (or (get-remote-info 'user-info)
      (retrieve-remote-user-info ref)))

(define (get-remote-user-info-about var ref)
  (and-with info (get-remote-user-info ref)
    (and-with vals (ahash-ref (list->ahash-table info) var)
      (if (null? vals) :loading (car vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-remote-picture)
  (:secure #t)
  "$HOME/me.png")

(tm-define (ext-remote-name)
  (:secure #t)
  (with name (get-remote-user-info-about "name" (current-buffer))
    (cond ((== name :loading) `(greyed (em "Loading...")))
          ((not name) (get-metadata "author"))
          (else name))))

(tm-define (ext-remote-email)
  (:secure #t)
  (with email (get-remote-user-info-about "email" (current-buffer))
    (cond ((== email :loading) `(greyed (em "Loading...")))
          ((not email) (string-append (get-metadata "pseudo") "@localhost"))
          (else email))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-remote-shortlist name)
  (list "test1.tm" "test2.tm" "test3.tm"))

(tm-define (ext-remote-menu-item item-name)
  `(row (cell ,item-name)))

(tm-define (ext-remote-menu name icon-name title)
  (:secure #t)
  `(remote-menu-table
     (table
      (row (cell (remote-big-icon ,icon-name)))
      (row (cell ,title))
      ,@(map ext-remote-menu-item (ext-remote-shortlist name)))))
