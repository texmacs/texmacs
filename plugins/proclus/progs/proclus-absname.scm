
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 22bb11ba-035c-4785-8c26-ac216a3383e1
;;
;; MODULE      : proclus-absname.scm
;; DESCRIPTION : Support procedures for Proclus absolute names
;; COPYRIGHT   : (C) 2003--2004  Alain Herreman, David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (proclus-absname)
  (:use (proclus-lib))
  (:export
    absolute-name-exists?
    absolute-name-valid?

    absname-choose-file/sub ;; FIXME: for choose-file

    absolute-name->url
    absolute-name-fold

    has-absolute-name?
    has-conflicting-absolute-name?
    get-absolute-name
    register-buffer-absolute-name-maybe

    absolute-name-message
    absolute-name-reregister-buffer
    interactive-absolute-name
    absname-choose-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Absolute names dictionnary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic loading and saving

(define proclus-names-file "$TEXMACS_HOME_PATH/system/proclus-names.scm")

(define (save-absolute-names object)
  (save-object proclus-names-file object))

(define (load-absolute-names)
  (if (url-exists? proclus-names-file)
      (load-object proclus-names-file)
      #f))

;; High-level loading and saving

(define absolute-names-alist
  (delay (or (load-absolute-names) '())))

(define (get-absolute-names-alist)
  (force absolute-names-alist))

(define (set-absolute-names-alist! alist)
  (set! absolute-names-alist (delay alist))
  (save-absolute-names alist))

(define (register-absolute-name absname location)
  (set-absolute-names-alist! (cons (list absname location)
                                   (get-absolute-names-alist))))

(define (forget-absolute-name absname)
  (set-absolute-names-alist!
   (list-filter (get-absolute-names-alist)
                (lambda (x) (!= (car x) absname)))))

;; Public functions

(define (absolute-name-exists? s)
  (not (not (assoc s (get-absolute-names-alist)))))

(define (absolute-name-valid? s)
  (let ((buf (get-strg-name-buffer)))
    (dynamic-wind
        noop
        (lambda ()
          (and-let* (((absolute-name-exists? s))
                     (filename (absolute-name->url s))
                     ((url-exists? filename))
                     ((switch-to-active-buffer filename))
                     ((has-valid-absolute-name?)))))
        (cut switch-to-active-buffer buf))))

(define (absolute-name->url s)
  (or (and-let* ((l (assoc s (get-absolute-names-alist))))
        (second l))
      (let ((msg "Absolute name unknown"))
        (set-message msg s)
        (texmacs-error 'absolute-name->url msg (list s)))))

(define (absolute-name-fold kons knil)
  (list-fold (lambda (kar kdr)
               (kons (first kar) (second kar) kdr))
             knil (get-absolute-names-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Absolute name of the current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (has-absolute-name?)
  ;; Has the current buffer been assigned an absolute name?
  (and (init-has? "absolute-name")
       (not (string-null? (get-init-env "absolute-name")))))

(define (check-has-absolute-name where)
  (if (not (has-absolute-name?))
      (let ((msg "Buffer has no absolute name")
            (buf (get-strg-name-buffer)))
        (set-message msg buf)
        (texmacs-error where msg (list buf)))))

(define (check-has-file-name where)
  ;; FIXME: this test really sucks. We should be able to make a difference
  ;; between scratch buffers and buffers associated to files.
  (if (no-name?)
      (let ((msg "Buffer has no file name")
            (buf (get-strg-name-buffer)))
        (set-message msg buf)
        (texmacs-error where msg (list buf)))))

(define (has-conflicting-absolute-name?)
  (and-let* (((has-absolute-name?))
             (absname (get-init-env "absolute-name"))
             ((absolute-name-exists? absname))
             ((!= (get-strg-name-buffer)
		  (absolute-name->url absname))))))

(define (absolute-name-reregister-buffer)
  (check-has-absolute-name 'absolute-name-moved)
  (check-has-file-name 'absolute-name-moved)
  (forget-absolute-name (get-absolute-name))
  (register-buffer-absolute-name-maybe))

(define (has-valid-absolute-name?)
  (and-let* (((has-absolute-name?))
             (absname (get-init-env "absolute-name"))
             ((absolute-name-exists? absname))
             ((== (get-strg-name-buffer)
                  (absolute-name->url absname))))))

(define (get-absolute-name)
  ;; Intrinsic absname of the current buffer.
  (check-has-absolute-name 'get-absolute-name)
  (check-has-file-name 'get-absolute-name)
  (get-init-env "absolute-name"))

(define (register-buffer-absolute-name-maybe)
  ;; Assume the current buffer has an absolute name.
  ;; If this name is not in the dictionnary, register it.
  ;; If this name is associated to a different url, error.
  ;; Evaluates to <unspecified>.
  (let ((self 'register-buffer-absolute-name-maybe))
    (check-has-absolute-name self)
    (check-has-file-name self)
    (let ((absname (get-absolute-name)))
      (if (not (absolute-name-exists? absname))
          (register-absolute-name absname (get-strg-name-buffer)))
      (if (not (has-valid-absolute-name?))
          (let ((msg "Conflit de nom absolu"))
            (set-message msg absname)
            (texmacs-error self msg (list absname)))))))

(define (absolute-name-message)
  (check-has-absolute-name 'absolute-name-message)
  (set-message (string-append "Nom absolu: " (get-absolute-name))
               (get-strg-name-buffer)))

(define (interactive-absolute-name)
  (check-has-file-name 'interactive-absolute-name)
  (interactive '("Nom absolu:") interactive-absolute-name/callback))

(define (interactive-absolute-name/callback s)
  (if (or (string-null? s) (absolute-name-exists? s))
      (interactive-absolute-name)
      (begin (register-absolute-name s (get-strg-name-buffer))
             (init-env "absolute-name" s))))

(define (absname-choose-file)
  (let ((from (get-strg-name-buffer)))
    (choose-file "Mémoriser le nom absolu de ce fichier" "texmacs"
                 `(lambda (x) (absname-choose-file/sub x ,from)))))

(define (absname-choose-file/sub u from)
  (switch-to-active-buffer u)
  (register-buffer-absolute-name-maybe)
  (switch-to-active-buffer from))
