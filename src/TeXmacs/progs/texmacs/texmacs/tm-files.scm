
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : files.scm
;; DESCRIPTION : file handling
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-files)
  (:use (texmacs texmacs tm-server))
  (:export
    ;; general purpose loading and saving
    save-buffer load-buffer
    conditional-save-buffer conditional-load-buffer ;; due to interactive
    conditional-recover-autosave ;; due to interactive
    ;; shortcuts for special formats or extra actions
    buffer-loader buffer-saver
    load-in-new-window load-browse-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conditional-save-buffer file fm confirm)
  (if (yes? confirm) (texmacs-save-buffer file fm)))

(define (secure-save-buffer file fm)
  (with file* (url-concretize file)
    ;; FIXME: concretization should not be necessary
    ;; due to bad current implementation of 'interactive'
    (if (url-exists? file)
	(interactive '("File already exists. Overwrite existing file?")
		     `(lambda (confirm)
			(conditional-save-buffer ,file* ,fm confirm)))
	(texmacs-save-buffer file fm))))

(define (save-buffer . l)
  (cond ((= (length l) 0)
	 (if (no-name?)
	     (interactive '("Save as:") 'save-buffer)
	     (texmacs-save-buffer (get-name-buffer) "generic")))
	((= (length l) 1) (secure-save-buffer (car l) "generic"))
	(else (secure-save-buffer (car l) (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conditional-load-buffer file fm where confirm)
  (if (yes? confirm)
      (texmacs-load-buffer (url-glue file "~") fm where #t)
      (texmacs-load-buffer file fm where #f)))

(define (load-buffer-sub file fm where)
  (with file* (url-concretize file)
    ;; FIXME: concretization should not be necessary
    ;; due to bad current implementation of 'interactive'
    (if (and (not (== fm "help"))
	     (not (url-rooted-web? file))
	     (url-exists? file)
	     (url-exists? (url-glue file "~"))
	     (url-newer? (url-glue file "~") file))
	(interactive
	 '("Load more recent autosave file?")
	 `(lambda (confirm)
	    (conditional-load-buffer ,file* ,fm ,where confirm)))
	(texmacs-load-buffer file fm where #f))))

(define (load-buffer . l)
  (with file (url-append "$TEXMACS_FILE_PATH" (car l))
    (cond ((= (length l) 1)
	   (load-buffer-sub file "generic" 0))
	  ((and (= (length l) 2) (string? (cadr l)))
	   (load-buffer-sub file (cadr l) 0))
	  ((and (= (length l) 2) (integer? (cadr l)))
	   (load-buffer-sub file "generic" (cadr l)))
	  (else (load-buffer-sub file (cadr l) (caddr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autosave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conditional-recover-autosave confirm)
  (with name "$TEXMACS_HOME_PATH/system/autosave.tm"
    (if (yes? confirm)
	(with t (texmacs-load-tree name "texmacs")
	  (set-buffer (get-name-buffer) t))
	(system-remove name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (buffer-loader fm) (lambda (s) (load-buffer s fm)))
(define (buffer-saver fm) (lambda (s) (save-buffer s fm)))
(define (load-in-new-window s) (load-buffer s 1))
(define (load-browse-buffer s)
  (if (help-buffer?) (load-buffer s "help") (load-buffer s)))
