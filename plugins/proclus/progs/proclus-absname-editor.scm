
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 3093c14f-09fc-4277-9617-a34c53d2200f
;;
;; MODULE      : proclus-absname-editor.scm
;; DESCRIPTION : User interface for the absolute names editor
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

(texmacs-module (proclus-absname-editor)
  (:use (proclus-absname)))

(tm-define (absname-editor)
  (switch-to-active-buffer
   "$TEXMACS_HOME_PATH/system/tmp/Proclus - Absolute names")
  (tree-assign (buffer-tree) (absname-editor/cons))
  (pretend-save-buffer))

(define (absname-editor/cons)
  `(document
    (section "Proclus constellation")
    (description-long
     (document
      ,@(absolute-name-fold
         (lambda (absname url kdr)
           (cons `(concat (item* ,absname)
                          ,url
                          ,@(name-conflict absname)) kdr))
         '())))))

(define (name-conflict absname)
  (cond
   ((not (url-exists? (absolute-name->url absname)))
    `((next-line) (with "color" "red" "Unexistent file.")))
   ((not (absolute-name-valid? absname))
    `((next-line) (with "color" "red" "Absolute name conflict.")))
   (else '())))
