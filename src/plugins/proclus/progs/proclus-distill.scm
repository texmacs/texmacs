
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: fd8ce2d4-a87b-4522-9deb-e9247fe8a166
;;
;; MODULE      : proclus-distill.scm
;; DESCRIPTION : Reporting features for the 'proclus' plugin
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

(texmacs-module (proclus-distill)
  (:use (proclus-lib) (proclus-list) (search-in-tree)
        (proclus-locus) (proclus-types) (proclus-absname)
        (proclus-source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode Fichier: crée un fichier sattellite contenant
;;  - le texte de la source (titre)
;;  pour chaque lien de:
;;    type de lien
;;    contenu de la destination

;; Mode Commutateur: dans le document du LOCUS, place un SWITCH qui contien
;; la source et chacune des destination. On perd l'info de type pour le
;; destination. L'operation peut être inversé en activant la diapositive
;; contenat le LOCUS initial puis en détruisant le SWITCH avec A-backspace.

;; Mode plié: le texte source devient le titre d'un FOLD. Le contenu du FOLD
;; est le texte qui est utilisé en mode Fichier.

;; Mode tableau: (expérimental)... etc...

;; Mode déplacement: Circuler dans les documents contenant les destinations des
;; liens.

(tm-define (locus-action)
  (:secure #t)
  (and-let* ((t (get-locus-or-not-locus)))
    (set-last-locus! (locus-self-link t))
    ;; TODO: extend action to account for nested loci.
    (proclus-edit-links)))

(tm-define (proclus-links-action)
  (:secure #t)
  ;; WARNING: there must not be an "interactive" action before opening
  ;; the target document in go-to-locus.
  (and-let* ((t (get-locus-or-not-locus)))
    (save-excursion (go-to-locus (locus-self-link t)))
    (if (style-has? "proclus-links-dtd")
	(kill-buffer))
    (go-to-locus (locus-self-link t))))

(tm-define (remove-link)
  (and-let* ((t (get-locus-or-not-locus))
	     (s (get-source-link)))
    (remove-link-end s (locus-self-link t))
    (go-to-locus s)
    (proclus-edit-links)))

(tm-define (remove-link-type)
  (:interactive #t)
  (and-let* ((t (get-locus-or-not-locus))
             (b (locus-self-link t))
             (s (get-source-link)))
    (ask-types-to-remove
     (cut remove-link-type/callback s b <>)
     (save-excursion (locus-link-types s b)))))

(define (remove-link-type/callback s b types)
  (locus-set-link-types s b types)
  (go-to-locus s)
  (proclus-edit-links))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (proclus-edit-loci)
  (let* ((src-absname (get-absolute-name))
	 (src-buff (get-strg-name-buffer))
	 (the-loci (extract locus? (tree->stree (the-buffer)))))
    (if (nnull? the-loci)
	(begin
          (new-buffer-clear (string-append src-buff " - loci"))
	  (init-style "generic") ;; FIXME: combine styles from sources
	  (init-add-package "proclus-links")
          (set-source-link! (make-root-link src-absname))
	  (tm-assign (the-buffer-path) `(document ,@the-loci))
          (pretend-save-buffer)))))

(define (new-buffer-clear name)
  ;; Create a new buffer with the given name and switch to this buffer. If
  ;; there is already a buffer with the given name, delete it and create a new
  ;; empty buffer.
  (switch-to-active-buffer name)
  (if (equal? name  (get-strg-name-buffer)) (kill-buffer))
  (new-buffer)
  (set-name-buffer name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on loci and links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select-links links types)
  (list-filter links (lambda (lnk)
                       (not (disjoint? types (link-types lnk))))))

(define (sort-links-by-file links)
  ;; regroupe  les buts d'un même fichier
  ;; ((abs-name link ...)
  ;;  (abs-name link ...) ...)
  (map (lambda(y)
         (cons y (list-filter links
                              (lambda (x) (== y (link-absname x))))))
       (no-repetition-list (map link-absname links))))

(define (extract-loci ids t)
  (extract (lambda (x) (and (locus? x)
                            (in? (locus-id x) ids)))
           t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (proclus-edit-links)
  (register-buffer-absolute-name-maybe)
  (and-let* ((the-locus (get-locus-or-not-locus))
             (src-buff (get-strg-name-buffer))
             (the-stree (proclus-edit-links/cons the-locus)))
    (new-buffer-clear
     (string-append src-buff " - source - " (locus-id the-locus)))
    (init-style "generic") ;; FIXME: combine styles from sources
    (init-add-package "proclus-links")
    (init-env "magnification" "1")
    (set-source-link! (locus-self-link the-locus))
    (tm-assign (the-buffer-path) the-stree)
    (pretend-save-buffer)))

(define (absname-error . args)
  `((concat (with "mode" "math" "<longrightarrow>")
            (with "color" "red" ,(string-concatenate args)))))


(define (proclus-edit-links/cons the-locus)
  `(document
    (with "font size" "1.19" "paragraph mode" "center"
          (document
           ,(locus-drop-links the-locus)
           ""
           ,@(list-concatenate
              (map (lambda(y)
                     (cond
                      ((not (absolute-name-exists? (car y)))
                       (absname-error "Unknown absolute name: " (car y)))
                      ((not (url-exists? (absolute-name->url (car y))))
                       (absname-error "Unexistent file: " (car y)))
                      ((not (absolute-name-valid? (car y)))
                       (absname-error "Absolute name conflict: " (car y)))
                      (else
                       (switch-to-active-buffer (absolute-name->url (car y)))
		       `((section ,(car y))
			 ,@(map (cut link->edit <> (cdr y))
				(extract-loci (map link-id (cdr y))
					      (tree->stree (the-buffer))))))))
                   (sort-links-by-file
                    (select-links (locus-links the-locus)
                                  (active-types)))))))))

(define (link->edit t lk)
  ;; t: locus scheme object, pointed to by the edited locus
  ;; lk: links, from the edited locus, pointing to the document containing @t
  `(concat
    "---- "
    (with "color" "blue"
      ;; type of links pointing to @t from the edited locus
      ,(word-list->comma-string (id->types (locus-id t) lk)))
    (with  "mode" "math"  "<longrightarrow>")
    ,(locus-drop-links t)))

(define (word-list->comma-string l)
  (string-join l ", "))

(define (id->types id links)
  (link-types (list-find links (lambda(x) (== id (link-id x))))))
