
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
        (proclus-target) (proclus-types) (proclus-absname)
        (proclus-source))
  (:export has-last-target?
           go-to-last-target
           has-source-buffer?
           go-to-source-buffer
           target-action
           proclus-links-action
           edit-targets
           edit-links))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode Fichier: crée un fichier sattellite contenant
;;  - le texte de la source (titre)
;;  pour chaque lien de:
;;    type de lien
;;    contenu de la destination

;; Mode Commutateur: dans le document du TARGET, place un SWITCH qui contien
;; la source et chacune des destination. On perd l'info de type pour le
;; destination. L'operation peut être inversé en activant la diapositive
;; contenat le TARGET initial puis en détruisant le SWITCH avec A-backspace.

;; Mode plié: le texte source devient le titre d'un FOLD. Le contenu du FOLD
;; est le texte qui est utilisé en mode Fichier.

;; Mode tableau: (expérimental)... etc...

;; Mode déplacement: Circuler dans les documents contenant les destinations des
;; liens.

(define (target-action)
  (and-let* ((t (get-target-or-not-target)))
    (set-last-target! (target-self-link t))
    ;; TODO: extend action to account for nested targets.
    (edit-links)))

(define (proclus-links-action)
  (and-let* ((t (get-target-or-not-target)))
    (go-to-target (target-self-link t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-targets)
  (let* ((src-buff (get-strg-name-buffer))
	 (the-nw-buff (string-append src-buff "-targets"))
	 (the-tree (object->tree `(document  ,@(targets-tree)))))
    (if (not (equal? (texmacs->verbatim the-tree) ""))
	(begin
          (new-buffer-clear the-nw-buff)
	  (init-style "proclus-links")
          (set-source-buffer! src-buff)
	  (insert-tree the-tree)
          (pretend-save-buffer)))))

(define (targets-tree)
  (extract target? (tree->object (the-buffer))))

(define (new-buffer-clear name)
  ;; Create a new buffer with the given name and switch to this buffer. If
  ;; there is already a buffer with the given name, delete it and create a new
  ;; empty buffer.
  (switch-to-active-buffer name)
  (if (equal? name  (get-strg-name-buffer)) (kill-buffer))
  (new-buffer)
  (set-name-buffer name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on targets and links
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

(define (extract-targets ids t)
  (extract (lambda (x) (and (target? x)
                            (in? (target-id x) ids)))
           t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-links)
  (register-buffer-absolute-name-maybe)
  (and-let* ((the-target (get-target-or-not-target))
             (src-buff (get-strg-name-buffer))
             (the-object (edit-links/cons the-target)))
    (new-buffer-clear
     (string-append src-buff "-source-" (target-id the-target)))
    (init-style "proclus-links")
    (init-env "magnification" "1")
    (set-source-buffer! src-buff)
    (insert-object the-object)
    (pretend-save-buffer)))

(define (absname-error . args)
  `((concat (with "mode" "math" "<longrightarrow>")
            (with "color" "red" ,(string-concatenate args)))))


(define (edit-links/cons the-target)
  `(document
    (with "font size" "1.19" "paragraph mode" "center"
          (document
           ,(target-drop-links the-target)
           ""
           ,@(list-concatenate
              (map (lambda(y)
                     (cond
                      ((not (absolute-name-exists? (car y)))
                       (absname-error "Nom absolu inconnu: " (car y)))
                      ((not (url-exists? (absolute-name->url (car y))))
                       (absname-error "Fichier absent: " (car y)))
                      ((not (absolute-name-valid? (car y)))
                       (absname-error "Conflit de nom absolu: " (car y)))
                      (else
                       (switch-to-active-buffer (absolute-name->url (car y)))
                       (map (cut link->edit <> (cdr y))
                            (extract-targets (map link-id (cdr y))
                                             (tree->object (the-buffer)))))))
                   (sort-links-by-file
                    (select-links (target-links the-target)
                                  (active-types)))))))))

(define (link->edit t lk)
  ;; t: target scheme object, pointed to by the edited target
  ;; lk: links, from the edited target, pointing to the document containing @t
  `(concat
    "---- "
    (with "color" "blue"
      ;; type of links pointing to @t from the edited target
      ,(word-list->comma-string (id->types (target-id t) lk)))
    (with  "mode" "math"  "<longrightarrow>")
    ,(target-drop-links t)))

(define (word-list->comma-string l)
  (string-join l ", "))

(define (id->types id links)
  (link-types (list-find links (lambda(x) (== id (link-id x))))))
