
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-speech-en.scm
;; DESCRIPTION : generic editing using French speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-speech-fr)
  (:use (generic generic-speech)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General speech commands for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map french any
  ("point" ".")
  ("comma" ",")
  ("double point" ":")
  ("point virgule" ";")
  ("point d'exclamation" "!")
  ("point d'interrogation" "?")

  ("entrée" (kbd-return))
  ("retour à la ligne" (kbd-return))
  ("nouvelle ligne" (kbd-return))
  ("nouveau paragraphe" (kbd-return))
  ("effacer" (kbd-backspace))
  ("gauche" (kbd-left))
  ("droite" (kbd-right))
  ("monter" (kbd-up))
  ("descendre" (kbd-down))
  ("début ligne" (kbd-start-line))
  ("fin ligne" (kbd-end-line))
  ("début document" (go-start))
  ("fin document" (go-end))

  ("supprimer la balise" (remove-structure-upwards))
  ("supprimer la structure" (remove-structure-upwards))
  ("supprimer vers la droite" (structured-remove-right))
  ("supprimer vers la gauche" (structured-remove-left))
  ("supprimer vers le haut" (structured-remove-up))
  ("supprimer vers le bas" (structured-remove-up))
  ("insérer à gauche" (structured-insert-left))
  ("insérer à droite" (structured-insert-right))
  ("insérer en haut" (structured-insert-up))
  ("insérer en bas" (structured-insert-down))

  ("sortir" (speech-leave))
  ("sortir sortir" (speech-leave) (speech-leave))
  ("sortir sortir sortir" (speech-leave) (speech-leave) (speech-leave))
  ("sortir à droite" (speech-leave))
  ("sortir à gauche" (structured-exit-left))

  ("annuler" (undo 0))
  ("refaire" (redo 0))
  ("annuler annuler" (undo 0) (undo 0))
  ("refaire refaire" (redo 0) (redo 0))
  ("annuler annuler annuler" (undo 0) (undo 0) (undo 0))
  ("refaire refaire refaire" (redo 0) (redo 0) (redo 0))
  ("annuler annuler annuler annuler" (undo 0) (undo 0) (undo 0) (undo 0))
  ("refaire refaire refaire refaire" (redo 0) (redo 0) (redo 0) (redo 0))
  ("couper" (kbd-cut))
  ("coller" (kbd-paste))
  ("copier" (kbd-copy))

  ("précédent" (cursor-history-backward))
  ("suivant" (cursor-history-backward))
  ("numéroter" (numbered-toggle (focus-tree)))
  ("basculer" (alternate-toggle (focus-tree)))
  ("zoomer" (zoom-in (sqrt (sqrt 2.0))))
  ("dézoomer" (zoom-out (sqrt (sqrt 2.0))))
  
  ("tout sur l'écran" (fit-all-to-screen))
  ("grand comme l'écran" (fit-to-screen))
  ("largeur de l'écran" (fit-to-screen-width)))
