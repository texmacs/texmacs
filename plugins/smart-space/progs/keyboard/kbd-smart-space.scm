;; arch-tag: 5658a050-5014-4a3e-9052-2788c0b891b7

(texmacs-module (keyboard kbd-smart-space)
  (:use (texmacs edit smart-space)))

(kbd-map
  (:mode in-text?)
  ("space" (smart-space)))
