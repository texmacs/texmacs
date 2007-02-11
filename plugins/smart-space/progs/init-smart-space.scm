;; arch-tag: 086e08fb-2a42-41d5-9f11-7c0fdabedb96

(plugin-configure smart-space
  (:require #t)
  (:initialize (lazy-keyboard (keyboard kbd-smart-space) in-text?)))
