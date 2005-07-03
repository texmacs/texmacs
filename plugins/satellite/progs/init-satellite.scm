
(lazy-menu (satellite-menu) satellite-menu)

(define (satellite-initialize)
  (import-from (satellite))
  (menu-extend tools-menu
    (-> "Satellite" (link satellite-menu))))

(plugin-configure plug-sat
  (:require #t)
  (:initialize (satellite-initialize)))
