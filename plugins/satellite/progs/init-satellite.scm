
(define (satellite-initialize)
  (use-modules (satellite))
  (menu-bind satellite-menu
    (-> "Table"
	("Contents"
	 (create-file-with-env '(section subsection subsubsection)))
	("Remarks"
	 (create-file-with-env '(remark note)))
	---
	("Other" ... (create-satellite))))
  (menu-extend texmacs-extra-menu
    (=> "Satellite" (link satellite-menu))))

(plugin-configure plug-sat
  (:require #t)
  (:initialize (satellite-initialize)))
