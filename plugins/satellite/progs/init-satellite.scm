
(define (satellite-initialize)
  (use-modules (satellite))
  (menu-bind satellite-menu
    (-> "Table"
	("Contents"
	 (create-file-with-env '(section subsection subsubsection)))
	("Theorems"
	 (create-file-with-env '(theorem lemma proposition corollary)))
	("Remarks"
	 (create-file-with-env '(remark note)))
	---
	("Other" ... (create-satellite))))
  (menu-extend texmacs-extra-menu
    (=> "Satellite" (link satellite-menu))))

(plugin-configure plug-sat
  (:require #t)
  (:initialize (satellite-initialize)))
