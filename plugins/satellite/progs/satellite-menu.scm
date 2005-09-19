
(texmacs-module (satellite-menu))

(menu-bind satellite-menu
  ("Sections"
   (create-file-with-env
    '(chapter section subsection subsubsection paragraph subparagraph
      chapter* section* subsection* subsubsection* paragraph* subparagraph*)))
  ("Theorems"
   (create-file-with-env
    '(theorem lemma proposition corollary
      theorem* lemma* proposition* corollary*)))
  ("Remarks"
   (create-file-with-env
    '(remark note
      remark* note*)))
  ("Equations"
   (create-file-with-env
    '(equation eqnarray leqnarray
      equation* eqnarray* leqnarray*)))
  ("Tables"
   (create-file-with-env
    '(small-table big-table
      small-table* big-table*)))
  ("Figures"
   (create-file-with-env
    '(small-figure big-figure
      small-figure* big-figure*)))
  ---
  ("Other" (create-satellite)))
