<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Edition structurée>

  En règle générale, le comportement de la plupart des opérations d'édition
  structurée est guidé par le <em|<rigid|current focus>>. Par défaut, le tag
  le plus intérieur contient le curseur. Même si une selection est active, le
  focus courant est le tag le plus intérieur à la sélection. Pendant des
  opérations structurées, telles que la navigation entre des tags similaires,
  le focus courant peut être temporairement mis sur quelque chose d'autre. Le
  focus courant est caractérisé visuellement par la boite bleue la plus
  intérieure au curseur.\ 

  Par exemple, les commandes d'<em|insertion structurée>
  <shortcut|(structured-insert-left)>, <shortcut|(structured-insert-right)>,
  <shortcut|(structured-insert-up)> et <shortcut|(structured-insert-down)>
  ont une signification particulière dans les tableaux et les arbres. Dans
  les tableaux, elles permettent d'insérer de nouvelles lignes et colonnes
  (voir la figure<nbsp><reference|matrix-insert-fig>). Dans les arbres, elles
  insèrent de nouveaux noeuds (voir la figure<nbsp><reference|tree-insert-fig>).
  Chaque fois que vous insérez un arbre dans un tableau, le tag le plus
  intérieur est l'arbre et l'insertion de noeud prend le dessus sur
  l'insertion de lignes et de colonnes.

  Dans beaucoup de cas, un \S comportement par défaut \T a été défini pour
  les tags excepté pour une petite minorité. Dans notre exemple d'insertion
  structurée, le défaut de <shortcut|(structured-insert-left)> et
  <shortcut|(structured-insert-right)> est d'insérer un nouvel argument au
  tag à gauche ou à droite (si autorisé).\ 

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|>|<cell|<value|cursor>>|<cell|>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|>|<cell|<value|cursor>>|<cell|>>|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>>
    </equation*>
  <|big-figure>
    <label|matrix-insert-fig>Supposons que le curseur soit à la position
    <value|cursor> dans la matrice à l'extrème gauche. Alors les quatres
    matrices suivantes correspondent à l'insertion d'une nouvelle colonne à
    gauche<nbsp>(<shortcut|(structured-insert-left)>) ou a
    droite<nbsp>(<shortcut|(structured-insert-right)>), ou d'une nouvelle
    ligne au dessus<nbsp>(<shortcut|(structured-insert-up)>) ou en
    dessous<nbsp>(<shortcut|(structured-insert-down)>).
  </big-figure>

  <\big-figure|<tree|a|b|c<value|cursor>|d><space|3em><space|3em><tree|a|b|<value|cursor>|c|d><space|3em><tree|a|b|c|<value|cursor>|d><space|3em><tree|a|b|<tree|<value|cursor>|c>|d><space|3em><tree|a|b|<tree|c|<value|cursor>>|d>>
    <label|tree-insert-fig>Supposons que le curseur soit à la position
    <value|cursor> dans l'arbre à l'extrème gauche. Alors les quatres arbres
    suivants correspondent respectivement à l'insertion d'un nouveau noeud à
    gauche<nbsp>(<shortcut|(structured-insert-left)>),
    à<nbsp>droite<nbsp>(<shortcut|(structured-insert-right)>), au
    dessus<nbsp>(<shortcut|(structured-insert-up)>) ou en
    dessous<nbsp>(<shortcut|(structured-insert-down)>).
  </big-figure>

  De la même façon, dans le cas des matrices, les touches
  <shortcut|(structured-insert-start)> et <shortcut|(structured-insert-end)>
  peuvent être utilisée pour l'insertion d'une nouvelle colonne et première
  ou en dernière position new, <abbr|resp.>
  <shortcut|(structured-insert-top)> et <shortcut|(structured-insert-bottom)>
  jouent un rôle identique pour les lignes. Les touches
  <shortcut|(structured-remove-left)> et <shortcut|(structured-remove-right)>
  sont définies pour la <em|destruction structurée ><abbr|vers l'avant resp.>
  vers l'arrière. Dans le cas des matrices, cela entraine la destruction de
  la colonne avant resp. sous le curseur (see
  figure<nbsp><reference|matrix-remove-fig>). Pour détruire l'environment
  englobant, vous pouvez utiliser <shortcut|(remove-structure-upwards)> et
  <shortcut|(remove-structure-upwards)>.

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>c>>|<row|<cell|d>|<cell|f>>>>><space|2em>b<value|cursor>
    </equation*>
  <|big-figure>
    <label|matrix-remove-fig>Supposons que le curseur soit à la position
    <value|cursor> dans la matrice à l'extrème gauche. Alors la pression des
    touches <shortcut|(structured-remove-left)> et
    \ <shortcut|(structured-remove-right)> donne respectivement les deux
    matrices suivantes. Presser l'une des touches
    <shortcut|(remove-structure-upwards)> ou
    <shortcut|(remove-structure-upwards)> remplace la matrice avec le contenu
    de la cellule dans laquelle vous êtes, laissant le curseur à droite de
    <math|b>.
  </big-figure>

  <tmdoc-copyright|1998--2005|Joris van der Hoeven|Denis Raux>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>