<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Mouvement structuré du curseur>

  <TeXmacs> possède trois mécanismes principaux pour le \S mouvement du
  curseur structuré \T :

  <\enumerate>
    <item>Parmi toute la structure du document.

    <item>Parmi les tags similaires à celui le plus intérieur.

    <item>Déplacement à l'intérieur du tag le plus intérieur.
  </enumerate>

  La plupart des raccourcis pour le déplacement structuré du curseur peut
  être utilisée en combinaison avec la touche<nbsp><prefix|S-> afin de
  sélectionner le texte durant le déplacement.

  <todo|personnaliser le comportement>

  <paragraph*|Parcours structuré du document>

  Les touches <shortcut|(traverse-left)>, <shortcut|(traverse-right)>,
  <shortcut|(traverse-up)> et <shortcut|(traverse-down)> sont utilisées pour
  la traversée structurée de tout le document. En mode texte,
  <shortcut|(traverse-left)> et <shortcut|(traverse-right)> permettent de se
  déplacer de mot en mot, tandis que \ <shortcut|(traverse-up)> et
  <shortcut|(traverse-down)> permettent un déplacement de paragraphe en
  paragraphe.

  En présence d'autres tags, le touches \ <shortcut|(traverse-left)> et
  <shortcut|(traverse-right)> permettent d'accéder à toutes les positions du
  curseur à l'intérieur du document; Toutefois, le déplacement mot à mot est
  conservé en mode texte. Le comportement des touches
  <shortcut|(traverse-up)> et <shortcut|(traverse-down)> est plus dépendant
  du contexte. A l'intérieur des matrices, elles permettent typiquement de se
  déplacer de ligne à ligne.

  <paragraph*|Parcours structuré des tags>

  Ce type de mouvement permet de parcourir rapidement les tags
  <em|similaires> au \ tag le plus intérieur. Les touches
  <shortcut|(traverse-previous)> et <shortcut|(traverse-next)> permettent de
  se déplacer vers le suivant ou le précédent, alors que
  <shortcut|(traverse-first)> et <shortcut|(traverse-last)> permettent
  d'atteindre directement le premier ou le dernier tag similaire.

  Par exemple, lorsque vous être dans une section de titre, vous pouvez
  rejoindre la section précédente (qui peut aussi être le titre d'une
  sous-section ou un chapitre, par exemple) en utilisant
  <shortcut|(traverse-previous)>. Remarquez que vous pouvez utiliser
  <key|C-Ÿ> pour sauter au titre de la section précédente.

  <paragraph*|Déplacement à l'intérieur d'un tag>

  Il est aussi possible de se déplacer dans le tag le plus intérieur sans le
  quitter. Les raccourcis <shortcut|(structured-left)>,
  <shortcut|(structured-right)>, <shortcut|(structured-start)> et
  \ <shortcut|(structured-end)> fournissent un moyen pour aller à l'argument
  précédent, au suivant, au premier ou au dernier. En outre, les raccourcis
  <shortcut|(structured-exit-left)> et <shortcut|(structured-exit-right)>
  peuvent être utilisés pour quitter le tag le plus intérieur vers la gauche
  ou vers la droite.

  Ce comportement par défaut peut être modifié dans des contextes
  particuliers. Par exemple, à l'intérieur de tableaux ou d'arbres, \ ils
  correspondent plutôt à des mouvements de cellule à cellule ou de noeud à
  noeud. En plus, des mouvements verticaux peuvent être effectués à l'aide de
  <shortcut|(structured-up)>, <shortcut|(structured-down)>,
  <shortcut|(structured-top)> et<nbsp><shortcut|(structured-bottom)>.

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