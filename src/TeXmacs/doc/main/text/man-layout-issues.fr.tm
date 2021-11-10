<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Problèmes de mise en page>

  En général, <TeXmacs> se charge de la mise en page du texte. Par
  conséquent, nous vous recommandons de ne pas la faire vous-même, bien que
  cela ne soit pas impossible. Par exemple, vous ne devez pas insérer
  d'espaces ou de lignes vierges supplémentaires entre les mots ou les
  lignes. Ces espaces verticaux ou horizontaux doivent être insérés
  explicitement avec <menu|Insérer|Espace>. Cela vous permettra de gérer
  votre document de manière plus souple lors de changements mineurs affectant
  les sauts de page ou de ligne, ou de changements majeurs comme la
  modification du style du document.

  On a implémenté différents types d'espaces explicites. Tout d'abord, on
  peut insérer des espaces fixes ; leur largeur et leur hauteur sont fixes.
  Les espaces horizontaux ont une hauteur nulle et sont soit étirables, soit
  non étirables. La longueur des espaces étirables dépend de la césure du
  paragraphe. De plus, il est possible d'insérer des tabulations. Les espaces
  verticaux peuvent être insérés au début ou à la fin d'un paragraphe.
  L'espace vertical réel entre deux paragraphes correspond au maximum entre
  l'espace vertical après le premier paragraphe et l'espace vertical avant le
  second (ceci permet d'éviter un espace disproportionné entre deux
  théorèmes, contrairement à \ <TeX>).

  En ce qui concerne le paragraphe, l'utilisateur peut indiquer le style du
  paragraphe (justifié, cadré à gauche, centré, cadré à droite), les marges
  et l'indentation à gauche (resp. à droite) de la première (resp. dernière)
  ligne. On peut aussi contrôler l'espace entre paragraphes et lignes d'un
  même paragraphe.

  Vous pouvez indiquer la mise en page avec <menu|Document|Page>. Tout
  d'abord, choisissez la façon dont les pages sont affichées sur l'écran ; si
  vous choisissez \Spapier\T comme type de page dans
  <menu|Document|Page|Type>, les sauts de page seront visibles. Par défaut,
  le type de page est \Spapyrus\T, ce qui évite de voir les sauts de page
  lors de la création du document. Le type de page \Sautomatique\T correspond
  à une taille de papier identique à la taille de la fenêtre. Les marges de
  la page et la largeur du texte sont spécifiés avec <menu|Document|Page|Mise
  en page>. Il est souvent pratique de réduire les marges de la page
  lorsqu'on la visualise ; on peut le faire avec
  <menu|Document|Page|Apparence à l'écran>.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Michèle Garoche, Daouda
  Niang Diatta>

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