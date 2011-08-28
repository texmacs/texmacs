<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Conventions typographiques>

  <paragraph|Accés aux menus>

  Dans le guide <TeXmacs>, les accès aux menus sont indiqués par une police
  <em|sans serif> : <menu|Document>, <menu|File|Load> ou
  \ <menu|Format|Police|Forme|Italique>.

  <paragraph|Modificateurs clavier>

  Voici les abréviations utilisées pour les raccourcis clavier dans <TeXmacs>
  :

  <\description>
    <item*|<prefix|S->>Combinaison avec touche majuscule temporaire.

    <item*|<prefix|C->>Combinaison avec touche ctrl.

    <item*|<prefix|A->>Combinaison avec touche alt.

    <item*|<prefix|M->>Combinaison avec touche meta.
  </description>

  Par exemple, <key*|M-S-x> correspond à l'action consistant à appuyer
  simultanément sur les touches <prefix|M->, <prefix|S-> et <key|x>.

  <paragraph|Raccourcis clavier>

  Des raccourcis clavier plus complexes sont obtenus en appuyant sur
  plusieurs touche à la suite. Par exemple, le raccourci <key*|-\<gtr\>>
  correspond à appuyer d'abord sur la touche <key*|-> puis sur la touche
  <key*|\<gtr\>>. Dans une formule mathématique, ce racourci insert la flèche
  <math|\<rightarrow\>>. De même, le raccourci clavier <key*|C-x><key*|C-f>
  consiste à appuyer simultanément, sur les touches <key*|C-> et <key*|x>
  d'abord, puis sur les touches <key*|C-> et <key*|f> ensuite. En mode
  <name|Emacs>, ce raccourci clavier vous permettra d'ouvrir un nouveau
  fichier.

  Certains préfixes clavier communs sont détaillés dans la section sur
  <hlink|les règles générales du clavier|../text/keyboard/man-general-rules.fr.tm>.
  Dans les cas où les raccourcis clavier <TeXmacs> sont remplacés par les
  raccourcis clavier du système d'exploitation, nous remarquons que les
  <hlink|équivalents pour les modificateurs
  clavier|../config/man-config-keyboard.fr.tm#kbd-escape-table> peuvent être
  obtenus en utilisant la touche <key*|escape>. Par exemple, <key*|escape>
  est équivalente à <key*|M-> et <key*|escape><key*|escape> est équivalente à
  <key*|A->.

  Notez que, dans <TeXmacs>, les menus et le clavier sont <em|contextuels>,
  c'est-à-dire qu'ils dépendent du mode actif (mode texte ou
  \S<space|0.2spc>mode math<space|0.2spc>\T, par exemple), du langage utilisé
  et de la position du curseur dans le document. En mode math, par exemple,
  il existe des raccourcis clavier spéciaux pour saisir facilement des
  formules mathématiques ; ces raccourcis sont évidemment inopérants en mode
  texte.

  <paragraph|Touches spéciales>

  Sur certaines plateformes, certaines touches spéciales telles que la touche
  <key*|Return> sont présentées par des glyphes courts. Le tableau suivant
  présentes ces touches spéciales et leur signification.

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|3|3|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-row-span|1>|<cwith|1|-1|1|-1|cell-col-span|1>|<cwith|7|11|1|1|cell-lborder|1ln>|<cwith|7|11|1|2|cell-halign|l>|<cwith|7|11|1|2|cell-row-span|1>|<cwith|7|11|1|2|cell-col-span|1>|<table|<row|<cell|Touche>|<cell|Signification>|<cell|Touche>|<cell|Signification>>|<row|<cell|<key*|S->>|<cell|Modificateur
  touche majuscule temporaire>|<cell|<key*|left>>|<cell|Cuuseur vers la
  gauche>>|<row|<cell|<key*|capslock>>|<cell|Verrouillage des
  majuscules>|<cell|<key*|right>>|<cell|Curseur vers la
  droite>>|<row|<cell|<key*|C->>|<cell|Modificateur touche
  ctrl>|<cell|<key*|up>>|<cell|Cursor vers le
  haut>>|<row|<cell|<key*|A->>|<cell|Modificateur touche
  alt>|<cell|<key*|down>>|<cell|Curseur vers le
  bas>>|<row|<cell|<key*|M->>|<cell|Modificateur touche
  meta>|<cell|<key*|home>>|<cell|Racine>>|<row|<cell|<key*|return>>|<cell|Return>|<cell|<key*|end>>|<cell|Fin>>|<row|<cell|<key*|delete>>|<cell|Supprimer
  vers l'avant>|<cell|<key*|pageup>>|<cell|Haut de
  page>>|<row|<cell|<key*|backspace>>|<cell|Backspace>|<cell|<key*|pagedown>>|<cell|Bas
  de page>>|<row|<cell|<key*|escape>>|<cell|Escape>|<cell|<key*|space>>|<cell|Espace>>|<row|<cell|<key*|tab>>|<cell|Tab>|<cell|>|<cell|>>>>>>
    Touches spéciales.
  </big-table>

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