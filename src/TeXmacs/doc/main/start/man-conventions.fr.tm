<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conventions typographiques>

  Dans le guide <TeXmacs>, les accès aux menus sont indiqués par une police
  <em|sans serif> : <apply|menu|Document>, <apply|menu|File|Load> ou
  \ <apply|menu|Format|Police|Forme|Italique>. \ Les frappe-clavier sont
  indiquées par une police <em|typewriter> dans un cadre : <key|C-s>. Dans
  les menus, les raccourcis clavier, quand ils existent, sont situés à
  l'extrême droite. Voici les abréviations utilisées pour les raccourcis
  clavier :

  <\description>
    <expand|item*|<key|S->>Combinaison avec touche majuscule temporaire.

    <expand|item*|<key|C->>Combinaison avec touche ctrl.

    <expand|item*|<verbatim|><key|A->>Combinaison avec touche alt.

    <expand|item*|<key|M->>Combinaison avec touche meta.

    <expand|item*|<key|H->>Combinaison avec touche hyper.
  </description>

  Par exemple, <key|A-C-b> correspond à <key|alt-ctrl-b>. Les espaces dans
  les raccourcis clavier indiquent des frappe-clavier successives. Par
  exemple, <expand|kbd-table|N b> correspond à <key|alt-t> <key|N> <key|b>.

  Les touches <key|alt>, <key|meta> et <key|hyper> ne sont pas disponibles
  sur tous les claviers. Sur les PC récents, la touche <key|meta> est souvent
  remplacée par la touche <key|windows>. Vous pouvez utiliser <key|escape> au
  lieu de <key|M->, <key|escape escape> au lieu de <key|A-> et <key|F5>,
  <key|escape escape escape> ou <key|A-C-> au lieu de <key|H->, si votre
  clavier ne comporte pas ces touches spéciales. Par exemple, la combinaison
  <key|escape w> est équivalente à <key|A-w>. Vous pouvez aussi
  <apply|hyper-link|configurer les touches
  spéciales|../config/man-config-kbd-modkeys.fr.tm> de façon à avoir accès à
  tous les raccourcis clavier fournis par <TeXmacs>.

  Notez que, dans <TeXmacs>, les menus et le clavier sont <em|contextuels>,
  c'est-à-dire qu'ils dépendent du mode actif (mode texte ou
  <space|0.2spc>mode math<space|0.2spc>, par exemple), du langage utilisé
  et de la position du curseur dans le document. En mode math, par exemple,
  il existe des raccourcis clavier spéciaux pour saisir facilement des
  formules mathématiques ; ces raccourcis sont évidemment inopérants en mode
  texte.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Michèle Garoche>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Document>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Fichier>|<with|font
      family|<quote|ss>|Charger>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Format>|<with|font
      family|<quote|ss>|Police>|<with|font
      family|<quote|ss>|Forme>|<with|font
      family|<quote|ss>|Italique>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
