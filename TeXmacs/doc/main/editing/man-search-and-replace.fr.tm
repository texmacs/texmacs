<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Rechercher et remplacer>

  On commence la recherche de texte avec <key|C-s> ou
  <apply|menu|Edit|Search>. Pendant la recherche, la <space|0.2spc>chaîne de
  recherche<space|0.2spc> est affichée dans le pied de page à gauche. Tout
  caractère saisi est ajouté à la chaîne de recherche et son occurrence
  suivante est entourée d'un cadre rouge. Si vous appuyez sur <key|C-s> une
  seconde fois pendant la recherche, le programme recherchera une seconde
  occurrence de la chaîne. Un bip vous signale qu'il n'y a plus d'autres
  occurrences de la chaîne à rechercher dans le document ; si vous appuyez
  encore une fois sur <key|C-s> à ce moment-là, la recherche reprendra au
  début du document. Vous pouvez appuyez sur <key|retour arrière> pour
  annuler les frappes et les déplacements effectués pendant la recherche.

  En général, la recherche de texte s'effectue vers le bas à partir de la
  position du curseur. Vous pouvez aussi rechercher vers le haut avec
  <key|C-r>. La recherche est restreinte au mode et à la langue actifs au
  début de la recherche. En d'autres termes, si vous cherchez
  <with|mode|math|x> en mode math, la recherche ne trouvera pas les éventuels
  x en mode texte. Actuellement, la chaîne de recherche ne peut contenir que
  du texte ordinaire, aucun symbole mathématique ou texte structuré n'est
  pris en compte.

  Pour effectuer un remplacement, utilisez \ <key|C-=> or
  <apply|menu|Edit|Replace>. Vous devrez alors fournir une chaîne de
  recherche et une chaîne de remplacement. À chaque occurrence de la chaîne
  de recherche, vous devrez choisir entre remplacer la chaîne (y), ne pas la
  remplacer (n) ou remplacer la chaîne et toutes les occurrences suivantes
  (a). Comme dans le cas de la recherche, le remplacement s'effectue sur la
  base d'un mode et d'une langue donnés.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Chercher>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Remplacer>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
