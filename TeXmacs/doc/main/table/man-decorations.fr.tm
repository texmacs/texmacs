<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Bordures, espace intercellulaire et couleur de fond>

  Vous pouvez définir la largeur des bordures et de l'espace intercellulaire
  dans toutes les directions (voir <apply|menu|Table|Cell border>). Il existe
  aussi des raccourcis clavier de la forme <key|table b
 ><render-key|<with|mode|math|x>> et <key|table p><render-key|<with|mode|math|x>>.

  La largeur par défaut des bordures de cellules en environnement bloc est de
  <verbatim|1ln>, soit la largeur d'une ligne dans la police active
  (identique à la largeur d'une barre de fraction). Cette largeur est
  utilisée à droite et en dessous de chaque cellule (sauf pour pour la
  première ligne et première colonne). L'espace intercellulaire horizontal
  par défaut est de <verbatim|1spc> : largeur d'une espace dans la police
  active. L'espace intercellulaire vertical par défaut est de <verbatim|1sep>
  : séparation minimale entre deux rectangles consécutifs.

  On peut appliquer une couleur de fond aux cellules avec
  <apply|menu|Table|Cell background color>.

  On peut aussi définir une bordure et un espacement intercellulaire pour le
  tableau avec <apply|menu|Table|Special table properties|Border>. Dans ce
  cas, l'espacement intercellulaire est appliqué à l'extérieur de la bordure.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Bordure de la cellule>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Couleur de la cellule>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Propriétés spéciales du tableau>|<with|font
      family|<quote|ss>|Bordure>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
