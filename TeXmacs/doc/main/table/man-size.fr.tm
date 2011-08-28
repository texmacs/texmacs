<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Choix de la taille du tableau et des cellules>

  Vous pouvez définir la largeur ou la hauteur d'une cellule avec
  <apply|menu|Table|Cell width|Set width> resp. <apply|menu|Table|Cell
  height|Set height>. En fait, la hauteur (ou largeur) spécifiée peut être
  prise en compte de trois façons différentes :\ 

  <\description>
    <expand|item*|Mode minimum.>La largeur réelle de la cellule est le
    minimum entre la largeur spécifiée et la largeur du rectangle intérieur
    de la cellule.

    <expand|item*|Mode exact.>La largeur de la cellule est celle spécifiée.

    <expand|item*|Mode maximum.>La largeur réelle de la cellule est le
    maximum entre la largeur spécifiée et la largeur du rectangle intérieur
    de la cellule.
  </description>

  La largeur de la bordure et l'espace intercellulaire (voir explication plus
  bas) sont pris en compte dans le calcul de la taille du rectangle intérieur
  de la cellule.

  Vous pouvez aussi définir la largeur et la hauteur du tableau avec
  <apply|menu|Table|Special table properties>. En particulier, vous pouvez
  spécifier que le tableau doit s'étendre sur la largeur d'un paragraphe.
  Quand vous définissez une largeur (ou une hauteur) de tableau, vous pouvez
  spécifier la façon dont l'espace inutilisé sera réparti entre les cellules
  avec <apply|menu|Table|Special cell properties|Distribute unused space>.
  Par défaut, l'espace inutilisé est réparti de façon égale entre toutes les
  cellules.

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
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Largeur de la cellule>|<with|font
      family|<quote|ss>|Changer largeur>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Hauteur de la cellule>|<with|font
      family|<quote|ss>|Changer hauteur>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Propriétés spéciales du tableau>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Propriétés spéciales de la cellule>|<with|font
      family|<quote|ss>|Distribuer espace non utilisé>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
