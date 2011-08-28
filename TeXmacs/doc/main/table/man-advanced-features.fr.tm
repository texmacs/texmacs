<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Autres caractéristiques>

  Dans les menus, vous trouverez d'autres propriétés applicables aux
  tableaux. Les voici résumées, très brièvement :

  <\itemize>
    <item>Changement de la <space|0.2spc>portée<space|0.2spc> d'une cellule
    de façon à ce qu'elle s'étende sur les cellules voisines à droite et en
    dessous.

    <item>Création de sous-tableaux à l'intérieur de cellules.

    <item>Changement de la hauteur et de la largeur d'un texte pour que les
    lignes de base correspondent.

    <item>Césure horizontale du contenu d'une cellule et césure vertical du
    tableau.

    <item>Regroupement de lignes et/ou colonnes, de façon à ce que les
    cellules regroupées fassent partie des bordures des autres cellules.

    <item>Désactivation de la table pour voir son <space|0.2spc>code
    source<space|0.2spc>.

    <item>Définition du <space|0.2spc>centre d'extension<space|0.2spc> du
    tableau. À partir de là, les propriétés de mise en page de la cellule
    concernée seront appliquées à toute nouvelle cellule créée autour de ce
    centre.

    <item>Spécification de la taille maximale et minimale d'un tableau, qui
    sera respectée lors de modifications ultérieures (ceci est très utile
    quand on crée des macros tableau).
  </itemize>

  À l'heure actuelle, tous les tableaux sont insérés dans un environnement de
  type <markup|tabular>, <markup|block>, <markup|matrix>, etc... Quand vous
  créez vos propres macros tableau, vous pouvez utiliser
  <apply|menu|Table|Special table properties|Extract format> pour extraire le
  format d'un tableau donné.

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
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|matrix>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Propriétés spéciales du tableau>|<with|font
      family|<quote|ss>|Extraire format>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
