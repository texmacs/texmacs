<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Environnements pour les objets flottants>

  Le d.t.d. <tmdtd|env-float> définit des balises pour les objets flottants.
  La balise suivante est la seule de haut niveau :

  <\description>
    <expand|item*|<markup|footnote>>Crée une note en bas de page.
  </description>

  Les balises de bas niveau suivantes peuvent être utilisées pour définir des
  environnements de haut niveau pour les figures ou les tableaux, tels
  <markup|big-figure>, <markup|small-figure>, <markup|big-table> et
  <markup|small-table> :

  <\description>
    <expand|item*|<markup|small-figure*>>Macro d'affichage de petite figure.
    Les arguments sont : un nom court (tels
    <space|0.2spc>figure<space|0.2spc> ou
    <space|0.2spc>tableau<space|0.2spc>) pour la liste des figures, son nom
    réel (tels <space|0.2spc>Figure 2.3<space|0.2spc> ou
    <space|0.2spc>Tableau 5<space|0.2spc>), la figure elle-même et une
    légende.

    <expand|item*|<markup|big-figure*>>Variante de <markup|small-figure*>
    pour afficher une grande figure.
  </description>

  Les balises suivantes peuvent être utilisées pour personnaliser l'apparence
  du texte autour des figures, tableaux et notes en bas de page :

  <\description>
    <expand|item*|<markup|figurename>>Macro qui gère l'apparence du texte
    <space|0.2spc>Figure<space|0.2spc>. Par défaut, on utilise un style
    gras.

    <expand|item*|<markup|figuresep>>Séparateur entre la figure suivie de son
    numéro et la légende. Par défaut, c'est un point suivi d'une espace.

    <expand|item*|<markup|footnotesep>>Séparateur entre le numéro de la note
    en bas de page et son texte. Par défaut, c'est un point suivi d'une
    espace.
  </description>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-float>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|footnote>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-figure>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-table>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-table>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure*>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-figure*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|figurename>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|figuresep>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|footnotesep>>|<pageref|idx-12>>
    </associate>
  </collection>
</auxiliary>
