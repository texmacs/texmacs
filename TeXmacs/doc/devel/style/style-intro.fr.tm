<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Fichiers de style <TeXmacs>>

  Une des caractéristiques fondamentales de <TeXmacs> est la possibilité
  d'écrire des fichiers de style personnalisés et des modules. L'utilité des
  fichiers de style est multiple :

  <\itemize>
    <item>Permettre l'abstraction d'éléments répétitifs dans les textes :
    sections, théorèmes, énumérations, etc...

    <item>Créer un mécanisme de structuration du texte. Par exemple, on peut
    indiquer qu'une partie du texte est une abréviation, une citation ou est
    <space|0.2spc>importante<space|0.2spc>.

    <item>Écrire des documents de qualité professionnelle. C'est ce à quoi
    servent les styles de documents standards ; ils ont été écrits avec
    beaucoup de soin par des personnes versées dans l'art de la typographie
    et l'esthétisme.
  </itemize>

  Il est possible d'associer à un document un ou plusieurs styles de
  document, qu'ils soient standards ou personnalisés. Le style principal d'un
  document est défini avec <apply|menu|Document|Style>. D'autres styles
  peuvent être ajoutés avec <apply|menu|Document|Use package>.

  Pour l'éditeur, chaque style correspond à un fichier <verbatim|.ts>. Les
  fichiers correspondant à chaque style sont traités comme des documents
  ordinaires, mais, à la fin, l'éditeur ne conserve que l'environnement
  final, qui sert alors d'environnement initial au document principal. Plus
  précisément, les fichiers de style sont traités dans l'ordre, ainsi que
  leurs propres fichiers de style, de façon récursive.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Style>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Utiliser paquetage>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
