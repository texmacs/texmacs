<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Structure d'un texte>

  En général, les documents importants ont une structure. Ils sont organisés
  en chapitres, sections et sous-sections ; ils contiennent différentes
  sortes de texte, comme du texte ordinaire, des citations, des notes de bas
  de page, des théorèmes, etc... Après que vous avez choisi un
  <expand|def-index|style de document> dans <apply|menu|Document|Style>,
  <apply|TeXmacs> se charge de la mise en page, telles la numérotation des
  sections, pages et théorèmes, la typographie des citations, notes en bas de
  page et théorèmes.

  Il existe actuellement quatre styles de document : lettre, article, livre
  et séminaire. Le style séminaire sert à faire des transparents. Dès que
  vous avez sélectionné un style, vous pouvez organiser votre texte en
  sections (voir <apply|menu|Texte|Section>) et utiliser des
  <expand|def-index|environnements> spécifiques. Par exemple un théorème, une
  proposition, une remarque... (voir <apply|menu|Texte|Environnement>). Ou
  encore des listes ordinaires (voir <apply|menu|Texte|Liste>) ou des listes
  numérotées (voir <apply|menu|Texte|Énumeration>).

  Quand vous vous sentirez plus à l'aise avec <apply|TeXmacs>, vous pourrez
  ajouter de nouveaux environnements dans un fichier de style personnalisé.
  Supposons, par exemple, que vous faites de nombreuses citations et que vous
  voulez qu'elles apparaissent en italique avec des marges gauche et droite
  d'un centimètre. Au lieu de changer manuellement les propriétés du texte et
  du paragraphe à chaque fois que vous faites une citation, il vaut mieux
  créer un environnement citation. Cela vous permettra non seulement
  d'insérer plus vite une citation, mais aussi de changer systématiquement la
  mise en page de toutes vos citations dans le document en ne changeant que
  la définition de l'environnement citation. Vous vous trouverez dans ce cas,
  lorsque vous vous rendrez compte <with|font shape|italic|a posteriori>
  qu'il vaudrait mieux, par exemple, utiliser une police plus petite pour
  afficher les citations.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|style de document>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Style>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Section>>|<pageref|idx-3>>

      <tuple|<tuple|environnements>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Environnement>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Liste>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Énumeration>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
