<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Problèmes de mise en page>

  En général, <apply|TeXmacs> se charge de la mise en page du texte. Par
  conséquent, nous vous recommandons de ne pas la faire vous-même, bien que
  cela ne soit pas impossible. Par exemple, vous ne devez pas insérer
  d'espaces ou de lignes vierges supplémentaires entre les mots ou les
  lignes. Ces espaces verticaux ou horizontaux doivent être insérés
  explicitement avec <apply|menu|Insérer|Espace>. Cela vous permettra de
  gérer votre document de manière plus souple lors de changements mineurs
  affectant les sauts de page ou de ligne, ou de changements majeurs comme la
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
  théorèmes, contrairement à \ <apply|TeX>).

  En ce qui concerne le paragraphe, l'utilisateur peut indiquer le style du
  paragraphe (justifié, cadré à gauche, centré, cadré à droite), les marges
  et l'indentation à gauche (resp. à droite) de la première (resp. dernière)
  ligne. On peut aussi contrôler l'espace entre paragraphes et lignes d'un
  même paragraphe.

  Vous pouvez indiquer la mise en page avec <apply|menu|Document|Page>. Tout
  d'abord, choisissez la façon dont les pages sont affichées sur l'écran ; si
  vous choisissez papier comme type de page dans
  <apply|menu|Document|Page|Type>, les sauts de page seront visibles. Par
  défaut, le type de page est papyrus, ce qui évite de voir les sauts de
  page lors de la création du document. Le type de page automatique
  correspond à une taille de papier identique à la taille de la fenêtre. Les
  marges de la page et la largeur du texte sont spécifiés avec
  <apply|menu|Document|Page|Mise en page>. Il est souvent pratique de réduire
  les marges de la page lorsqu'on la visualise ; on peut le faire avec
  <apply|menu|Document|Page|Apparence à l'écran>.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Espace>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>|<with|font family|<quote|ss>|Type>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>|<with|font family|<quote|ss>|Mise en
      page>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>|<with|font family|<quote|ss>|Apparence à
      l'écran>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
