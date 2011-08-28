<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Étude d'un exemple>

  Avant de créer vos propres fichiers de style, il est utile d'étudier
  certains fichiers de style standards. Par exemple, vous pouvez charger le
  fichier <verbatim|book.ts> avec <apply|menu|File|Load> (pas besoin de
  chemin ici, puisque le répertoire de style est inclus dans le chemin par
  défaut des fichiers).

  Après avoir chargé <verbatim|book.ts>, vous verrez de nombreuses
  déclarations de fonction et d'environnement (ces déclarations sont
  visibles, car les fichiers de style sont écrits en <space|0.2spc>mode
  préambule<space|0.2spc> - voir <apply|menu|Document|Mode de préambule>).
  D'autres déclarations sont contenues dans les fichiers <verbatim|basic.ts>,
  <verbatim|list.ts>, <verbatim|theorem.ts> et <verbatim|program.ts>, sur
  lesquels <verbatim|book.ts> est basé. Ces fichiers contiennent
  respectivement les environnements de base, de listes, de théorèmes et de
  programmation.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Fichier>|<with|font
      family|<quote|ss>|Charger>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Mode de préambule>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
