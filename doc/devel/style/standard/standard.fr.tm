<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Styles et modules standards <TeXmacs>>

  Actuellement, les styles de documents standards suivants ont été
  implémentés :

  <\itemize>
    <item>Livre,

    <item>Article,

    <item>Lettre,

    <item>Séminaire (pour les transparents).
  </itemize>

  Chacun de ces styles exporte un certain nombre de fonctions et
  d'environnement standards, dont la liste figure ci-dessous. Tout futur
  style de document standard devra géré au moins les commandes et
  environnements décrits ci-dessus. Nous vous suggérons d'en faire autant si
  vous écrivez vos propres fichiers de style.

  <\itemize>
    <item>Commandes de sections.

    <item>Environnements de liste et d'énumération.

    <item>Environnements d'équations.

    <item>Environnements de théorèmes.

    <item>Environnements de programmation.
  </itemize>

  Vous noterez que les environnements de théorèmes ne sont pas standards dans
  <apply|LaTeX>, ce qui est la principale source d'incompatibilité. On peut
  ajouter de nouveaux  théorèmes<space|0.2spc> avec la commande
  <verbatim|newtheorem>. On peut aussi ajouter de nouvelles
  <space|0.2spc>remarques<space|0.2spc> avec la commande
  <verbatim|newremark> ; les <space|0.2spc>remarques<space|0.2spc> sont
  différentes des <space|0.2spc>théorèmes<space|0.2spc> en ce sens que leur
  corps n'est généralement pas écrit avec une police grasse.

  Les environnements de programmation ne sont pas non plus gérés par
  <apply|LaTeX>. Ces environnements sont actuellement en cours de
  développement.

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
