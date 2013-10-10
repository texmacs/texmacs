<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Environnements théorèmes>

  Le d.t.d. <tmdtd|env-theorem> \ définit les balises d'affichage des
  environnements théorèmes. Les balises principales sont les suivantes :

  <\explain|<markup|render-theorem>>
    Macro d'affichage des environnements théorèmes. Le premier argument donne
    le nom du théorème, tel \S<space|0.2spc>Théorème 1.2<space|0.2spc>\T et
    le second argument est le corps du théorème. Cet environnement est
    utilisé par les environnements définis avec <markup|new-theorem>.
  </explain>

  <\explain|<markup|render-remark>>
    Identique à <markup|render-theorem> pour les environnements remarques.
  </explain>

  <\explain|<markup|render-exercise>>
    Identique à <markup|render-theorem> pour les environnements exercices.
  </explain>

  <\explain|<markup|render-proof>>
    Identique à <markup|render-theorem> pour les démonstrations. Cet
    environnement est principalement utilisé pour personnaliser le nom d'une
    démonstration, comme dans \S<space|0.2spc>Fin de la démonstration du
    théorème 1.2<space|0.2spc>\T.\ 
  </explain>

  <\explain|<markup|dueto>>
    Environnement qui peut être utilisé pour indiquer des auteurs d'un
    théorème.
  </explain>

  <\explain|<markup|corollary*>>
    Pour les corollaires non numérotés. Cet environnement se base sur
    <markup|render-theorem>.
  </explain>

  <\explain|<markup|proof>>
    Pour la démonstration des théorèmes. Cet environnement se base sur
    <markup|render-proof>.
  </explain>

  Les balises suivantes peuvent être utilisées pour personnaliser les
  environnements.

  <\explain|<markup|theorem-name>>
    Macro qui gère l'apparence des noms des environnements théorèmes et
    remarques. La plupart utilisent un style gras ou des petites majuscules.
  </explain>

  <\explain|<markup|exercise-name>>
    Identique à <markup|theorem-name> pour les exercices.
  </explain>

  <\explain|<markup|theorem-sep>>
    Séparateur entre le nom d'un environnement théorème ou remarque et son
    corps. Par défaut, il s'agit d'un point suivi d'une espace.
  </explain>

  <\explain|<markup|exercise-sep>>
    Identique à <markup|theorem-sep> pour les exercices.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>