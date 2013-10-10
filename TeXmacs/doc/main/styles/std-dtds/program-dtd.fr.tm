<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Balises spéciales pour les programmes et les sessions>

  Le d.t.d. <tmdtd|program> fournit principalement les environnement pour les
  sessions de calcul formel. Ce sont les suivants :

  <\explain|<markup|session>>
    Macro à trois arguments : le langage de calcul formel, le nom de la
    session et le corps de la session.
  </explain>

  <\explain|<markup|input>>
    Macro à deux arguments : une invite et l'entrée.
  </explain>

  <\explain|<markup|output>>
    Macro qui fournit le corps de la sortie en argument.
  </explain>

  En fait, ces environnements sont basés sur les environnements
  <markup|<em|lan>-session>, <markup|<em|lan>-input> et
  <markup|<em|lan>-output> pour chaque langage <verbatim|<em|lan>>.

  Le d.t.d. <tmdtd|program> définit aussi des balises d'affichage de
  programmes informatiques. Mais ces balises sont très instables. On a prévu
  de les remplacer par un jeu de balises beaucoup plus étendu. Les voici :

  <\explain|<markup|algorithm>>
    Macro à deux arguments : le nom de l'algorithme et l'algorithme lui-même,
    avec éventuellement ses spécifications.
  </explain>

  <\explain|<markup|body>>
    Corps de l'algorithme.
  </explain>

  <\explain|<markup|indent>>
    Pour indenter une partie de l'algorithme.
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