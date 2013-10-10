<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Listes standards>

  Les listes standards de <TeXmacs> sont définies dans <tmdtd|std-list>. Les
  listes non numérotées sont :

  <\explain|<markup|itemize>>
    La marque devant chaque article dépend de la profondeur de l'imbrication.
  </explain>

  <\explain|<markup|itemize-minus>>
    La marque est un tiret.
  </explain>

  <\explain|<markup|itemize-dot>>
    La marque est une puce.
  </explain>

  <\explain|<markup|itemize-arrow>>
    La marque est une flèche.
  </explain>

  Les listes numérotées correspondent aux environnements suivants :

  <\explain|<markup|enumerate>>
    Le style des nombres devant chaque article dépend de la profondeur de
    l'imbrication.
  </explain>

  <\explain|<markup|enumerate-numeric>>
    Numérotation de style 1, 2, 3, <abbr|etc...>
  </explain>

  <\explain|<markup|enumerate-roman>>
    Numérotation de style i, ii, iii, <abbr|etc...>
  </explain>

  <\explain|<markup|enumerate-Roman>>
    Numérotation de style I, II, III, <abbr|etc...>
  </explain>

  <\explain|<markup|enumerate-alpha>>
    Numérotation de style a), b), c), <abbr|etc.>..
  </explain>

  <\explain|<markup|enumerate-Alpha>>
    Numérotation de style A, B, C, <abbr|etc.>..
  </explain>

  Les environnements suivants sont utilisés pour les listes descriptives.

  <\explain|<markup|description>>
    Environnement par défaut pour les descriptions (en général
    <markup|description-compact>).
  </explain>

  <\explain|<markup|description-compact>>
    Aligne les articles à gauche et les fait suivre immédiatement de leur
    description.
  </explain>

  <\explain|<markup|description-dash>>
    Identique à <markup|description-compact>, mais utilise un tiret pour
    séparer chaque article de sa description.
  </explain>

  <\explain|<markup|description-align>>
    Aligne les articles à droite et leur description à gauche.
  </explain>

  <\explain|<markup|description-long>>
    Les articles et leur description sont affichés sur deux lignes
    différentes.
  </explain>

  Un nouvel article dans une liste est repéré par les balises <markup|item>,
  ou <markup|item*> dans le cas de descriptions. Les développeurs trouveront
  quelques autres macros instables dans <tmdtd|std-list> qui leur permettront
  de définir d'autres types de listes.

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