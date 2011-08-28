<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Listes standards>

  Les listes standards de <TeXmacs> sont définies dans <tmdtd|std-list>. Les
  listes non numérotées sont :

  <\description>
    <expand|item*|<markup|itemize>>La marque devant chaque article dépend de
    la profondeur de l'imbrication.

    <expand|item*|<markup|itemize-minus>>La marque est un tiret.

    <expand|item*|<markup|itemize-dot>>La marque est une puce.

    <expand|item*|<markup|itemize-arrow>>La marque est une flèche.
  </description>

  Les listes numérotées correspondent aux environnements suivants :

  <\description>
    <expand|item*|<markup|enumerate>>Le style des nombres devant chaque
    article dépend de la profondeur de l'imbrication.

    <expand|item*|<markup|enumerate-numeric>>Numérotation de style 1, 2, 3,
    <abbr|etc...>

    <expand|item*|<markup|enumerate-roman>>Numérotation de style i, ii, iii,
    <abbr|etc...>

    <expand|item*|<markup|enumerate-Roman>>Numérotation de style I, II, III,
    <abbr|etc...>

    <expand|item*|<markup|enumerate-alpha>>Numérotation de style a), b), c),
    <abbr|etc.>..

    <expand|item*|<markup|enumerate-Alpha>>Numérotation de style A, B, C,
    <abbr|etc.>..
  </description>

  Les environnements suivants sont utilisés pour les listes descriptives.

  <\description>
    <expand|item*|<markup|description>>Environnement par défaut pour les
    descriptions (en général <markup|description-compact>).

    <expand|item*|<markup|description-compact>>Aligne les articles à gauche
    et les fait suivre immédiatement de leur description.

    <expand|item*|<markup|description-dash>>Identique à
    <markup|description-compact>, mais utilise un tiret pour séparer chaque
    article de sa description.

    <expand|item*|<markup|description-align>>Aligne les articles à droite et
    leur description à gauche.

    <expand|item*|<markup|description-long>>Les articles et leur description
    sont affichés sur deux lignes différentes.
  </description>

  Un nouvel article dans une liste est repéré par les balises <markup|item>,
  ou <markup|item*> dans le cas de descriptions. Les développeurs trouveront
  quelques autres macros instables dans <tmdtd|std-list> qui leur permettront
  de définir d'autres types de listes.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-list>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-minus>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-dot>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-arrow>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-numeric>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-roman>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-Roman>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-alpha>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-Alpha>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-dash>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-align>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-long>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|item>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|item*>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-list>>|<pageref|idx-21>>
    </associate>
  </collection>
</auxiliary>
