<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Liste standard >

  Le liste standard in <TeXmacs> sono definite in <tmdtd|std-list>. Le liste
  non numerate sono:

  <\description>
    <expand|item*|<markup|itemize>>Il tag prima di ciascun elemento dipende
    dalla profondità di annidamento

    <expand|item*|<markup|itemize-minus>>Usa <with|mode|math|-> per il tag.

    <expand|item*|<markup|itemize-dot>>Usa <with|mode|math|\<bullet\>> per il
    tag.

    <expand|item*|<markup|itemize-arrow>>Usa <with|mode|math|\<rightarrow\>>
    per il tag.
  </description>

  Le liste numerate corrispondono ai seguenti ambienti:

  <\description>
    <expand|item*|<markup|enumerate>>Il tipo di numero prima di ciascun
    elemento dipende dalla profondità di annidamento.

    <expand|item*|<markup|enumerate-numeric>>Numera gli elementi con 1, 2, 3,
    <abbr|ecc.>

    <expand|item*|<markup|enumerate-roman>>Numera gli elementi con i, ii,
    iii, <abbr|ecc.>

    <expand|item*|<markup|enumerate-Roman>>Numera gli elementi conI, II, III,
    <abbr|ecc.>

    <expand|item*|<markup|enumerate-alpha>>Numera gli elementi con a), b),
    c), <abbr|ecc.>

    <expand|item*|<markup|enumerate-Alpha>>Numera gli elementi con A, B, C,
    <abbr|ecc.>
  </description>

  I seguenti ambienti possono essere utilizzati per le liste descrittive.

  <\description>
    <expand|item*|<markup|description>>L'ambiente per le liste descrittive di
    default (solitamente <markup|description-compact>).

    <expand|item*|<markup|description-compact>>Allinea a sinistra gli
    elementi e pone le loro descrizioni immediatamente dopo di essi.

    <expand|item*|<markup|description-dash>>Analogo a
    <markup|description-compact>, ma usa  per separare ciascun elemento
    dalla relativa descrizione.

    <expand|item*|<markup|description-align>>Allinea a sinistra le
    descrizioni, mentre allinea a destra gli elementi.

    <expand|item*|<markup|description-long>>Pone gli elementi e le loro
    descrizioni su linee distinte.
  </description>

  I nuovi elementi in una lista sono indicati con il <markup|item>, o con il
  tag unario <markup|item*> nel caso di descrizioni. Gli sviluppatori
  troveranno qualche macro addizionale, ma instabile, in <tmdtd|std-list> per
  definire altre strutture di liste.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia
  Gecchelin|Andrea Centomo>

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
    <associate|language|italian>
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
