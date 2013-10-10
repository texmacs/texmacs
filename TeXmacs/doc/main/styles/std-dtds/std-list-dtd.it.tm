<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Liste standard >

  Le liste standard in <TeXmacs> sono definite in <tmdtd|std-list>. Le liste
  non numerate sono:

  <\explain|<markup|itemize>>
    Il tag prima di ciascun elemento dipende dalla profondità di annidamento
  </explain>

  <\explain|<markup|itemize-minus>>
    Usa <math|-> per il tag.
  </explain>

  <\explain|<markup|itemize-dot>>
    Usa <math|\<bullet\>> per il tag.
  </explain>

  <\explain|<markup|itemize-arrow>>
    Usa <math|\<rightarrow\>> per il tag.
  </explain>

  Le liste numerate corrispondono ai seguenti ambienti:

  <\explain|<markup|enumerate>>
    Il tipo di numero prima di ciascun elemento dipende dalla profondità di
    annidamento.
  </explain>

  <\explain|<markup|enumerate-numeric>>
    Numera gli elementi con 1, 2, 3, <abbr|ecc.>
  </explain>

  <\explain|<markup|enumerate-roman>>
    Numera gli elementi con i, ii, iii, <abbr|ecc.>
  </explain>

  <\explain|<markup|enumerate-Roman>>
    Numera gli elementi conI, II, III, <abbr|ecc.>
  </explain>

  <\explain|<markup|enumerate-alpha>>
    Numera gli elementi con a), b), c), <abbr|ecc.>
  </explain>

  <\explain|<markup|enumerate-Alpha>>
    Numera gli elementi con A, B, C, <abbr|ecc.>
  </explain>

  I seguenti ambienti possono essere utilizzati per le liste descrittive.

  <\explain|<markup|description>>
    L'ambiente per le liste descrittive di default (solitamente
    <markup|description-compact>).
  </explain>

  <\explain|<markup|description-compact>>
    Allinea a sinistra gli elementi e pone le loro descrizioni immediatamente
    dopo di essi.
  </explain>

  <\explain|<markup|description-dash>>
    Analogo a <markup|description-compact>, ma usa \V per separare ciascun
    elemento dalla relativa descrizione.
  </explain>

  <\explain|<markup|description-align>>
    Allinea a sinistra le descrizioni, mentre allinea a destra gli elementi.
  </explain>

  <\explain|<markup|description-long>>
    Pone gli elementi e le loro descrizioni su linee distinte.
  </explain>

  I nuovi elementi in una lista sono indicati con il <markup|item>, o con il
  tag unario <markup|item*> nel caso di descrizioni. Gli sviluppatori
  troveranno qualche macro addizionale, ma instabile, in <tmdtd|std-list> per
  definire altre strutture di liste.

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia Gecchelin|Andrea
  Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|italian>
  </collection>
</initial>