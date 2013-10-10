<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Sezioni in stile <LaTeX> >

  Il <abbr|d.t.d.> <tmdtd|section-latex> fornisce i tag standard per le
  sezioni, che sono uguali a quelle del <LaTeX>. La maggior parte dei tag di
  sezione prendono un solo argomento: il nome della sezione. In futuro,
  verranno creati dei tag alternativi con due argomenti, che consentiranno di
  vedere il corpo di una sezione come parte della struttura. I tag seguenti
  solitamente generano sezioni numerate, che sono riportate nell'indice:

  <\explain|<markup|chapter>>
    Macro per produrre il titolo di un capitolo che può essere numerato.
  </explain>

  <\explain|<markup|section>>
    Macro per produrre il titolo di una sezione che può essere numerato.
  </explain>

  <\explain|<markup|subsection>>
    Macro per produrre il titolo di una sottosezione che può essere numerata.
  </explain>

  <\explain|<markup|subsubsection>>
    Macro per produrre il titolo di una sotto-sottosezione che può essere
    numerata.
  </explain>

  <\explain|<markup|paragraph>>
    Macro per produrre il titolo di un paragrafo che può essere numerato.
  </explain>

  <\explain|<markup|subparagraph>>
    Macro per produrre il titolo di un sottoparagrafo che può essere
    numerato.
  </explain>

  I tag <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> e <markup|subparagraph*>
  possono essere utilizzati per le varianti non numerate dei tag appena
  descritti, che non vengono riportate nell'indice. Il <abbr|d.t.d.>
  <tmdtd|section-latex> fornisce anche i tag seguenti:

  <\explain|<markup|chapter**>>
    Macro con due argomenti: un tipo speciale di capitolo (come ``Epilogo'')
    e il nome del capitolo.
  </explain>

  <\explain|<markup|appendix>>
    Una variante di <markup|chapter> o <markup|section> per produrre le
    appendici.
  </explain>

  <\explain|<markup|section-sep>>
    Macro per personalizzare il separatore tra il numero di una sezione e il
    suo titolo. Per default, usiamo due spazi.
  </explain>

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