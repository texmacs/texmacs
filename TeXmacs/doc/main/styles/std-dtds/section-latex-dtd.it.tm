<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Sezioni in stile <LaTeX> >

  Il <abbr|d.t.d.> <tmdtd|section-latex> fornisce i tag standard per le
  sezioni, che sono uguali a quelle del <LaTeX>. La maggior parte dei tag di
  sezione prendono un solo argomento: il nome della sezione. In futuro,
  verranno creati dei tag alternativi con due argomenti, che consentiranno di
  vedere il corpo di una sezione come parte della struttura. I tag seguenti
  solitamente generano sezioni numerate, che sono riportate nell'indice:

  <\description>
    <expand|item*|<markup|chapter>>Macro per produrre il titolo di un
    capitolo che può essere numerato.

    <expand|item*|<markup|section>>Macro per produrre il titolo di una
    sezione che può essere numerato.

    <expand|item*|<markup|subsection>>Macro per produrre il titolo di una
    sottosezione che può essere numerata.

    <expand|item*|<markup|subsubsection>>Macro per produrre il titolo di una
    sotto-sottosezione che può essere numerata.

    <expand|item*|<markup|paragraph>>Macro per produrre il titolo di un
    paragrafo che può essere numerato.

    <expand|item*|<markup|subparagraph>>Macro per produrre il titolo di un
    sottoparagrafo che può essere numerato.
  </description>

  I tag <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> e <markup|subparagraph*>
  possono essere utilizzati per le varianti non numerate dei tag appena
  descritti, che non vengono riportate nell'indice. Il <abbr|d.t.d.>
  <tmdtd|section-latex> fornisce anche i tag seguenti:

  <\description>
    <expand|item*|<markup|chapter**>>Macro con due argomenti: un tipo
    speciale di capitolo (come ``Epilogo'') e il nome del capitolo.

    <expand|item*|<markup|appendix>>Una variante di <markup|chapter> o
    <markup|section> per produrre le appendici.

    <expand|item*|<markup|sectionsep>>Macro per personalizzare il separatore
    tra il numero di una sezione e il suo titolo. Per default, usiamo due
    spazi.
  </description>

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
    <associate|idx-10|<tuple|<uninit>|?>>
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
      magenta>|section-latex>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubsection>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|paragraph>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subparagraph>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection*>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubsection*>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|paragraph*>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subparagraph*>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|section-latex>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter**>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|appendix>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|sectionsep>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
