<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambienti per oggetti fluttuanti>

  Il <abbr|d.t.d.> <tmdtd|<abbr|>env-float> fornisce i tag per gli oggetti
  fluttuanti. Il tag seguente è l'unico di alto livello:

  <\description>
    <expand|item*|<markup|footnote>>crea una nota a piè di pagina.
  </description>

  I seguenti tag di basso livello possono essere utilizzati per definire
  ambienti di alto livello per le figure e le tabelle: <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> e <markup|small-table>:

  <\description>
    <expand|item*|<markup|small-figure*>>Una macro per visualizzare una
    figura piccola. Gli argomenti sono un nome breve (come ``figura'' o
    ``tabella'') per la lista di figure, il suo nome reale (come ``Figura
    2.3'' o ``Tabella <format|no line break>5''), la figura stessa e una
    didascalia.

    <expand|item*|<markup|big-figure*>>Una variante di <markup|small-figure*>
    per visualizzare una figura grande.
  </description>

  I tag seguenti possono essere utilizzati per personalizzare l'aspetto del
  testo attorno alle figure, alle tabelle e alle note a piè di pagina:

  <\description>
    <expand|item*|<markup|figurename>>Una macro che controlla l'aspetto del
    testo ``Figura''. Per default, viene utilizzato il grassetto.

    <expand|item*|<markup|figuresep>>Il separatore tra la figura , il suo
    numero e la didascalia. Per default, questo è un punto seguito da uno
    spazio.

    <expand|item*|<markup|footnotesep>>Il separatore tra il numero della nota
    e il testo. Per default, questo è un punto seguito da uno spazio.
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Lucia
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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|<group|>env-float>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|footnote>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-figure>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-table>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-table>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure*>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-figure*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|figurename>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|figuresep>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|footnotesep>>|<pageref|idx-12>>
    </associate>
  </collection>
</auxiliary>
