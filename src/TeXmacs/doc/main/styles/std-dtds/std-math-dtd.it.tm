<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Markup matematici standard>

  I markup matematici standard sono definiti in <tmdtd|std-math>.

  <\description>
    <expand|item*|<markup|binom>>Per i coefficienti binomiali
    <with|mode|math|<binom|n|m>>.

    <expand|item*|<markup|choose>>Nome alternativo per <markup|binom>
    (obsoleto).

    <expand|item*|<markup|shrink-inline>>Macro che permette di passare alla
    dimensione del testo quando non si è nello stile di visualizzazione.
    Questa macro viene usata principalmente dagli sviluppatori. Per esempio,
    la macro <markup|binom> la utilizza.
  </description>

  I seguenti sono ambienti matematici standard per le tabelle:

  <\description>
    <expand|item*|<markup|matrix>>Per le matrici
    <with|mode|math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.

    <expand|item*|<markup|det>>Per i determinanti
    <with|mode|math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.

    <expand|item*|<markup|choice>>Per le liste di scelta
    <with|mode|math|\|x\|=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<with|mode|text|if
    >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<with|mode|text|if
    >x\<geqslant\>0>>>>>>
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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-math>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|binom>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|choose>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|binom>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|shrink-inline>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|binom>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|matrix>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|det>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|choice>>|<pageref|idx-9>>
    </associate>
  </collection>
</auxiliary>
