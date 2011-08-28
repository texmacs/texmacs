<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Specificare le dimensioni della tabella e delle celle>

  Utilizzando <apply|menu|Table|Cell width|Set width> e
  <apply|menu|Table|Cell height|Set height> si possono specificare la
  larghezza e l'altezza di una cella. La larghezza (o altezza) specificata
  può essere considerata in tre modi diversi:

  <\description>
    <expand|item*|Modo minimo>La larghezza attuale della cella sarà la minima
    tra la larghezza specificata e la larghezza dello spazio interno alla
    cella.

    <expand|item*|Modo esatto>La larghezza della cella sarà esattamente
    quella specificata.

    <expand|item*|Modo massimo>La larghezza attuale della cella sarà la
    massima tra la larghezza specificata e la larghezza dello spazio interno
    alla cella.
  </description>

  La larghezza del bordo e lo spazio tra le celle, che vedremo in seguito,
  vengono considerati nel calcolo delle dimenioni dello spazio interno alla
  cella.

  Si possono inoltre specificare la larghezza e l'altezza dell'intera tabella
  in <apply|menu|Table|Special table properties>. In particolare, si può
  specificare che la tabella occupi l'intera larghezza del paragrafo. Quando
  si specificano una larghezza o un'altezza, si può decidere come distribuire
  tra le celle lo spazio inutilizzato attraverso il menu
  <apply|menu|Table|Special cell properties|Distribute unused space>. Per
  default, lo spazio inutilizzato viene distribuito equamente.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Larghezza della cella>|<with|font
      family|<quote|ss>|Imposta larghezza>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Altezza della cella>|<with|font
      family|<quote|ss>|Imposta altezza>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Proprietà speciali della tabella>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Proprietà speciali della cella>|<with|font
      family|<quote|ss>|Distribuisci spazio inutilizzato>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
