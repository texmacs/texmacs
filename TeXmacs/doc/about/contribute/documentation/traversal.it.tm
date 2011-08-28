<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Spostamenti automatici nella documentazione >

  Come regola generale, è preferibile evitare, all'interno della
  documentazione di <TeXmacs>, il ricorso a testi lunghi e molto articolati,
  cercando di scrivere invece documenti brevi e su argomenti ben precisi. In
  una seconda fase conviene predisporre dei ``meta file'' di aiuto per
  agevolare gli spostamenti automatici da una parte all'altra della
  documentazione. Questa strategia consente, tra l'altro, di riutilizzare una
  stessa pagina del manuale in ambienti diversi (manuale stampato, tutorial
  sul web, ecc...).

  Lo stile <tmstyle|tmdoc> prevede la definizione di tre possibili macro per
  indicare come spostarsi all'interno della documentazione. La macro
  <markup|traverse> viene usata per racchiudere regioni di testo che
  contengono informazioni trasversali. La macro <markup|branch> indica una
  pagina di aiuto considerata come una sottosezione e la macro
  <markup|continue> indica una pagina che segue. Sia la macro <markup|branch>
  che la macro <markup|continue> hanno per riferimento due argomenti. Il
  primo argomento descrive il link mentre il secondo fornisce l'indirizzo
  fisico della pagina a cui il link si riferisce.

  Tipicamente al termine di un meta file di aiuto si troveranno numerose
  macro di tipo <markup|branch> o <markup|continue> all'interno di una macro
  di tipo <markup|traverse>. All'inizio del documento il titolo viene
  specificato utilizzando la macro <markup|tmdoc-title>. Nel momento in cui
  si desidera generare un manuale stampato la struttura
  titolo-capitolo-sezione-paragrafo verrà automaticamente generata, a partire
  da tutte le informazioni contenute in questo ambiente di macro. In
  alternativa è possibile generare dei bottoni addizionali per consentire la
  navigazione all'interno del documento, utilizzando un qualsiasi browser.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

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
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdoc-title>>|<pageref|idx-10>>
    </associate>
  </collection>
</auxiliary>
