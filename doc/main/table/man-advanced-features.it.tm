<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Caratteristiche avanzate>

  Nei menu, si possono trovare molte altre caratteristiche speciali per le
  tabelle. Molto brevemente, queste includono le seguenti:

  <\itemize>
    <item>Modifiche dello ``span'' di una cella e per consentirle di
    sovrapporsi alle celle vicine a destra e in basso.

    <item>Creazione di intere sottotabelle all'interno delle celle.

    <item>Correzione di fondo e altezza del testo, per poter far coincidere
    le linee base.

    <item>Sillabazione orizzontale del contenuto delle celle e sillabazione
    verticale dell'intera tabella.

    <item>Incollamento di più righe e/o colonne, in modo che le celle
    incollate diventino ``parte dei bordi'' delle celle rimanenti.

    <item>Disattivazione della tabella, per poter vedere il suo ``codice
    sorgente''

    <item>Impostazione del ``centro dell'estensione'' di una tabella. In
    seguito a questa impostazione, le proprietà di formattazione di questa
    cella saranno utilizzate per le nuove celle create intorno a questo
    centro.

    <item>Specificazione delle dimensioni massime e minime della tabella, che
    verranno rispettate durante le successive fasi di composizione del
    documento (ciò è particolarmente utile nella creazione di macro di
    tabelle).
  </itemize>

  Attualmente, tutte le tabelle vengono create in un ambiente come
  <markup|tabella>, <markup|blocco>, <markup|matrice>, ecc. Quando ci si crea
  una propria macro di tipo tabella, si può utilizzare
  <apply|menu|Table|Special table properties|Extract format> per estrarre il
  formato di una data tabella.

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
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabella>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|blocco>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|matrice>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Proprietà speciali della tabella>|<with|font
      family|<quote|ss>|Estrai formato>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
