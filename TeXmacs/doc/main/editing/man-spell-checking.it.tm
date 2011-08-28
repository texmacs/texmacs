<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Controllo ortografico>

  Se sul proprio sistema è stato installato il programma <verbatim|ispell>,
  allora lo si può utilizzare per controllare la presenza di errori
  ortografici tramite <shortcut|(spell-start)> o <apply|menu|Edit|Spell>. Si noti
  che si deve verificare che i dizionari corrispondenti alle lingue
  utilizzate nel proprio testo siano stati installati nel sistema; cosa che
  generalmente accade per la lingua inglese.

  Quando si attiva il controllo ortografico, sia sull'intero testo che su una
  sua regione selezionata, ad ogni parola che presenta un errore il programma
  porrà una richiesta all'utilizzatore e nella barra a piè di pagina
  compariranno le opzioni disponibili:

  <\description>
    <expand|item*|a)>Accettare la parola e tutte le sue ripetizioni
    successive nel testo.

    <expand|item*|r)>Sostituire la parola con una correzione che si inserirà.

    <expand|item*|i)>Indicare che la parola ``scorretta'' è in realtà
    corretta e che deve essere inserita nel proprio dizionario.

    <expand|item*|1-9)>Diversi suggerimenti di correzione per la parola.
  </description>

  Si noti che <verbatim|ispell> controlla solamente le parole scorrette. Ma
  non verrà segnalato alcun errore grammaticale.

  Il controllore ortografico utilizzerà il dizionario della lingua attiva
  nella posizione corrente del cursore o all'inizio di una parte selezionata.
  Solamente il testo in quella lingua verrà controllato. Se il proprio
  documento contiene parti di testo in lingue diverse, allora si dovrà
  attivare un controllo ortografico per ciascuna lingua utilizzata.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Ortografía>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
