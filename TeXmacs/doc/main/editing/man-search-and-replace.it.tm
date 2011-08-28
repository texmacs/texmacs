<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cerca e sostituisci>

  Si può cominciare la ricerca in un testo premendo <key|C-s> o utilizzando
  il menu <apply|menu|Edit|Search>. Durante una ricerca, la stringa cercata
  viene visualizzata a sinistra nella barra a piè di pagina. Ciascun
  carattere che si scrive viene aggiunto a questa stringa di ricerca e ogni
  sua ripetizione successiva \ viene racchiusa in un rettangolo rosso.
  Premendo <key|C-s> una seconda volta durante una ricerca, viene cercata la
  ripetizione successiva. Un suono indica che nel documento non sono state
  trovate altre ripetizioni; premendo ancora <key|C-s> la ricerca continuerà
  dall'inizio del documento. Si può premere il tasto
  <key|backspace> per annullare l'azione attivata dopo aver
  premuto un tasto durante la ricerca.

  Generalmente, un testo viene cercato nel documento dalla posizione corrente
  del cursore in avanti. Si può eseguire una ricerca all'indietro utilizzando
  <key|C-r>. In una ricerca, verrà trovato solamente del testo nella stessa
  modalità e nello stesso linguaggio attivi nella posizione da dove comincia
  la ricerca. In altre parole, quando si cerca una <with|mode|math|x> in
  modalità matematica, non si troverà alcuna x nel testo ordinario. Per il
  momento, la stringa di ricerca può contenere solamente testo ordinario e
  non simboli matematici o altre strutture di testo più complicate.

  Per effettuare una sostituzione si utilizza <key|C-=> o
  <apply|menu|Edit|Replace>. Il programma richiede una stringa che dovrà
  essere messa al posto di quella che si vuole sostituire. Ad ogni
  ripetizione della stringa che si vuole sostituire il programma richiederà
  di scegliere tra sostituire (y) o no (n) la stringa, oppure sostituire
  questa e tutte le ripetizione successiva (a). Come nel caso della ricerca,
  anche il comando per la sostituzione e' sensibile alla modalità e alla
  lingua delle stringhe in gioco.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Cerca>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Sustituisci>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
