<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversione da <LaTeX> a <TeXmacs>>

  Lo scopo attuale del programma di conversione da <apply|LaTeX> a
  <apply|TeXmacs> è di <with|font shape|italic|aiutare> nella traduzione in
  <TeXmacs> di vecchi documenti. In genere, le conversioni da <apply|LaTeX> a
  <apply|TeXmacs> sono più problematiche delle conversioni contrarie.
  Tuttavia, se ci si è limitati ad usare i comandi <apply|LaTeX> più comuni,
  dovrebbe essere possibile convertire i propri vecchi documenti
  ragionevolmente bene. Per esempio, tutti i file di aiuto di <apply|TeXmacs>
  sono stati scritti in <apply|LaTeX> per verificare il programma di
  conversione da <apply|LaTeX> a <apply|TeXmacs>.

  Si può convertire un documento <apply|LaTeX> <verbatim|name.tex> in
  <apply|TeXmacs> usando <apply|menu|File|Import|Latex> e salvarlo con il
  nome <verbatim|name.tm>. Se il proprio documento <apply|LaTeX> è stato
  scritto sufficientemente bene, allora il risultato della conversione
  dovrebbe essere più o meno accettabile, a parte alcuni comandi che non sono
  stati riconosciuti che compaiono in rosso. Una buona soluzione sarebbe di
  scrivere un proprio file di stile per i documenti convertiti, basato sullo
  stile originale, nel quale siano definiti i comandi sconosciuti.

  Tuttavia, in alcuni casi meno fortunati, il documento convertito apparirà
  assai confuso. Ciò è generalmente causato dal fatto che <apply|TeX> e
  <apply|LaTeX> permettono agli utenti di modificare dinamicamente
  l'analizzatore sintattico, per esempio, utilizzando il comando
  <verbatim|\\catcode>. In questo caso, il programma di conversione potrebbe
  confondersi e compiere delle assunzioni errate sulla modalità o
  sull'ambiente. Come risultato, il testo potrebbe essere convertito come
  modalità matematica, le parti matematiche come verbatim, e così via.
  Comunque, nel file sorgente <verbatim|name.tex> i comandi che confondono il
  programma di conversione solitamente si localizzano facilmente confrontando
  la versione in formato <LaTeX> con la corrispondente conversione in
  <apply|TeXmacs>. Dopo qualche ricerca nel file sorgente e dopo aver rimosso
  qualche parte del codice che crea problemi, il documento dovrebbe poter
  essere convertito correttamente.

  Nel futuro, il programma di conversione verrà esteso con un convertitore di
  file di stile e qualche caratteristica aggiuntiva per facilitare la
  traduzione dei comandi definiti dall'utente, che sono definiti in un
  documento distinto da quello che si desidera convertire.

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
      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Importa>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
