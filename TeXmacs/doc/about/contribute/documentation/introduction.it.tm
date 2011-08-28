<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Introduzione a come contribuire>

  La creazione di documentazione di qualità è questione sia di contenuti che
  di struttura. I contenuti devono essere quanto più pedagogici possibile in
  relazione al gruppo di lettori a cui essi sono rivolti. Per questo conviene
  corredare le esposizioni con esempi e, se necessario, con immagini.
  Nonostante la documentazione non debba essere necessariamente completa è
  auspicabile che una sua parte sia stabile e corretta dal punto di vista
  dell'ortografia. Le parti sperimentali di documentazione verranno inserite
  nella directory <verbatim|incoming> o in <apply|hyper-link|<TeXmacs>
  Wiki|http://alqua.com/tmresources>.

  Un ulteriore importante aspetto riguarda la struttura da conferire ai
  documenti che deve essere curata e organizzata ricorrendo principalmente
  all'uso dei marcatori inclusi nello stile <tmstyle|tmdoc>. La
  strutturazione dei documenti serve per poter compilare automaticamente
  libri stampabili, per poter utilizzare diverse modalità di visualizzazione
  degli stessi e per rendere più efficenti le ricerche di informazioni
  contenute al loro interno. In modo particolare ogni documento deve
  contenere indicazioni sul <apply|hyper-link|copyright e sulla
  licenza|copyright.it.tm> e, se esso fa riferimento a
  <apply|hyper-link|molti file|file-names.it.tm>, deve contenere le
  indicazioni per poter <apply|hyper-link|spostarsi|traversal.it.tm>
  automaticamente all'interno della restante documentazione.

  <\warning>
    Se state traducendo un documento non dimenticate di selezionare il
    comando <apply|menu|Document|Language|Your language>. In questo modo
    alcune parti del documento, come i menu o le istruzioni, verranno
    automaticamente tradotte. Si raccomanda anche l'utilizzo del correttore
    ortografico di <TeXmacs> il cui uso è chiaramente condizionato dalla
    scelta della lingua in cui è redatto il documento.
  </warning>

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Lingua>|<with|font family|<quote|ss>|Your
      language>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
