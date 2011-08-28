<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Comporre testi strutturati>

  In genere, i documenti lunghi possiedono una struttura: essi sono
  organizzati in capitoli, sezioni e sottosezioni, contengono diversi tipi di
  testo, come testo ordinario, citazioni, note a piè di pagina, teoremi, ecc.
  Dopo aver selezionato uno <expand|def-index|stile di documento> in
  <apply|menu|Document|Style>, <apply|TeXmacs> si occupa dei problemi
  specifici di impaginazione, come la numerazione delle sezioni, delle
  pagine, dei teoremi e della composizione delle citazioni e delle note a piè
  di pagina, e così via.

  Attualmente, sono stati implementati quattro stili standard per i
  documenti: lettera, articolo, libro e seminario. Lo stile seminario viene
  utilizzato per comporre dei lucidi per una presentazione. Appena viene
  selezionato uno di questi stili, si può organizzare il proprio testo in
  sezioni (si veda <apply|menu|Insert|Section>) e utilizzare specifici
  <expand|def-index|ambienti>. Esempi di ambienti sono teorema, proposizione,
  nota e così via (si veda <apply|menu|Insert|Environment>). Altri esempi sono
  le liste di oggetti (si veda <apply|menu|Insert|Itemize>) o le liste numerate
  (si veda <apply|menu|Insert|Enumerate>).

  Quando si è acquisita una certa dimestichezza nell'uso di <apply|TeXmacs>,
  è possibile aggiungere dei nuovi ambienti in un proprio file di stile.
  Assumiamo per esempio che si facciano spesso delle citazioni e che si
  voglia che queste appaiano in italico, con margini sinistro e destro di 1
  cm. Invece di cambiare manualmente le proprietà del testo e del paragrafo
  ogni volta che si fa una citazione, è più comodo creare un ambiente
  citazione. Non solo questo renderà più rapida la creazione di una nuova
  citazione, ma renderà possibile modificare sistematicamente l'aspetto di
  tutte le citazioni presenti nel documento cambiando la definizione
  dell'ambiente citazione. Quest'ultima situazione si presenta per esempio
  quando si scopre <with|font shape|italic|a posteriori> che si preferisce
  che le citazioni appaiano con un font più piccolo.

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
    <associate|preamble|false>
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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|stile di documento>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Stile>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Sezione>>|<pageref|idx-3>>

      <tuple|<tuple|ambienti>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Ambiente>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Lista puntata>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Lista numerata>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
