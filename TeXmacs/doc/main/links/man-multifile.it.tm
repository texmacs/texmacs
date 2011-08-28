<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Libri e documenti composti di molti file>

  Quando un documento assume dimensioni molto grandi, si può desiderare di
  suddividerlo in parti più piccole. Questo permette sia di riutilizzare più
  facilmente i pezzi individuali in altri lavori, sia di migliorare i tempi
  di risposta dell'editor. Si può inserire un intero file in un altro
  utilizzando <apply|menu|Insert|Link|Include>. Per poter rendere più rapido
  il trattamento dei documenti inclusi, questi vengono messi in un buffer.
  Per aggiornare tutti i documenti inclusi si utilizza
  <apply|menu|Tools|Update|Inclusions>.

  Quando si scrive un libro, generalmente si mettono i singoli capitoli in
  file <verbatim|c1.tm>, <verbatim|c2.tm> until <verbatim|cn.tm>. Poi si crea
  un file <verbatim|book.tm> per l'intero libro, nel quale i file
  <verbatim|c1.tm>, <verbatim|c2.tm> until <verbatim|cn.tm> sono inclusi
  utilizzando il meccanismo ora spiegato. Il sommario, la bibliografia, ecc.
  vengono generalmente messi nel file <verbatim|book.tm>.

  Per poter vedere i riferimenti incrociati agli altri capitoli quando si
  compone un particolare capitolo <verbatim|ci.tm>, si può specificare
  <verbatim|book.tm> come ``file principale'' per i file da <verbatim|c1.tm>
  a <verbatim|cn.tm> utilizzando <apply|menu|Document|Master|Attach>.
  Attualmente, la numerazione dei capitoli non è gestita mediante questo
  meccanismo. Per assegnare una numerazione corretta, si deve assegnare
  manualmente la variabile d'ambiente <verbatim|chapternr> all'inizio di
  ciascun file contenente un capitolo.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Includi>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Strumenti>|<with|font
      family|<quote|ss>|Aggiorna>|<with|font
      family|<quote|ss>|Inclusioni>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Master>|<with|font
      family|<quote|ss>|Collega>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
