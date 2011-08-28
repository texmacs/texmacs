<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creare etichette, collegamenti e riferimenti>

  Si può creare una nuova etichetta inattiva utilizzando <shortcut|(make-label)> o
  <apply|menu|Insert|Link|Label> e un riferimento a questa etichetta
  utilizzando <shortcut|(make 'reference)> o <apply|menu|Insert|Link|Reference>. Si
  faccia attenzione a porre l'etichetta in un punto in cui il suo numero sarà
  corretto. Quando si assegnano etichette alle sezioni, la posizione
  raccomandata è quella subito dopo il nome della sezione. Quando si
  etichettano le equazioni, la posizione raccomandata è all'inizio
  dell'equazione.

  È possibile creare degli hyperlink ad altri documenti utilizzando
  <key|inactive \<gtr\>> o <apply|menu|Insert|Link|Hyperlink>. Il primo
  campo dell'hyperlink è associato al testo, che risulta visualizzato in blu
  quando viene attivato. Il secondo campo contiene il nome di un documento,
  che può torvarsi sul web. Come usualmente accade per gli hyperlink, un
  collegamento della forma <verbatim|#<with|font shape|italic|etichetta>>
  punta ad una etichetta nello stesso documento e un collegamento della forma
  <verbatim|<with|font shape|italic|url>#<with|font shape|italic|etichetta>>
  punta ad una etichetta che si trova nel documento all'indirizzo
  <verbatim|<with|font shape|italic|url>>.

  In maniera simile, si può associare un'azione ad una parte di testo o a
  degli elementi grafici utilizzando <key|inactive *> o
  <apply|menu|Insert|Link|Action>. In questo caso, il secondo campo contiene
  uno script Guile/Scheme, che viene eseguito ogni volta che si fa doppio
  click due volte sul testo, dopo la sua attivazione. Per ragioni di
  sicurezza, tali script non sono sempre accettati. Per default, viene
  chiesto se si vuole accettare lo script; questo comportamento di default
  può essre modificato in <apply|menu|Edit|Preferences|Security>. Si noti che
  il comando Guile/Scheme:\ 

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  valuta <verbatim|shell-command> come comando shell.

  Infine, si possono includere direttamente altri documenti in un dato
  documento utilizzando <key|inactive i> o <apply|menu|Insert|Link|Include>.
  Ciò consente per esempio di includere il listato di un programma nel
  proprio testo, in modo che le modifiche che verranno compiute sul programma
  si riflettano automaticamente nel testo in cui è esso stato incluso.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Etichetta>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Referenza>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Collegamento ipertestuale>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Azione>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Sicurezza>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Includi>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
