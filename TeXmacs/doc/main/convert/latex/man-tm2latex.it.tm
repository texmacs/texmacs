<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversione da <TeXmacs> a <LaTeX>>

  La situazione più comune è che si voglia convertire un articolo da
  <apply|TeXmacs> a <apply|LaTeX>, per poterlo spedire a qualche rivista.
  Dato un file <apply|TeXmacs> <verbatim|name.tm>, lo si può convertire in un
  file <apply|LaTeX> <verbatim|name.tex> mediante
  <apply|menu|File|Export|Latex>. Come primo passo, si può provare ad
  eseguire <apply|LaTeX> su <verbatim|name.tex> e vedere se il risultato che
  si ottiene è già soddisfacente. Se ciò accade, si può spedire alla rivista
  il file <verbatim|name.tex> insieme al file di stile
  <verbatim|TeXmacs.sty>, che si trova nella
  <verbatim|$TEXMACS_PATH/misc/latex>.

  Spesso, la rivista utilizza un proprio file di stile, detto
  <verbatim|journal.sty>. In tal caso, si dovrebbe copiare anche il file\ 

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/styles/article.ts
  </verbatim>

  in\ 

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  e usare <verbatim|journal> come proprio stile di documento in
  <apply|menu|Document|Style|Other style>. Si può anche modificare
  <verbatim|journal.ts>, in modo che l'impaginazione dell'articolo diventi
  più simile allo stile della rivista. In alcuni casi, si deve creare anche
  un'altra copia di <verbatim|TeXmacs.sty>, e modificare qualche ambiente per
  ottenere la compatibilità con il file di stile della rivista
  <verbatim|journal.sty>.

  Se il primo tentativo di convertire un documento in <apply|LaTeX> non ha
  prodotto un risultato soddisfacente, generalmente si osserverà che
  solamente piccole parti di testo non sono state convertite correttamente.
  Questo può essere dovuto a tre cause principali:

  <\itemize>
    <item>Nel documento vengono utilizzate caratteristiche specifiche di
    <apply|TeXmacs>.

    <item>E' stata utilizzata una caratteristica di<apply|TeXmacs> che non è
    ancora stata implementata nell'algoritmo di conversione.

    <item>E' presente un bug nell'algoritmo di conversione.
  </itemize>

  Questi aspetti verranno discussi più dettagliatamente nella prossima
  sezione.

  In caso di problemi, una stategia naive sarebbe di correggere il file
  <apply|LaTeX> prodotto e spedirlo alla rivista. Comunque questa strategia
  ha lo svantaggio che si devono ripetere queste correzioni ogni volta che si
  converte il file <apply|TeXmacs> file <verbatim|name.tm>, dopo avere
  compiuto qualche altra modifica. Una strategia migliore è di usare
  <apply|menu|Format|Specific|Latex> e <apply|menu|Format|Specific|Texmacs>
  per scrivere del testo visibile solamente nel file convertito e originale
  rispettivamente.\ 

  Per esempio, si assuma che la parola ``blauwbilgorgel'' sia sillabata
  correttamente nel sorgente <apply|TeXmacs>, ma non nella conversione
  <apply|LaTeX>. Allora si può procedere nel modo seguente:

  <\enumerate>
    <item>Selezionare ``blauwbilgorgel''.

    <item>Cliccare su <apply|menu|Format|Specific|Texmacs> per rendere il
    testo ``blauwbilgorgel'' come specifico di <apply|TeXmacs>.

    <item>Cliccare su <apply|menu|Format|Specific|Latex>.

    <item>Scrivere il codice latex <verbatim|blauw\\-bil\\-gor\\-gel> con la
    sillabazione corretta.

    <item>Premere <key|return> per attivare il testo specifico
    di <apply|LaTeX>.
  </enumerate>

  In maniera simile, si possono inserire interruzioni di riga, interruzioni
  di pagina, spaziature verticali, modifiche dei parametri di stile, ecc.,
  specifiche di <apply|LaTeX>.

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
    <associate|idx-5|<tuple|2.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-7|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Esporta>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Stile>|<with|font family|<quote|ss>|Altri
      stili>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Specifico>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Specifico>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Specifico>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Specifico>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
