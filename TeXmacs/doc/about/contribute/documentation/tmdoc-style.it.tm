<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Usare lo stile tmdoc>

  Oltre alle macro relative alle <hlink|informazioni sul
  copyright|copyright.it.tm> e agli <hlink|spostamenti|traversal.it.tm>
  all'interno della documentazione di cui abbiamo parlato in precedenza, lo
  stile <tmstyle|tmdoc> offre un certo numero di macro e di funzioni che, se
  necessario, possono essere di volta in volta attivate:

  <\explain|<markup|key>>
    questa macro viene utilizzata per indicare comandi da tastiera come
    <shortcut|(save-buffer)>. Le macro specializzate <markup|kbd-gen>,
    <markup|kbd-text>, <markup|kbd-math>, <markup|kbd-symb>,
    <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>, <markup|kbd-exec>
    e <markup|kbd-table> vengono utilizzate per comandi da tastiera
    corrispondenti ad uno specifico tipo di azione o di modalità. Ad esempio,
    <markup|kbd-math> corrisponde al comando da tastiera per l'inserimento di
    espressioni matematiche come <key|math f> che indica l'inizio di una
    frazione.
  </explain>

  <\explain|<markup|menu>>
    questa funzione, che si riferisce ad un arbitrario numero di argomenti,
    permette di scrivere dei menu come <menu|File> o
    <menu|Document|Language>. La funzione esegue automaticamente la
    traduzione delle voci del menu che vengono inserite.
  </explain>

  <\explain|<markup|markup>>
    questa macro viene utilizzata per indicare macro o funzioni come
    <markup|section>.
  </explain>

  <\explain|<markup|tmstyle>>
    questa macro indica il nome di un file di stile di <TeXmacs> come
    <tmstyle|article>.
  </explain>

  <\explain|<markup|tmpackage>>
    questa macro indica il nome di un pacchetto <TeXmacs> come ad esempio il
    pacchetto <tmpackage|std-markup>.
  </explain>

  <\explain|<markup|tmdtd>>
    questa macro indica il nome di un <TeXmacs> <abbr|d.t.d.> come
    <tmdtd|number-env>.
  </explain>

  Osserviamo che nessun nome di macro deve essere tradotto in lingua
  straniera! Oltre a questo si ricorda di non tradurre mai le voci dei menu,
  le quali vengono tradotte automaticamente in modo da garantire la
  sincronizzazione tra la traduzione dei manuali e la localizzazione dei menu
  di <TeXmacs>. Nel caso di marcatori, stili e pacchetti <abbr|d.t.d.>, è
  importante non alterarne il nome originale in quanto, il più delle volte,
  esso corrisponde al nome di un file.

  Le seguenti macro e funzioni, pensate per realizzare link e per gestire gli
  indici, verranno implementate in futuro:

  <\explain|<markup|simple-link>>
    questa macro ha come argomento un hyperlink URL <math|x>, dove <math|x>
    rappresenta nome e destinazione dell'hyperlink;
  </explain>

  <\explain|<markup|hyper-link>>
    questa macro definisce un normale hyperlink;
  </explain>

  <\explain|<markup|concept-link>>
    questa macro ha come argomento un concetto. Successivamente potrebbe
    essere creato automaticamente un appropriato hyperlink a partire da essa
    e da altra documentazione;
  </explain>

  <\explain|<markup|only-index>>
    indice di una stringa;
  </explain>

  <\explain|<markup|def-index>>
    definizione di un nuovo concetto: il testo viene scritto in italico e
    indicizzato;
  </explain>

  <\explain|<markup|re-index>>
    comparsa di un concetto definito in precedenza: il testo viene scritto in
    roman e inserito nell'indice.
  </explain>

  In conclusione elenchiamo i seguenti tag di uso frequente:

  <\explain|<markup|icon>>
    link ad una icona in una directory centrale come
    \ <verbatim|$TEXMACS_PATH/doc/images/pixmaps>
  </explain>

  <\explain|<markup|screenshot>>
    link a una schermata. Le schermate sono attualmente contenuta nella
    directory centrale <verbatim|$TEXMACS_PATH/doc/images/screenshots>
  </explain>

  <\explain|<markup|scheme>>
    il linguaggio <scheme>;
  </explain>

  <\explain|<markup|cpp>>
    il linguaggio <c++>;
  </explain>

  <\explain|<markup|framed-fragment>>
    per visualizzare una parte di codice in un riquadro;
  </explain>

  <\explain|<markup|scheme-fragment>>
    per un codice <scheme> multi-paragrafo;
  </explain>

  <\explain|<markup|cpp-fragment>>
    per un codice <c++> multi-paragrafo;
  </explain>

  <\explain|<markup|tm-fragment>>
    per una parte di codice <TeXmacs> in formato <scheme>;
  </explain>

  <\explain|<markup|scheme-code>>
    per una parte di codice <scheme>;
  </explain>

  <\explain|<markup|cpp-code>>
    per una parte di codice <c++>;
  </explain>

  <\explain|<markup|descriptive-table>>
    per tavole descrittive che possono essere utilizzate per realizzare liste
    di comandi da tastiera, di marcatori, ecc....
  </explain>

  Lo stile <tmstyle|tmdoc> eredita numerose macro dallo stile
  <tmstyle|generic> percui, se necessario, è possibile utilizzare macro come
  <markup|em>, <markup|verbatim>, <markup|itemize>, <abbr|e così di seguito>.

  <tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|italian>
  </collection>
</initial>