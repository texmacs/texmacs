<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Usare lo stile tmdoc>

  Oltre alle macro relative alle <apply|hyper-link|informazioni sul
  copyright|copyright.it.tm> e agli <apply|hyper-link|spostamenti|traversal.it.tm>
  all'interno della documentazione di cui abbiamo parlato in precedenza, lo
  stile <tmstyle|tmdoc> offre un certo numero di macro e di funzioni che, se
  necessario, possono essere di volta in volta attivate:

  <\description>
    <expand|item*|<markup|key>>questa macro viene utilizzata per indicare
    comandi da tastiera come <shortcut|(save-buffer)>. Le macro specializzate
    <markup|kbd-gen>, <markup|kbd-text>, <markup|kbd-math>,
    <markup|kbd-symb>, <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>,
    <markup|kbd-exec> e <markup|kbd-table> vengono utilizzate per comandi da
    tastiera corrispondenti ad uno specifico tipo di azione o di modalità. Ad
    esempio, <markup|kbd-math> corrisponde al comando da tastiera per
    l'inserimento di espressioni matematiche come <key|math f> che
    indica l'inizio di una frazione.

    <expand|item*|<markup|menu>>questa funzione, che si riferisce ad un
    arbitrario numero di argomenti, permette di scrivere dei menu come
    <apply|menu|File> o <apply|menu|Document|Language>. La funzione esegue
    automaticamente la traduzione delle voci del menu che vengono inserite.

    <expand|item*|<markup|markup>>questa macro viene utilizzata per indicare
    macro o funzioni come <markup|section>.

    <expand|item*|<markup|tmstyle>>questa macro indica il nome di un file di
    stile di <TeXmacs> come <tmstyle|article>.

    <expand|item*|<markup|tmpackage>>questa macro indica il nome di un
    pacchetto <TeXmacs> come ad esempio il pacchetto <tmpackage|std-markup>.

    <expand|item*|<markup|tmdtd>>questa macro indica il nome di un <TeXmacs>
    <abbr|d.t.d.> come <tmdtd|number-env>.
  </description>

  Osserviamo che nessun nome di macro deve essere tradotto in lingua
  straniera! Oltre a questo si ricorda di non tradurre mai le voci dei menu,
  le quali vengono tradotte automaticamente in modo da garantire la
  sincronizzazione tra la traduzione dei manuali e la localizzazione dei menu
  di <TeXmacs>. Nel caso di marcatori, stili e pacchetti <abbr|d.t.d.>, è
  importante non alterarne il nome originale in quanto, il più delle volte,
  esso corrisponde al nome di un file.

  Le seguenti macro e funzioni, pensate per realizzare link e per gestire gli
  indici, verranno implementate in futuro:

  <\description>
    <expand|item*|<markup|simple-link>>questa macro ha come argomento un
    hyperlink URL <with|mode|math|x>, dove <with|mode|math|x> rappresenta
    nome e destinazione dell'hyperlink;

    <expand|item*|<markup|hyper-link>>questa macro definisce un normale
    hyperlink;

    <expand|item*|<markup|concept-link>>questa macro ha come argomento un
    concetto. Successivamente potrebbe essere creato automaticamente un
    appropriato hyperlink a partire da essa e da altra documentazione;

    <expand|item*|<markup|only-index>>indice di una stringa;

    <expand|item*|<markup|def-index>>definizione di un nuovo concetto: il
    testo viene scritto in italico e indicizzato;

    <expand|item*|<markup|re-index>>comparsa di un concetto definito in
    precedenza: il testo viene scritto in roman e inserito nell'indice.
  </description>

  In conclusione elenchiamo i seguenti tag di uso frequente:

  <\description>
    <expand|item*|<markup|icon>>link ad una icona in una directory centrale
    come \ <verbatim|$TEXMACS_PATH/doc/images/pixmaps>

    <expand|item*|<markup|screenshot>>link a una schermata. Le schermate sono
    attualmente contenuta nella directory centrale
    <verbatim|$TEXMACS_PATH/doc/images/screenshots>

    <expand|item*|<markup|scheme>>il linguaggio <value|scheme>;

    <expand|item*|<markup|cpp>>il linguaggio <value|cpp>;

    <expand|item*|<markup|framed-fragment>>per visualizzare una parte di
    codice in un riquadro;

    <expand|item*|<markup|scheme-fragment>>per un codice <value|scheme>
    multi-paragrafo;

    <expand|item*|<markup|cpp-fragment>>per un codice <value|cpp>
    multi-paragrafo;

    <expand|item*|<markup|tm-fragment>>per una parte di codice <TeXmacs> in
    formato <value|scheme>;

    <expand|item*|<markup|scheme-code>>per una parte di codice
    <value|scheme>;

    <expand|item*|<markup|cpp-code>>per una parte di codice <value|cpp>;

    <expand|item*|<markup|descriptive-table>>per tavole descrittive che
    possono essere utilizzate per realizzare liste di comandi da tastiera, di
    marcatori, ecc....
  </description>

  Lo stile <tmstyle|tmdoc> eredita numerose macro dallo stile
  <tmstyle|generic> percui, se necessario, è possibile utilizzare macro come
  <markup|em>, <markup|verbatim>, <markup|itemize>, <abbr|e così di seguito>.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|key>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-gen>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-text>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-symb>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-big>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-large>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-ia>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-exec>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-table>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|menu>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|File>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Lingua>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|markup>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmstyle>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|article>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmpackage>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|std-markup>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdtd>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|number-env>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|simple-link>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hyper-link>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|concept-link>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|only-index>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|def-index>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|re-index>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|icon>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|screenshot>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cpp>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|framed-fragment>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme-fragment>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cpp-fragment>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tm-fragment>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme-code>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cpp-code>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|descriptive-table>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-41>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|generic>>|<pageref|idx-42>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-43>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-44>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-45>>
    </associate>
  </collection>
</auxiliary>
