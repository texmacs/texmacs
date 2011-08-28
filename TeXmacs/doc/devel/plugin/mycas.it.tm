<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|L'esempio di ``mycas''>

  Il modo migliore per iniziare ad implementare in <TeXmacs> una nuova
  interfaccia, consiste nello studiare attentamente l'esempio del sistema di
  computer algebra <verbatim|mycas>, che è contenuto nella directory
  <verbatim|$TEXMACS_PATH/misc/mycas>. Il file <verbatim|mycas.cpp>, il cui
  listato è disponibile alla fine di questa sezione, contiene un esempio
  molto semplice di programma che può essere interfacciato con
  <apply|TeXmacs>. Per testare il programma è necessario compilarlo
  utilizzando il comando:\ 

  <\verbatim>
    \ \ \ \ g++ mycas.cpp -o mycas
  </verbatim>

  e spostare il file binario <verbatim|mycas> in qualche punto del vostro
  path. All'avvio di <apply|TeXmacs> è quindi necessario avere a disposizione
  la voce <apply|menu|Mycas> nel menu <apply|menu|Insert|Session>.

  <section|Studiamo il codice sorgente passo dopo passo>

  Iniziamo a studiare, passo dopo passo, il codice di <verbatim|mycas>. In
  primo luogo osserviamo come ogni comunicazione avvenga utilizzando delle
  pipes e coinvolga standard input e output. Per permettere a <apply|TeXmacs>
  di comprendere quando l'output del sistema ha terminato è necessario che
  ogni output sia incapsulato in blocchi, utilizzando tre speciali caratteri
  di controllo:\ 

  <\verbatim>
    \ \ \ \ #define DATA_BEGIN \ \ ((char) 2)<format|next line> \ \ \ #define
    DATA_END \ \ \ \ ((char) 5)<format|next line> \ \ \ #define DATA_ESCAPE
    \ ((char) 27)
  </verbatim>

  Il carattere <verbatim|DATA_ESCAPE> seguito da un qualsiasi altro carattere
  <with|mode|math|c> può essere usato per produrre <with|mode|math|c>, anche
  se <with|mode|math|c> è uno dei tre caratteri di controllo.
  Un'illustrazione di come utilizzare \ <verbatim|DATA_BEGIN> e
  <verbatim|DATA_END> è data dal seguente messaggio di inizio:\ 

  <\verbatim>
    \ \ \ \ int<format|next line> \ \ \ main () {<format|next line>
    \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ cout \<less\>\<less\> "Welcome to my test computer
    algebra system for TeXmacs\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "This software comes with no warranty
    whatsoever\\n";<format|next line> \ \ \ \ \ cout \<less\>\<less\> "(c)
    2001 \ by Joris van der Hoeven\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ fflush (stdout);
  </verbatim>

  La prima linea del <verbatim|main> definisce il formato del carattere del
  messaggio di inizio che è di tipo ``verbatim''. La funzione
  <verbatim|next_input>, che viene richiamata al termine del messaggio di
  inizio, serve per stampare un prompt e se ne discuterà in dettaglio nel
  seguito. In conclusione <verbatim|DATA_END> chiude il blocco del messaggio
  di inizio e informa <apply|TeXmacs> che <verbatim|mycas> è in attesa di un
  input. Non dimenticate alla fine di svuotare lo standard output, in modo
  che <apply|TeXmacs> possa ricevere l'intero messaggio.

  Il ciclo principale inizia chiedendo l'input dallo standard input:\ 

  <\verbatim>
    \ \ \ \ \ \ while (1) {<format|next line> \ \ \ \ \ \ \ char
    buffer[100];<format|next line> \ \ \ \ \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ \ \ \ \ if (strcmp (buffer, "quit") == 0)
    break;
  </verbatim>

  L'output che viene restituito deve essere ancora una volta incluso in un
  blocco tipo <verbatim|DATA_BEGIN>-<verbatim|DATA_END>.\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> "You
    typed " \<less\>\<less\> buffer \<less\>\<less\> "\\n";
  </verbatim>

  All'interno di tale blocco è possibile inviare ricorsivamente altri blocchi
  che possono essere specificati in formati diversi. Ad esempio, il seguente
  codice spedisce una formula in <apply|LaTeX>:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula:
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "latex:" \<less\>\<less\> "$x^2+y^2=z^2$"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> "\\n";
  </verbatim>

  Per alcuni scopi particolari può essere utile spedire direttamente l'output
  in formato <apply|TeXmacs> utilizzando una rappresentazione <apply|scheme>:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And finally a fraction
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "scheme:" \<less\>\<less\> "(frac \\"a\\" \\"b\\")"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> ".\\n";
  </verbatim>

  Per concludere, dobbiamo nuovamente inviare l'output <verbatim|DATA_END> e
  svuotare lo standard output:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ fflush
    (stdout);<format|next line> \ \ \ \ \ }<format|next line>
    \ \ \ \ \ return 0;<format|next line> \ \ \ }
  </verbatim>

  Osserviamo che non è possibile inviare più di un blocco
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> alla volta. Appena il primo
  blocco <verbatim|DATA_BEGIN>-<verbatim|DATA_END> è stato ricevuto da
  <apply|TeXmacs>, si assume che il sistema sia in attesa dell'input. Se si
  vuole inviare più di un blocco <verbatim|DATA_BEGIN>-<verbatim|DATA_END>
  tutti i blocchi da inviare devono essere inclusi in un blocco principale.

  Un ``canale'' speciale viene impiegato per inviare il prompt di input. I
  canali sono specificati come blocchi <verbatim|DATA_BEGIN>-<verbatim|DATA_END>
  speciali:\ 

  <\verbatim>
    \ \ \ \ static int counter= 0;<format|next line><format|next line>
    \ \ \ void<format|next line> \ \ \ next_input () {<format|next line>
    \ \ \ \ \ counter++;<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    DATA_BEGIN \<less\>\<less\> "channel:prompt" \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ \ \ cout \<less\>\<less\> "Input "
    \<less\>\<less\> counter \<less\>\<less\> "] ";<format|next line> \ \ \ }
  </verbatim>

  All'interno del canale di prompt è possibile utilizzare ancora blocchi
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> tuttavia in modo annidato. In
  questo modo è possibile ad esempio utilizzare come prompt una formula. Sono
  disponibili tre canali standard:

  <\description>
    <expand|item*|<verbatim|output>.>canale predefinito per un output
    normale;

    <expand|item*|<verbatim|prompt>.>per inviare prompt di input;

    <expand|item*|<verbatim|input>.>per specificare un valore predefinito per
    l'input successivo.
  </description>

  <section|Output grafici>

  È possibile anche inviare output grafici in formato postscript. Immaginiamo
  ad esempio di avere una figura <verbatim|picture.ps> nella home directory.
  Allora inserendo le linee di codice:

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";<format|next
    line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "ps:";<format|next line> \ \ \ \ \ \ \ fflush (stdout);<format|next line>
    \ \ \ \ \ \ \ system ("cat $HOME/picture.ps");<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> "\\n";
  </verbatim>

  nel punto appropriato del ciclo principale si otterrà la visualizzazione
  della figura nel centro dell'output.

  <section|Il listato completo del programma>

  <\verbatim>
    #include \<less\>stdio.h\<gtr\><format|next line>#include
    \<less\>stdlib.h\<gtr\><format|next line>#include
    \<less\>string.h\<gtr\><format|next line>#include
    \<less\>iostream.h\<gtr\><format|next line><format|next line>#define
    DATA_BEGIN \ \ ((char) 2)<format|next line>#define DATA_END
    \ \ \ \ ((char) 5)<format|next line>#define DATA_ESCAPE \ ((char)
    27)<format|next line><format|next line>static int counter= 0;<format|next
    line><format|next line>void<format|next line>next_input () {<format|next
    line> \ counter++;<format|next line> \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "channel:prompt" \<less\>\<less\> DATA_END;<format|next
    line> \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";<format|next line>}<format|next line><format|next
    line>int<format|next line>main () {<format|next line> \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ cout \<less\>\<less\> "Welcome to my test computer algebra system
    for TeXmacs\\n";<format|next line> \ cout \<less\>\<less\> "This software
    comes with no warranty whatsoever\\n";<format|next line> \ cout
    \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ next_input ();<format|next line> \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ fflush (stdout);<format|next
    line><format|next line> \ while (1) {<format|next line> \ \ \ char
    buffer[100];<format|next line> \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ if (strcmp (buffer, "quit") == 0)
    break;<format|next line> \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "verbatim:";<format|next line> \ \ \ cout
    \<less\>\<less\> "You typed " \<less\>\<less\> buffer \<less\>\<less\>
    "\\n";<format|next line><format|next line> \ \ \ cout \<less\>\<less\>
    "And now a LaTeX formula: ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:" \<less\>\<less\>
    "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;<format|next line> \ \ \ cout
    \<less\>\<less\> "\\n";<format|next line><format|next line> \ \ \ cout
    \<less\>\<less\> "And finally a fraction ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:" \<less\>\<less\>
    "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ cout \<less\>\<less\> ".\\n";<format|next line><format|next line>
    \ \ \ next_input ();<format|next line> \ \ \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ fflush (stdout);<format|next line>
    \ }<format|next line> \ return 0;<format|next line>}
  </verbatim>

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Sessione>>|<pageref|idx-2>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Studying the source code step by
      step><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      output><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>The complete
      listing><value|toc-dots><pageref|toc-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
