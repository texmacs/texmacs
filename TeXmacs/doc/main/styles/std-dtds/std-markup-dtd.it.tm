<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Markup standard>

  Vari markup standard sono definiti in <tmdtd|std-markup>. I seguenti tag
  contestuali prendono tutti un solo argomento. La maggior parte può essere
  trovata nel menu <menu|Insert|Content tag>.

  <\explain|<markup|strong>>
    Indica una regione <strong|importante> del testo. Si può attivare questo
    tag attraverso <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<markup|em>>
    Enfatizza una regione del testo come in ``la cosa <em|vera>''. Questo tag
    corrisponde alla voce di menu <menu|Insert|Content tag|Emphasize>.
  </explain>

  <\explain|<markup|dfn>>
    Per le definizioni come ``uno <dfn|gnu> è una bestia con le corna''.
    Questo tag corrisponde a <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<markup|samp>>
    Una sequenza di caratteri letterali, come i caratteri <samp|ae> nella
    legatura æ. Si può attivare questo tag attraverso <menu|Insert|Content
    tag|Sample>.
  </explain>

  <\explain|<markup|name>>
    Il nome di una cosa particolare o un concetto come il sistema
    <name|Linux>. Questo ta si ottiene usando <menu|Insert|Content tag|Name>.
  </explain>

  <\explain|<markup|person>>
    Il nome di una persona come <name|Joris>. Questo tag corrisponde a
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<markup|cite*>>
    Una citazione bibliografica come un libro o una rivista. Per esempio,
    <cite*|Moby Dick> di Melville. Questo tag, che si ottieneusando
    <menu|Insert|Content tag|Cite>, non deve essere confuso con
    <markup|cite>. Quest'ultimo tag viene usato anche per le citazioni, ma
    dove l'argomento si riferisce a una voce in un database con riferimenti
    bibliografici.
  </explain>

  <\explain|<markup|abbr>>
    Un'abbreviazione. Esempio: io lavoro al <abbr|C.N.R.S.> Un'abbreviazione
    viene creata usando <menu|Insert|Content tag|Abbreviation> o la
    scorciatoia da tastiera <key|text a>.
  </explain>

  <\explain|<markup|acronym>>
    Un acronimo è un'abbreviazione formata dalla prima lettera di ciascuna
    parola in un nome o una frase, come <acronym|HTML> o <acronym|IBM>. In
    particolare, le lettere non sono separate da punti. Si può inserire un
    acronimo usando <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<markup|verbatim>>
    Testo verbatim come l'output di un programma informatico. Esempio: il
    programma restituisce <verbatim|hello>. Si può inserire del testo
    verbatim attraverso <menu|Insert|Content tag|Verbatim>. Il tag può anche
    essere usato come un ambiente per un testo con molti paragrafi.
  </explain>

  <\explain|<markup|kbd>>
    Testo che deve essere inserito da tastiera. Esempio: per favore premere
    <kbd|return>. Questo tag corrisponde alla voce di menu
    <menu|Insert|Content tag|Keyboard>.
  </explain>

  <\explain|<markup|code*>>
    Codice di un programma come in ``<code*|cout \<less\>\<less\> 1+1;>
    yields <verbatim|2>''. Questo si attiva usando <menu|Insert|Content
    tag|Code>. Per pezzi di codice più lunghi, si dovrebbe usare l'ambiente
    <markup|code>.
  </explain>

  <\explain|<markup|var>>
    Variabili in un programma informatico come in <verbatim|cp <var|src-file>
    <var|dest-file>>. Questo tag corrisponde alla voce di menu
    <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<markup|math>>
    Questo è un tag che verrà usato in futuro per inserire parti di carattere
    matematico nel testo regolare. Esempio: la formula <math|sin<rsup|2>
    x+cos<rsup|2> x=1> è ben nota.
  </explain>

  <\explain|<markup|op>>
    Questo tag può essere utilizzato nelle parti matematiche per specificare
    che un operatore deve essere considerato per se stesso, senza alcun
    argomento. Esempio: l'operazione <math|<op|+>> è una funzione da
    <math|\<bbb-R\><rsup|2>> in <math|\<bbb-R\>>. Questo tag potrebbe
    diventare obsoleto.
  </explain>

  <\explain|<markup|tt>>
    Questo è un tag fisico per per la fase di composizione tipografica. Esso
    è utilizzato per assicurare la compatibilità con il formato <name|HTML>,
    ma non ne raccomandiamo l'uso.
  </explain>

  I seguenti sono ambienti standard:

  <\explain|<markup|verbatim>>
    Descritto sopra.
  </explain>

  <\explain|<markup|code>>
    Analogo a <markup|code*>, ma per parti di codice con molte righe.
  </explain>

  <\explain|<markup|quote>>
    Ambiente per brevi (un paragrafo) citazioni.
  </explain>

  <\explain|<markup|quotation>>
    Ambiente per citazioni lunghe (molti paragrafi).
  </explain>

  <\explain|<markup|verse>>
    Ambiente per la poesia.
  </explain>

  <\explain|<markup|center>>
    Questo è un tag fisico per centrare una o più righe di testo. E'
    utilizzato per la compatibilità con il formato <name|HTML>, ma non ne
    raccomandiamo l'uso.
  </explain>

  Alcuni ambienti standard per le tabelle:

  <\explain|<markup|tabular*>>
    Tabelle centrate.
  </explain>

  <\explain|<markup|block>>
    Tabelle allineate a sinistra con un bordo standard di larghezza
    <verbatim|1ln>.
  </explain>

  <\explain|<markup|block*>>
    Tabelle centrate con un bordo di larghezza standard di <verbatim|1ln>.
  </explain>

  I seguenti tag non hanno alcun argomento:

  <\explain|<markup|TeXmacs>>
    Il logo <TeXmacs>.
  </explain>

  <\explain|<markup|TeX>>
    Il logo <TeX>.
  </explain>

  <\explain|<markup|LaTeX>>
    Il logo <LaTeX>.
  </explain>

  <\explain|<markup|hflush>>
    Utilizzato dagli sviluppatori per il flushing a destra nella definizione
    di ambienti.
  </explain>

  <\explain|<markup|hrule>>
    Una riga orizzontale come quella qui sotto:

    <hrule>
  </explain>

  I tag seguenti ammettono uno o più argomenti:

  <\explain|<markup|overline>>
    Per <overline|disegnare una linea sopra> il testo, che può estendersi su
    più righe.
  </explain>

  <\explain|<markup|underline>>
    Per <underline|sottolineare> il testo, che può estendersi su più righe.
  </explain>

  <\explain|<markup|fold>>
    Macro con due argomenti. Il primo argomento viene visualizzato e il
    secondo è ignorato: la macro corrisponde alla presentazione piegata di un
    pezzo del contenuto associato a un breve titolo o un abstract. Il secondo
    argomento si può rendere visibile usando <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<markup|unfold>>
    Macro con due argomenti <var|x> e <var|y>, che genera la presentazione
    non piegata di un pezzo di contenuto <var|y> associato a un breve titolo
    o abstract <var|x>. Il secondo argomento può essere reso invisibile
    usando <menu|Insert|Switch|Fold>.
  </explain>

  <\explain|<markup|switch>>
    Macro con due argomenti <var|x> e <var|y>, dove <var|y> è un insieme di
    possibili rappresentazioni dello switch e <var|x> la rappresentazione
    attuale. I tasti funzione <key|F9>, <key|F10>, <key|F11> e <key|F12>
    possono essere usati per lo switch tra rappresentazioni diverse.
  </explain>

  <\explain|<markup|phantom>>
    Funzione con un argomento <var|x>. Questo tag permette di ottenere tanto
    spazio quanto ne occuperebbe l'argomento <var|x>, ma <var|x> non viene
    visualizzato. Per esempio, il testo ``phantom'' come argomento di
    <markup|phantom> produce ``<phantom|phantom>''.
  </explain>

  <\explain|<markup|set-header>>
    Funzione con un argomento per modificare in modo permanente
    l'intestazione. Si noti che alcuni tag nel file di stile, come i tag di
    sezione, possono sovrascrivere queste modifiche manuali.
  </explain>

  <\explain|<markup|set-footer>>
    Funzione con un argomento per modificare in maniera permanente il piè di
    pagina.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia Gecchelin|Andrea
  Centomo>

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