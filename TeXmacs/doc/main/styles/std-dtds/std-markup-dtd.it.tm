<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Markup standard>

  Vari markup standard sono definiti in <tmdtd|std-markup>. I seguenti tag
  contestuali prendono tutti un solo argomento. La maggior parte può essere
  trovata nel menu <apply|menu|Insert|Content tag>.

  <\description>
    <expand|item*|<markup|strong>>Indica una regione <strong|importante> del
    testo. Si può attivare questo tag attraverso <apply|menu|Insert|Content
    tag|Strong>.

    <expand|item*|<markup|em>>Enfatizza una regione del testo come in ``la
    cosa <em|vera>''. Questo tag corrisponde alla voce di menu
    <apply|menu|Insert|Content tag|Emphasize>.

    <expand|item*|<markup|dfn>>Per le definizioni come ``uno <dfn|gnu> è una
    bestia con le corna''. Questo tag corrisponde a <apply|menu|Insert|Content
    tag|Definition>.

    <expand|item*|<markup|samp>>Una sequenza di caratteri letterali, come i
    caratteri <samp|ae> nella legatura æ. Si può attivare questo tag
    attraverso <apply|menu|Insert|Content tag|Sample>.

    <expand|item*|<markup|name>>Il nome di una cosa particolare o un concetto
    come il sistema <name|Linux>. Questo ta si ottiene usando
    <apply|menu|Insert|Content tag|Name>.

    <expand|item*|<markup|person>>Il nome di una persona come <name|Joris>.
    Questo tag corrisponde a <apply|menu|Insert|Content tag|Person>.

    <expand|item*|<markup|cite*>>Una citazione bibliografica come un libro o
    una rivista. Per esempio, <expand|cite*|Moby Dick> di Melville. Questo
    tag, che si ottieneusando <apply|menu|Insert|Content tag|Cite>, non deve
    essere confuso con <markup|cite>. Quest'ultimo tag viene usato anche per
    le citazioni, ma dove l'argomento si riferisce a una voce in un database
    con riferimenti bibliografici.

    <expand|item*|<markup|abbr>>Un'abbreviazione. Esempio: io lavoro al
    <abbr|C.N.R.S.> Un'abbreviazione viene creata usando
    <apply|menu|Insert|Content tag|Abbreviation> o la scorciatoia da tastiera
    <key|text a>.

    <expand|item*|<markup|acronym>>Un acronimo è un'abbreviazione formata
    dalla prima lettera di ciascuna parola in un nome o una frase, come
    <acronym|HTML> o <acronym|IBM>. In particolare, le lettere non sono
    separate da punti. Si può inserire un acronimo usando
    <apply|menu|Insert|Content tag|Acronym>.

    <expand|item*|<markup|verbatim>>Testo verbatim come l'output di un
    programma informatico. Esempio: il programma restituisce
    <verbatim|hello>. Si può inserire del testo verbatim attraverso
    <apply|menu|Insert|Content tag|Verbatim>. Il tag può anche essere usato
    come un ambiente per un testo con molti paragrafi.

    <expand|item*|<markup|kbd>>Testo che deve essere inserito da tastiera.
    Esempio: per favore premere <kbd|return>. Questo tag corrisponde alla
    voce di menu <apply|menu|Insert|Content tag|Keyboard>.

    <expand|item*|<markup|code*>>Codice di un programma come in
    ``<expand|code*|cout \<less\>\<less\> 1+1;> yields <verbatim|2>''. Questo
    si attiva usando <apply|menu|Insert|Content tag|Code>. Per pezzi di codice
    più lunghi, si dovrebbe usare l'ambiente <markup|code>.

    <expand|item*|<markup|var>>Variabili in un programma informatico come in
    <verbatim|cp <var|src-file> <var|dest-file>>. Questo tag corrisponde alla
    voce di menu <apply|menu|Insert|Content tag|Variable>.

    <expand|item*|<markup|math>>Questo è un tag che verrà usato in futuro per
    inserire parti di carattere matematico nel testo regolare. Esempio: la
    formula <math|sin<rsup|2> x+cos<rsup|2> x=1> è ben nota.

    <expand|item*|<markup|op>>Questo tag può essere utilizzato nelle parti
    matematiche per specificare che un operatore deve essere considerato per
    se stesso, senza alcun argomento. Esempio: l'operazione <math|<op|+>> è
    una funzione da <with|mode|math|\<bbb-R\><rsup|2>> in
    <with|mode|math|\<bbb-R\>>. Questo tag potrebbe diventare obsoleto.

    <expand|item*|<markup|tt>>Questo è un tag fisico per per la fase di
    composizione tipografica. Esso è utilizzato per assicurare la
    compatibilità con il formato <name|HTML>, ma non ne raccomandiamo l'uso.
  </description>

  I seguenti sono ambienti standard:

  <\description>
    <expand|item*|<markup|verbatim>>Descritto sopra.

    <expand|item*|<markup|code>>Analogo a <markup|code*>, ma per parti di
    codice con molte righe.

    <expand|item*|<markup|quote>>Ambiente per brevi (un paragrafo) citazioni.

    <expand|item*|<markup|quotation>>Ambiente per citazioni lunghe (molti
    paragrafi).

    <expand|item*|<markup|verse>>Ambiente per la poesia.

    <expand|item*|<markup|center>>Questo è un tag fisico per centrare una o
    più righe di testo. E' utilizzato per la compatibilità con il formato
    <name|HTML>, ma non ne raccomandiamo l'uso.
  </description>

  Alcuni ambienti standard per le tabelle:

  <\description>
    <expand|item*|<markup|tabular*>>Tabelle centrate.

    <expand|item*|<markup|block>>Tabelle allineate a sinistra con un bordo
    standard di larghezza <verbatim|1ln>.

    <expand|item*|<markup|block*>>Tabelle centrate con un bordo di larghezza
    standard di <verbatim|1ln>.
  </description>

  I seguenti tag non hanno alcun argomento:

  <\description>
    <expand|item*|<markup|TeXmacs>>Il logo <TeXmacs>.

    <expand|item*|<markup|TeX>>Il logo <TeX>.

    <expand|item*|<markup|LaTeX>>Il logo <LaTeX>.

    <expand|item*|<markup|hflush>>Utilizzato dagli sviluppatori per il
    flushing a destra nella definizione di ambienti.

    <expand|item*|<markup|hrule>>Una riga orizzontale come quella qui sotto:

    <value|hrule>
  </description>

  I tag seguenti ammettono uno o più argomenti:

  <\description>
    <expand|item*|<markup|overline>>Per <overline|disegnare una linea sopra>
    il testo, che può estendersi su più righe.

    <expand|item*|<markup|underline>>Per <underline|sottolineare> il testo,
    che può estendersi su più righe.

    <expand|item*|<markup|fold>>Macro con due argomenti. Il primo argomento
    viene visualizzato e il secondo è ignorato: la macro corrisponde alla
    presentazione piegata di un pezzo del contenuto associato a un breve
    titolo o un abstract. Il secondo argomento si può rendere visibile usando
    <apply|menu|Insert|Switch|Unfold>.

    <expand|item*|<markup|unfold>>Macro con due argomenti <var|x> e <var|y>,
    che genera la presentazione non piegata di un pezzo di contenuto <var|y>
    associato a un breve titolo o abstract <var|x>. Il secondo argomento può
    essere reso invisibile usando <apply|menu|Insert|Switch|Fold>.

    <expand|item*|<markup|switch>>Macro con due argomenti <var|x> e <var|y>,
    dove <var|y> è un insieme di possibili rappresentazioni dello switch e
    <var|x> la rappresentazione attuale. I tasti funzione <key|F9>,
    <key|F10>, <key|F11> e <key|F12> possono essere usati per lo switch tra
    rappresentazioni diverse.

    <expand|item*|<markup|phantom>>Funzione con un argomento <var|x>. Questo
    tag permette di ottenere tanto spazio quanto ne occuperebbe l'argomento
    <var|x>, ma <var|x> non viene visualizzato. Per esempio, il testo
    ``phantom'' come argomento di <markup|phantom> produce
    ``<apply|phantom|phantom>''.

    <expand|item*|<markup|set-header>>Funzione con un argomento per
    modificare in modo permanente l'intestazione. Si noti che alcuni tag nel
    file di stile, come i tag di sezione, possono sovrascrivere queste
    modifiche manuali.

    <expand|item*|<markup|set-footer>>Funzione con un argomento per
    modificare in maniera permanente il piè di pagina.
  </description>

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
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-50|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-51|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-52|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-53|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-54|<tuple|<uninit>|?>>
    <associate|idx-55|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-56|<tuple|<uninit>|?>>
    <associate|idx-46|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-57|<tuple|<uninit>|?>>
    <associate|idx-47|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-58|<tuple|<uninit>|?>>
    <associate|idx-48|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-59|<tuple|<uninit>|?>>
    <associate|idx-49|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-markup>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|strong>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Enfatizzato>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Enfatizza>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dfn>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Definizione>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|samp>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Esempio>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|name>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Nome>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|person>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Persona>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite*>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Citazione>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|abbr>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Abbreviazione>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|acronym>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Acronimo>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font family|<quote|ss>|Testo
      semplice>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Codice>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|var>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Content tag>|<with|font
      family|<quote|ss>|Variabile>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|math>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|op>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tt>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quote>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quotation>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verse>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|center>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular*>>|<pageref|idx-41>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-42>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block*>>|<pageref|idx-43>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeXmacs>>|<pageref|idx-44>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeX>>|<pageref|idx-45>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|LaTeX>>|<pageref|idx-46>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hflush>>|<pageref|idx-47>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hrule>>|<pageref|idx-48>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|overline>>|<pageref|idx-49>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|underline>>|<pageref|idx-50>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|fold>>|<pageref|idx-51>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Switch>|<with|font
      family|<quote|ss>|Unfold>>|<pageref|idx-52>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|unfold>>|<pageref|idx-53>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Switch>|<with|font
      family|<quote|ss>|Arrotola>>|<pageref|idx-54>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|switch>>|<pageref|idx-55>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-56>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-57>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-header>>|<pageref|idx-58>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-footer>>|<pageref|idx-59>>
    </associate>
  </collection>
</auxiliary>
