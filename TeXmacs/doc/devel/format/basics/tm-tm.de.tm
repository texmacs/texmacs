<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Normale Linearisierung>

  Dokumente werden in der Standard-<TeXmacs>-Sprache geschrieben. Dies
  entspricht den Datei-Endungen <verbatim|.tm> und <verbatim|.ts>. Die
  Standard-<TeXmacs>-Syntax ist relativ leicht zu verstehen und zu lesen, so
  dass der Inhalt eines Dokuments, ähnlich wie bei <LaTeX>, schon mit einem
  einfachen Texteditor verstanden werden kann. Beispielsweise wird die Formel

  <\tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </tm-fragment>

  durch

  <\quote-env>
    <framed-fragment|<verbatim|\<less\>with\|mode\|math\|x+y+\<less\>frac\|1\|2\<gtr\>+\<less\>sqrt\|y+z\<gtr\>\<gtr\>>>
  </quote-env>

  dargestellt.

  Dagegen ist die <TeXmacs>-Sprache, in der <TeXmacs>-Stile definiert werden,
  schwer zu lesen und nicht dazu gedacht, von Hand editiert zu werden: Die
  Semantik einer Sprache, mit der Leerräume (Weiÿraum) beschrieben werden
  kann, ist sehr komplex und schwer darzustellen. Deshalb sollte man es
  vermeiden, <TeXmacs>-Dokumente und vor allen Stil-Definitionen direkt von
  Hand zu editieren, es sei denn, man ist mit alle Details vertraut und weiÿ
  genau, was man tut.\ 

  <paragraph*|Das Prinzip der Linearisierung>

  Das <TeXmacs>-Format benutzt die Sonderzeichen <verbatim|\<less\>>,
  <verbatim|\|>, <verbatim|\<gtr\>>, <verbatim|\\> und <verbatim|/>, um Bäume
  zu linearisieren. Ein Baum wie

  <\equation>
    <label|gen-tree-tm><tree|f|x<rsub|1>|\<cdots\>|x<rsub|n>>
  </equation>

  wird linearisiert zu

  <\tm-fragment>
    <verbatim|\<less\>f\|x<rsub|1>\|...\|x<rsub|n>\<gtr\>>
  </tm-fragment>

  Wenn eines der Argumente \ <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>>
  aus mehreren Absätzen besteht, was in diesem Zusammenhang heiÿt, dass
  dieses Argument den Standard-Operator<markup| document> \ oder den
  Standard-Operator <markup|collection> enthält, dann wird eine andere Form
  der Linearisierung verwendet. Wenn beispielsweise <verbatim|f> nur
  Argumente mit mehreren Absätzen enthält, (Polyabsätze), dann würde der Baum
  (<reference|gen-tree-tm>) folgendermaÿen linearisiert werden:

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        \<less\>\\f\<gtr\>

        \ \ x<rsub|1>

        \<less\>\|f\<gtr\>

        \ \ ...

        \<less\>\|f\<gtr\>

        \ \ x<rsub|n>

        \<less\>/f\<gtr\>
      </verbatim>
    </with>
  </tm-fragment>

  Generell werden Argumente, die nicht Polyabsätze sind, in der Kurzform
  linearisiert und Polyabsätze in der Langform. Beispielsweise, wenn
  <verbatim|n=5> ist, und <verbatim|x<rsub|3>> sowie <verbatim|x<rsub|5>>
  Polyabsätze sind, \ <verbatim|x<rsub|1>,> <verbatim|x<rsub|2>> und
  <verbatim|x<rsub|4>> dagegen nicht, dann wird (<reference|gen-tree-tm>)
  linearisiert zu

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        \<less\>\\f\|x<rsub|1>\|x<rsub|2>\<gtr\>

        \ \ x<rsub|3>

        \<less\>\|f\|x<rsub|4>\<gtr\>

        \ \ x<rsub|5>

        \<less\>/f\<gtr\>
      </verbatim>
    </with>
  </tm-fragment>

  Die <em|Escape-Sequenzen> <verbatim|\\\<less\>>, <verbatim|\\\|>,
  <verbatim|\\\<gtr\>> und <verbatim|\\\\> können zur Darstellung der Zeichen
  <verbatim|\<less\>>, <verbatim|\|>, <verbatim|\<gtr\>> und <verbatim|\\>
  benutzt werden. Beispielsweise wird <with|mode|math|\<alpha\>+\<beta\>> zu
  <verbatim|\\\<less\>alpha\\\<gtr\>+\\\<less\>beta\\\<gtr\>> linearisiert.

  <paragraph*|Formatierung und Leerraum>

  Die Grundoperationen <markup|document> und <markup|concat> werden auf
  spezielle Weise linearisiert. Die Linearisierung der Grundoperation
  <markup|concat> besteht in der üblichen Verkettung von Zeichenketten.
  Beispielsweise wird der Text ``an <em|important> note'' linearisiert zu:

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        an \<less\>em\|important\<gtr\> note
      </verbatim>
    </with>
  </tm-fragment>

  Die Grundoperation <markup|document> wird linearisiert, indem Absätze durch
  zwei \R<translate|new line|english|german>``-Sonderzeichen, dies entspricht
  einer Leerzeile, getrennt werden. So wird das Zitat

  <\quote-env>
    Dies ist der erste Absatz.

    Das ist der zweite Absatz.
  </quote-env>

  zu

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        \<less\>\\quote-env\<gtr\>

        \ \ Dies ist der erste Absatz.

        \;

        \ \ Das ist der zweite Absatz.

        \<less\>/quote-env\<gtr\>
      </verbatim>
    </with>
  </tm-fragment>

  linearisiert.

  Beachten Sie bitte, dass Leerraum am Anfang und am Ende von Absätzen
  ignoriert wird. Innerhalb von Absätzen wird Leerraum beliebiger Länge als
  ein einziges Leerzeichen interpretiert. Entsprechend wird eine Folge von
  mehr als zwei \R<translate|new line|english|german>``-Zeichen als genau
  zwei \R<translate|new line|english|german>``-Zeichen interpretiert. Wenn
  also die linearisierte Form des obigen Beispiels folgendermaÿen auf der
  Festplatte gespeichert wäre:

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        \<less\>\\quote-env\<gtr\>

        \ \ Dies ist der \ \ \ \ \ \ \ \ \ \ erste Absatz.

        \;

        \;

        \ \ Das ist der zweite \ \ \ \ \ \ \ \ \ Absatz.

        \<less\>/quote-env\<gtr\>
      </verbatim>
    </with>
  </tm-fragment>

  würde das Zitat trotzdem unverändert dargestellt.

  Ein Leerzeichen kann explizit durch die Zeichenkombination \R<verbatim|\\
  >'' erzeugt werden und ein leerer Absatz durch \R<verbatim|\\;>''.

  <paragraph*|Rohdaten>

  Die Grundoperation <markup|raw-data> (Rohdaten) dient in <TeXmacs> zur
  Darstellung von Binärdaten wie beispielsweise Bilddateien innerhalb eines
  Dokuments. Solche Binärdaten werden folgendermaÿen linearisiert:

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        \<less\>#<em|binary-data>\<gtr\>
      </verbatim>
    </with>
  </tm-fragment>

  Darin ist \R<verbatim|<em|binary-data>>`` eine Zeichenkette von
  Hexadezimalzahlen, eine Kette von Bytes.

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
    <associate|preamble|false>
  </collection>
</initial>