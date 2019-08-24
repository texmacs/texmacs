<TeXmacs|1.99.11>

<style|<tuple|tmdoc|german|old-spacing>>

<\body>
  <tmdoc-title|Mathematik-Konstrukte>

  <\explain>
    <explain-macro|left|large-delimiter>

    <explain-macro|left|large-delimiter|size>

    <explain-macro|left|large-delimiter|bottom|top>

    <explain-macro|mid|large-delimiter|<math|\<cdots\>>>

    <explain-macro|right|large-delimiter|<math|\<cdots\>>><explain-synopsis|Groÿe
    Klammern>
  <|explain>
    Diese Konstrukte erzeugen groÿe Klammern, wie z.B.:

    <\equation*>
      <around*|\<langle\>|<frac|1|a<rsub|1>><mid|\|><frac|1|a<rsub|2>><mid|\|>\<cdots\><mid|\|><frac|1|a<rsub|n>>|\<rangle\>>.
    </equation*>

    Zu einander passende rechte und linke Klammern werden in ihrer Gröÿe
    automatisch dem Inhalt angepasst. Dazwischen kann sich eine beliebige
    Anzahl rechter und linker mittlerer Klammern befinden, die in ähnlicher
    Weise angepasst werden. Anders als in <TeX> stehen rechte und linke
    Klammern nicht notwendiger Weise auf gleicher Höhe. Daher können auch
    Formeln wie

    <\equation*>
      f<around*|(|<frac|1|x+<frac|1|y+<frac|1|z>>>|)>
    </equation*>

    korrekt gesetzt werden. Der Nutzer kann die automatisch bestimmte Gröÿe
    anders einstellen, indem er zusätzliche Parameter wie <src-arg|size>,
    <src-arg|bottom> und <src-arg|top> festlegt. So wird aus

    <\tm-fragment>
      <inactive*|f<left|(|-8mm|4mm>x<mid|\||8mm>y<right|)|-4mm|8mm>>
    </tm-fragment>

    <\equation*>
      f<left|(|-8mm|4mm>x<mid|\||8mm>y<right|)|-4mm|8mm>
    </equation*>

    <src-arg|size> kann auch eine ganze Zahl <math|n> sein. In diesem Fall
    wird die <math|n>-te Gröÿe der vorhandenen Klammergröÿen verwendet. Z.B.

    <\tm-fragment>
      <inactive*|g<left|(|0><left|(|1><left|(|2><left|(|3>z<right|)|3><right|)|2><right|)|1><right|)|0>>
    </tm-fragment>

    liefert

    <\equation*>
      g<left|(|0><left|(|1><left|(|2><left|(|3>z<right|)|3><right|)|2><right|)|1><right|)|0>
    </equation*>
  </explain>

  \;

  <\explain>
    <explain-macro|big|big-symbol><explain-synopsis|groÿe Symbole>
  <|explain>
    Dieses Konstrukt erzeugt groÿe mathematische Symbole wie z.B. in

    <\equation>
      <label|big-example><big|sum><rsub|i=0><rsup|\<infty\>>a<rsub|i>*z<rsup|i>
    </equation>

    Die Gröÿe des Operators hängt davon ab, ob es sich um eine eigenständige
    Formel oder eine Formel in Flieÿtext handelt. Formeln wie
    \ (<reference|big-example>) heiÿen eigenständig im Gegensatz zu Formeln
    wie <math|<big|sum><rsub|i=0><rsup|\<infty\>>a<rsub|i>*z<rsup|i>>. Im
    Menü <menu|Format|Display style> können die Voreinstellungen angepasst
    werden.

    Beachten Sie, dass die Formel(<reference|big-example>) intern als\ 

    <\tm-fragment>
      <inactive*|<big|sum><rsub|i=0><rsup|\<infty\>>a<rsub|i>*z<rsup|i><big|.>>
    </tm-fragment>

    gespeichert wird.

    Das unsichtbare Konstrukt <inactive*|<big|.>> dient als Stoptag für den
    Starttag <inactive*|<big|sum>>.
  </explain>

  <\explain>
    <explain-macro|frac|num|den><explain-synopsis|Brüche>
  <|explain>
    Das <markup|frac>-Konstrukt erzeugt Brüche wie <math|<frac|x|y>>. In
    eigenständigen Formeln wird der Zähler <src-arg|num> und der Nenner
    <src-arg|den> in normaler Gröÿe dargestellt. Während des Schriftsetzens
    innerhalb des Zählers bzw. innerhalb des Nenners wird der für
    eigenständige Formeln benutzte Stil ausgeschaltet. Daher werden Argumente
    innerhalb eigenständiger Formeln in Indexgröÿe dargestellt, wie z.B. in\ 

    <\tm-fragment>
      <inactive*|<frac|1|a<rsub|0>+<frac|1|a<rsub|1>+<frac|1|a<rsub|2>+\<ddots\>>>>>
    </tm-fragment>

    das, wie folgt, aussieht:

    <\equation*>
      <frac|1|a<rsub|0>+<frac|1|a<rsub|1>+<frac|1|a<rsub|2>+\<ddots\>>>>
    </equation*>
  </explain>

  <\explain>
    <explain-macro|sqrt|content>

    <explain-macro|sqrt|content|n><explain-synopsis|Wurzeln>
  <|explain>
    <markup|sqrt> erzeugt Quadratwurzeln wie <math|<sqrt|x>> oder
    <src-arg|n>-te Wurzeln wie \ <math|<sqrt|x|3>>. Das Wurzelzeichen wird
    automatisch der Gröÿe des Inhalts, <src-arg|content>, angepasst:

    <\equation*>
      <sqrt|<frac|f<around|(|x|)>|y<rsup|2>+z<rsup|2>>|i+j>
    </equation*>
  </explain>

  <\explain>
    <explain-macro|lsub|script>

    <explain-macro|lsup|script>

    <explain-macro|rsub|script>

    <explain-macro|rsup|script><explain-synopsis|Indices>
  <|explain>
    Diese Konstrukte fügen Indices, <src-arg|script>, einer Box in einer
    horizontalen Verkettung hinzu und zwar rechte Indices an eine
    linksstehende Box und umgekehrt. Sie können aber auch allein stehen.
    Auÿerdem wird ein oberer Index mit einem unteren Index auf der selben
    Seite automatisch zusammengefasst. Dies

    <\tm-fragment>
      <inactive*|x<rsub|a><rsup|b>+<lsub|1><lsup|2>x<rsub|3><rsup|4>=y<rsub|1>+z<lsub|c>>
    </tm-fragment>

    liefert das

    <\equation*>
      x<rsub|a><rsup|b>+<lsub|1><lsup|2>x<rsub|3><rsup|4>=y<rsub|1>+z<lsub|c>
    </equation*>

    Wenn ein rechter Index an ein Symbol oder Konstrukt angebunden wird,
    welches untere bzw. obere Grenzen akzeptiert, dann wird der Index
    automatisch als Grenze interpretiert und entsprechend gesetzt:

    <\equation*>
      lim<rsub|n\<rightarrow\>\<infty\>>a<rsub|n>
    </equation*>

    Indices werden im Flieÿtext in einer kleineren Schriftgröÿe dargestellt,
    um die Lesbarkeit zu erhalten jedoch nicht kleiner als doppelte
    Indexgröÿe.\ 
  </explain>

  <\explain>
    <explain-macro|lprime|prime-symbols>

    <explain-macro|rprime|prime-symbols><explain-synopsis|hochgestellte
    Symbole>
  <|explain>
    Linke und rechte hochgestellte Symbole wie<math|f<rprime|'>> ähneln
    linken oder rechten oberen Indices. Sie benehmen sich aber anders, wenn
    sie editiert werden. Wenn ihr Cursor hinter dem hochgestellten Symbol in
    <math|f<rprime|'>> steht und sie die <key|Rücktaste> drücken, dann wird
    das hochgestellte Symbol entfernt. Wenn sie hinter dem oberen Index in
    <math|f<rsup|n>> stehen und die <key|Rücktaste> mehrmals drücken, dann
    bewegen sie sich zuerst in den Index, dann wird <math|n> entfernt und
    schlieÿlich der obere Index. Beachten Sie bitte, dass
    <src-arg|prime-symbols> eine Verkettung von Symbolen ist. So ist
    <math|f<rprime|'\<dag\>>> die Darstellung von
    <inactive*|f<rprime|'\<dag\>>>.
  </explain>

  <\explain>
    <explain-macro|below|content|script>

    <explain-macro|above|content|script><explain-synopsis|Symbole unter und
    über Text>
  <|explain>
    Die Konstrukte <markup|below> und <markup|above> erzeugen Symbole,
    <src-arg|script>, unter oder über einem Inhalt, <src-arg|content>. Beide
    können gleichzeitig verwendet werden wie in:

    <\equation*>
      <above|<below|xor|i=1>|\<infty\>> x<rsub|i>
    </equation*>

    das durch

    <\tm-fragment>
      <inactive*|<above|<below|xor|i=1>|\<infty\>> x<rsub|i>>
    </tm-fragment>
  </explain>

  erzeugt wird.

  <\explain>
    <explain-macro|wide|content|wide-symbol>

    <explain-macro|wide*|content|wide-symbol><explain-synopsis|breite
    Zeichen>
  <|explain>
    Diese Konstrukte erzeugen breite Zeichen über oder unter einem
    mathematischen Inhalt <src-arg|content>. Beispielsweise entspricht
    <math|<wide|x+y|\<bar\>>> \ <inactive*|<wide|x+y|\<bar\>>>.
  </explain>

  <\explain>
    <explain-macro|neg|content><explain-synopsis|Negationen>
  <|explain>
    Dieses Konstrukt erzeugt \ verneinten Inhalt wie
    <math|<neg|\<rightarrowtail\>>>, <math|<neg|a>> und <neg|abc>.
  </explain>

  <\explain>
    <explain-macro|tree|root|child-1|<math|\<cdots\>>|child-n><explain-synopsis|Bäume>
  <|explain>
    Dieses Konstrukt erzeugt einen Baum mit der Wurzel, <src-arg|root>, und
    Kinder <src-arg|child-1> bis <src-arg|child-n>. Er sollte rekursiv zur
    Erzeugung von Bäumen benutzt werden. Z.B. entspricht

    <\equation*>
      <tree|+|x|y|<tree|\<times\>|2|y|z>>
    </equation*>

    dem Konstrukt

    <\tm-fragment>
      <inactive*|<tree|+|x|y|<tree|\<times\>|2|y|z>>>
    </tm-fragment>

    Für die Zukunft planen wir weitere Stil-Parameter, um die Darstellung zu
    steuern.
  </explain>

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
    <associate|preamble|false>
  </collection>
</initial>