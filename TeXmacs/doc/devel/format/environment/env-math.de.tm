<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematischer Schriftsatz>

  <\explain>
    <label|math-level><var-val|math-level|0><explain-synopsis|Indexhöhe>
  <|explain>
    In geschachtelten Indizierungen (Potenzen) oder Brüchen kennzeichnet die
    <em|Indexhöhe>, <em|index level>, das Ausmaÿ der Verschachtelung. Sie
    nimmt innerhalb bestimmter mathematischer zu. Ein hoher Wert führt zur
    Darstellung in einer kleineren Schriftgröÿe. Das gilt allerdings nur bis
    zur Indexhöhe 2. Ab 2 bleibt die Gröÿe gleich, damit die Formeln lesbar
    bleiben:\ 

    <\equation*>
      \<mathe\><rsup|\<mathe\><rsup|\<mathe\><rsup|\<mathe\><rsup|x>>>>=<frac|1+<frac|1|x+\<mathe\><rsup|x>>|1+<frac|1|\<mathe\><rsup|x>+<frac|1|\<mathe\><rsup|\<mathe\><rsup|x>>>>>
    </equation*>

    Die Indexhöhe kann im Menü <menu|Format|Index level> manuell verändert
    werden, um beispielsweise <with|mode|math|x<rsup|y<rsup|z>>>so
    darzustellen

    <\equation*>
      x<rsup|<with|math-level|0|y<rsup|<with|math-level|0|z>>>>
    </equation*>

    <\tm-fragment>
      <with|mode|math|<inactive*|x<rsup|<with|math-level|0|y<rsup|<with|math-level|0|z>>>>>>
    </tm-fragment>

    anstatt so

    <\with|par-mode|center>
      <with|mode|math|<rsup|>x<rsup|y<rsup|z>>>
    </with>

    \;
  </explain>

  \;

  \;

  <\explain>
    <var-val|math-display|false><explain-synopsis|eigenständige Formel>
  <|explain>
    Diese Variable steuert den Stil der Formel nämlich, ob die Formel wie
    eine eigenständige Formel oder wie eine Formel im laufenden Text gesetzt
    wird. Eigenständige Formeln wie

    <\equation*>
      <frac|n|H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)>=<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>
    </equation*>

    sind anders gesetzt als solche Formeln
    <with|mode|math|<frac|n|H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)>=<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>>
    im laufenden Text. der Stil für eigenständige Formeln erzeugt weitere
    Abstände. Er wird in manchen mathematischen Konstrukten wie z.B. in
    Brüchen, Indices, Binomialkoeffizienten usw. automatisch abgeschaltet.
    Daher haben im folgenden Beispiel die Brüche im Nenner eine kleinere
    Schriftgröÿe:

    <\equation*>
      H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)=<frac|n|<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>>
    </equation*>

    Sie könne die Vorgaben im Menü <menu|Format|Display style> ändern.
  </explain>

  <\explain>
    <var-val|math-condensed|false><explain-synopsis|Dichte eigenständige
    Formeln>
  <|explain>
    Formeln wie <with|mode|math|a+\<cdots\>+z> werden mit weiten Abständen
    zum <with|mode|math|<op|+>> Symbol gesetzt. In Formeln mit Indices wie
    <with|mode|math|\<mathe\><rsup|a+\<cdots\>+z>+\<mathe\><rsup|\<alpha\>+\<cdots\>+\<zeta\>>>
    wird die Lesbarkeit erhöht, indem innerhalb der Indices kürzere Abstände
    verwendet werden. Das hilft, Symbole in Indices von den Basis-Zeichen zu
    unterscheiden ganz besonders dann, wenn die Indices sehr lang sind.
    Dieses vorgegebene Verhalten kann im Menü <menu|Format|Condensed>
    angepasst werden.
  </explain>

  <\explain>
    <var-val|math-vpos|0><explain-synopsis|Indexposition in Brüchen>
  <|explain>
    Um Brüche in hoher Qualität zu setzen, muss Vorsorge getroffen werden,
    dass untere Indices in Zählern nicht zu weit nach unten reichen, und dass
    obere Indices in Nennern nicht zu weit nach oben kommen. Deshalb gibt es
    in <TeXmacs> eine weitere Variable <src-var|math-vpos>, die den Wert 1 in
    Zählern annimmt und <with|mode|math|\<um\>1> in Nennern. Auÿerhalb von
    Brüchen bleibt sie 0. Das folgende Beispiel zeigt diesen Effekt. Die
    Unterschiede sind sehr gering aber im qualitativ hochwertigem Drucksatz
    sichtbar.

    <\equation*>
      <with|math-vpos|-1|<group|a<rsub|1><rsup|2>>>+<with|math-vpos|0|<group|a<rsub|1><rsup|2>>>+<with|math-vpos|1|<group|a<rsub|1><rsup|2>>>
    </equation*>

    <\tm-fragment>
      <compound|inactive*|<with|math-vpos|-1|<group|a<rsub|-1><rsup|2>>>+<with|math-vpos|0|<group|a<rsub|-1><rsup|2>>>+<with|math-vpos|1|<group|a<rsub|1><rsup|2>>>>
    </tm-fragment>

    Die Gruppierung ist in diesem Beispiel notwendig, damit sich die
    vertikalen Effekte auf die ganze Gruppe ausdehnen.
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
    <associate|language|german>
    <associate|preamble|false>
    <associate|sfactor|4>
  </collection>
</initial>