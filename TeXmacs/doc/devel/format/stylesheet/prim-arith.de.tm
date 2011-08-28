<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Arithmetische Operatoren>

  <\explain>
    <explain-macro|plus|expr-1|expr-2>

    <explain-macro|minus|expr-1|expr-2><explain-synopsis|Addition und
    Subtraktion>
  <|explain>
    Addiere oder subtrahiere zwei Zahlen oder Längen. Wenn <src-arg|expr-1>
    und <src-arg|expr-2> Längen sind, werden die Einheiten in die interne
    Längeneinheit umgerechnet. <inactive*|<plus|1|2.3>> ergibt also
    <plus|1|2.3> und <inactive*|<plus|1cm|5mm>> produziert <plus|1cm|5mm>.
  </explain>

  <\explain>
    <explain-macro|times|expr-1|expr-2><explain-synopsis|Multiplikation>
  <|explain>
    Multipliziere zwei Zahlen <src-arg|expr-1> und <src-arg|expr-2> oder
    multipliziere eine Zahl mit einer Länge. <inactive*|<times|3|3>>
    evaluiert zu <times|3|3> und <inactive*|<times|3|2cm>> zu <times|3|2cm>.
  </explain>

  <\explain>
    <explain-macro|over|expr-1|expr-2><explain-synopsis|Division>
  <|explain>
    Dividiere zwei Zahlen <src-arg|expr-1> und <src-arg|expr-2>, dividiere
    eine Länge durch eine Zahl oder dividiere zwei Längen.
    <inactive*|<over|1|3>> evaluiert zu <over|1|3>, <inactive*|<over|3cm|7>>
    zu <over|3cm|7> und <inactive*|<over|1cm|1pt>> zu <over|1cm|1pt>.
  </explain>

  <\explain>
    <explain-macro|div|expr-1|expr-2>

    <explain-macro|mod|expr-1|expr-2><explain-synopsis|Ganzzahlendivision>
  <|explain>
    <explain-macro|div|expr-1|expr-2> gibt das Ergebnis einer
    Ganzzahlendivision, das Modul <explain-macro|mod|expr-1|expr-2> den Rest.
    <inactive|<plus|<inactive|<times|<explain-macro|div|expr-1|expr-2>|<src-arg|expr-2>>>|<explain-macro|mod|expr-1|expr-2>>>
    evaluiert zu <src-arg|expr-1>, \ <inactive*|<div|18|7>> zu <div|18|7> und
    <inactive*|<mod|18|7>> zu <mod|18|7>.
  </explain>

  <\explain>
    <explain-macro|equal|expr-1|expr-2>

    <explain-macro|unequal|expr-1|expr-2>

    <explain-macro|less|expr-1|expr-2>

    <explain-macro|lesseq|expr-1|expr-2>

    <explain-macro|greater|expr-1|expr-2>

    <explain-macro|greatereq|expr-1|expr-2><explain-synopsis|Zahlen oder
    Längen vergleichen>
  <|explain>
    Gibt wahr (true) oder falsch (false) zurück für die Tests gleich?,
    ungleich?, kleiner?, kleiner-oder-gleich?, gröÿer und
    gröÿer-oder-gleich?. Z.B. <inactive*|<less|123|45>> liefert <less|123|45>
    und <inactive*|<less|123mm|45cm>> \ <less|123mm|45cm>.
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
  </collection>
</initial>