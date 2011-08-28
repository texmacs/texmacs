<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Operatoren für Tupel>

  <\explain>
    <explain-macro|tuple|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|Konstruktion
    eines Tupels>
  <|explain>
    Macht einen Tupel aus den Ausdrücken <src-arg|expr-1> bis
    <src-arg|expr-n>.
  </explain>

  <\explain>
    <explain-macro|is-tuple|expr><explain-synopsis|Ein Tuple?>
  <|explain>
    Testet, ob <src-arg|expr> zu einem Tuple evaluiert.
  </explain>

  <\explain>
    <explain-macro|length|expr><explain-synopsis|Dimension eines Tupels>
  <|explain>
    Wenn <src-arg|expr> ein Tuple ist, wird seinen Dimension ausgegeben.
    <inactive*|<length|<tuple|hop|hola>>> evaluiert zu
    <length|<tuple|hop|hola>>.
  </explain>

  <\explain>
    <explain-macro|look-up|tuple|which><explain-synopsis|Element eines Tupels
    extrahieren>
  <|explain>
    Gibt das Element an der Position <src-arg|which> (Ganzzahl mit 0) in dem
    Tupel <src-arg|tuple> zurück. Die Zählung der Elemente beginnt mir 0.
    <inactive*|<look-up|<tuple|a|b|c>|1>> gibt <look-up|<tuple|a|b|c>|1>.
  </explain>

  <\explain>
    <explain-macro|range|expr|start|end><explain-synopsis|Extrahiere ein
    Untertupel>
  <|explain>
    Gibt ein Untertupel von <src-arg|expr> zurück, welches an der Position
    <src-arg|start> beginnt und an der Position <src-arg|end> endet. Das
    Element an der Position <src-arg|end> \ ist nicht mehr dabei.
    <inactive*|<range|<tuple|a|hola|hop|b|c>|2|4>> evaluiert zu
    <range|<tuple|a|hola|hop|b|c>|2|4>.
  </explain>

  <\explain>
    <explain-macro|merge|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|Tupel
    verketten>
  <|explain>
    Mehrere Tupel \ <src-arg|expr-1> bis <src-arg|expr-n> werden unter Erhalt
    der Reihenfolge zu einem Tupel zusammengefasst.
    <inactive*|<merge|<tuple|1|2>|<tuple|3|4|5>>> ergibt
    <merge|<tuple|1|2>|<tuple|3|4|5>>.
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