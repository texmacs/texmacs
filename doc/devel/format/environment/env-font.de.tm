<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Festlegung der aktuellen Schriftart>

  In diesem Abschnitt werden wir die Kontextvariablen beschreiben, die die
  Darstellung von Schriften steuern. Vier Parameter bestimmen die Schrift
  (<translate|name|english|german>, <translate|variant|english|german>,
  <translate|series|english|german>, <translate|shape|english|german>).
  Einige Kontextvariablen steuern das Verhalten unabhängig von diesen
  Parametern, während andere für davon abhängig sind. Schrift-Eigenschaften
  können global über das Menü \ <menu|Document|Font> und lokal über das Menü
  <menu|Format|Font> gesetzt werden.

  Aus einem abstrakten Gesichtspunkt ist eine Schriftart eine in sich
  konsistente graphische Darstellungsweise von Zeichen wie z.B. \ \Rx'',
  \Rffi'', \R<with|mode|math|\<alpha\>>'',
  \R<with|mode|math|<op|<big|sum>><big|.>>'', <abbr|usw..> Wenn eine
  Zeichenkette dargestellt werden soll, dann wird sie zuerst in Zeichen
  zerlegt, um z.B. Ligaturen wie fi, fl, ff, ffi, ffl zu berücksichtigen.
  Dann werden die einzelnen Zeichen positioniert, wobei die individuellen
  Eigenschaften der einzelnen Zeichen berücksichtigte werden. So wird z.B. in
  \Rxo'' das Zeichen \Ro'' ein wenig nach links gerückt, um das \RLoch'' in
  \Rx'' zu berücksichtigen. Im Fall von mathematischen Schriftarten stellt
  \ <TeXmacs> eine kohärente Darstellung von gröÿenveränderlichen Zeichen
  bereit, wie z.B. Klammern:\ 

  <\equation*>
    <left|(|0><left|(|1><left|(|2><right|)|2><right|)|1><right|)|0>.
  </equation*>

  Eine Schriftfamilie ist eine Anzahl von Schriftvarianten mit verschiedenen
  Charakteristiken wie Schriftstärke, Neigung usw., die aber alle gemeinsame,
  in sich konsistente typographische Regeln besitzen. Die Schriftarten einer
  Familie passen gut zusammen und werden deshalb im gemeinsam im gleichen
  Dokument verwendet. Dabei hat oft jede Schriftart seine spezielle Aufgabe.
  So passen die Schriftarten der Familie \RRoman'' z.B. die Varianten
  <with|font-series|bold|fett> und <with|font-shape|italic|italic> gut
  zueinander, während die Schriftart <with|font|avant-garde|Avant Garde>
  nicht dazu passt.

  <\remark>
    Für die Zukunft planen wir die Variablen
    <translate|variant|english|german> und <translate|shape|english|german>
    durch eine gröÿere Zahl von Variablen zu ersetzen, um Eigenschaften wie
    Neigung, Serifen, Kapitälchen usw. individuell zu steuern. Es ist
    auÿerdem geplant Unicode Schriftarten, möglicherweise mit zusätzlichen
    mathematischen Zeichen, zu verwenden. Dies sollte automatisch zu
    landesspezifisch korrekten Schriften führen, so dass z.B. kyrillische
    Schriftzeichen in russischen Texten zur Verfügung stehen.
  </remark>

  <\explain>
    <var-val|font|roman>

    <var-val|math-font|roman>

    <var-val|prog-font|roman><explain-synopsis|Schrift-Name>
  <|explain>
    Diese Variablen setzen die Schriftfamilie. Beispiele sind:

    <\tm-fragment>
      <with|font|roman|Roman>, <with|font|pandora|Pandora>,
      <with|font|chancery|Chancery>, <with|font|palatino|Palatino>
    </tm-fragment>

    Genauso unterstützt <TeXmacs> verschiedenen mathematische Schriftarten:

    <\tm-fragment>
      Roman: <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>

      Adobe: <with|mode|math|<with|math-font|adobe|a<rsup|2>+b<rsup|2>=c<rsup|2>>>

      New roman: <with|mode|math|<with|math-font|ENR|a<rsup|2>+b<rsup|2>=c<rsup|2>>>

      Concrete: <with|mode|math|<with|math-font|concrete|a<rsup|2>+b<rsup|2>=c<rsup|2>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-family|rm>

    <var-val|math-font-family|mr>

    <var-val|prog-font-family|tt><explain-synopsis|Schriftvariante>
  <|explain>
    Diese Variable wählt eine Variante aus der Schriftfamilie aus, wie z.B:
    Sans-Serif, Schreibmaschine usw.. Wie bereits erklärt, passen Varianten
    einer Schriftfamilie gut zu einander. Aber nicht alle Schriftfamilien
    haben alle möglichen Varianten. Wenn eine Variante gewählt wird, die
    nicht vorhanden ist, dann versucht <TeXmacs> eine passende Alternative zu
    finden.

    Typisch Varianten für Text-Schriftarten sind \ <verbatim|rm> (Roman),
    <verbatim|tt> (Schreibmaschine) und <verbatim|ss> (Sans- Serif):

    <\tm-fragment>
      Roman, <with|font-family|tt|Schreibmaschine> und
      <with|font-family|ss|Sans-Serif>
    </tm-fragment>

    Die Schriftvarianten des Mathematik-Modus \ <verbatim|mr> (Roman),
    <verbatim|mt> (Schreibmaschine) und <verbatim|ms> (Sans-Serif)
    unterscheiden sich von ihren Text-Entsprechungen <verbatim|rm> (Roman),
    usw.. In der Mathematik-Variante haben Variablen und Operatoren in der
    Regel unterschiedliche Neigungen, was in der Textversion fehlt.

    <\tm-fragment>
      <verbatim|ms>: <with|mode|math|<with|math-font-family|ms|sin (x+y)=sin
      x*cos y+cos x*sin y>>

      <verbatim|ss>: <with|mode|math|<with|math-font-family|ss|sin (x+y)=sin
      x*cos y+cos x*sin y>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-series|medium>

    <var-val|math-font-series|medium>

    <var-val|prog-font-series|medium><explain-synopsis|Schriftstärke>
  <|explain>
    Diese Kontextvariablen regeln die Schriftstärke. Mögliche Werte sind:
    light, medium, bold (mager, mittel, fett). Die meisten Schriften besitzen
    nur die Varianten mittel und fett.

    <\tm-fragment>
      medium, <with|font-series|bold|bold>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-shape|right>

    <var-val|math-font-shape|normal>

    <var-val|prog-font-shape|right><explain-synopsis|Schriftform>
  <|explain>
    Diese Parameter bestimmen die <translate|shape|english|german>, d.h.
    solche Eigenschaften wie Neigung, Kapitälchen, Proportionalschrift usw.,
    wie in den folgenden Beispielen:\ 

    <\tm-fragment>
      <with|font-shape|right|<translate|upright|english|german>> = right,
      <with|font-shape|slanted|<translate|slanted|english|german>> = slanted,
      <with|font-shape|italic|<translate|italic|english|german>> = italic,
      <with|font-shape|left-slanted|<translate|left slanted|english|german>>
      = left-slanted, <with|font-shape|small-caps|<translate|small
      capitals|english|german>> = small-caps,
      <with|font-shape|proportional|<with|font-family|tt|<translate|proportional
      typewriter|english|german>>> = proportional,
      <with|font-shape|condensed|<with|font-series|bold|<translate|bold|english|german><space|1spc><translate|condensed|english|german>>>
      = condensed, <with|font-shape|flat|<with|font-family|ss|<translate|sans
      serif|english|german><space|1spc><translate|flat|english|german>>> =
      flat, <with|font-shape|long|<translate|long|english|german>> = long
    </tm-fragment>

    \;
  </explain>

  <\explain>
    <label|font-base-size><var-val|font-base-size|10><explain-synopsis|Basis-Schriftgröÿe>
  <|explain>
    Die Basis-Schriftgröÿe wird in <hyper-link|<verbatim|Punkten,
    pt>|../basics/lengths.en.tm> festgelegt und ist normalerweise für das
    ganze Dokument fest eingestellt. Üblicherweise ist die Basisgröÿe
    <verbatim|9pt>, <verbatim|10pt>, <verbatim|11pt> oder <verbatim|12pt>.
    Andere Gröÿen werden normalerweise durch Festlegung des
    <hyper-link|<src-var|Vergröÿerungsfaktor>|env-general.de.tm#Vergröÿerung>
    oder des Gröÿenverhältnisses <hyper-link|font-size|#font-size> erzeugt.

    <\tm-fragment>
      <with|font-base-size|9|9pt>, <with|font-base-size|10|10pt>,
      <with|font-base-size|11|11pt>, <with|font-base-size|12|12pt>
    </tm-fragment>
  </explain>

  <\explain>
    <label|font-size><var-val|font-size|1><explain-synopsis|Schriftgröÿe>
  <|explain>
    Die aktuelle Schriftgröÿe wird aus der Basis-Schriftgröÿe
    <src-var|font-base-size> durch Multiplikation mit dem Gröÿenverhältnis
    <src-var|font-size> ermittelt. Die folgenden Standard-Schriftgröÿen sind
    im Menü <menu|Format|Size> einzustellen:

    <big-table|<descriptive-table|<tformat|<cwith|4|5|1|4|cell-bsep|0.25fn>|<cwith|4|5|1|4|cell-tsep|0.25fn>|<table|<row|<cell|Schriftgröÿe>|<cell|Multiplikator>|<cell|Schriftgröÿe>|<cell|Multiplikator>>|<row|<cell|<with|font-size|0.59|Tiny>>|<cell|0.59>|<cell|<with|font-size|0.71|Very
    small>>|<cell|0.71>>|<row|<cell|<with|font-size|0.84|Small>>|<cell|0.84>|<cell|<with|font-size|1|Normal>>|<cell|1>>|<row|<cell|<with|font-size|1.19|Large>>|<cell|1.19>|<cell|<with|font-size|1.41|Very
    large>>|<cell|1.41>>|<row|<cell|<with|font-size|1.68|Huge>>|<cell|1.68>|<cell|<with|font-size|2|Really
    huge>>|<cell|2>>>>>|Standard Schriftgröÿen.>

    Die Multiplikatoren bilden eine geometrische Folge mit dem Faktor
    <no-break><with|mode|math|<sqrt|2|4>>. Beachten Sie bitte, dass die
    Schriftgröÿe auch noch von der Kontextvariablen <hyper-link|index
    level|env-math.en.tm#math-level> abhängt.
  </explain>

  <\explain>
    <var-val|dpi|600><explain-synopsis|Auflösung>
  <|explain>
    Die Auflösung von Rasterschriftarten (auch Typ 3 fonts genannt), wie sie
    beispielsweise von dem <name|Metafont> Programm erzeugt werden, ist
    abhängig von der Präzision der Rasterung in Punkten pro Zoll, dots per
    inch, dpi. Heutzutage liefern die meisten Laserdrucker eine Auflösung von
    wenigstens <verbatim|600 dpi>, was auch die Vorgabe für <TeXmacs> ist.
    Für professionellen Hochqualitätsdruck werden heutzutage meist
    <verbatim|1200 dpi> benutzt. Die Auflösung wird normalerweise nur einmal
    für das gesamte Dokument eingestellt.
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
  </collection>
</initial>