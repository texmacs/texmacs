<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Formatier-Konstrukte>

  Dieser Abschnitt enthält einige wichtige Anmerkungen über
  Formatier-Konstrukte, die nicht wirklich Teil der Stil-Definitions-Sprache
  sind, aber damit in einem engen Zusammenhang stehen.

  Erstens gehören fast alle <TeXmacs>-Makro-Befehle, die die Darstellung von
  Text betreffen, in eine von zwei Klassen. Es handelt sich entweder um
  Flieÿtext-Konstrukte oder um Block-Konstrukte, deren Argument also
  <em|Flieÿtext> oder ein <em|Block> ist. Der Unterschied wird in
  <hlink|Basis-Konstrukte|../../format/regular/prim-fundamental.de.tm>
  genauer erklärt. Ein <em|Block> ist dabei ein Textelement, dass aus
  mehreren Absätzen bestehenden Text, <em|Multi-Absatz-Text>, enthält.
  Dagegen kann Flieÿtext zwar aus mehreren Textelementen bestehen, enhält
  selbst aber keine weiteren Absätze. Beispielsweise ist <markup|frac> ein
  typisches Flieÿtext-Konstrukt, wohingegen
  <markup|<translate|theorem|english|german>> ein typisches Blockkonstrukt
  ist. Einige Konstrukte richten sich nach ihren Argumenten, sie sind
  Flieÿtext-Konstrukte, wenn ihr Argument Flieÿtext oder Flieÿtext-Konstrukte
  sind bzw. Blockkonstrukte, wenn ihr Argument ein Block oder ein
  Blockkonstrukt ist. Ein Beipiel dafür ist
  <markup|<translate|strong|english|german>>. Wenn man Makros schreibt, ist
  es sehr wichtig, dass man sich darüber im Klaren ist, ob ein Konstrukt ein
  Flieÿtext-- oder ein Blockkonstrukt ist, denn Blockkonstrukte innerhalb
  einer horizontalen Verkettung werden nicht richtig dargestellt. Wenn man
  ein Blockkonstrukt mit Flieÿtext umgeben muÿ, dann muÿ man das
  <markup|surround><markup|>-Konstrukt einsetzen:

  <\tm-fragment>
    <inactive*|<assign|mein-theorem|<macro|Rumpf|<surround|<no-indent><with|font-series|bold|<active*|<translate|theorem|english|german>>>|<right-flush>|<arg|Rumpf>>>>>
  </tm-fragment>

  In diesem Beispiel wurde der Rumpf mit dem
  <strong|<translate|bold|english|german>> dargestellten Text
  \R<translate|theorem|english|german>'' links und einem
  \R<translate|rigth-flush|english|german>'' rechts umgeben (surround).
  Dieser Flushing \R<translate|rigth-flush|english|german>'' dient zur
  besseren Darstellung der blauen Hilfslinien, wenn der Cursor sich innerhalb
  des Wirkungsbereichs eines Kontextes befindet. Eer dehnt den
  Wirkungsbereich (für die Hilfslinien) bis an den rechten Rand aus.

  In den meisten Fällen findet <TeXmacs> selbständig heraus, welche Befehle
  Zeilen-Befehle und welche Block-Befehle sind. Manchmal möchte man aber
  einen Befehl zwangsweise zum Block-Befehl erklären. Beispielsweise kann ein
  Konstrukt <markup|sehr-wichtig>, das folgendermaÿen definiert ist:

  <\tm-fragment>
    <inactive*|<assign|sehr-wichtig|<macro|body|<with|font-series|bold|color|red|<arg|body>>>>>
  </tm-fragment>

  sowohl als Block-Befehl als auch als Zeile-Befehl dienen. Wenn Sie nun den
  Cursor genau vor den <markup|with>-Tag plazieren und \ <key|Wagenrücklauf>
  mit nachfolgender \ <key|Rücktaste> eingeben, dann erhalten Sie:

  <\tm-fragment>
    <inactive*|<assign|sehr-wichtig|<\macro|body>
      <with|font-series|bold|color|red|<arg|body>>
    </macro>>>
  </tm-fragment>

  Weil der Rumpf, <em|<with|color|red|body<em|>>> nun ein Block ist, wird das
  Makro <markup|sehr-wichtig> automatisch zu einem Block-Befehl. In Zukunft
  wird sich mit dem <markup|drd-props>-Konstrukt noch besser steuern lassen,
  welche Befehle Zeilen-Befehle und welche Block-Befehle sind.

  Eine weitere wichtige Eigenschaft von Tags ist es, ob sie normalen Text als
  Inhalt haben, oder ob der Inhalt tabellarisch ist. Nehmen wir einmal die
  folgende Definition eines Standard-Formel-Blocks, \ <markup|eqnarray*>,
  allerdings ohne einen Teil der Darstellungs-Tags:

  <\tm-fragment>
    <inactive*|<assign|eqnarray*|<macro|body|<with|par-mode|center|mode|math|math-display|true|par-sep|0.45fn|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<arg|body>>>>>>>
  </tm-fragment>

  Die Verwendung von <markup|surround> zeigt, dass <markup|eqnarray*> ein
  Block-Befehl ist und die Verwendung von <markup|tformat> spezifiziert ein
  Tabellen-Konstrukt. Auÿerdem wurden <markup|twith> und <markup|cwith>
  benutzt, um zusätzliche Formatierinformationen festzulegen. Weil es sich um
  einen Block-Kontext handelt, ermöglichen wir Zeilenumbruch und lassen für
  die Tabelle die gesamte Absatzbreite zu (freier Leerraum wird auf die erste
  und letzte Spalte gleich verteilt). Auÿerdem haben wir festgelegt, dass es
  genau drei Spalten gibt.

  Schlieÿlich sollte man daran denken, dass Stildefinitionen nicht nur die
  Darstellung des fertigen Dokuments festlegen, sonder dass sie auch die
  Darstellung während der Erstellung regeln. Oben haben wir bereits den
  <markup|right-flush>-Befehl erwähnt, der die Darstellung von sichtbaren
  Hinweisen von sichtbaren Inhalten verbessert. Ganz ähnlich kann auf
  unsichtbarere Inhalte durch Flags hingewiesen werden:

  <\tm-fragment>
    <inactive*|<assign|labeled-theorem|<macro|id|body|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<no-indent><flag|Id:
    <arg|id>|blue|id><with|font-series|bold|Theorem.
    >>|<right-flush>|<arg|body>>>>>>
  </tm-fragment>

  ganz allgemein kann mit dem <markup|specific>-Befehl, dem als erstes
  Argument \Rscreen'' übergeben wird, ein sichtbarer Hinweis eingefügt
  werden, der im Druck nicht erscheint.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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