<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Funktionelle Befehle>

  In den Menüs <menu|Source|Arithmetic>, <menu|Source|Text>,
  <menu|Source|Tuple> und <menu|Source|Condition>, die im Quellcode-Modus
  erreichbar sind, finden Sie eine Anzahl verschiedener Konstrukte zum
  Operieren mit Ganzzahlen, Zeichenketten, Tupel und boolschen Werten bzw.
  Variablen. Im folgenden finden sie ein Beipiel, in dem mit einem Makro
  <markup|new-important> ein neuer \Rimportant'' -Befehl definiert werden
  kann gleichzeitig mit einer Variante in Rot:

  <\tm-fragment>
    <inactive*|<assign|new-important|<macro|name|<quasi|<style-with|src-compact|none|<style-with|src-compact|none|<assign|<unquote|<arg|name>>|<macro|x|<with|font-series|bold|<arg|x>>>>><style-with|src-compact|none|<assign|<unquote|<merge|<arg|name>|-red>>|<macro|x|<with|font-series|bold|color|red|<arg|x>>>>>>>>>>
  </tm-fragment>

  Hier wurde das <markup|merge>-Konstrukt benutzt, um zwei Zeichenketten
  zusammenzufügen. Die verschiedenen berechnenden Konstrukte finden sich im
  Abschnitt <hyper-link|Funktionelle Operatoren|../../format/stylesheet/prim-functional.de.tm>
  von <hyper-link|Konstrukte für Stildefinitionen|../../format/stylesheet/stylesheet.de.tm>
  genauer erläutert.

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
  </collection>
</initial>