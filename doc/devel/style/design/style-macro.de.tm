<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Makro Expansion>

  Das Interessanteste an der <TeXmacs>-Stil-Definitions-Sprache ist die
  Möglichkeit, Makros selbst zu definieren. Dabei gibt es drei verschiedene
  Makro-Typen: gewöhnliche Makros, Makros mir einer unbestimmten Anzahl von
  Argumenten und externe Makros, die durch \ <name|Scheme> oder ein Plug-In
  ausgewertet werden. Die Makro-bezogenen Konstrukte sind über das
  <menu|Source|Macro>-Menü erreichbar. Anschlieÿend werden wir nur die
  gewöhnlichen Makros beschreiben. Weitere Details finden Sie
  <hyper-link|hier|../../format/stylesheet/prim-macro.de.tm>.

  Gewöhnliche Makros werden üblicherweise mit einer Zuweisung (<em|assign>)
  definiert:

  <\tm-fragment>
    <inactive*|<assign|mein-makro|<macro|<active*|x<rsub|1>>|<active*|<with|mode|math|\<cdots\>>>|<active*|x<rsub|n>>|Rumpf>>>
  </tm-fragment>

  Nach solch einer Zuweisung wird <markup|mein-makro> ein neues Konstrukt mit
  <with|mode|math|n> Argumenten, das mit:

  <\tm-fragment>
    <inactive|><inactive|<compound|mein-makro|y1|...|<active*|y<rsub|n>>>>
  </tm-fragment>

  aufgerufen werden kann.

  Innerhalb des Rumpfes kann das <markup|arg>-Konstrukt benutzt werden, um
  auf die Werte von Argumenten zuzugreifen, z.B.:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <arg|name>, Sie sehen heute
    aber gut aus!>>>
  </tm-fragment>

  Man kann ein Makro mit mehr oder weniger Argumenten aufrufen als eigentlich
  vorgeshen sind. Überflüssige Argumente werden einfach ignoriert. Fehlende
  Argumente werden durch eine Nullgröÿe, das <markup|uninit>-Konstrukt
  ersetzt:

  <\tm-fragment>
    <inactive*|<assign|hallo-hallo|<macro|erster|zweiter|<style-with|src-compact|none|<if|<equal|<arg|zweiter>|<uninit>>|Hallo
    \ <arg|erster>, Sie sehen heute einsam aus...|Hallo <arg|erster> und
    <arg|zweiter>, Sie sind ein hübsches Paar!>>>>>
  </tm-fragment>

  Angemerkt sei, dass Sie mit Makros ähnlich wie beim funktionalen
  Programmieren rechnen können, nur sind <TeXmacs>-Makros keine
  <em|Closures>, noch nicht. Beispielsweise:\ 

  <\tm-fragment>
    <inactive|<assign|meine-makro-kopie|<inactive|<value|mein-makro>>>>
  </tm-fragment>

  Das <markup|compound>-Konstrukt kann zur Anwendung von Makros dienen, die
  das Resultat einer Berechnung sind:

  <\tm-fragment>
    <inactive*|<assign|overloaded-hi|<macro|name|<style-with|src-compact|none|<compound|<if|<gutes-wetter>|<value|fröhliches-hi>|<value|trauriges-hi>>|<arg|name>>>>>>
  </tm-fragment>

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
    <associate|preamble|false>
  </collection>
</initial>