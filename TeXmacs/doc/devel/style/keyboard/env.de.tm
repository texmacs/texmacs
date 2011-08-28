<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Makros und <TeXmacs>-Umgebungsvariable>

  Die wichtigsten Tastenkombinationen, die Sie kennen sollten, um
  Stildefinitionen selbst zu schreiben, sind die folgenden:

  <\description>
    <item*|<key|inactive =>>erzeugt eine neue Befehlsanweisung. Das erste Argument
    ist ein neuer Befehlsname und das zweite Argument ein Ausdruck.

    <item*|<key|inactive w>>erzeugt eine \RWith-Anweisung``. Diese erlaubt
    <TeXmacs>-Umgebungsvariablen lokal zu ändern. Sie haben die Form
    <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>.
    Dabei sind die <with|mode|math|x<rsub|i>> die Namen der Variablen,
    <with|mode|math|a<rsub|i>> die lokalen Werte und b ein Text, auf den die
    Variablen angewendet werden.

    <item*|<key|inactive m>>erzeugt ein Makro. Argumente können mit der
    \ <key|Tab>-Taste eingefügt werden.

    <item*|<key|inactive #>>hole den Wert eines Makroarguments.

    <item*|<key|inactive v>>hole den Wert einer <TeXmacs>-Umgebungsvariablen.

    <item*|<key|inactive e>>expandiert ein Makro mit keinem oder mehreren
    Argumenten.
  </description>

  Genauer, wenn ein mit <key|inactive e> expandiertes Makro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> ausgewertet wird,
  laufen folgende Vorgänge ab:

  <\itemize>
    <item>Wen <with|mode|math|a> weder eine Zeichenkette noch ein Makro ist,
    wird a genau einmal evaluiert. Das Ergebnis ist entweder ein Makroname
    <with|mode|math|f> oder ein Makroausdruck <with|mode|math|f>.

    <item>Wenn es ein Makroname ist, wird <with|mode|math|f> durch den Wert
    der <TeXmacs>-Umgebungsvariablen <with|mode|math|f> ersetzt. Wenn danach
    <with|mode|math|f> immer noch kein Makroausdruck ist, wird
    <with|mode|math|f> zurückgegeben.

    <item>Wenn <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> die Argumente
    von <with|mode|math|f> sind und <with|mode|math|b> der Rumpf
    (überflüssige Argumente werden ignoriert; fehlende Argumente erhalten den
    Vorgabewert, die leere Zeichenkette). Dann werden die
    <with|mode|math|y<rsub|i>> in <with|mode|math|b> durch die
    <with|mode|math|x<rsub|i>> ersetzt und das Resultat zurückgegeben.
  </itemize>

  Funktionen verhalten sich ähnlich wie Makros, nur sind die Argumente von
  Funktionen evaluiert und können nicht direkt geändert werden. Man muÿ
  zuerst die Funktion deaktivieren, dann die Argumente ändern und schlieÿlich
  wieder aktivieren. Auÿerdem werden <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>>
  als lokale Variablen mit den Werten <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>>
  betrachtet. Diese lokalen Variablen gehen verloren, wenn die Funktion, die
  sie benutzt, verlassen wird.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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