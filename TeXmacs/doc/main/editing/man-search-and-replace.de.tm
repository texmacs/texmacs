<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Suchen und Ersetzen>

  Man kann Text suchen mit den Befehlen <key|C-s> oder <menu|Edit|Search>.
  Der zu suchende Text ist in die Fuÿ-Zeile auf der linken Seite hinter dem
  Text \R<localize|search>`` einzugeben. Während der Suche bleibt er dort
  sichtbar. Jeder Buchstabe, den sie eingeben wird angehängt und im Text wird
  das jeweilige erstmalige Auftreten mit einer roten Box markiert. Wenn Sie
  <key|C-s> erneut eingeben, wird weiter gesucht. Ein Piep zeigt, dass die
  Suche beendet ist. Mit <key|C-s> beginnt die Suche erneut am Beginn des
  Dokuments. Man kann <key|<with|mode|math|\<longleftarrow\>>> benutzen, um
  Eingaben während der Suche zurückzunehmen. Die Suche unterscheidet zwischen
  Klein- und Groÿ-Buchstaben.

  Normalerweise wird Text von der Cursorposition ab in vorwärts Richtung
  durchsucht. Sie können mit <key|C-r> auch rückwärts suchen. Während der
  Suche wird Text nur im gleichen Modus und in der Sprache,die an der
  Startposition aktiv war, gefunden. Mit anderen Worten, wenn Sie von einer
  Gleichung aus, als im Mathematik-Modus etwa nach x suchen, finden sie kein
  x, das sich im normalen Text befindet. Weiterhin kann zur Zeit nur nach
  einfachen Textstücken gesucht werden, nicht nach mathematischen Symbolen
  oder einem kompliziert strukturierten Text.

  Gleichzeitig Suchen und Ersetzen kann man mit <key|C-=> oder
  <menu|Edit|Replace>. Sie werden zur Eingabe eines zu suchenden Textstücks
  aufgefordert und zur Eingabe des Textes, der an seiner Stelle eingefügt
  werden soll. Wird ein zu ersetzendes Textstück gefunden, werden Sie
  aufgefordert, durch Eingabe von y, n oder a anzugeben, ob sie ersetzen
  wollen (y), nicht ersetzen wollen (n) alle alle Vorkommen ohne Rückfrage
  ersetzt haben wollen (a). Es gelten ansonsten die gleichen Beschränkungen
  wie bei der Suche.

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
  </collection>
</initial>