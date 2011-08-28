<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematische Kontexte>

  Die <tmdtd|env-math> <abbr|D.T.D.> spezifiziert, welche mathematischen
  Kontexte im Text-Modus benutzt werden können. Mit anderen Worten, diese
  Kontexte sollen innerhalb von Text eingesetzt werden, ihre Rümpfe enthalten
  aber mathematische Formeln oder Tabellen von mathematischen Formeln.

  <\explain|<explain-macro|equation|body>>
    Eine nummerierte Gleichung.
  </explain>

  <\explain|<explain-macro|equation*|body>>
    Eine unnummerierte Gleichung.
  </explain>

  <\explain|<explain-macro|eqnarray|table>>
    Ein Array von nummerierten Gleichungen (noch nicht implementiert).
  </explain>

  <\explain|<explain-macro|eqnarray*|table>>
    Ein Array von unnummerierten Gleichungen.
  </explain>

  Innerhalb von <markup|eqnarray*> kann man <markup|eq-number> benutzen, um
  die Gleichung zu nummerieren.

  <\warning>
    Die Nummerierung von Gleichungen innerhalb von Tabellen funktioniert noch
    nicht so, wie sie eigentlich sollte. Zur Zeit sind <markup|eqnarray> und
    <markup|eqnarray*> dasselbe. Später soll aber <markup|eqnarray> korrekt
    implementiert werden. Dann wird es einen Befehl <markup|no-number> geben,
    um die Nummerierung zu unterdrücken sowie ein Stil-Paket, um linke oder
    rechte Nummerierung zu einstellen zu können.
  </warning>

  <\warning>
    Es gibt zur Zeit keine Option zur Platzierung der Nummerierung an der
    linken Seite. Man kann dafür aber manuell <markup|leq-number> benutzen.
    Es existiert auch der Befehl <markup|next-number>, der die nächste Zahl
    auf dem Bildschirm zeigt und den Zähler um 1 erhöht.
  </warning>

  <\warning>
    Wir raten vom Gebrauch der AMS-<TeX>-Befehle <verbatim|align>,
    <verbatim|gather> und <verbatim|split> ab. Trotzdem sind sie unter den
    Namen <markup|align>, <markup|gather>, <markup|eqsplit> mit den Varianten
    <markup|align*>, <markup|gather*> und <markup|eqsplit*> vorhanden. Für
    die Zukunft sind mächtigere Konstrukte geplant.
  </warning>

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