<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs>-Bäume>

  <TeXmacs> repräsentiert alle Dokumente oder Teile von Dokumenten, also alle
  Formen von Text, durch Bäume. Zum Beispiel repräsentiert der Baum

  <\quote-env>
    <with|mode|math|<tree|<with|math-font-series|bold|concat>|x+y+|<tree|<with|math-font-series|bold|frac>|1|2>|+|<tree|<with|math-font-series|bold|sqrt>|y+z>>>
  </quote-env>

  die Formel

  <\tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </tm-fragment>

  Die Knoten eines solchen Baum sind Standard-<def-index|Operatoren> des Typs
  <verbatim|tree_label> (siehe <verbatim|Basic/Data/tree.gen.h>), z.B.:
  <strong|concat>, <strong|frac>, <strong|sqrt>. Der Inhalt der "Blätter" des
  Baums sind Zeichenketten (Strings). Diese sind entweder auf dem Bildschirm
  sichtbar und können auch gedruckt werden, dies ist der eigentliche Text, -
  oder sie sind auf dem Bildschirm nicht sichtbar wie z.B. Längen- oder
  Makro-Definitionen und werden daher auch nicht ausgedruckt. Die
  Baumdarstellung von Dokumenten in <TeXmacs> spiegelt ihren logischen Aufbau
  wieder. Dateien sind jedoch linear aufgebaut, sodass Bäume zur Speicherung
  in einen fortlaufenden Text umgeformt werden müssen (serialization,
  deutsch: <dfn|Linearisierung>), der von einem sogenannten Parser
  zurückübersetzt werden kann. Dies ist auf verschiedene Weise möglich. Der
  oben gezeigte Beispielbaum kann in der Notation von <value|scheme>
  geschrieben werden als:

  <\scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </scheme-fragment>

  \;

  Wie ein solcher Text interpretiert wird und wie das Schriftsatz-Programm
  <TeXmacs> den Text schlieÿlich setzt, hängt vom aktuellen Kontext ab.
  Dieser Kontext ist im wesentlichen eine Hash-Tabelle, welche die
  Kontextvariable mit den Bauminhalten verknüpft. Die aktuelle Sprache, die
  aktuelle Schrift und die aktuelle Farbe sind Beispiele für Kontextvariable;
  Variablen können durch den Nutzer geändert definiert werden. Zum Beispiel
  erzeugt der <value|scheme> Ausdruck

  <\scheme-fragment>
    (concat

    \ \ "Ein "

    \ \ (with "color" "blue" "blauer")

    \ \ " Text.")
  </scheme-fragment>

  das folgende Textfragment

  <\tm-fragment>
    Ein <with|color|blue|blauer> Text.
  </tm-fragment>

  Der <TeXmacs>-Befehl <verbatim|with|variable> setzt eine Kontextvariable
  neu. Diese Änderung ist lokal, d.h., sie bezieht sich nur auf Ausdrücke in
  der Klammer (variable). Daher ist auch \Rblauer'' blau.

  Im nachfolgenden beschreiben wir im Detail, wie die verschiedenen Standard
  <TeXmacs>-Konstrukte (Befehle) und Kontextvariablen funktionieren. Es
  sollte erwähnt werden, dass an dem <TeXmacs> Datenformat noch gearbeitet
  wird. Im letzten Abschnitt werden diese Änderungen beschrieben. Für
  gewöhnlich wird der Anwender von einer Erweiterung des Datenformats nichts
  bemerken, da solch eine Änderung immer zusammen mit einem
  Konvertierungs-Programm entwickelt wird, das die bestehenden Dokumente
  automatisch auf das neue Format ergänzt. Die meisten Änderungen bestehen im
  Hinzufügen von neuen Konstrukten. Für Entwickler kann die Kenntnis solcher
  Änderungen dennoch wichtig sein.

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