<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Dokumente sind Bäume>

  <TeXmacs> repräsentiert alle Texte durch Bäume (für einen bestimmten Text
  wird der zugehörige Baum der <def-index|Editierbaum> genannt). Die inneren
  Knoten eines solchen Baum werden mit Standard-<def-index|Operatoren> des
  Typs <verbatim|tree_label> (siehe <verbatim|Basic/Data/tree.gen.h>)
  aufgebaut. Der Inhalt der "Blätter" des Baums sind Zeichenketten (Strings),
  welche entweder unsichtbar (z. B. Längen oder Makro-Definitionen), oder
  sichtbar (der richtige Text) erscheinen. <TeXmacs> Bäume können auf
  verschiedene Weise beschrieben werden. Zum Beispiel repräsentiert der Baum

  <\quote-env>
    <with|mode|math|<tree|concat|x+y+|<tree|frac|1|2>|+|<tree|sqrt|y+z>>>
  </quote-env>

  die Formel

  <\tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </tm-fragment>

  und kann in der Notation von <value|scheme> geschrieben werden als:

  <\scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </scheme-fragment>

  \;

  Die Bedeutung des Textes und die Art, wie er gesetzt wird, hängt essenziell
  von der aktuellen Umgebung ab. Die Umgebung besteht hauptsächlich aus einer
  Hash-Tabelle, welche die Umgebungsvariablen mit den Baum-Inhalten
  verknüpft. Die aktuelle Sprache, aktuelle Schrift und aktuelle Farbe sind
  Beispiele für Systemumgebungs-Variablen; neue Variablen können durch den
  Nutzer definiert werden. Zum Beispiel erzeugt der <value|scheme> Ausdruck

  <\scheme-fragment>
    (concat

    \ \ "Ein "

    \ \ (with "color" "blue" "blauer")

    \ \ " Text.")
  </scheme-fragment>

  das entsprechende Textfragment

  <\tm-fragment>
    Ein <with|color|blue|blauer> Text.
  </tm-fragment>

  Das <TeXmacs>-Konstrukt <verbatim|with> beschreibt eine lokale Änderung der
  Umgebungsvariablen.

  Im nachfolgenden werden wir im Detail beschreiben, wie die verschiedenen
  Standard <TeXmacs> Operatoren und Umgebungsvariablen funktionieren. Es
  sollte erwähnt werden, dass sich das <TeXmacs> Datenformat ein Punkt ist,
  an dem noch gearbeitet wird. Im letzten Abschnitt werden diese Änderungen
  beschrieben. Für gewöhnlich wird der Anwender von einer Erweiterung des
  Datenformats nichts bemerken, da solch eine Änderung immer zusammen mit
  einem Konvertierungs-Programm entwickelt wird, das die bestehenden
  Dokumente automatisch auf das neue Format ergänzt. Dennoch sind sie
  manchmal wichtig für die Entwickler, wenn auch die meisten Änderungen nur
  das Hinzufügen von neuen Konstrukten betreffen.

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