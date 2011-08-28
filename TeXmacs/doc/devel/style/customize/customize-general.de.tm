<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Anpassung, Allgemeines>

  Stil-Definitionen und -Pakete bereichern den Kontext mit einer Kombination
  von

  <\itemize>
    <item>Kontextvariablen.

    <item>Anwenderbefehlen.

    <item>Anpassungsmakros.
  </itemize>

  Auÿerdem können sie einige interne Befehle definieren, die nicht in diesem
  Handbuch behandelt werden. Auch können sie einige logische Eigenschaften
  von Befehlen definieren, indem sie das <markup|drd-props>-Konstrukt
  benutzen.

  Kontext-Varaible sind fast immer Attribute, die die Darstellung
  irgendwelcher Inhalte steuern, oder die Zähler enthalten für Abschnitte,
  Gleichungen usw.. Obwohl viele einfache Anwender-Befehle wie z.B.
  <markup|<translate|strong|english|german>> in eignen Stil-Definitionen
  geändert werden können, wird diese Praxis nicht empfohlen für
  kompliziertere Befehle, wie z.B. <markup|<translate|section|english|german>>.
  In der Tat beinhaltet der <markup|<translate|section|english|german>>-Befehl
  eine Menge verschiedener Operationen wie das Setzen von Unterzählern, den
  Titel in das Inhaltsverzeichnis eintragen usw.. Darum gibt es spezielle
  zusätzliche Makros, zur leichteren Anpassung, in diesem Fall z.B.:
  <markup|section-title>, <markup|section-clean> und <markup|section-toc>.

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