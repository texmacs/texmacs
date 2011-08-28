<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|<translate|standard length units|english|german>>

  Die Blätter von <TeXmacs> - Bäumen enthalten entweder normalen Text oder
  spezielle Daten. \ <TeXmacs> kennt die folgenden atomaren Datentypen:

  <\description>
    <item*|Boolesche Zahlen>Entweder <verbatim|true> oder <verbatim|false>
    (richtig, falsch - ja, nein).

    <item*|Ganzzahl>Folge von Ziffern, vor denen ein Plus- oder Minus-Zeichen
    stehen darf.

    <item*|Gleitpunktzahlen>Zahlen mit Dezimalpunkt in der üblichen
    wissenschaftlichen Darstellung.

    <item*|Längen>Gleitpunktzahl gefolgt von einer Längeneinheit z. B.
    <verbatim|29.7cm> oder <verbatim|2fn>.
  </description>

  In diesem Abschnitt besprechen wir die <TeXmacs> - Längeneinheiten.

  Es gibt zwei prinzipiell verschiedene Arten von Längeneinheiten/Längen:
  fixe Längeneinheiten und Kontext-abhängige Längeneinheiten. Fixe
  Längeneinheiten sind die üblichen Längeneinheiten: ihre Länge auf dem
  Bildschirm oder dem Papier ist vorgegeben und unveränderlich. Die Länge
  Kontext-abhängiger Einheiten wird dagegen den vorliegenden Gegebenheiten
  angepasst. Solche können beispielsweise Schriftart und Schriftgröÿe sein.

  Einige der variablen Längeneinheiten sind <em|dehnbar>. Drei Kennzahlen
  charakterisieren eine <em|dehnbare> Länge: die minimale Länge, die
  Vorgabelänge und die maximale Länge. Wenn Zeilen oder Seiten im Blocksatz
  umgebrochen werden, werden <em|dehnbare Längen> so angepasst, dass ein
  optimales Druckbild entsteht.\ 

  Im Fall des Seitenumbruchs erlaubt die <src-var|page-flexibility> -
  Umgebung eine zusätzliche Steuerung der Dehnbarkeit von Leerräumen. Setzt
  man die <src-var|page-flexibility> auf <with|mode|math|1>, so verhält sich
  dehnbarer Leerraum wie gewohnt. Wird die <src-var|page-flexibility> dagegen
  auf <with|mode|math|0> gesetzt, dann werden dehnbare Längen
  <em|<strong|fix><strong|>>. Andere Werte beeinflussen das Verhalten linear.

  <paragraph*|Fixe Längeneinheiten>

  <\description>
    <item*|<code*|cm>>1 Zentimeter.

    <item*|<code*|mm>>1 Millimeter.

    <item*|<code*|in>>1 inch (Zoll).

    <item*|<code*|pt>>1 typographischer Punkt: 1/72 inch = 0.353 mm.
  </description>

  <paragraph*|kontext-abhängige Längeneinheiten>

  <\description>
    <item*|<code*|fn>>Die Nenngröÿe der Schrift. Typischerweise sind die
    Basislinien zweier aufeinander folgender Zeilen durch den Abstand
    <verbatim|1fn> getrennt (in <TeXmacs> und <LaTeX> wird ein geringfügig
    gröÿerer Abstand benutzt, um obere und untere Indices besser darstellen
    zu können. Die Dehnbarkeit liegt für 1fn zwischen <verbatim|0.5fn> und
    <verbatim|1.5fn>.

    <item*|<verbatim|fn*>>Ist eine Variante von <verbatim|fn>, mit der
    Vorgabelänge Null, die aber bis <verbatim|1fn> gedehnt werden kann.

    <item*|<code*|spc>>Die dehnbare Breite eines Leerzeichens in der
    aktuellen Schrift.

    <item*|<verbatim|ex>>Die Höhe des Buchstabens ``x'' in der aktuellen
    Schrift.\ 

    <item*|<code*|ln>>Die Breite eines gut aussehenden Bruchstrichs in der
    aktuellen Schrift.

    <item*|<verbatim|yfrac>>Der Abstand eines Bruchstrichs von der Basislinie
    in der aktuellen Schrift. <line-break>(ungefähr <verbatim|0.5ex>).

    <item*|<code*|sep>>Ein typischer Abstand zwischen Text und Graphik in der
    aktuellen Schrift, der benötigt wird, um den Text lesbar darzustellen.
    Beispielsweise wird der Zähler eines Bruchs um den Betrag <verbatim|1sep>
    nach oben gesetzt.
  </description>

  <paragraph*|weitere Längeneinheiten>

  <\description>
    <item*|<code*|par>>Die zulässige Text-breite in einem Absatz. Sie hängt
    von der Papiergröÿe, den Rändern, der Spaltenzahl, dem Spaltenabstand
    usw. ab.\ 

    <item*|<verbatim|pag>>Die Länge des Haupt-Textes einer Seite. Ähnlich wie
    \ <verbatim|par>, wird diese Längeneinheit von der Papiergröÿe, den
    Rändern usw. beeinflusst.

    <item*|<code*|px>>1 Bildschirm-Pixel. Diese Längeneinheit hängt von der
    Bildschirmauflösung ab, die \ <TeXmacs> vom X Server beim Start
    mitgeteilt bekommt.

    <item*|<code*|unit>>px/256. Diese Längeneinheit wird von <TeXmacs> intern
    für Längenberechnungen benutzt.
  </description>

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