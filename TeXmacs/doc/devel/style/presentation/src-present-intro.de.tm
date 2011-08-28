<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|ASCII-basierte oder Baum-basierte Editierung: problematische
  Alternativen>

  Die meisten Nutzer sind daran gewöhnt, Quellcode mit einem konventionellen
  Editor wie <name|Emacs> zu editieren, die den Quellcode im ASCII-Format
  darstellen. <TeXmacs>-Dokumente werden aber als
  <hyper-link|Bäume|../../format/basics/basics.de.tm> (tree) gespeichert. Es
  ist daher eine interessante aber sehr komplizierte Frage, welches Format
  für die Editierung am besten geeignet ist. Eine Option wäre, ein
  ASCII-basiertes Format wie \ XML oder Scheme zu verwenden oder auch das
  ASCII-basierte Format, in dem Dateien auf einer Diskette oder Festplatte
  gespeichert werden. Die andere Option besteht darin die Bäume als solche zu
  verwenden und damit Textdokumente und Quellcode gleich zu behandeln.\ 

  Wir haben uns entschlossen in <TeXmacs> die zweite Alternative zu
  verwenden. Genauer gesagt, jedes Dokument kann im \RQuellmodus'' editiert
  werden. Das ist nur eine spezielle Form der Darstellung, die die
  Baumstruktur besser erkennbar macht. Es ist sehr instruktiv, irgendein
  Dokument zu nehmen und es im Quellmodus zu betrachten. Dazu wählen Sie
  \ <menu|Dokument|Ansicht|Quellmodus>. Auf diese Weise können Sie vorallem
  bei Verweisen jeder Art das Ziel erkennen.

  Die Wahl zwischen ASCII-basiertem Editieren und Baum-basierten Editieren
  ist nicht trivial, denn <TeXmacs> Stil-Dateien und Pakete haben eine
  Doppelnatur. Sie sind einerseits Programme, die spezifizieren, wie Makros
  dargestellt werden, andererseits enhalten sie normalen Text. Es gibt eine
  Reihe von Gründen, warum Nutzer ein ASCII-basiertes Format vorziehen:

  <\enumerate>
    <item>Der Code kann leicht so formatiert werden, daÿ man ihn besser lesen
    kann.

    <item>Kommentare können leicht hinzugefügt werden.

    <item>Standard Editoren wie <name|Emacs> stellen Werkzeuge für
    automatische Hervorhebungen, Einzüge usw. bereit.

    <item><label|structure-constraints>Man wird nicht durch irgendwelche
    \RStrukturen'' in der Entwicklungsphase behindert.
  </enumerate>

  Wir versuchen möglichst viele dieser Vorteile in unser Konzept einer a
  strukturierten Dokument- Darstellung einflieÿen zu lassen, obwohl es
  offensichtlich schwierig ist, Punkt 4 angemessen zu berücksichtigen. Wir
  glauben, daÿ die in ersten drei Punkten genannten Vorteile in einer solchen
  strukturierten Umgebungen sogar deutlicher ausgeprägt sein werden. Jedoch
  erfordert die Verwirklichung dieses Konzeptes ein tiefes Verständnis davon,
  wie Nutzer Quellcode tatsächlich formatieren und editieren.\ 

  Lassen Sie uns beispielsweise dieses Stück formatierten Code betrachten:

  <\cpp-fragment>
    if (cond) hop \ \ = 2;

    else \ \ \ \ \ holala= 3;
  </cpp-fragment>

  Man erkennt, daÿ dieser Code mit in spezieller Weise formatiert ist. Die
  Formatierungsrichtlinien, die derjenige, der den Code formatierte, im Kopf
  hatte sind, im Code nicht enthalten. Wenn etwa die Variable cond in c
  umbenannt wird, oder wenn die Variable holala in hola umbenannt wird, muÿ
  der Code manuell neu formatiert werden.

  Zum jetzigen Zeitpunkt stellt <TeXmacs> keine Werkzeuge bereit, die das
  Problem dieses \ Beispiels automatisch lösen könnten, aber einige wichtige
  Werkzeuge sind schon vorhanden. So hat der Anwender viele Möglichkeiten,
  Quellcode durch Einzüge zu gestalten und vernünftige Standardformatierungen
  sind vorhanden. Weitere Werkzeuge sollen in Zukunft entwickelt werden. Wir
  sind für Anregungen dankbar.\ 

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