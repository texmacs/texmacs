<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Die Schnittstelle verbessern.>

  Wenn Sie eine funktionierende Schnittstelle zwischen einer Anwendung und
  <TeXmacs> geschrieben haben, wollen Sie sie möglicherweise weiter
  verbessern. Im folgenden wollen wir einige mögliche Ansätze diskutieren.

  Meist besteht der Wunsch, das Verhalten der Tastatur innerhalb einer
  <verbatim|myplugin>-Sitzung anzupassen. Wie man das tun kann und wie man
  dies in die Datei init-myplugin.scm aufnimmt, wird im Kapitel über die
  <hlink|<name|Guile/Scheme> Erweiterungs-Sprache|../../main/scheme/man-scheme.de.tm>
  erläutert. Wir empfehlen, dass Sie die Plugins studieren, die mit der
  <TeXmacs>-Distribution kommen und in dem Unterverzeichnis \Rplugins`` Ihres
  <TeXmacs>-Verzeichnisses enthalten sind.

  Oft brauchen einige Ausgaben Ihrer Anwendung spezielle Formatierungen. So
  könnten Sie z.B. jeden Unter-Ausdruck mit einem unsichtbaren Daten-Typ
  assoziieren. Dazu können Sie ein Makro <verbatim|exprtype> mit zwei
  Argumenten <verbatim|myplugin.ts> erzeugen, dass <LaTeX>-artige Ausdrücke
  wie z.B. <verbatim|\\exprtype{1}{Integer}> an <TeXmacs> während der Ausgabe
  sendet.

  Wenn Ihre Anwendung mit <TeXmacs> über eine\RPipeline`` kommuniziert,
  können Sie direkt <TeXmacs>-Befehle ausführen lassen, indem Sie sie in
  ihren Ausgabecode folgendermaÿen implementieren:

  <\verbatim>
    \ \ \ \ [DATA_BEGIN]command:scheme-program[DATA_END]
  </verbatim>

  Umgekehrt, wenn sich der Cursor innerhalb einer Sitzung befindet, können
  Sie mit dem <name|Scheme>-Befehl:\ 

  <\verbatim>
    \ \ \ \ (extern-exec plugin-command)
  </verbatim>

  ein \Rplugin-command'' von Ihrer Anwendung ausführen lassen.

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