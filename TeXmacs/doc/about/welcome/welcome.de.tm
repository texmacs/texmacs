<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Willkommen zu GNU <TeXmacs>>

  <\itemize>
    <item>Im Men� <menu|Help|Manual><hlink||../../main/man-manual.de.tm>,
    finden Sie ein Handbuch mit den wesentlichen Informationen zur Benutzung
    von <TeXmacs>.

    <item>Unter <menu|Help|Tutorial> finden Sie ein <TeXmacs>-Tutorial, das
    sich gerade in der Entwicklung befindet und sich vor allem an Anf�nger
    wendet. Es wird aber auch Kapitel f�r fortgeschrittene Benutzer
    enthalten. Leider ist es noch recht unvollst�ndig.

    <item>Im Men� <menu|Help|Manual|Writing your own style files> sind die
    verschiedenen <TeXmacs>-Dokument-Stile erl�utert.\ 

    <item>Das Men� <menu|Help |Apropos> enth�lt weitere Informationen �ber
    <TeXmacs>, wie beispielsweise �ber seine
    <hlink|Autoren|../../about/authors/authors.de.tm>, deren
    <hlink|Kontaktadressen|../../about/authors/contact.de.tm>, und
    <hlink|�nderungen|../../about/changes/changes.de.tm> in den verschiedenen
    Versionen des Programms.\ 

    <item>Man kann immer neuere Dokumentationen �ber das Internet mit
    <menu|Help|Online help> erhalten. Dokumentationen in Form ganzer B�cher
    bekommt man unter <menu|Help|Full manuals>. Tats�chlich k�nnen Sie
    Buch-Versionen von jedem Artikel und jedem Handbuch selbst erstellen mit
    den Befehlen <menu|Help|Full manuals|Compile article> bzw.
    <menu|Help|Full manuals|Compile book>. Das dauert aber einiges.
  </itemize>

  <section|Kommentar zur �bertragung ins Deutsche>

  Die �bertragung von Programm-Beschreibungen in eine andere Sprache ist
  immer problematisch. Ein Teil der Probleme resultiert daraus, dass
  Fachbegriffe oft noch nicht in die andere Sprache eingedrungen sind. Dazu
  kommt, dass h�ufig eigene Fachbegriffe eingef�hrt werden, die aus der
  Umgangssprache stammen und im Kontext in der Ursprungssprache leicht zu
  verstehen sind, bei der �bertragung dagegen nicht.\ 

  Hier kommt noch dazu, dass <TeXmacs> kein Textverarbeitungsprogramm ist
  sondern wie <LaTeX> ein Schriftsatz-Programm. Deshalb sind au�er den
  gewohnten Begriffen der Textverarbeitung viele neue Begriffe von N�ten, die
  dem Schriftsetzer vertraut sind, dem Autor dagegen kaum. Da aber die Kunst
  des Schriftsatzes sich �ber die Jahrhunderte in den verschiedenen
  Sprachgebieten unterschiedlich entwickelt hat, gibt es h�ufig Begriffe, die
  keine Entsprechung in anderen Sprachr�umen haben.\ 

  Im folgenden werden einige Konventionen erkl�rt, die in der deutschen
  �bertragung verwendet wurden.\ 

  <\itemize-dot>
    <item><strong|Konstrukt> wird als �bersetzung des englischen
    <em|primitive> verwendet. Ein Konstrukt kann auch ausf�hrbar, also ein
    <strong|Befehl> sein.

    <item><strong|Kontext> wird f�r das englische <em|environment> benutzt.
    Dies ist einen Befehl, der sich �ber ein Textst�ck erstreckt und den
    Schriftsatz steuert. Die w�rtliche �bersetzung, Umgebung, wird zur
    Kennzeichnung der Betriebssystem-Umgebung benutzt. Dementsprechend sind
    \ die <em|environment variables> auf Deutsch die
    <strong|Kontextvariablen>.

    <item>Die linearisierte Darstellung der logischen Baumstrukturen f�hrt zu
    Darstellungen, die wie in HTML aus Marken, <em|tags>, bestehen. Diese
    <strong|Tags> k�nnen als einzeln stehende Marke vorkommen, oft
    umschlie�en <strong|Start- und Stoptags> einen Kontext/Befehl.

    <item>Mit <strong|Kurzbefehl> werden Tastenkombinationen bezeichnet, die
    <TeXmacs>-Befehlen entsprechen.

    <item><strong|Box> ist ein rechteckiger Raum, indem Zeichen in bestimmter
    Weise angeordnet sind, der als ganzes in den Text eingef�gt wird. Ein
    Beispiel daf�r ist ein mathematischer Bruch.

    <item><strong|Zeileninhalt>, im englischen Text <em|inline content>, sind
    kurze Textst�cke und Konstrukte, die kurze Textst�cke speziell
    darstellen, im Gegensatz zu <strong|Blockkontext> oder
    <strong|Blockinhalt>, der mehrere Abs�tze umfasst. \ 

    <item><strong|Flag> ist eine boolesche Variable mit den Werten ja oder
    nein (bzw. an/aus, +/-).

    <item><strong|Label> ist eine Zielmarke f�r Referenzen

    <item><strong|Dimension>, englisch arity, die Anzahl der erlaubten oder
    vorhandenen Argumente

    <item><strong|Flags> sind Kennzeichen im Editor, die nicht gedruckt
    werden. Das Wort Flag wird auch f�r boolescher Variablen
    (Kontextvariablen) gebraucht.
  </itemize-dot>

  <section|Selbst beitragen zu <TeXmacs>>

  <em|Freie Software> kann und darf jeder an seine speziellen Bed�rfnisse
  anpassen. Dies und die M�glichkeit, seine Erfahrungen mit anderen zu teilen
  ist eine gro�artige Sache, die davon lebt, dass sich viele freiwillig
  beteiligen. Vielleicht wollen Sie das ja auch - und es ist leichter, als
  Sie sich das vorstellen. Mehr Information dar�ber, wie Sie selbst zum
  <TeXmacs>-Projekt beitragen k�nnen, finden Sie
  \ <hlink|hier|../../about/contribute/contribute.en.tm>. Nat�rlich sind wir
  auch f�r eine <hlink|Spende|../../about/contribute/material/donations.en.tm>
  dankbar.

  Wenn Sie also einen Beitrag zu<TeXmacs> leisten wollen, oder wenn Sie das
  Programm speziell anpassen wollen, dann finden Sie wichtige Informationen
  im <menu|Help>-Men�. <hlink|<menu|Help|Document
  format>|../../devel/format/format.en.tm> gibt Informationen �ber das
  <TeXmacs> Dokumentenformat und unter <hlink|<menu|Help|Interfacing>|../../devel/plugin/plugin.en.tm>
  wird \ erkl�rt, wie<TeXmacs> mit anderen Programmen zusammenarbeiten kann.
  Ein Teil des Quellcodes ist unter <hlink|<menu|Help|Source
  code>|../../devel/source/source.en.tm> dokumentiert.

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

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