<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Willkommen zu GNU <TeXmacs>>

  <\itemize>
    <item>Im Menü <menu|Help|Manual><hyper-link||../../main/man-manual.de.tm>,
    finden Sie ein Handbuch mit den wesentlichen Informationen zur Benutzung
    von <TeXmacs>.

    <item>Unter <menu|Help|Tutorial> finden Sie ein <TeXmacs>-Tutorial, das
    sich gerade in der Entwicklung befindet und sich vor allem an Anfänger
    wendet. Es wird aber auch Kapitel für fortgeschrittene Benutzer
    enthalten. Leider ist es noch recht unvollständig.

    <item>Im Menü <menu|Help|Manual|Writing your own style files> sind die
    verschiedenen <TeXmacs>-Dokument-Stile erläutert.\ 

    <item>Das Menü <menu|Help |About> enthält weitere Informationen über
    <TeXmacs>, wie beispielsweise über seine
    <hlink|Autoren|../../about/authors/authors.de.tm>, deren
    <hyper-link|Kontaktadressen|../../about/authors/contact.de.tm>, und
    <hyper-link|Änderungen|../../about/changes/changes.de.tm> in den
    verschiedenen Versionen des Programms.\ 

    <item>Man kann immer neuere Dokumentationen über das Internet mit
    <menu|Help|Online help> erhalten. Dokumentationen in Form ganzer Bücher
    bekommt man unter <menu|Help|Full manuals>. Tatsächlich können Sie
    Buch-Versionen von jedem Artikel und jedem Handbuch selbst erstellen mit
    den Befehlen <menu|Help|Full manuals|Compile article> bzw.
    <menu|Help|Full manuals|Compile book>. Das dauert aber einiges.
  </itemize>

  <section|Kommentar zur Übertragung ins Deutsche>

  Die Übertragung von Programm-Beschreibungen in eine andere Sprache ist
  immer problematisch. Ein Teil der Probleme resultiert daraus, dass
  Fachbegriffe oft noch nicht in die andere Sprache eingedrungen sind. Dazu
  kommt, dass häufig eigene Fachbegriffe eingeführt werden, die aus der
  Umgangssprache stammen und im Kontext in der Ursprungssprache leicht zu
  verstehen sind, bei der Übertragung dagegen nicht.\ 

  Hier kommt noch dazu, dass <TeXmacs> kein Textverarbeitungsprogramm ist
  sondern wie <LaTeX> ein Schriftsatz-Programm. Deshalb sind auÿer den
  gewohnten Begriffen der Textverarbeitung viele neue Begriffe von Nöten, die
  dem Schriftsetzer vertraut sind, dem Autor dagegen kaum. Da aber die Kunst
  des Schriftsatzes sich über die Jahrhunderte in den verschiedenen
  Sprachgebieten unterschiedlich entwickelt hat, gibt es häufig Begriffe, die
  keine Entsprechung in anderen Sprachräumen haben.\ 

  Im folgenden werden einige Konventionen erklärt, die in der deutschen
  Übertragung verwendet wurden.\ 

  <\itemize-dot>
    <item><strong|Konstrukt> wird als Übersetzung des englischen
    <em|primitive> verwendet. Ein Konstrukt kann auch ausführbar, also ein
    <strong|Befehl> sein.

    <item><strong|Kontext> wird für das englische <em|environment> benutzt.
    Dies ist einen Befehl, der sich über ein Textstück erstreckt und den
    Schriftsatz steuert. Die wörtliche Übersetzung, Umgebung, wird zur
    Kennzeichnung der Betriebssystem-Umgebung benutzt. Dementsprechend sind
    \ die <em|environment variables> auf Deutsch die
    <strong|Kontextvariablen<strong|>>.

    <item>Die linearisierte Darstellung der logischen Baumstrukturen führt zu
    Darstellungen, die wie in HTML aus Marken, <em|tags>, bestehen. Diese
    <strong|Tags> können als einzeln stehende Marke vorkommen, oft
    umschlieÿen <strong|Start- und Stoptags> einen Kontext/Befehl.

    <item>Mit <strong|Kurzbefehl> werden Tastenkombinationen bezeichnet, die
    <TeXmacs>-Befehlen entsprechen.

    <item><strong|Box> ist ein rechteckiger Raum, indem Zeichen in bestimmter
    Weise angeordnet sind, der als ganzes in den Text eingefügt wird. Ein
    Beispiel dafür ist ein mathematischer Bruch.

    <item><strong|Zeileninhalt>, im englischen Text <em|inline content>, sind
    kurze Textstücke und Konstrukte, die kurze Textstücke speziell
    darstellen, im Gegensatz zu <strong|Blockkontext> oder
    <strong|Blockinhalt>, der mehrere Absätze umfasst. \ 

    <item><strong|Flag> ist eine boolesche Variable mit den Werten ja oder
    nein (bzw. an/aus, +/-).

    <item><strong|Label> ist eine Zielmarke für Referenzen

    <item><strong|Dimension>, englisch arity, die Anzahl der erlaubten oder
    vorhandenen Argumente

    <item><strong|Flags> sind Kennzeichen im Editor, die nicht gedruckt
    werden. Das Wort Flag wird auch für boolescher Variablen
    (Kontextvariablen) gebraucht.
  </itemize-dot>

  <section|Selbst beitragen zu <TeXmacs>>

  <em|Freie Software> kann und darf jeder an seine speziellen Bedürfnisse
  anpassen. Dies und die Möglichkeit, seine Erfahrungen mit anderen zu teilen
  ist eine groÿartige Sache, die davon lebt, dass sich viele freiwillig
  beteiligen. Vielleicht wollen Sie das ja auch - und es ist leichter, als
  Sie sich das vorstellen. Mehr Information darüber, wie Sie selbst zum
  <TeXmacs>-Projekt beitragen können, finden Sie
  \ <hyper-link|hier|../../about/contribute/contribute.en.tm>. Natürlich sind
  wir auch für eine <hyper-link|Spende|../../about/contribute/material/donations.en.tm>
  dankbar.

  Wenn Sie also einen Beitrag zu<TeXmacs> leisten wollen, oder wenn Sie das
  Programm speziell anpassen wollen, dann finden Sie wichtige Informationen
  im <menu|Help>-Menü. <hyper-link|<menu|Help|Document
  format>|../../devel/format/format.en.tm> gibt Informationen über das
  <TeXmacs> Dokumentenformat und unter <hyper-link|<menu|Help|Interfacing>|../../devel/plugin/plugin.en.tm>
  wird \ erklärt, wie<TeXmacs> mit anderen Programmen zusammenarbeiten kann.
  Ein Teil des Quellcodes ist unter <hyper-link|<menu|Help|Source
  code>|../../devel/source/source.en.tm> dokumentiert.

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