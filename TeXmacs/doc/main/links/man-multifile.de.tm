<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Bücher, aus mehreren Dateien bestehende Dokumente>

  Wenn ein Dokument wirklich groÿ wird, ist es sinnvoll, es in mehrere Teile
  zu zerlegen. Das macht es leichter, die einzelnen Teile an anderer Stelle
  weiter zu verwenden und macht zugleich den Editor schneller. \ Eine ganze
  Datei kann in eine andere mit dem Befehl <menu|Insert|Link|Include>
  einfügen. Um den Zugriff auf die einzelnen Dokumente zu beschleunigen,
  werden die eingefügten Dokumente in einen Puffer geladen. Um alle
  eingefügten Dokumente zu aktualisieren, benutzt man
  <menu|Tools|Update|Inclusions>.

  Wenn man ein Buch schreibt, nimmt man üblicherweise für jedes Kapitel eine
  Datei, z.B. <verbatim|c1.tm>, <verbatim|c2.tm> bis <verbatim|cn.tm>. Dann
  erzeugt man eine Datei book.tm, in die man die Dateien <verbatim|c1.tm>,
  <verbatim|c2.tm> bis <verbatim|cn.tm> auf die oben beschriebene Weise
  einfügt. Auch das Inhaltsverzeichnis, Literaturverzeichnis usw. werden
  normalerweise in <verbatim|book.tm> eingefügt.

  Um Referenzen zu anderen Kapiteln zu sehen, wenn man ein bestimmtes Kapitel
  <verbatim|ci.tm> editiert, kann man <verbatim|book.tm> zur
  <localize|master> für die Dateien <verbatim|c1.tm> bis <verbatim|cn.tm>
  <menu|Document|Master|Attach> machen. Im Moment existiert in diesem Fall
  noch keine automatische Kapitel-Nummerierung. Deshalb muss man die
  Kapitel-Nummern mit Hand über die Kontext-Variable <src-var|chapter-nr>
  während der Editierung einfügen.

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