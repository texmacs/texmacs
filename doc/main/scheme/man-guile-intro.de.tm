<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Einführung in die Guile-Erweiterungs-Sprache>

  Ebenso wie <name|Emacs> besitzt auch <TeXmacs> eine <name|Lisp>-artige
  Erweiterungs-Sprache, den <with|font-shape|small-caps|Guile Scheme> Dialekt
  des <with|font-shape|small-caps|Gnome> Projektes. Bezüglich einer
  Dokumentation für <with|font-shape|small-caps|Guile Scheme> verweisen wir
  auf

  <\verbatim>
    \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <scheme> hat den Vorteil, dass durch externe C und C++ Typen und Prozeduren
  erweitert werden kann. In unsere, Fall haben wir <scheme> mit Routinen
  ausgestattet, mit denen Sie Ihre eigenen Menüs und Tastenkombinationen
  erzeugen können. Sie können damit auch eigene Erweiterungen von <TeXmacs>
  schreiben.

  Wenn Sie den Quellcode von <TeXmacs> heruntergeladen haben, dann sollten
  Sie vielleicht eine Blick auf folgende Dateien werfen:

  <\verbatim>
    \ \ \ Guile/Glue/build-glue-basic.scm<next-line>
    \ \ Guile/Glue/build-glue-editor.scm<next-line>
    \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  Die drei \Rglue'' Dateien enthalten C++ Routinen, die in <scheme> zur
  Verfügung stehen. Im folgenden werden wir einige der wichtigsten
  diskutieren. Wir später einmal eine umfassendere Dokumentation zu
  erstellen. Es ist eine gute Idee auch die <scheme>-Dateien (mit
  Datei-Erweiterung <verbatim|scm> ) im <TeXmacs>-Unter-Verzeichnis
  <verbatim|progs> zu betrachten.

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