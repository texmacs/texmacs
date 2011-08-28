<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Bilder einfügen>

  Man kann Bilder mit den Befehlen aus dem Menü <menu|Insert|Image> in den
  Text einfügen. Zur Zeit erkennt <TeXmacs> <verbatim|ps>, <verbatim|eps>,
  <verbatim|tif>, <verbatim|pdf>, <verbatim|pdm>, <verbatim|gif>,
  <verbatim|ppm>, <verbatim|xpm> und <verbatim|fig> Datei-Formate. <TeXmacs>
  benutzt selbst das <verbatim|gs>-Format (mit anderen Worten ghostscript)
  zur Darstellung von Postscript-Bildern. Wenn Sie ghostscript noch nicht auf
  ihrem Rechner haben, dann können Sie es von

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  herunterladen. Zur Zeit werden die anderen Formate in Postscript
  umgewandelt. Dazu werden die Skripte <verbatim|tiff2ps>, <verbatim|pdf2ps>,
  <verbatim|pnmtops>, <verbatim|giftopnm>, <verbatim|ppmtogif> und
  <verbatim|xpmtoppm> verwendet. Wenn diese nicht auf Ihrem Rechner sind,
  sollten Sie sich diese besorgen.

  Gemäÿ Vorgabe werden die Bilder in Ihrer ursprünglichen Gröÿe dargestellt.
  Die folgenden Operationen können vorgenommen werden:

  <\itemize>
    <item>Ausschneiden eines Rechtecks. Die untere linke Ecke ist der
    Bezugspunkt für die Rechteck-Gröÿe.

    <item>Gröÿen-Änderung. Wenn nur die Höhe oder nur die Breite spezifiziert
    wird, bleibt das Seitenverhältnis bei der Änderung erhalten.

    <item>Vergröÿerung des Maÿstabs. Höhe und Breite werden mit dem gleichen
    Multiplikator versehen.
  </itemize>

  <TeXmacs> enthält auÿerdem ein Skript zur Konvertierung von Bildern nach
  eps (encapsulated PostScript). Die Bilder dürfen <LaTeX>-Formeln enthalten.
  Es sei daran erinnert, dass man <LaTeX>-Formeln in <verbatim|xfig>-Bilder
  einfügen kann, indem man sie als Text, mit der Schriftart (font) <LaTeX>
  und das spezielle Flag in den Textflags setzt.

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