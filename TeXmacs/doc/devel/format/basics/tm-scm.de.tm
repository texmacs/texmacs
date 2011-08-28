<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<value|scheme>-Linearisierung>

  In der Sprache <value|scheme> kann man leicht <TeXmacs>-Erweiterungen
  schreiben. In diesem Kontext werden <TeXmacs>-Bäume üblicherweise als
  \ <value|scheme>-Ausdrücke dargestellt. Die <value|scheme>-Syntax wurde
  geschaffen, um eine Sprache zu haben, die leicht von Hand zu editieren ist,
  voraussagbare Ergebnisse liefert und bei dem die interne Struktur von
  Dokumenten vollständig erkennbar ist. Beispielsweise wird die Formel\ 

  <\tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </tm-fragment>

  \ in <value|scheme> dargestellt durch

  <\tm-fragment>
    <verbatim|(with "mode" "math" (concat "x+y+" (frac "1" "2") "+" (sqrt
    "y+z")))>
  </tm-fragment>

  Die Darstellung in <value|scheme> kann sehr nützlich sein, wenn komplexe
  Makros mit hohem Programmieraufwand geschrieben werden sollen. Schlieÿlich
  ist <value|scheme> das sicherste Format, wenn Textstücke aus <TeXmacs> per
  Email versandt werden sollen, da sowohl das Standard-<TeXmacs>-Format und
  als auch die XML-Linearisierung empfindlich auf Leerraum reagieren kann.

  Um ein Dokument im <value|scheme>-Format zu sichern oder zu laden, können
  Sie die Menüs <menu|File|Export|Scheme> bzw. <menu|File|Import|Scheme>
  verwenden. Dateien, die im <value|scheme>-Format gespeichert wurden, können
  in der Regel ohne weiteres von externen <value|scheme>-Programmen
  verarbeitet werden, ganz so wie Dateien im XML-Format von XML-Werkzeugen,
  wie z.B. XSLT.

  Um einen Teil eines Dokuments im <value|scheme>-Format in eine Email zu
  kopieren, können Sie das Menü <menu|Edit|Copy to|Scheme> benutzen.
  Entsprechend können Sie externen <value|scheme>-Code in einen
  <TeXmacs>-Text einfügen, indem Sie <menu|Edit|Paste from|Scheme> benutzen.
  Das <value|scheme>-Format kann auch mit interaktiven Kurzbefehlen verwendet
  werden. Z.B. führt die Eingabe von <shortcut|(interactive exec-interactive-command)> \ mit nachfolgendem

  <\scheme-fragment>
    (insert '(frac "1" "2"))
  </scheme-fragment>

  zur Einfügung von <frac|1|2> an der Cursorposition.

  Schlieÿlich ist dieses Format zur interaktiven Eingabe geeignet, wenn
  <TeXmacs> als Oberfläche für <value|scheme>-Sitzungen eingesetzt wird.

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