<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|‘rodowiska>

  W podobny sposób jak znaczniki zawarto±ci, ±rodowiska s¡ u»ywane do nadania
  cz¦±ci tekstu specjalnego znaczenia. Jednak, podczas gdy
  <hyper-link|znaczniki zawarto±ci|man-content-tags.pl.tm> oznaczaj¡
  przewa»nie maªe partie tekstu, ±rodowisko cz¦sto rozci¡ga si¦ na kilka
  paragrafów. Cz¦sto u»ywanymi ±rodowiskami matematycznymi s¡
  <markup|twierdzenie> i <markup|dowód>, tak jak w poni»szym przykªadzie:

  <\theorem>
    Nie istniej¡ dodatnie liczby caªkowite <with|mode|math|a>,
    <with|mode|math|b>, <with|mode|math|c> i <with|mode|math|n> gdzie
    <with|mode|math|n\<geqslant\>3>, takie, »e
    <with|mode|math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    Nie mam tu miejsca aby zapisa¢ dowód.
  </proof>

  ‘rodowisko mo»na wprowadzi¢ u»ywaj¡c <menu|Tekst|‘rodowisko>. Innymi
  ±rodowiskami z podobnym wygl¡dem s¡ \ <markup|lemat>, <markup|wniosek>,
  <markup|definicja>, <markup|aksjomat>. Ciekawostk¡ jest makro pozwalaj¡ce
  wpisa¢ autora twierdzenia: <markup|dueto> (<key|\\ d u e t o enter>), na
  przykªad

  <\theorem>
    <dueto|Pitagoras>Przy ªadnych zaªo»eniach mamy
    <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  Generalnie dobrze jest wybra¢ adekwatne ±rodowisko do prezentowania tekstu.

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
    <associate|language|polish>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>