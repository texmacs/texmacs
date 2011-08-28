<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard-Mathematik>

  <tmdtd|std-math> definiert die mathematischen Standard-Formate:

  <\explain|<explain-macro|binom|among|nr>>
    Binomialkoeffizienten, z.B. mit <em|<with|color|brown|among><with|color|brown|>>
    = n und <with|color|brown|<em|nr<with|color|brown|>>> = m:
    <with|mode|math|<binom|n|m>>.
  </explain>

  <\explain|<explain-macro|choose|among|nr>>
    Alternative für <markup|binom>, aber überholt.
  </explain>

  <\explain|<explain-macro|shrink-inline|among|nr>>
    Ein Makro, das auf die Gröÿe von Index-Stilen schrumpft, wenn es nicht in
    eigenständigen Formeln verwendet wird. Dieses Makro wird hauptsächlich
    von Entwicklern benutzt. Z.B. setzt das Makro <markup|binom> dieses Makro
    ein.
  </explain>

  Auÿerdem werden die folgenden tabellarischen Standard-Formate definiert:

  <\explain|<explain-macro|matrix|table>>
    Für Matrizen wie z.B. <with|mode|math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<explain-macro|det|table>>
    Für Determinanten wie z.B. <with|mode|math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<explain-macro|choice|table>>
    Für mehrfache Bedingungen o.ä. <with|mode|math|\|x\|=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<with|mode|text|if
    >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<with|mode|text|if
    >x\<geqslant\>0>>>>>>.
  </explain>

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