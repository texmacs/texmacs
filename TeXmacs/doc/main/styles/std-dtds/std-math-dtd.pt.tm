<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Maração matemática padrão>

  A marcação matemática padrão é definida em <tmdtd|std-math>.

  <\explain|<markup|binom>>
    Para coeficientes binomiais <math|<binom|n|m>>.
  </explain>

  <\explain|<markup|choose>>
    Outro nome para <markup|binom> (obsoleto).
  </explain>

  <\explain|<markup|shrink-inline>>
    Um macro que muda o tamanho do texto para ``scriptsize'' quando você não
    está no estilo de exibição. Este macro é usado principalmente pelos
    desenvolvedores. Por exemplo, é usado em <markup|binom>.
  </explain>

  Abaixo estão os ambientes matemáticos padrão:

  <\explain|<markup|matrix>>
    Para matrizes <math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|det>>
    Para determinantes <math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|choice>>
    Para listas de escolhas <math|<around|\||x|\|>=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<text|if
    >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<text|if >x\<geqslant\>0>>>>>>
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|info-flag|minimal>
    <associate|language|portuguese>
  </collection>
</initial>