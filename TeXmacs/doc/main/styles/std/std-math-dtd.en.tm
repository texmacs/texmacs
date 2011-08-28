<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard mathematical markup>

  Standard mathematical markup is defined in <tmdtd|std-math>.

  <\explain|<explain-macro|binom|among|nr>>
    For binomial coefficients, like <with|mode|math|<binom|n|m>>.
  </explain>

  <\explain|<explain-macro|choose|among|nr>>
    Alternative name for <markup|binom>, but depreciated.
  </explain>

  <\explain|<explain-macro|shrink-inline|among|nr>>
    A macro which switches to scriptsize text when you are not in display
    style. This macro is mainly used by developers. For instance, the
    <markup|binom> macro uses it.
  </explain>

  The following are standard mathematical tabular environments:

  <\explain|<explain-macro|matrix|table>>
    For matrices <with|mode|math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<explain-macro|det|table>>
    For determinants <with|mode|math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<explain-macro|choice|table>>
    For choice lists <with|mode|math|\|x\|=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<with|mode|text|if
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
    <associate|language|english>
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