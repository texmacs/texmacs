<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Standard mathematical markup>

  Standard mathematical markup is defined in <tmdtd|std-math>.

  <\description>
    <expand|item*|<markup|binom>>For binomial coefficients
    <with|mode|math|<binom|n|m>>.

    <expand|item*|<markup|choose>>Alternative name for <markup|binom>
    (depreciated)

    <expand|item*|<markup|shrink-inline>>A macro which switches to scriptsize
    text when you are not in display style. This macro is mainly used by
    developers. For instance, the <markup|binom> macro uses on it.
  </description>

  The following are standard mathematical tabular environments:

  <\description>
    <expand|item*|<markup|matrix>>For matrices
    <with|mode|math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<c\
    ell|3>|<cell|4>>>>>>.

    <expand|item*|<markup|det>>For determinants
    <with|mode|math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<r\
    ow|<cell|3>|<cell|4>>>>>>.

    <expand|item*|<markup|choice>>For choice lists
    <with|mode|math|\|x\|=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|\
    <with|mode|text|if >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<with|mode|text\
    |if >x\<geqslant\>0>>>>>>
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
