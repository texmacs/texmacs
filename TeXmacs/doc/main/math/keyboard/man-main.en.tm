<TeXmacs|1.0.2.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Main mathematical constructs>

  The main mathematical objects are created using the <key|math> prefix as
  follows:

  <big-table|<descriptive-table|<tformat|<table|<row|<cell|Shortcut>|<cell|Purpose>|<cell|Example>>|<row|<cell|<key|math $>>|<cell|Text>|<cell|<with|mode|math|L={x\|x<with|mode|text|
  is sufficiently large>}>>>|<row|<cell|<key|math f>>|<cell|Fractions>|<cell|<with|mode|math|<frac|a|b+c>>>>|<row|<cell|<key|math s>>|<cell|Square
  roots>|<cell|<with|mode|math|<sqrt|x+y>>>>|<row|<cell|<key|math S>>|<cell|<with|mode|math|n>-th
  Roots>|<cell|<with|mode|math|<sqrt|x<rsup|3>+y<rsup|3>|3>>>>|<row|<cell|<key|math n>>|<cell|Negations>|<cell|<with|mode|math|<neg|<frac|a|b+c>>>>>>>>|Creation
  of major mathematical markup.>

  Primes, subscripts and superscripts are created as follows:

  <big-table|<descriptive-table|<tformat|<table|<row|<cell|Shortcut>|<cell|Purpose>|<cell|Example>>|<row|<cell|<key|'>>|<cell|Primes>|<cell|<with|mode|math|f<rprime|'>>
  or <with|mode|math|<with|mode|math|><with|mode|math|(g+h)<rprime|'''>>>>>|<row|<cell|<key|`>>|<cell|Back-primes>|<cell|<with|mode|math|<lprime|`>f>>>|<row|<cell|<key|_>>|<cell|Subscripts>|<cell|<with|mode|math|x<rsub|n>>
  or <with|mode|math|x<rsub|i<rsub|3>>>>>|<row|<cell|<key|^>>|<cell|Superscripts>|<cell|<with|mode|math|x<rsup|2>>,
  <with|mode|math|x<rsub|n><rsup|2>> or <with|mode|math|\<mathe\><rsup|\<mathe\><rsup|x>>>>>|<row|<cell|<key|math l
  _>>|<cell|Left subscripts>|<cell|<with|mode|math|<lsub|2>x>>>|<row|<cell|<key|math l
  ^>>|<cell|Left superscripts>|<cell|<with|mode|math|<lsup|\<pi\>>x> or
  <with|mode|math|<lsub|\<ast\>><lsup|\<ast\>>He<rsub|\<ast\>><rsup|\<ast\>>>>>>>>|Creation
  of primes, subscripts and superscripts>

  Some important mathematical constructs are actually <hyper-link|tabular
  constructs|../../table/man-create-table.en.tm> and are documented
  separately.

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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
    <associate|gly-2|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Creation of major mathematical markup.|<pageref|gly-1>>

      <tuple|normal|Creation of primes, subscripts and
      superscripts|<pageref|gly-2>>
    </associate>
  </collection>
</auxiliary>