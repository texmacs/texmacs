<TeXmacs|1.0.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematical formulas>

  In order to type mathematical formulas, you should first enter ``math
  mode'' by pressing the <key|$>-key or by inserting an equation (using
  <menu|Text|Mathematics|Equation>). In math mode, you have specific commands
  and key-combinations to type mathematical symbols and formulas. For
  instance, the <key|H-> prefix can be used in order to enter Greek symbols
  (recall that <key|H-> is equivalent to <key|F5>, <key|<key-escape>
  <key-escape> <key-escape>> or <key|A-C->).

  The editor favors typing mathematics with a certain meaning. This feature,
  which will be developed more in future releases, is useful when
  communicating with a computer algebra package. At this moment, you should
  for instance explicitly type the multiplication <key|*> between symbols
  <with|mode|math|a> and <with|mode|math|b>. By default, typing <key|a b>
  will yield <with|mode|math|mode|text|ab> and not <with|mode|math|a*b>.

  <\traverse>
    <branch|Main mathematical constructs|keyboard/man-main.en.tm>

    <branch|Mathematical symbols|keyboard/man-symbols.en.tm>

    <branch|Big operators|keyboard/man-big.en.tm>

    <branch|Large delimiters|keyboard/man-large.en.tm>

    <branch|Wide accents|keyboard/man-wide.en.tm>
  </traverse>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Mathematics>|<with|font-family|<quote|ss>|Equation>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>