<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Boolean operations>

  <\explain>
    <explain-macro|or|expr-1|<with|mode|math|\<cdots\>>|expr-n>

    <explain-macro|and|expr-1|<with|mode|math|\<cdots\>>|expr-n>
  <|explain>
    Returns the result of the boolean or/and on the expressions
    <src-arg|expr-1> until <src-arg|expr-n>. For instance,
    <inactive*|<or|false|<equal|1|1>|false>> yields
    <or|false|<equal|1|1>|false>.
  </explain>

  <\explain>
    <explain-macro|xor|expr-1|expr-2>
  <|explain>
    Returns the exclusive or of two expressions <src-arg|expr-1> and
    <src-arg|expr-2>, <abbr|i.e.> <inactive*|<xor|true|true>> yields
    <xor|true|true>.
  </explain>

  <\explain>
    <explain-macro|not|expr>
  <|explain>
    Returns the negation of <src-arg|expr>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>