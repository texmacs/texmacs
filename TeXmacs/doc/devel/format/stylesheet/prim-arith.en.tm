<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Arithmetic operations>

  <\explain>
    <explain-macro|plus|expr-1|<with|mode|math|\<cdots\>>|expr-n>

    <explain-macro|minus|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|addition
    and subtraction>
  <|explain>
    Add or subtract numbers or lengths. For instance,
    <inactive*|<plus|1|2.3|5>> yields <plus|1|2.3|5> and
    <inactive*|<plus|1cm|5mm>> produces <plus|1cm|5mm>. In the case of
    subtractions, the last argument is subtracted from the sum of the
    preceding arguments. For instance, <inactive*|<minus|1>> produces
    <minus|1> and <inactive*|<minus|1|2|3|4>> yields
    <no-break><minus|1|2|3|4>.
  </explain>

  <\explain>
    <explain-macro|times|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|multiplication>
  <|explain>
    Multiply two numbers <src-arg|expr-1> until <src-arg|expr-n>. One of the
    arguments is also allowed to be a length, in which case a length is
    returned. For instance, <inactive*|<times|3|3>> evaluates to <times|3|3>
    and <inactive*|<times|3|2cm>> to <times|3|2cm>.
  </explain>

  <\explain>
    <explain-macro|over|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|division>
  <|explain>
    Divide the product of all but the last argument by the last argument. For
    instance, <inactive*|<over|1|2|3|4|5|6|7>> evaluates to
    <over|1|2|3|4|5|6|7>, <inactive*|<over|3spc|7>> to <over|3spc|7>, and
    <inactive*|<over|1cm|1pt>> to <over|1cm|1pt>.
  </explain>

  <\explain>
    <explain-macro|div|expr-1|expr-2>

    <explain-macro|mod|expr-1|expr-2><explain-synopsis|division with
    remainder>
  <|explain>
    Compute the result of the division of an integer <src-arg|expr-1> by an
    integer <src-arg|expr-2>, or its remainder. For instance,
    <inactive*|<div|18|7>>=<div|18|7> and <inactive*|<mod|18|7>>=<mod|18|7>.
  </explain>

  <\explain>
    <explain-macro|equal|expr-1|expr-2>

    <explain-macro|unequal|expr-1|expr-2>

    <explain-macro|less|expr-1|expr-2>

    <explain-macro|lesseq|expr-1|expr-2>

    <explain-macro|greater|expr-1|expr-2>

    <explain-macro|greatereq|expr-1|expr-2><explain-synopsis|comparing
    numbers or lengths>
  <|explain>
    Return the result of the comparison between two numbers or lengths. For
    instance, <inactive*|<less|123|45>> yields <less|123|45> and
    <inactive*|<less|123mm|45cm>> yields <less|123mm|45cm>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>