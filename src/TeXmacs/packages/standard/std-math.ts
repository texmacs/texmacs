<TeXmacs|1.0.2.5>

<\body>
  <assign|std-math-package|1.0>

  <assign|std-math-dtd|1.0>

  \;

  <\with|mode|math>
    <assign|shrink-inline|<macro|x|<if|<equal|<value|formula
    style>|false>|<with|index level|<plus|<value|index level>|1>|display
    style|false|<arg|x>>|<with|display style|false|<arg|x>>>>>

    <drd_props|shrink-inline|accessible|all>

    <assign|matrix|<macro|x|<shrink-inline|<left|(><tformat|<cwith|1|-1|1|-1|cell
    halign|c>|<arg|x>><right|)>>>>

    <assign|det|<macro|x|<shrink-inline|<left|\|><tformat|<cwith|1|-1|1|-1|cell
    halign|c>|<arg|x>><right|\|>>>>

    <assign|choice|<macro|x|<shrink-inline|<left|{><tformat|<arg|x>><right|.>>>>

    <assign|stack|<macro|x|<tformat|<twith|table
    valign|C>|<cwith|1|-1|1|-1|cell halign|c>|<cwith|1|-1|1|1|cell
    lsep|0spc>|<cwith|1|-1|-1|-1|cell rsep|0spc>|<cwith|1|-1|1|-1|cell
    bsep|0.5sep>|<cwith|1|-1|1|-1|cell tsep|0.5sep>|<cwith|1|1|1|-1|cell
    tsep|0sep>|<cwith|-1|-1|1|-1|cell bsep|0sep>|<arg|x>>>>

    \;

    <assign|mathord|<macro|x|<with|math condensed|true|<arg|x>>>>

    <assign|mathopen|<value|op>>

    <assign|mathclose|<value|op>>

    <assign|mathpunct|<value|op>>

    <assign|mathpunct|<value|alpha>>

    <assign|mathbin|<macro|x|<space|0.5spc><with|math
    condensed|true|<arg|x>><space|0.5spc>>>

    <assign|mathrel|<value|mathbin>>

    <assign|mathop|<macro|x|<with|math condensed|true|<arg|x>><space|1spc>>>

    \;

    <assign|binom|<macro|x|y|<shrink-inline|<left|(><resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell
    lsep|0spc>|<cwith|1|-1|1|1|cell rsep|0spc>|<table|<row|<cell|<arg|x>>>|<row|<cell|<arg|y>>>>>>||b+2.5sep||t-2.5sep|><right|)>>>>

    <assign|choose|<value|binom>>

    \;
  </with>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|reduction page bottom margin|15mm>
    <associate|page type|a4>
    <associate|reduction page left margin|25mm>
    <associate|even page margin|30mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
  </collection>
</initial>