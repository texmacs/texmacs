<TeXmacs|1.0.3.4>

<style|<tuple|source|common-base>>

<\body>
  <assign|std-math-package|1.0>

  <assign|std-math-dtd|1.0>

  \;

  <assign|shrink-inline|<macro|x|<if|<equal|<value|math-display>|false>|<with|math-level|<plus|<value|math-level>|1>|display
  style|false|<arg|x>>|<with|display style|false|<arg|x>>>>>

  <drd-props|shrink-inline|arity|1|accessible|all>

  <assign|matrix|<macro|x|<shrink-inline|<left|(><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>><right|)>>>>

  <assign|det|<macro|x|<shrink-inline|<left|\|><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>><right|\|>>>>

  <assign|choice|<macro|x|<shrink-inline|<left|{><tformat|<arg|x>><right|.>>>>

  <assign|stack|<macro|x|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0.5sep>|<cwith|1|-1|1|-1|cell-tsep|0.5sep>|<cwith|1|1|1|-1|cell-tsep|0sep>|<cwith|-1|-1|1|-1|cell-bsep|0sep>|<arg|x>>>>

  \;

  <assign|mathord|<macro|x|<with|math-condensed|true|<arg|x>>>>

  <assign|mathopen|<value|op>>

  <assign|mathclose|<value|op>>

  <assign|mathpunct|<value|op>>

  <assign|mathpunct|<value|alpha>>

  <assign|mathbin|<macro|x|<space|0.5spc><with|math-condensed|true|<arg|x>><space|0.5spc>>>

  <assign|mathrel|<value|mathbin>>

  <assign|mathop|<macro|x|<with|math-condensed|true|<arg|x>><space|1spc>>>

  \;

  <assign|binom|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<left|(><resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||b+2.5sep||t-2.5sep|><right|)>>>>>>

  <assign|choose|<value|binom>>

  \;
</body>

<\initial>
  <\collection>
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
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>