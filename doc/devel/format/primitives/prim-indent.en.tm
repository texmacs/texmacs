<TeXmacs|1.0.3.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Indentation primitives>

  <big-table|<assign|tag-info-table|<macro|x|<descriptive-table|<tformat|<cwith|1|2|1|-1|cell-halign|c>|<cwith|1|1|1|-1|cell-row-span|2>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|3|3|cell-bborder|0ln>|<cwith|1|2|1|-1|cell-lborder|1ln>|<cwith|2|2|1|-1|cell-background|pastel
  blue>|<cwith|1|1|3|3|cell-col-span|2>|<cwith|1|1|3|3|cell-row-span|1>|<twith|table-min-rows|3>|<twith|table-min-cols|6>|<twith|table-max-cols|6>|<arg|x>>>>><tag-info-table|<tformat|<cwith|1|-1|4|4|cell-halign|c>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|3|3|4|4|cell-halign|c>|<cwith|3|3|2|2|cell-halign|c>|<cwith|5|5|4|4|cell-halign|c>|<cwith|5|5|2|2|cell-halign|c>|<cwith|4|4|4|4|cell-halign|c>|<cwith|4|4|2|2|cell-halign|c>|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|no-indent>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|yes-indent>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|no-indent*>>|<cell|0>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|yes-indent*>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>>>>|Indentation
  primitives>

  <\description-dash>
    <item*|<markup|no-indent>>Disable indentation for the current paragraph.

    <item*|<markup|yes-indent>>Enable indentation for the current paragraph.

    <item*|<markup|no-indent*>>Disable indentation for the next paragraph.

    <item*|<markup|yes-indent*>>Enable indentation for the next paragraph.
  </description-dash>

  The indentation of a paragraph is the horizontal white space inserted
  before the first line of the paragraph. Indentation directives can be used
  to locally enable or disable indentation: <markup|no-indent> and
  <markup|yes-indent> apply to the current paragraph, <markup|no-indent*> and
  <markup|yes-indent*> apply the next physical paragraph. Only the last
  indentation directive for a given paragraph is effective.

  <\scheme-fragment>
    (document

    \ \ (concat "This " (yes-indent) "indents me." (yes-indent*))

    \ \ (concat "Indented by yes-indent* on previous paragraph.")

    \ \ (concat "Though " (yes-indent) "I'm not indented." (no-indent))

    \ \ (concat "Try to indent next paragraph." (yes-indent*))

    \ \ (concat "No, I do not want to be indented." (no-indent)))
  </scheme-fragment>

  In this example, <markup|no-indent> overrides <markup|yes-indent*> in the
  previous paragraph because it comes later.

  <tmdoc-copyright|2004|David Allouche>

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
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-1|<tuple|1|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-12|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|idx-13|<tuple|1|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-14|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Indentation primitives|<pageref|gly-1>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-indent>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|yes-indent>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-indent*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|yes-indent*>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>