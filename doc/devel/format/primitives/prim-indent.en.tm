<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Indentation primitives>

  <big-table|<tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|no-indent>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|yes-indent>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|no-indent*>>|<cell|0>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|yes-indent*>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>>>>|Indentation
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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>