<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Line breaking primitives>

  <big-table|<tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|new-line>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|next-line>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|line-break>>|<cell|0>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|no-break>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>>>>|Line
  breaking primitives>

  A simple document is a sequence of <def-index|logical paragraphs>, one for
  each subtree of a <markup|document> node. Paragraphs whose width exceed the
  available horizontal space are broken in <def-index|soft lines> by the
  hyphenation algorithm. Hyphenated lines are <em|filled>, horizontal spaces
  can be shrunk or extended in order to produce a good-looking layout.

  <\description-dash>
    <item*|<markup|new-line>>Start a new paragraph.

    Split a logical paragraph in <def-index|physical paragraphs>. Physical
    paragraphs are typeset in the same way as logical paragraphs, but are not
    distinct subtrees of a <markup|document> node.

    Paragraphs are important structures for the typesetting process. Many
    primitives and environment variables (vertical spacing, paragraph style,
    indentation, page breaking, etc.) operate on whole paragraphs or at the
    boundaries of the enclosing paragraph.

    <item*|<markup|next-line>>Start a new line.

    Split a paragraph in <def-index|hard lines>. Hard lines are typeset in a
    way similar to hyphenated lines, but their boundaries are specified by
    <markup|next-line> tags. The last soft line in a hard line is never
    filled.

    <item*|<markup|line-break>>Line breaking hint, with filling.

    Print an invisible space with zero hyphenation penalty. The line breaking
    algorithm searches for the set of hyphenation points minimizing the total
    penalty, so line breaking is much more likely to occur at a
    <markup|line-break> than anywhere else in its vicinity.

    Unlike <markup|next-line>, this is a hint which may or may not be obeyed
    by the typesetter, and it does not prevent the previous line from being
    filled.

    <item*|<markup|no-break>>Forbid line breaking at this point.

    Set an hyphenation point with an infinite penalty. That is useful when
    the hyphenation patterns for a language fall short of preventing some
    forbidden patterns like ``arse-nal'' or ``con-genital''.
  </description-dash>

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
    <associate|sfactor|5>
  </collection>
</initial>