<TeXmacs|1.0.3.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Line breaking primitives>

  <big-table|<assign|tag-info-table|<macro|x|<descriptive-table|<tformat|<cwith|1|2|1|-1|cell-halign|c>|<cwith|1|1|1|-1|cell-row-span|2>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|3|3|cell-bborder|0ln>|<cwith|1|2|1|-1|cell-lborder|1ln>|<cwith|2|2|1|-1|cell-background|pastel
  blue>|<cwith|1|1|3|3|cell-col-span|2>|<cwith|1|1|3|3|cell-row-span|1>|<twith|table-min-rows|3>|<twith|table-min-cols|6>|<twith|table-max-cols|6>|<arg|x>>>>><tag-info-table|<tformat|<cwith|1|-1|4|4|cell-halign|c>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|3|3|4|4|cell-halign|c>|<cwith|3|3|2|2|cell-halign|c>|<cwith|5|5|4|4|cell-halign|c>|<cwith|5|5|2|2|cell-halign|c>|<cwith|4|4|4|4|cell-halign|c>|<cwith|4|4|2|2|cell-halign|c>|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|new-line>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|next-line>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|line-break>>|<cell|0>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|no-break>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>>>>|Line
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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|5>
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
    <associate|idx-1|<tuple|1|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-12|<tuple|1|?>>
    <associate|idx-13|<tuple|1|?>>
    <associate|idx-14|<tuple|1|?>>
    <associate|idx-15|<tuple|1|?>>
    <associate|idx-16|<tuple|1|?>>
    <associate|idx-17|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Line breaking primitives|<pageref|gly-1>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-line>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|next-line>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|line-break>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-break>>|<pageref|idx-4>>

      <tuple|<tuple|logical paragraphs>|<pageref|idx-5>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|document>>|<pageref|idx-6>>

      <tuple|<tuple|soft lines>|<pageref|idx-7>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-line>>|<pageref|idx-8>>

      <tuple|<tuple|physical paragraphs>|<pageref|idx-9>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|document>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|next-line>>|<pageref|idx-11>>

      <tuple|<tuple|hard lines>|<pageref|idx-12>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|next-line>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|line-break>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|line-break>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|next-line>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-break>>|<pageref|idx-17>>
    </associate>
  </collection>
</auxiliary>