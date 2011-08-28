<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Line breaking primitives>

  A simple document is a sequence of <def-index|logical paragraphs>, one for
  each subtree of a <markup|document> or <markup|paragraph> node. Paragraphs
  whose width exceed the available horizontal space are broken into
  <em|physical lines> by the hyphenation algorithm. By default, hyphenated
  lines are justified: horizontal spaces can be shrunk or extended in order
  to produce a good-looking layout.

  <\explain>
    <explain-macro|new-line><explain-synopsis|start a new paragraph>
  <|explain>
    This is a deprecated tag in order to split a logical paragraph into
    several logical paragraphs without creating explicit subtrees for all
    paragraphs.

    We recall that logical paragraphs are important structures for the
    typesetting process. Many primitives and environment variables (vertical
    spacing, paragraph style, indentation, page breaking, etc.) operate on
    whole paragraphs or at the boundaries of the enclosing paragraph.
  </explain>

  <\explain>
    <explain-macro|next-line><explain-synopsis|start a new line>
  <|explain>
    This is a tag which will become deprecated as soon as the
    <markup|paragraph> primitive will be correctly implemented. Its usage is
    similar to the <markup|new-line> tag with the difference that we start a
    new logical paragraph unit instead of a new logical paragraph.

    Currently, the <markup|next-line> tag can also be used in order to force
    a line break with the addional property that the line before the break is
    not justified or filled.
  </explain>

  <\explain>
    <explain-macro|line-break><explain-synopsis|line breaking hint, with
    filling>
  <|explain>
    Print an invisible space with zero hyphenation penalty. The line breaking
    algorithm searches for the set of hyphenation points minimizing the total
    penalty, so line breaking is much more likely to occur at a
    <markup|line-break> than anywhere else in its vicinity.

    Unlike <markup|next-line>, this is a hint which may or may not be obeyed
    by the typesetter, and it does not prevent the previous line from being
    filled.
  </explain>

  <\explain>
    <explain-macro|no-break><explain-synopsis|forbid line breaking at this
    point>
  <|explain>
    Set an hyphenation point with an infinite penalty. That is useful when
    the hyphenation patterns for a language fall short of preventing some
    forbidden patterns like ``arse-nal'' or ``con-genital''. An alternative
    way to prevent breaks is to use the <markup|rigid> tag.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>