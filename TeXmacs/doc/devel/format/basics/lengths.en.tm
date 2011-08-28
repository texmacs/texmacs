<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs> lengths>

  A simple <TeXmacs> length is a number followed by a length unit, like
  <verbatim|1cm> or <verbatim|1.5mm>. <TeXmacs> supports three main types of
  units:

  <\description>
    <item*|Absolute units>The length of an absolute unit like <verbatim|cm>
    or <verbatim|pt> on print is fixed.

    <item*|Context dependent units>Context-dependent length units depend on
    the current font or other environment variables. For instance,
    <verbatim|1ex> corresponds to the height of the ``x'' character in the
    current font and <verbatim|1par> correspond to the current paragraph
    width.

    <item*|User defined units>Any nullary macro, whose name contains only
    lower case roman letters followed by <verbatim|-length>, and which
    returns a length, can be used as a unit itself. For instance, the
    following macro defines the <verbatim|dm> length:

    <\tm-fragment>
      <inactive*|<assign|dm-length|<macro|10cm>>>
    </tm-fragment>
  </description>

  Furthermore, length units can be <em|stretchable>. A stretchable length is
  represented by a triple of rigid lengths: a minimal length, a default
  length and a maximal length. When justifying lines or pages, stretchable
  lengths are automatically sized so as to produce nicely looking layout.

  In the case of page breaking, the <src-var|page-flexibility> environment
  provides additional control over the stretchability of white space. When
  setting the <src-var|page-flexibility> to <math|1>, stretchable spaces
  behave as usual. When setting the <src-var|page-flexibility> to <math|0>,
  stretchable spaces become rigid. For other values, the behaviour is linear.

  <paragraph*|Absolute length units>

  <\description>
    <item*|<code*|cm>>One centimeter.

    <item*|<code*|mm>>One millimeter.

    <item*|<code*|in>>One inch.

    <item*|<code*|pt>>The standard typographic point corresponds to
    <math|1/72.27> of an inch.

    <item*|<verbatim|bp>>A big point corresponds to <math|1/72> of an inch.

    <item*|<verbatim|dd>>The Didôt point equals 1/72 of a French inch,
    <abbr|i.e.> <verbatim|0.376mm>.

    <item*|<verbatim|pc>>One ``pica'' equals 12 points.

    <item*|<verbatim|cc>>One ``cicero'' equals 12 Didôt points.
  </description>

  <paragraph*|Rigid font-dependent length units>

  <\description>
    <item*|<verbatim|fs>>The font size. When using a <verbatim|12pt> font,
    <verbatim|1fs> corresponds to <verbatim|12pt>.

    <item*|<verbatim|fbs>>The base font size. Typically, when selecting
    <verbatim|10> as the font size for your document and when typing large
    text, the base font size is <verbatim|10pt> and the font size
    <verbatim|12pt>.

    <item*|<code*|ln>>The width of a nicely looking fraction bar for the
    current font.

    <item*|<code*|sep>>A typical separation between text and graphics for the
    current font, so as to keep the text readable. For instance, the
    numerator in a fraction is shifted up by <verbatim|1sep>.

    <item*|<verbatim|yfrac>>The height of the fraction bar for the current
    font (approximately <verbatim|0.5ex>).

    <item*|<verbatim|ex>>The height of the ``x'' character in the current
    font.

    <item*|<verbatim|emunit>>The width of the ``M'' character in the current
    font.
  </description>

  <paragraph*|Stretchable font-dependent length units>

  <\description>
    <item*|<code*|fn>>This is a stretchable variant of <verbatim|1quad>. The
    default length of <verbatim|1fn> is <verbatim|1quad>. When stretched,
    <verbatim|1fn> may be reduced to <verbatim|0.5fn> and extended to
    <verbatim|1.5fn>.

    <item*|<verbatim|fns>>This length defaults to zero, but it may be
    stretched up till <verbatim|1fn>.

    <item*|<verbatim|bls>>The ``base line skip'' is the sum of
    <verbatim|1quad> and <src-var|par-sep>. It corresponds to the distance
    between successive lines of normal text.

    Typically, the baselines of successive lines are separated by a distance
    of <verbatim|1fn> (in <TeXmacs> and <LaTeX> a slightly larger space is
    used though so as to allow for subscripts and superscripts and avoid a
    too densely looking text. When stretched, <verbatim|1fn> may be reduced
    to <verbatim|0.5fn> and extended to <verbatim|1.5fn>.

    <item*|<code*|spc>>The (stretchable) width of space character in the
    current font.

    <item*|<verbatim|xspc>>The additional (stretchable) width of a space
    character after a period.
  </description>

  <paragraph*|Box lengths><label|box-lengths>

  Box length units can only be used within some special markup elements, such
  as <markup|move>, <markup|shift>, <markup|resize>, <markup|clipped> and
  <markup|image>. The principal body of this content (<abbr|e.g.> the content
  being ``moved'' in the case of <markup|move>) is typeset as a box. The
  following lengths units then correspond to the size and the extents of the
  box.

  <\description>
    <item*|<code*|w>>The width of the box.

    <item*|<verbatim|h>>The height of the box.

    <item*|<code*|l>>The logical left <math|x>-coordinate of the box.

    <item*|<code*|r>>The logical right <math|x>-coordinate of the box.

    <item*|<code*|b>>The logical bottom <math|y>-coordinate of the box.

    <item*|<code*|t>>The logical top <math|y>-coordinate of the box.
  </description>

  For instance, the code

  <\tm-fragment>
    <inactive*|<move|Hello there||<plus|-0.5b|-0.5t>>>
  </tm-fragment>

  can be used to center <move|Hello there||<plus|-0.5b|-0.5t>> at the
  base-line.

  <paragraph*|Other length units>

  <\description>
    <item*|<code*|par>>The width of the paragraph. That is the length the
    text can span. It is affected by paper size, margins, number of columns,
    column separation, cell width (if in a table), <abbr|etc.>

    <item*|<verbatim|pag>>The height of the main text in a page. In a similar
    way as <verbatim|par>, this length unit is affected by page size,
    margins, <abbr|etc.>

    <item*|<code*|px>>One screen pixel, the meaning of this unit is affected
    by the shrinking factor.

    <item*|<code*|tmpt>>The smallest length unit for internal length
    calculations by <TeXmacs>. <verbatim|1px> divided by the shrinking factor
    corresponds to <verbatim|256tmpt>.
  </description>

  <paragraph*|Different ways to specify lengths>

  There are three types of lengths in <TeXmacs>:

  <\description>
    <item*|Simple lengths>A string consisting of a number followed by a
    length unit.

    <item*|Abstract lengths>An abstract length is a macro which evaluates to
    a length. Such lengths have the advantage that they may depend on the
    context.

    <item*|Normalized lengths>All lengths are ultimately converted into a
    normalized length, which is a tag of the form <explain-macro|tmlen|l>
    (for rigid lengths) or <explain-macro|tmlen|min|def|max> (for stretchable
    lengths). The user may also use this tag in order to specify stretchable
    lengths. For instance, <inactive*|<tmlen|<minus|1quad|1pt>|1quad|1.5quad>>
    evaluates to a length which is <verbatim|1quad> by default, at least
    <verbatim|1quad-1pt> and at most <verbatim|1.5quad>.
  </description>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>