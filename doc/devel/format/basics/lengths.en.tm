<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard length units>

  The leafs of <TeXmacs> trees either contain ordinary text or special data.
  <TeXmacs> supports the following atomic data types:

  <\description>
    <item*|Boolean numbers>Either <verbatim|true> or <verbatim|false>.

    <item*|Integers>Sequences of digits which may be preceded by a minus
    sign.

    <item*|Floating point numbers>Specified using the usual scientific
    notation.

    <item*|Lengths>Floating point numbers followed by a length unit, like
    <verbatim|29.7cm> or <verbatim|2fn>.
  </description>

  In this section, we describe in more detail the available length units.

  There are mainly two types of length units: relative units and
  context-dependent units. Absolute length units are the usual: their length
  on screen or on print is fixed. Context-dependent length units depend on
  the environment such as the current font.

  Furthermore, some length units are <em|stretchable>. A stretchable length
  is represented by a triple of rigid lengths: a minimal length, a default
  length and a maximal length. When justifying lines or pages, stretchable
  lengths are automatically sized so as to produce nicely looking layout.

  In the case of page breaking, the <src-var|page-flexibility> environment
  provides additional control over the stretchability of white space. When
  setting the <src-var|page-flexibility> to <with|mode|math|1>, stretchable
  spaces behave as usual. When setting the <src-var|page-flexibility> to
  <with|mode|math|0>, stretchable spaces become rigid. For other values, the
  behaviour is linear.

  <paragraph|Absolute length units>

  <\description>
    <item*|<code*|cm>>one centimeter.

    <item*|<code*|mm>>one millimeter.

    <item*|<code*|in>>one inch.

    <item*|<code*|pt>>the typographic point: 1/72 of an inch.
  </description>

  <paragraph|Font-dependent length units>

  <\description>
    <item*|<code*|fn>>the reference size for the font. Typically, the
    baselines of successive lines are separated by a distance of
    <verbatim|1fn> (in <TeXmacs> and <LaTeX> a slightly larger space is used
    though so as to allow for subscripts and superscripts and avoid a too
    densely looking text. When stretched, <verbatim|1fn> may be reduced to
    <verbatim|0.5fn> and extended to <verbatim|1.5fn>.

    <item*|<verbatim|fn*>>A variant of <verbatim|fn>, which defaults to zero,
    but which may be stretched until <verbatim|1fn>.

    <item*|<code*|spc>>The (stretchable) width of space character in the
    current font.

    <item*|<verbatim|ex>>the height of the ``x'' character in the current
    font.

    <item*|<code*|ln>>the width of a nicely looking fraction bar for the
    current font.

    <item*|<verbatim|yfrac>>the height of the fraction bar for the current
    font (approximately <verbatim|0.5ex>).

    <item*|<code*|sep>>A typical separation between text and graphics for the
    current font, so as to keep the text readable. For instance, the
    numerator in a fraction is shifted up by <verbatim|1sep>.
  </description>

  <paragraph|Other length units>

  <\description>
    <item*|<code*|par>>the width of the paragraph. That is the length the
    text can span. It is affected by paper size, margins, number of columns,
    column separation, cell width (if in a table), <abbr|etc.>

    <item*|<verbatim|pag>>the height of the main text in a page. In a similar
    way as <verbatim|par>, this length unit is affected by page size,
    margins, <abbr|etc.>

    <item*|<code*|px>>one screen pixel, the meaning of this unit is affected
    by the screen resolution <TeXmacs> assumes after querying the X server on
    startup.

    <item*|<code*|unit>>1/256th of a pixel. This length unit is used
    internally by <TeXmacs> for length calculations.
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