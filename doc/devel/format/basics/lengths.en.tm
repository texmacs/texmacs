<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard length units>

  In <hlink|TeXmacs|http://alqua.com/tmresources/TeXmacs>, lengths are
  denoted by decimal numbers immediately followed by a unit. For example
  <code*|29.7cm> or <code*|2fn>.

  There are mainly types of length units: relative and absolute. Absolute
  length units are the usual: their length on screen or on print is fixed.
  Relative length units, on the other hand, depend on the
  <hlink|context|http://alqua.com/tmresources/context> or, more precisely, on
  the metrics on the current font.

  Stretchable lenght units are relative length units augmented with
  stretching information. That information in only meaninful when used in the
  appropriate context. The strecthing information is always multiplied by the
  <code*|page flexibility>.

  A more narrative description of length units can be found at: <hlink|how to
  use length units|http://alqua.com/tmresources/how to use length units>.

  There are also various other length units which depend on various factors.

  <subsection|Absolute length units>

  <\description>
    <item*|<code*|cm>>the usual IS metric unit: 1/100 of a meter.

    <item*|<code*|mm>>close relative of cm: exactly 1/10 of a <code*|cm>.

    <item*|<code*|in>>the Anglo-American unit: one inch.

    <item*|<code*|pt>>the typographic point: 1/72 of an inch.
  </description>

  <subsection|Relative length units>

  <\description>
    <item*|<code*|fn>>the font base line height. That is the nominal distance
    between two lines of text.

    <item*|<code*|spc>>nominal width of space character, the one used in
    ragged or centered paragraphs.

    <item*|<code*|ln>>thickness of a line, for example, the fraction bar.

    <item*|<code*|sep>>(<hlink|please document
    me|http://alqua.com/tmresources/please document me>)
  </description>

  <subsection|Stretching information>

  <\description>
    <item*|<code*|fn>>base: 1fn / low: 0.5fn / high: 1.5fn

    <item*|<code*|spc>>base: 1spc / low and high: font dependant

    <item*|<code*|fn*>>base: 0 / low: 0 / high: 1fn
  </description>

  <subsection|Other length units>

  <\description>
    <item*|<code*|par>>the width of the paragraph. That is the length the
    text can span. It is affected by paper size, margins, number of columns,
    column separation, cell width (if in a table), etc...

    <item*|<code*|px>>one screen pixel, the meaning of this unit is affected
    by the screen resolution <hlink|TeXmacs|http://alqua.com/tmresources/TeXmacs>
    assumes after querying the X server on startup.

    <item*|<code*|unit>>1/256th of a pixel. That is the length unit used
    internally by <hlink|TeXmacs|http://alqua.com/tmresources/TeXmacs> for
    length calculations.
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