<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Environments for floating objects>

  The <tmdtd|env-float> <abbr|d.t.d.> provides tags for floating objects. The
  following tag is the only high-level one:

  <\description>
    <expand|item*|<markup|footnote>>Make a footnote.
  </description>

  The following low-level tags can be used for the definitions of high-level
  figure and table environments like <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> and <markup|small-table>:

  <\description>
    <expand|item*|<markup|small-figure*>>A macro for displaying a small
    figure. The arguments are a short name (like ``figure'' or ``table'') for
    the list of figures, its real name (like ``Figure 2.3'' or ``Table
    <format|no line break>5''), the figure itself and a caption.

    <expand|item*|<markup|big-figure*>>A variant of <markup|small-figure*>
    for displaying a big figure.
  </description>

  The following tags can be used for customizing the appearance the text
  around figures, tables and footnotes:

  <\description>
    <expand|item*|<markup|figurename>>A macro which controls the appearance
    of the text ``Figure''. By default, we use bold face.

    <expand|item*|<markup|figuresep>>The separator between the figure and its
    number and the caption. By default, this is a period followed by a space.

    <expand|item*|<markup|footnotesep>>The separator between the number of
    the footnote and the text. By default, this is a period followed by a
    space.
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
