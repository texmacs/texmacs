<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Environments for floating objects>

  The <tmdtd|env-float> <abbr|d.t.d.> provides the following environments for
  floating objects:

  <\description>
    <item*|<markup|small-figure>>An inline figure. This tag can for instance
    be used to typeset several small figures side by side inside a floating
    object.

    <item*|<markup|big-figure>>A big figure, which spans over the whole
    paragraph width.

    <item*|<markup|small-table>>Similar to <markup|small-figure>, but for
    tables.

    <item*|<markup|big-table>>Similar to <markup|big-figure>, but for tables.

    <item*|<markup|footnote>>Produces a footnote.
  </description>

  The figure-like environments also admit \ unnumbered versions
  <markup|small-figure*>, <markup|big-figure*>, <abbr|etc.>, which are
  obtained using <key|A-*>.

  The following macros can be used for customizing the rendering of
  figure-like environments:

  <\description>
    <item*|<markup|render-small-figure>>A macro for displaying a small
    figure. The arguments are a short name (like ``figure'' or ``table'') for
    the list of figures, its real name (like ``Figure 2.3'' or ``Table
    <no-break>5''), the figure itself and a caption.

    <item*|<markup|render-big-figure>>A variant of
    <markup|render-small-figure> for displaying a big figure.
  </description>

  The following tags can be used for customizing the appearance the text
  around figures, tables and footnotes:

  <\description>
    <item*|<markup|figure-name>>A macro which controls the appearance of the
    text ``Figure''. By default, we use bold face.

    <item*|<markup|figure-sep>>The separator between the figure and its
    number and the caption. By default, this is a period followed by a space.

    <item*|<markup|footnote-sep>>The separator between the number of the
    footnote and the text. By default, this is a period followed by a space.
  </description>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
    <associate|sfactor|4>
  </collection>
</initial>