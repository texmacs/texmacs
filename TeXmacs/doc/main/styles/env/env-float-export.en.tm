<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Using the environments for floating objects>

  The <tmdtd|env-float> <abbr|d.t.d.> provides the following environments for
  floating objects:

  <\explain|<explain-macro|small-figure|body|caption>>
    This macro produces an inline figure with <src-arg|body> as its main body
    and <src-arg|caption> as a caption. Inline figures may for instance be
    used to typeset several small figures side by side inside a floating
    object.
  </explain>

  <\explain|<explain-macro|big-figure|body|caption>>
    This macro produces a big figure with <src-arg|body> as its main body and
    <src-arg|caption> as a caption. Big figures span over the whole paragraph
    width.
  </explain>

  <\explain|<explain-macro|small-table|body|caption>>
    Similar to <markup|small-figure>, but for tables.
  </explain>

  <\explain|<explain-macro|big-table|body|caption>>
    Similar to <markup|big-figure>, but for tables.
  </explain>

  <\explain|<explain-macro|footnote|body>>
    Produces a footnote.
  </explain>

  The figure-like environments also admit \ unnumbered versions
  <markup|small-figure*>, <markup|big-figure*>, <abbr|etc.>, which are
  obtained using <shortcut|(numbered-toggle (focus-tree))>.

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