<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Miscellaneous physical markup>

  <\big-table>
    <tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|group>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|All>|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|float>>|<cell|<with|mode|math|3>>|<cell|Yes>|<cell|Last>|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|specific>>|<cell|<with|mode|math|2>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>>>>
  </big-table|Miscellaneous physical markup primitives>

  <\explain>
    <explain-macro|group|content><explain-synopsis|atomic entity>
  <|explain>
    Typeset the <src-arg|content>, which must be <re-index|line content>, as
    an atomic line item. Hyphenation within the <markup|group> and special
    spacing handling on its borders are disabled.
  </explain>

  <\explain>
    <explain-macro|float|type|where|body><explain-synopsis|floating page
    insertion>
  <|explain>
    Floating insertions are page items which are typeset ``out of band'',
    they are associated to two boxes: the anchor box marks the structural
    position of the <markup|float>, the floating box contains the typeset
    <src-arg|body> operand. This facility is used by footnotes and floating
    blocks.

    The first and second operands are evaluated, but for clarity the first
    operand appears as a literal string in the examples. Since the
    <src-arg|body> is typeset out of band, it may be <re-index|block content>
    even if the <markup|float> occurs in <re-index|line context>.

    <\indent>
      <explain-macro|float|<src-value|footnote>||body> produces a footnote
      insertion, this should only be used within the <markup|footnote> macro
      and is considered style markup. The floating box of a footnote is
      typeset at the end of the the page containing the anchor box.

      <explain-macro|float|<src-value|float>|where|body> produces a floating
      block, this is considered physical markup. The position of the floating
      box is chosen by the page breaker, which uses this extra freedom to
      minimize the page breaking penalty.

      The <src-arg|where> operand must evaluate to a string which may contain
      the following characters:

      <\description>
        <item*|t>Allow the floating box at page <em|top>.

        <item*|b>Allow the floating box at page <em|bottom>.

        <item*|h>Allow the floating box ``<em|here>'', in the middle of the
        page near the anchor box.

        <item*|f><em|Force> the floating box within the same page as the
        anchor box.
      </description>
    </indent>
  </explain>

  <\explain>
    <explain-macro|specific|medium|body><explain-synopsis|medium-specific
    content>
  <|explain>
    <todo|<verbatim|screen> and <verbatim|printer> variants seem broken, fix
    and document.>

    This primitive marks <src-arg|body> for output only on the specified
    <src-arg|medium>.

    If the <src-arg|medium> \ operand evaluates to <verbatim|texmacs>,
    typeset <src-arg|body> as line content. For (most) other values of
    <src-arg|medium>, typeset as an empty box.

    This tag is treated specially by conversion filters: the <LaTeX>
    (<abbr|resp.> <acronym|HTML>) export filter only convert <src-arg|body>
    if <src-arg|medium> \ evaluates to <verbatim|latex> (<abbr|resp.>
    <verbatim|html>). This feature relies on the fact that conversion filters
    are applied on the <em|evaluated> document contents.
  </explain>

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