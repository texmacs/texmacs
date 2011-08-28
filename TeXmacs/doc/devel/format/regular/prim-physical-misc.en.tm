<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Miscellaneous physical markup>

  <\explain>
    <explain-macro|rigid|content><explain-synopsis|atomic entity>
  <|explain>
    Typeset the <src-arg|content>, which must be <re-index|line content>, as
    an atomic line item. Hyphenation within the <markup|rigid> and special
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
    This primitive marks <src-arg|body> for output only on the specified
    <src-arg|medium>. The following values of <src-arg|medium> are supported:

    <\description>
      <item*|texmacs>The <src-arg|body> is typeset as usual line content.

      <item*|latex>The <src-arg|body>, which must be a string, is not visible
      from within <TeXmacs>, but it will be included in a verbatim way when
      the document is exported to <LaTeX>.

      <item*|html>Similar to the <verbatim|latex> medium, but for <name|HTML>
      exports.

      <item*|screen>The <src-arg|body> is only typeset when the document is
      visualized on a screen. This may be useful to provide additional visual
      information to the user during the editing phase which should disappear
      when printing out. A similar tag which may be used for this purpose is
      <markup|flag>.

      <item*|printer>This medium is complementary to <verbatim|screen>, when
      the <src-arg|body> should only be visible when printing out, but not
      when the document is displayed on the screen.
    </description>
  </explain>

  <\explain|<explain-macro|raw-data|data><explain-synopsis|binary content>>
    In some contexts you need to embed uneditable data inside a document,
    most of the time this is uneditable binary data. The <markup|raw-data>
    primitive makes it impossible to view or modify its subtree from within
    the editor.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>