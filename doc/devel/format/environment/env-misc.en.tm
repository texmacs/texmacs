<TeXmacs|1.0.3.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Miscellaneous environment variables>

  The following miscellaneous environment variables are mainly intended for
  internal use:

  <\explain>
    <var-val|save-aux|true><explain-synopsis|save auxiliary content>
  <|explain>
    This flag specifies whether auxiliary content has to be saved along with
    the document.
  </explain>

  <\explain>
    <var-val|sfactor|5><explain-synopsis|shrinking factor>
  <|explain>
    The shrinking factor which is used for rendering.
  </explain>

  <\explain>
    <var-val|par-no-first|false><explain-synopsis|disable first indentation
    for next paragraph?>
  <|explain>
    This flag disables first indentation for the next paragraph.
  </explain>

  <\explain>
    <src-var|cell-format><explain-synopsis|current cell format>
  <|explain>
    This variable us used during the typsetting of tables in order to store
    the with-settings which apply to the current cell.
  </explain>

  <\explain>
    <src-var|atom-decorations>

    <src-var|line-decorations>

    <src-var|page-decorations>

    <src-var|xoff-decorations>

    <src-var|yoff-decorations><explain-synopsis|auxiliary variables for
    decorations>
  <|explain>
    These environment variables store auxiliary information during the
    typsetting of decorations.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>