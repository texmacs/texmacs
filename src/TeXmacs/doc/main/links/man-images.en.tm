<TeXmacs|2.1.1>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Inserting images>

  You can include images in the text using the menu <menu|Insert|Image> or
  using drag-and-drop. Currently, <TeXmacs> recognizes the <verbatim|ps>,
  <verbatim|eps>, <verbatim|tif>, <verbatim|pdf>, <verbatim|svg>,
  <verbatim|png>, <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>,
  <verbatim|xpm> and <verbatim|fig> file formats.

  By default, images are displayed at their design sizes and aligned at their
  bottom lines. Alternative widths, heights and alignment offsets may be
  specified in the image chooser dialogue window, using <hlink|<TeXmacs>
  notation for lengths|../../devel/format/basics/lengths.en.tm>.\ 

  <\itemize>
    <item>When specifying a new width, but no height at the prompt (or vice
    versa), the image is resized so as to preserve the aspect ratio. For
    instance, entering a width of <verbatim|1par> will make the image span
    over the entire paragraph width and adjust the height proportionally.

    You may use <verbatim|w> and <verbatim|h> as special lengths for the
    default width and height of the image. For instance, specifying
    <verbatim|2w> and <verbatim|2h> for the width and the height, the image
    will be displayed at twice its default size.

    <item>When specifying an alternative alignment, you may use the
    <verbatim|w> and <verbatim|h> lengths for the displayed width and height
    (<abbr|i.e.> <verbatim|w> and <verbatim|h> no longer stand for the
    default width and height). For instance, using <verbatim|-0.5h> for the
    <math|y>-offset will vertically align the image at its center.
  </itemize>

  We also included a script to convert <name|Xfig> pictures, with optional
  <LaTeX> formulas in it, into encapsulated postscript. In order to include a
  <LaTeX> formula in an <verbatim|xfig> picture, we recall you should enter
  the formula as text, while selecting a <LaTeX> font and setting the special
  flag in the text flags.

  <\remark>
    For the support of certain formats, <TeXmacs> may rely on external
    converters to more standard formats such as <name|Ghostscript>,
    <name|ImageMagick>, <name|Inkscape>, or <name|rsvg-convert>. If
    necessary, then you may consider installing these tools or ask your
    system administrator.
  </remark>

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>