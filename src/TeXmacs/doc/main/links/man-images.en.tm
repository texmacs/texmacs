<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Inserting images>

  You can include images in the text using the menu <submenu|Insert|image>.
  Currently, <apply|TeXmacs> recognizes the <verbatim|ps>, <verbatim|eps>,
  <verbatim|tif>, <verbatim|pdf>, <verbatim|pdm>, <verbatim|gif>,
  <verbatim|ppm>, <verbatim|xpm> and <verbatim|fig> file formats. Here,
  <verbatim|gs> (i.e. ghostscript) is used to render postscript images. If
  ghostscript has not yet been installed on your system, you can download
  this package from\ 

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  Currently, the other file formats are converted into postscript files using
  the scripts <verbatim|tiff2ps>, <verbatim|pdf2ps>, <verbatim|pnmtops>,
  <verbatim|giftopnm>, <verbatim|ppmtogif>, <verbatim|xpmtoppm>. If these
  scripts are not available on your system, please contact your system
  administrator.

  By default, images are displayed at their design size. The following
  operations are supported on images:

  <\itemize>
    <item>Clipping the images following a rectangle. The lower left corner of
    the default image is taken as the origin for specifying a rectangle for
    clipping.

    <item>Resizing an image. When specifying a new width, but no height at
    the prompt (or vice versa), the image is resized so as to preserve the
    aspect ration.

    <item>Magnifying the image. An alternative way to resize an image, by
    multiplying the width and the height by a constant.
  </itemize>

  We also included a script to convert pictures, with optional <apply|LaTeX>
  formulas in it, into encapsulated postscript. In order to include a
  <apply|LaTeX> formula in an <verbatim|xfig> picture, we recall you should
  enter the formula as text, while selecting a <apply|LaTeX> font and setting
  the special flag in the text flags.

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
