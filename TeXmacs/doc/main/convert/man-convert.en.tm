<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Compatibility with other formats>

  <TeXmacs> documents can be saved without loss of information in three
  formats: the native <TeXmacs> format (file extension <verbatim|.tm>),
  <name|Xml> (<verbatim|.tmml>) and as a <name|Scheme> expression
  (<verbatim|.stm>). <TeXmacs> also provides bi-directional converters for
  <LaTeX>, <name|Html> and <name|MathML>.

  In addition to the above textual formats,<TeXmacs> documents can be
  exported in a <em|wysiwyg> (what-you-see-is-what-you-get) way to either
  <name|Postscript> or <name|Pdf>, which are used as the primary formats for
  <hlink|printing documents|../start/man-print.en.tm>. <TeXmacs> can also
  export document fragments to several vector or raster image formats.

  <TeXmacs> documents can be converted to other formats using the different
  items in the <menu|File|Export> menu. Similarly, the <menu|File|Import>
  menu contains all formats which can be imported into <TeXmacs>. Besides
  exporting or importing entire documents, it is also possible to copy and
  paste document fragments in various formats using <menu|Edit|Copy to> and
  <menu|Edit|Paste from>. The default formats for copy and pasting can be
  specified in <menu|Tools|Miscellaneous|Export selections as> and
  <menu|Tools|Miscellaneous|Import selections as>.

  <\traverse>
    <branch|<LaTeX>|latex/man-latex.en.tm>

    <branch|<name|Html> and <name|MathML>|html/man-html.en.tm>

    <branch|Exporting to image formats|man-graphics-export.en.tm>

    <branch|Adding new converters|new/man-newconv.en.tm>
  </traverse>

  \;

  <tmdoc-copyright|1998\U2022|Joris van der Hoeven and the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>