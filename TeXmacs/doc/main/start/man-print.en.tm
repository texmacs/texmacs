<TeXmacs|1.99.12>

<style|<tuple|tmdoc|english|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Printing documents>

  You can print the current file using <menu|File|Print|Print buffer...>. By
  default, <TeXmacs> assumes that you have a 600dpi printer for a4 paper.
  These default settings can be changed in <menu|File|Page setup...> . You
  can also print to a postscript file using <menu|File|Print|Print buffer to
  file...> (in which case the default printer settings are used for creating
  the output) or <menu|File|Export|Postscript> (in which case the printer
  settings are ignored).

  You may export to <acronym|PDF> using <menu|File|Export|Pdf>. Notice that
  you should set <menu|Edit|Preferences|Printer|Font type|Type 1> if you want
  the produced Postscript or <acronym|PDF> file to use <name|Type 1>
  fonts.<index|pdf> However, only the CM fonts admit <name|Type 1> versions.
  These CM fonts are of a slightly inferior quality to the EC fonts, mainly
  for accented characters. Consequently, you might prefer to use the EC fonts
  as long as you do not need a PDF file which looks nice in <name|Acrobat
  Reader>.

  When adequately configuring <TeXmacs>, the editor is guaranteed to be
  <em|wysiwyg>: the result after printing out is exactly what you see on your
  screen. In order to obtain full wysiwygness, you should in particular
  select <verbatim|paper> for <menu|Document|Page|Format|Page rendering> and
  <menu|Document|Page|Margins|Same screen margins as on paper>. You should
  also make sure that the characters on your screen use the same number of
  dots per inch as your printer. This rendering precision of the characters
  may be changed using <menu|File|Page setup...|Printer dpi>. Currently,
  minor typesetting changes may occur when changing the dpi, which may
  globally affect the document through line and page breaking. In a future
  release this drawback should be removed.

  <tmdoc-copyright|1998--2020|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>