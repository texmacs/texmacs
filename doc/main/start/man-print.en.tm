<TeXmacs|1.0.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Printing documents>

  You can print the current file using <menu|File|Print|Print all>. By
  default, <TeXmacs> assumes that you have a 600dpi printer for a4 paper.
  These default settings can be changed in <menu|Edit|Preferences|Printer>.
  You can also print to a postscript file using <menu|File|Print|Print all to
  file> (in which case the default printer settings are used for creating the
  output) or <menu|File|Export|Postscript> (in which case the printer
  settings are ignored). You may export to <acronym|PDF> using
  <menu|File|Export|Pdf>. Notice that you should set
  <menu|Edit|Preferences|Printer|Font type|True type> if you want the
  produced Postscript or <acronym|PDF> file to use Type 3 fonts.<index|pdf>

  When adequately configuring <TeXmacs>, the editor is guaranteed to be
  <em|wysiwyg>: the result after printing out is exactly what you see on your
  screen. In order to obtain full wysiwygness, you should in particular
  select <menu|Document|Page|Type|Paper> and <menu|Document|Page|Screen
  layout|Margins as on paper>. You should also make sure that the characters
  on your screen use the same number of dots per inch as your printer. This
  rendering precision of the characters may be changed using
  <menu|Document|Font|Dpi>. Currently, minor typesetting changes may occur
  when changing the dpi, which may globally affect the document through line
  and page breaking. In a future release this drawback should be removed.

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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Print>|<with|font-family|<quote|ss>|Print
      all>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Preferences>|<with|font-family|<quote|ss>|Printer>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Print>|<with|font-family|<quote|ss>|Print
      all to file>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Export>|<with|font-family|<quote|ss>|Postscript>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Page>|<with|font-family|<quote|ss>|Type>|<with|font-family|<quote|ss>|Paper>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Page>|<with|font-family|<quote|ss>|Screen
      layout>|<with|font-family|<quote|ss>|Margins as on
      paper>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Font>|<with|font-family|<quote|ss>|Dpi>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>