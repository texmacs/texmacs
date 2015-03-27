<TeXmacs|1.99.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Exporting beamer presentations>

  Once you have created a file with the <tmstyle|beamer> style you may want
  to export it to <abbr|PDF> in order to be able to give your presentation
  without using <TeXmacs>. There are two ways in which this can be done:
  <em|expanded> and <em|unexpanded>.

  <\enumerate>
    <item><em|Expanded> means that <markup|fold>s of all kinds are
    ``flattened'' out before they are exported. This is useful if you intend
    the resulting <abbr|PDF> file to be distributed and printed, since it
    will have exactly as many pages as slides your presentation.

    <item><em|Unexpanded> means that the <abbr|PDF> file will have as many
    pages as <em|steps> your presentation, which depending on your use of
    <markup|fold>, <markup|switch>, <markup|overlay>, etc. will typically
    result in many more pages.
  </enumerate>

  You can select which of these methods will be used with
  <menu|Preferences|Converters|TeXmacs-\<gtr\>Pdf/Postscript|Expand beamer
  slides>.

  <tmdoc-copyright|2015|Miguel de Benito Delgado>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>