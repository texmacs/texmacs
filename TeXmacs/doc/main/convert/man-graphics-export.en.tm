<TeXmacs|2.1.2>

<style|<tuple|tmdoc|doc|british|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Export or Copy selection to graphics>

  <TeXmacs> can export the active selection to many graphical formats, either
  as files (<menu|File|Export|Export selection as image>) or through the
  system clipboard (<menu|Edit|Copy to...|Image>), for pasting into other
  applications.\ 

  <paragraph|Specifying graphics format> \ 

  In case of file export, the desired graphical format is determined by the
  file extension you choose (for instance: pdf, eps, jpg...).

  For the clipboard mechanism, you need to set in advance the desired format
  in <menu|Edit|Preferences|Converters|TeXmacs -\<gtr\> image|Clipboard image
  format>. This menu offers a choice between <acronym|png>, <acronym|jpeg>,
  <acronym|tif,> <acronym|eps,> <acronym|svg> and <name|pdf> formats,
  provided a suitable converter is available (see below).

  For both the clipboard mechanism and file export, the resolution of bitmap
  formats is set by the preference <menu|Edit|Preferences|Converters|TeXmacs
  -\<gtr\> image|Bitmap export resolution (dpi)>.

  When \ <acronym|Svg> format is selected, <TeXmacs> annotates the image with
  the source information for the image's content. When inserted in
  <name|Inkscape> or <name|Libreoffice> documents, such images can easily be
  re-edited using the <hlink|<name|Equation editor>
  plugin|../../../plugins/equation-editor/doc/equation-editor.en.tm>.\ 

  <paragraph|Required external converters><label|external converters>

  <TeXmacs> can natively produce PDF vector images.\ 

  In order to produce the various other graphic formats, <TeXmacs> relies on
  various libraries or external programs, notably :\ 

  <\itemize-dot>
    <item><name|<name|Qt>>,

    <item><name|Ghostscript>,

    <item><name|pdf2svg>, for <acronym|Svg> output,

    <item><name|pdftocairo> (from the <name|Poppler> project) can can produce
    both vector (<acronym|svg>) and raster formats \ (<acronym|png><name|>,
    <acronym|jpg>),

    <item><name|ImageMagick>, for raster formats.
  </itemize-dot>

  Several of these converters are accessed through <scheme> <code*|converter>
  procedures (defined in <verbatim|$TEXMACS_PATH/progs/convert/images/init-images.scm>).
  <hlink|Additional converters|new/man-newconv.en.tm> can be similarly
  defined, when needed.\ 

  When attempting a conversion, <TeXmacs> looks for suitable external
  programs in the system PATH, and if none is found, it displays an error
  message. On <name|Linux> if these external programs are not already
  installed, they are easy to install from your distribution's package
  manager. On <name|MacOS>, <name|pdf2svg> and <name|ImageMagick> (as well as
  <name|Inkscape>) are available from <name|MacPorts>. On <name|Windows>,
  <name|pdftocairo> can be obtained <hlink|from
  here|https://github.com/oschwartz10612/poppler-windows>, or you can get
  <hlink|it bundled with <TeXmacs>|https://github.com/slowphil/mingw-w64-texmacs/releases>.

  <tmdoc-copyright|2016-2022|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>

  \;

  \;
</body>

<initial|<\collection>
</collection>>