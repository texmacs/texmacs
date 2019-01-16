<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Conversion from <TeXmacs> to <LaTeX>>

  A <TeXmacs> document can be exported to <LaTeX> using
  <menu|File|Export|LaTeX>. \ In the case of certain journal styles like
  <verbatim|svjour> or <verbatim|elsart>, the user should also make sure that
  the appropriate style files can be found by <LaTeX>, when compiling the
  result of the conversion. Please consult your <LaTeX> documentation for how
  to do this; one solution which usually works is to put the style file in
  the same directory as your file.

  Notice that the exportation of a <TeXmacs> document with images may cause
  the creation of additional image files. If your destination file is called
  <verbatim|name.tex>, these files are named <verbatim|name-1.eps>,
  <rigid|<verbatim|name-2.eps>>, <abbr|etc.> and they are stored in the same
  directory. In particular, all pictures drawn with the editor and all images
  which are not already in <name|Postscript> format will be converted to
  encapsulated <name|Postscript> files.

  In order to ensure that the generated <LaTeX> document compiles, style
  files and packages or macros with no <LaTeX> equivalents are either ignored
  or replaced by a reasonable substitute. The precise behaviour of the
  converter may be customized using several user preferences in the
  <menu|Edit|Preferences|Converters|LaTeX|TeXmacs--\<gtr\>LaTeX> menu:

  <\description-long>
    <item*|<menu|Replace unrecognized styles>>This option (which is set by
    default) tells <TeXmacs> to replace style files with no <LaTeX>
    equivalents by the <verbatim|article> style. Furthermore, all additional
    style packages are ignored.

    In case you know how to <hlink|write your own style
    files|../../../devel/style/style.en.tm>, you might wish to create
    <TeXmacs> equivalents of those journal styles which you use often.
    Similarly, you might wish to create a style package with your own macros
    together with its <LaTeX> counterpart. In both cases, you might want to
    disable the style replacement option.

    <item*|<menu|Replace unrecognized macros>>By default, all <TeXmacs>
    macros are expanded until they admit direct <LaTeX> counterparts.
    Primitives with no <LaTeX> counterparts (like graphics or trees) are
    ignored. Moreover, in order to convert certain frequently used macros
    like <verbatim|theorem> or <verbatim|strong>, <TeXmacs> may put
    additional definitions in the preamble.

    In some cases, the user may wish to keep unrecognized macros in their
    unexpanded form. For instance, this may be convenient if you want to
    import the generated document back into <TeXmacs>. Another typical
    situation is when you defined additional macros in a style package. In
    these cases, you may disable to macro replacement option. Of course, any
    missing macro definitions may result in <LaTeX> errors during the
    compilation.

    <item*|<menu|Expand user-defined macros>>When your document or its
    preamble contains macro definitions, then <TeXmacs> will convert these
    macro definitions into <LaTeX> macro definitions and keep all macro
    applications in their unexpanded forms. This allows you to preserve as
    much structure of your document as possible. When enabling the
    <menu|Expand user-defined macros> option, all macro definitions in your
    document will be ignored and all macro applications will be expanded.

    <item*|<menu|Export bibliographies as links>>In order to produce
    stand-alone <LaTeX> files whenever possible, it is assumed that you
    generate<nbsp>your bibliographies from within <TeXmacs>. When exporting
    to <LaTeX>, the generated bibliography will be directly included into
    your <LaTeX> file. In some cases however, the user might wish to
    regenerate the bibliography from the <LaTeX> and the bibliography files,
    using <name|Bib<TeX>>. In this case, you need to enable the <menu|Export
    bibliographies as links> option.

    <item*|<menu|Allow for macro definitions in preamble>>Certain <TeXmacs>
    macros like <markup|strong> have no direct <LaTeX> analogues. For a
    certain number of frequently used macros, <TeXmacs> automatically
    generates macro definitions in the preamble of the <LaTeX> target file.
    This allows you to preserve as much structure as possible of your
    document, which is for instance useful if you import the document back
    into <TeXmacs>.

    However, certain journals instruct authors to refrain from the definition
    of additional macros in the preamble. When disallowing for macro
    definitions in preambles, <TeXmacs> will automatically expand all
    corresponding macro applications.

    <item*|Dump <TeXmacs> document into <LaTeX> code>When this option is set,
    a copy of the <TeXmacs> document is appended to the <LaTeX> export in a
    lossless kind. This allows to re-import the document with as few
    conversion artifacts as possible .

    <item*|Character encoding>This option defines the behavior of the
    converter with respect to character encoding. There are three possible
    choices:

    <\description>
      <item*|Utf-8 with inputenc>This will generate <verbatim|utf-8> document
      with the package <verbatim|inputenc> loaded. If for any reason you
      don't want to rely on <verbatim|inputenc>, you should consider other
      options.

      <item*|Cork with catcodes>Keeps accented characters ``as is''. This can
      be achieved by allowing <TeXmacs> to put additional catcode definitions
      into your preamble. This provides a good trade-off between readability
      (accented characters are kept in an 8 bit charset) and simplicity (you
      don't need the <verbatim|inputenc> package).

      <item*|Ascii>This will generate pure <name|ascii> characters, using
      plain <TeX> sequences to compose non-<name|ascii> symbols.
    </description>
  </description-long>

  Sometimes, the converter does not produce a satisfactory <LaTeX> file even
  after some tinkering with the above preferences. The most frequent problem
  concerns bad line breaks. Occasionally, certain document fragments are also
  better converted by hand. In order to minimize the need for corrections in
  the generated <LaTeX> file (which would be lost when re-exporting the
  <TeXmacs> source file after some modifications), <TeXmacs> provides a
  mechanism to specify manual conversions to <LaTeX> in the <TeXmacs> source
  file: using <menu|Format|Specific|Texmacs> and
  <menu|Format|Specific|Latex>, you may force certain document fragments to
  appear only in the source file or the <LaTeX> target.

  For instance, assume that the word \Pblauwbilgorgel\Q is hyphenated
  correctly in the <TeXmacs> source, but not in the <LaTeX> conversion. Then
  you may proceed as follows:

  <\enumerate>
    <item>Select \Pblauwbilgorgel\Q.

    <item>Click on <menu|Format|Specific|Texmacs> to make the text
    \Pblauwbilgorgel\Q <TeXmacs>-specific.

    <item>Click on <menu|Format|Specific|Latex>.

    <item>Type the latex code <verbatim|blauw\\-bil\\-gor\\-gel> with the
    correct hyphenation.

    <item>Press <shortcut|(kbd-return)> to activate the <LaTeX>-specific
    text.
  </enumerate>

  In a similar fashion, you may insert <LaTeX>-specific line breaks, page
  breaks, vertical space, style parameter modifications, etc. You may also
  force arbitrary content to be exported as an image using
  <menu|Format|Specific|Image>.

  <tmdoc-copyright|1998--2013|Joris van der Hoeven, François Poulain>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>