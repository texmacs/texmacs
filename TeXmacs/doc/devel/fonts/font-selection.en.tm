<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Selecting fonts>

  <paragraph*|From the dialogs>

  The font of a whole document is chosen in <menu|Document|Font>, and the
  font of a piece of text in <menu|Format|Font>. Both open the font browser,
  which lets you pick a family, a shape and a size, and which can also
  search for a font by its properties; the <hlink|font selection
  system|../../main/text/man-fonts.en.tm> describes the browser and its
  filters.

  <menu|Document|Font> has, besides the browser, three submenus which set
  the font of one mode at a time, and two which set the sizes:

  <\description>
    <item*|<menu|Text font>>The font of ordinary text.

    <item*|<menu|Mathematical font>>The font of formulas. Its entries are
    the traditional <TeXmacs> math fonts, and, at the end of the list, the
    <name|OpenType> math fonts which are installed on your system and which
    <TeXmacs> knows how to use.

    <item*|<menu|Program font>>The font of program sessions and of verbatim
    text.

    <item*|<menu|Size>, <menu|Dpi>>The base size of the document in points
    and the resolution at which the glyphs are rendered.
  </description>

  Whatever you choose in these dialogs is stored in the document itself, as
  the value of an environment variable, so a document carries its fonts with
  it. Nothing else is stored: a document asks for <verbatim|TeX Gyre
  Pagella>, not for a file on your disk, and the request is resolved again
  every time the document is opened.

  <paragraph*|The environment variables>

  A dialog is a convenient way of setting the variables below; you may also
  set them by hand, which is what a style file does.

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<table|<row|<cell|<em|variable>>|<cell|<em|default>>|<cell|<em|what
  it says>>>|<row|<cell|<verbatim|font>>|<cell|<verbatim|roman>>|<cell|the
  font name in text mode>>|<row|<cell|<verbatim|font-family>>|<cell|<verbatim|rm>>|<cell|<verbatim|rm>,
  <verbatim|ss> or <verbatim|tt>>>|<row|<cell|<verbatim|font-series>>|<cell|<verbatim|medium>>|<cell|<verbatim|medium>,
  <verbatim|bold>, ...>>|<row|<cell|<verbatim|font-shape>>|<cell|<verbatim|right>>|<cell|<verbatim|right>,
  <verbatim|italic>, <verbatim|small-caps>, ...>>|<row|<cell|<verbatim|font-base-size>>|<cell|<verbatim|10>>|<cell|the
  base size in points>>|<row|<cell|<verbatim|font-size>>|<cell|<verbatim|1>>|<cell|a
  multiplier of the base size>>|<row|<cell|<verbatim|font-effects>>|<cell|>|<cell|extra
  effects, such as <verbatim|bold=0.5>>>|<row|<cell|<verbatim|math-font>>|<cell|<verbatim|roman>>|<cell|the
  font name in math mode>>|<row|<cell|<verbatim|math-font-family>>|<cell|<verbatim|mr>>|<cell|<verbatim|mr>,
  <verbatim|ms> or <verbatim|mt>>>|<row|<cell|<verbatim|math-font-series>>|<cell|<verbatim|medium>>|<cell|as
  in text mode>>|<row|<cell|<verbatim|math-font-shape>>|<cell|<verbatim|normal>>|<cell|<verbatim|normal>
  or <verbatim|right>>>|<row|<cell|<verbatim|prog-font>>|<cell|<verbatim|roman>>|<cell|the
  font name in program mode>>|<row|<cell|<verbatim|prog-font-family>>|<cell|<verbatim|tt>>|<cell|as
  in text mode>>|<row|<cell|<verbatim|magnification>>|<cell|<verbatim|1>>|<cell|magnifies
  everything, slides for instance>>>>>>
    The environment variables which select a font.
  </big-table>

  The three modes are independent: a formula inside a sans serif paragraph
  is still set in the mathematical font, and the program font is used inside
  sessions and verbatim text. The family, the series and the shape are
  <em|requests>: if the font you asked for has no bold italic, <TeXmacs>
  takes the closest match it can find, or emulates it.

  <paragraph*|In markup>

  Setting a variable on a piece of text is what the <markup|with> tag does:

  <\tm-fragment>
    <inactive*|<with|font|TeX Gyre Pagella|font-shape|italic|a line of
    Pagella italic>>
  </tm-fragment>

  Applied to a whole document, the same assignment goes into the initial
  environment of the document, which is what <menu|Document|Font> writes,
  or into a style file as

  <\tm-fragment>
    <inactive*|<assign|font|TeX Gyre Pagella>>
  </tm-fragment>

  A style file may of course compute the value; the <markup|assign> above is
  ordinary markup, and so are the definitions which use it.

  <paragraph*|From <scheme>>

  The same two operations are available in the editor language. The first
  applies a variable to the selection or to what you type next, and the
  second sets the document-wide value:

  <\scm-code>
    (make-with "font-shape" "italic")

    (init-env "font" "TeX Gyre Pagella")

    (init-default "font")
  </scm-code>

  <\explain>
    <scm|(make-with <var|var> <var|val>)><explain-synopsis|apply a variable
    locally>
  <|explain>
    Insert a <markup|with> tag which sets <var|var> to <var|val> around the
    current selection, or around what will be typed next.
  </explain>

  <\explain>
    <scm|(init-env <var|var> <var|val>)><explain-synopsis|set a variable for
    the document>
  <|explain>
    Set <var|var> to <var|val> in the initial environment of the current
    buffer. This is what the entries of <menu|Document|Font> do.
  </explain>

  <\explain>
    <scm|(init-default <var|var>)><explain-synopsis|forget a document-wide
    value>
  <|explain>
    Remove <var|var> from the initial environment, so that the value of the
    style is used again.
  </explain>

  <\explain>
    <scm|(get-env <var|var>)><explain-synopsis|read a variable>
  <|explain>
    The value of <var|var> at the current cursor position, as a string.
  </explain>

  The font of a formula is set in the same way, through <verbatim|math-font>
  and its companions, and a pair of fonts which belong together can be set
  at once:

  <\scm-code>
    (init-env "math-font" "STIXTwoMath")

    (init-font "pagella" "math-pagella")
  </scm-code>

  <paragraph*|Which fonts are there?>

  <TeXmacs> ships a few fonts of its own and finds the others on your
  system. If a font you have installed does not appear in the browser, ask
  for a scan with <menu|Tools|Fonts|Scan disk for fonts>; if a font behaves
  as if <TeXmacs> still remembered an older state of your system, empty the
  caches with <menu|Tools|Fonts|Clear font cache> and restart. What those
  two entries really do, and which files they read and write, is the subject
  of <hlink|the font configuration files|font-config.en.tm>.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>
