<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|The font selection system>

  In <TeXmacs>, the global document font can be specified using
  <menu|Document|Font>. It is also possible to locally use another font using
  <menu|Format|Font>. Both <menu|Document|Font> and <menu|Format|Font> open
  the <TeXmacs> font browser. Fonts have three main characteristics:

  <\description>
    <item*|Family>Fonts are grouped together into <em|families> with a
    similar design.

    <item*|Shape>Inside the same font family, individual fonts have different
    <em|shapes>, such as bold, italic, small capitals, etc.

    <item*|Size>The font <em|size> in points.
  </description>

  The user may directly specify these three characteristics in the font
  browser, which also displays some sample text for the selected font.

  The font browser also provides a way to quickly select fonts based on
  desirable font properties. For instance, by filtering on a \Pbold weight\Q
  and \Psans serif\Q, one may find a bold sans serif font which mixes as well
  as possible with the main font. <TeXmacs> allows you to filter on the
  following criteria:

  <\description>
    <item*|Weight>The font <em|weight> corresponds to the \Pthickness\Q of
    the font:

    <center|<block|<tformat|<table|<row|<cell|<with|font-series|thin|Thin>>|<cell|<with|font-series|light|Light>>|<cell|Medium>|<cell|<with|font-series|bold|Bold>>|<cell|<with|font-series|black|Black>>>>>>>

    <item*|Slant>The font <em|slant> determines the angle of the font:

    <center|<block|<tformat|<table|<row|<cell|<with|font-family|normal|Normal>>|<cell|<with|font-shape|italic|Italic>>|<cell|<with|font-shape|slanted|Oblique>>>>>>>

    <item*|Stretch>This property determines the horizontal width for a fixed
    vertical height:

    <center|<block|<tformat|<table|<row|<cell|<with|font-shape|condensed|Condensed>>|<cell|<with|font-shape|unextended|Unextended>>|<cell|<with|font-shape|wide|Wide>>>>>>>

    <item*|Case>This property determines how lowercase letters are
    capitalized:

    <center|<block|<tformat|<table|<row|<cell|<with|font-shape|mixed|Mixed>>|<cell|<with|font-shape|small-caps|Small
    capitals>>>>>>>

    <item*|Serif>This feature corresponds to the projecting features called
    \Pserifs\Q at the end of strokes:

    <center|<block|<tformat|<table|<row|<cell|<with|font-family|rm|Serif>>|<cell|<with|font-family|ss|Sans
    Serif>>>>>>>

    <item*|Spacing>This feature corresponds to the horizontal spacing between
    characters:

    <center|<block|<tformat|<table|<row|<cell|<with|font-family|rm|Proportional>>|<cell|<with|font-family|tt|Monospaced>>>>>>>

    <item*|Device>This property can be used to imitate specific \Pwriting
    devices\Q:

    <center|<block|<tformat|<table|<row|<cell|<with|font-family|rm|Print>>|<cell|<with|font-family|tt|Typewriter>>|<cell|<with|font-family|digital|Digital>>|<cell|<with|font-family|pen|Pen>>|<cell|<with|font-family|artpen|Art
    pen>>|<cell|<with|font-family|chalk|Chalk>>|<cell|<with|font-family|marker|Marker>>>>>>>

    <item*|Category>Various other font features:

    <htab|5mm><block|<tformat|<table|<row|<cell|<with|font-family|ancient|Ancient>>|<cell|<with|font-family|attached|Attached>>|<cell|<with|font-family|calligraphic|Calligraphic>>|<cell|<with|font-family|comic|Comic>>|<cell|<with|font-family|decorative|Decorative>>>|<row|<cell|<with|font-family|distorted|Distorted>>|<cell|<with|font-family|gothic|Gothic>>|<cell|<with|font-family|handwritten|Handwritten>>|<cell|<with|font-family|initials|Initials>>|<cell|<with|font-family|medieval|Medieval>>>|<row|<cell|<with|font-family|miscellaneous|Miscellaneous>>|<cell|<with|font-family|outline|Outline>>|<cell|<with|font-family|retro|Retro>>|<cell|<with|font-family|scifi|Scifi>>|<cell|<with|font-family|title|Title>>>>>><htab|5mm>
  </description>

  Each of the above properties really constitutes a <em|hint> on how the kind
  of font which <em|should> be used. If no suitable font can be found on your
  particular system, then setting these properties may have no effect.
  Whenever you apply one or more filters, the font browser indicates which
  fonts match the selected properties. It also indicates the closest match
  for the current font in use. When pressing the <menu|Ok> button without
  selecting any particular matching font, then the selected font properties
  will be inserted as <TeXmacs> markup and used as rendering hints. In that
  case, the rendering may change when selecting another global document font
  (for instance).

  It should be noticed that <TeXmacs> comes with a limited number of
  preinstalled fonts, such as the <with|font|Stix|Stix> fonts and several
  fonts prefixed by \PTeXmacs\Q. Documents which only use these fonts will be
  rendered the same on different systems (assuming the same version of
  <TeXmacs>). When your documents contain other fonts as well, then these
  fonts may be replaced by closest matches when opening your document under a
  different operating system.

  <tmdoc-copyright|1998--2014|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>