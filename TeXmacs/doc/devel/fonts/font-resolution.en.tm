<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|From markup to glyph>

  Between the font you asked for and the ink on the page there are several
  steps, and each of them can be configured. This page follows one character
  through them.

  <paragraph*|What the typesetter sees>

  The text of a document is a sequence of <em|characters> in the internal
  encoding of <TeXmacs>: ordinary ASCII characters stand for themselves, and
  everything else is written between angle brackets. The letter
  <math|<with|font-shape|italic|\<alpha\>>> is the character
  <verbatim|\<less\>alpha\<gtr\>>, an accented letter is
  <verbatim|\<less\>#E9\<gtr\>>, and a character which exists only as a
  Unicode code point is <verbatim|\<less\>#1D6FC\<gtr\>>. Stretchable
  characters have names of their own, such as
  <verbatim|\<less\>left-(-2\<gtr\>> for the second size of an opening
  parenthesis.

  A name is not a glyph, and it is not a code point either: it is what the
  editor stores and what the font system is asked to draw.

  <paragraph*|The logical font>

  When the typesetter reaches a piece of text it reads the mode it is in,
  text, mathematics or program, and the corresponding variables
  (<hlink|selecting fonts|font-selection.en.tm>). From them it builds a
  <em|logical font>: a family name followed by normalized features, such as
  <verbatim|(Pagella bold italic)>. The font a document asks for is a
  request in the same sense: the family may not exist on this machine, and
  the features may not all be available.

  <paragraph*|The smart font>

  What the typesetter receives back is a <em|smart font>. It is not one
  font: it is a router which keeps a list of subfonts and decides, character
  by character and once for each, which of them draws it. This is why a
  formula can mix a Greek letter from the math font, a blackboard bold
  letter which is drawn by hand and an arrow taken from a third font,
  without the document saying anything about it.

  The decision follows a ladder, from the most faithful to the most
  desperate:

  <\enumerate>
    <item>the main font of the request;

    <item>the mathematical rewritings: an italic Greek letter, a bold
    letter, a script or double-struck letter, which are either taken from
    the font's own alphabets or from the Unicode mathematical alphanumerics;

    <item>a few special characters, and the brackets which <TeXmacs> knows
    how to build out of pieces;

    <item>a <hlink|virtual font|virtual-fonts.en.tm> which defines the
    character as a construction over other glyphs;

    <item>the derived, or \Ppoor\Q, fonts: bold by thickening, italic by
    slanting, small capitals by scaling, blackboard bold by doubling a
    stroke;

    <item>another family which has the character, searched in the font
    database and rendered at a resolution adjusted so that its x-height
    matches the one of the main font;

    <item>failing everything, the error font, which draws the name of the
    character in red.
  </enumerate>

  A red name in a formula therefore does not mean that the character is
  unknown to <TeXmacs>: it means that no font was found for it. The most
  common cause is a font database which was written before the font you
  need was installed; see <hlink|the font configuration
  files|font-config.en.tm>.

  <paragraph*|From a name to a code point>

  Steps 2 and 6 need to know which Unicode character a name stands for. The
  tables of <verbatim|$TEXMACS_PATH/langs/encoding> answer that question:
  <verbatim|tmuniversaltounicode.scm> and its companions map
  <verbatim|\<less\>alpha\<gtr\>> to <verbatim|U+03B1> and back. The same
  tables serve the converters, which is why a symbol without an entry there
  can be typed and printed but not exported.

  <paragraph*|From a family to a file>

  Choosing the file is the work of the font database. A logical font is
  compared with every style of every family it knows, by a distance which
  counts the features that match and the ones that had to be dropped; the
  first attempt is strict, the following ones are increasingly tolerant,
  which is how the fallback of step<nbsp>6 finds a font for a rare
  character. The files which hold that knowledge are described in
  <hlink|the font configuration files|font-config.en.tm>.

  The <TeX> fonts are addressed differently: their metrics come from a
  <verbatim|.tfm> file and their glyph positions have no Unicode meaning, so
  an <em|encoding> file in <verbatim|$TEXMACS_PATH/fonts/enc> maps each
  position to a <TeXmacs> character name. Those files are simply lists of
  names with the position where they start, and a font rule says which one
  a font uses.

  <paragraph*|From a file to ink>

  <name|FreeType> opens the file and gives back an outline, which is
  rasterized at the size and the resolution in use; the resulting bitmaps
  are cached, per size and per resolution, under
  <verbatim|$TEXMACS_HOME_PATH/fonts>. When a document is exported to
  <name|PDF> or <name|PostScript> the same glyphs are written as vectors and
  the fonts are embedded as subsets, except for the glyphs which <TeXmacs>
  draws itself, which become small bitmap fonts.

  <paragraph*|When something looks wrong>

  <\description>
    <item*|A character appears as a red name>No font was found for it. Try
    <menu|Tools|Fonts|Scan disk for fonts>, and read
    <hlink|below|font-config.en.tm> about the merge of the shipped database.

    <item*|A font you installed is not proposed>The database was written
    before you installed it; the same scan adds it.

    <item*|A font looks like an older version of itself>The rendered glyphs
    are cached; <menu|Tools|Fonts|Clear font cache> empties the caches and
    the database, which are rebuilt at the next start.

    <item*|The document looks different on another machine>The fonts it
    asks for are not installed there, and the closest matches were used
    instead. Fonts shipped with <TeXmacs> do not have this problem.
  </description>

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
