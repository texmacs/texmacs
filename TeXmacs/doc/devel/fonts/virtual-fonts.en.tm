<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Virtual fonts>

  A <em|virtual font> defines characters as constructions over other
  characters: a glyph is flipped, cut in two, stacked on another one or
  drawn a second time a little to the right. <TeXmacs> uses them to draw the
  symbols that ordinary text fonts do not have, which is how mathematics can
  be typeset in a font which was never designed for it.

  The virtual fonts live in <verbatim|$TEXMACS_PATH/fonts/virtual>, in files
  with the extension <verbatim|.vfn>, and a file of the same name in
  <verbatim|$TEXMACS_HOME_PATH/fonts/virtual> is used in its place, which is
  how you may extend or correct one without touching the installation.

  <paragraph*|What a file looks like>

  A virtual font is a list of definitions, each of them a character name and
  an expression:

  <\scm-code>
    (virtual-font

    \ \ (mho (ver-flip (hor-flip omega)))

    \ \ (Backsigma (hor-flip Sigma))

    \ \ (Exists (join (0 0 exists) (0.25 -0.01 urcorner)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (0.25 0.02 lrcorner))))
  </scm-code>

  A name of one letter stands for that character, a longer name stands for
  the character written between angle brackets: the definitions above give
  <verbatim|\<less\>mho\<gtr\>>, <verbatim|\<less\>Backsigma\<gtr\>> and
  <verbatim|\<less\>Exists\<gtr\>>. Inside an expression, a name is the
  glyph of that character, taken from the font the virtual font is built on,
  or from the virtual font itself when it defines it, so definitions may be
  built on one another.

  <paragraph*|The vocabulary>

  The expressions are a small language of glyph algebra. The operations fall
  into a few groups:

  <\description>
    <item*|placing>A triple <verbatim|(<var|x> <var|y> <var|g>)> moves the
    glyph <var|g> by <var|x> and <var|y>, in units of the font size, so
    <verbatim|(0.25 -0.01 urcorner)> puts a corner a quarter of an em to the
    right and a hundredth of an em down.

    <item*|assembling><verbatim|join> superposes glyphs,
    <verbatim|glue> and <verbatim|row> put them side by side,
    <verbatim|stack> puts one above the other, and <verbatim|add>,
    <verbatim|min>, <verbatim|max>, <verbatim|intersect> and
    <verbatim|exclude> combine them as images.

    <item*|cutting><verbatim|part> takes a rectangular piece of a glyph in
    relative coordinates, <verbatim|crop> removes the white margins,
    <verbatim|hor-take> and <verbatim|ver-take> keep a band.

    <item*|transforming><verbatim|hor-flip>, <verbatim|ver-flip>,
    <verbatim|rot-left>, <verbatim|rot-right> and <verbatim|rotate> turn a
    glyph around, <verbatim|magnify>, <verbatim|scale>,
    <verbatim|hor-scale> and <verbatim|fscale> resize it,
    <verbatim|italic> slants it and <verbatim|unserif> removes its serifs.

    <item*|spacing><verbatim|enlarge>, <verbatim|widen>,
    <verbatim|deepen>, <verbatim|unindent> and the <verbatim|pretend>
    operations change the box around a glyph without changing the ink,
    which is how a construction is made to align with the rest of a line.

    <item*|decorating><verbatim|bar-left>, <verbatim|bar-right>,
    <verbatim|bar-top>, <verbatim|bar-bottom> add a stroke,
    <verbatim|negate> draws a slash through a glyph, <verbatim|circle>
    draws a circle around it, and <verbatim|flood-fill> fills an outline.

    <item*|measuring><verbatim|width>, <verbatim|height>, <verbatim|xpos>
    and <verbatim|ypos> give a length, the arithmetic operations
    <verbatim|+>, <verbatim|->, <verbatim|*>, <verbatim|/>, <verbatim|min>
    and <verbatim|max> compute with them, and <verbatim|with> binds the
    result to a name:

    <\scm-code>
      (emu-langle-bis (with sc (/ (height (crop [)) (height emu-langle-pre))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (align* (magnify emu-langle-pre * sc) [ * 0.5)))
    </scm-code>

    scales an angle bracket so that its height becomes the height of a
    square bracket.
  </description>

  A star at the end of the name of an operation, as in <verbatim|glue*> or
  <verbatim|unindent*>, means the same operation applied on the other side
  or with the other reference point.

  <paragraph*|How a character reaches a virtual font>

  There are two ways. The first is by name: three virtual fonts,
  <verbatim|tradi-long>, <verbatim|tradi-negate> and <verbatim|tradi-misc>,
  are consulted for every character which the current font does not have,
  and a character defined in one of them is drawn from there. Adding a
  definition to one of these three, in your home directory, is therefore
  enough to give a new symbol a shape everywhere.

  The second is by a font rule. A compound font may list a virtual font
  among its members, as the traditional mathematics font does:

  <\scm-code>
    (tradi-misc (virtual tradi-misc $s $d))
  </scm-code>

  and the stretchable characters are built the same way, out of the
  <verbatim|emu-*> virtual fonts: <verbatim|emu-bracket> assembles the large
  brackets, <verbatim|emu-large> the large operators, and so on. When a font
  carries an <name|OpenType> <verbatim|MATH> table, <TeXmacs> builds the
  same kind of construction from the parts the table gives, which is why the
  assembled delimiters of such a font export as real glyphs.

  <paragraph*|Adding a symbol>

  To give a shape to a symbol your font does not have:

  <\enumerate>
    <item>copy the virtual font you want to extend, say
    <verbatim|tradi-misc.vfn>, into
    <verbatim|$TEXMACS_HOME_PATH/fonts/virtual>;

    <item>add a definition to it, using the glyphs the text fonts do have;

    <item>make sure the symbol has a name in
    <verbatim|langs/encoding/tmuniversaltounicode.scm>, or it will be
    typed as <verbatim|\<less\>#XXXX\<gtr\>> and no palette will propose
    it;

    <item>restart <TeXmacs>; virtual fonts are read once and cached.
  </enumerate>

  A definition which mentions a character the underlying font does not have
  produces nothing, so it is wise to build on the letters and the
  punctuation, which every font provides.

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
