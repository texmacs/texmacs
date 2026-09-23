<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|The font configuration files>

  <TeXmacs> keeps what it knows about fonts in a handful of files. They come
  in two copies: the ones shipped with the program, under
  <verbatim|$TEXMACS_PATH/fonts>, and the ones of your own installation,
  under <verbatim|$TEXMACS_HOME_PATH/fonts>, which is
  <verbatim|~/.TeXmacs/fonts> on <name|Unix>. The local copy is the one in
  use; the shipped one is its starting point and its safety net.

  <paragraph*|The database>

  <\description>
    <item*|<verbatim|font-database.scm>>Which file holds which face. Each
    entry names a family and a style and gives the file, the index inside
    it, and its size in bytes:

    <\scm-code>
      ((Latin\\ Modern\\ Math Regular) ((latinmodern-math.otf 0 733736)))
    </scm-code>

    The size is what lets <TeXmacs> notice that a file has been replaced by
    another version of itself.

    <item*|<verbatim|font-features.scm>>Which <em|master> a family belongs
    to and which features it carries. The master is the name a document
    uses: the families <verbatim|Fira Sans> and <verbatim|Fira Mono> both
    belong to the master <verbatim|Fira>, and the variant asked for by
    <verbatim|font-family> picks one of them.

    <item*|<verbatim|font-characteristics.scm>>What a face looks like,
    measured on its glyphs: whether it is monospaced or sans serif, its
    slant, its x-height, the vertical and horizontal stroke widths, the
    fill rate, and so on. These numbers are what the font browser filters
    on and what the distance between a request and a face is computed
    from.

    <item*|<verbatim|font-substitutions.scm>>Which family to use when a
    style is missing, for the families where the obvious answer is wrong:
    the sans serif companion of <verbatim|FandolSong> is
    <verbatim|FandolHei> and not a slanted <verbatim|FandolSong>.

    <item*|<verbatim|shipped-stamp.scm>>The date and the size of the
    shipped files from which the local database was built. When they
    differ, <TeXmacs> merges the shipped entries into the local ones at the
    next start, so that a version which ships new fonts does not go
    unnoticed by an old home directory.
  </description>

  Rescanning is <menu|Tools|Fonts|Scan disk for fonts>, and emptying the
  whole thing is <menu|Tools|Fonts|Clear font cache>, after which the
  database is rebuilt from the shipped one. If you want to contribute the
  fonts of your system to <TeXmacs>, the developer routine
  <scm|(font-database-save-local-delta)> writes the difference with respect
  to the shipped database into <verbatim|delta-database.scm> and its
  companions.

  <paragraph*|The encodings>

  <\description>
    <item*|<verbatim|fonts/enc/*.enc>>The translation tables of the
    <TeX> fonts: a list of <TeXmacs> character names, with the
    position at which they start in the font. This is how position<nbsp>0 of
    <verbatim|cmr> becomes <verbatim|\<less\>Gamma\<gtr\>> and
    position<nbsp>26 becomes <verbatim|\<less\>ae\<gtr\>>.

    <item*|<verbatim|fonts/virtual/*.vfn>><hlink|Virtual
    fonts|virtual-fonts.en.tm>: characters described as constructions over
    other characters. A virtual font is loaded exactly like an encoding
    table, and a copy in your home directory shadows the shipped one.

    <item*|<verbatim|langs/encoding/*.scm>>The tables which map a <TeXmacs>
    character name to a Unicode code point and back. They are what makes a
    symbol convertible, searchable and available to the fallback search,
    and they are read by the converters as well as by the font system.
  </description>

  <paragraph*|The font rules>

  The modules of <verbatim|$TEXMACS_PATH/progs/fonts> translate a request
  into a physical font. A rule is a pattern and a replacement, matched
  against the tuple made of the family, the variant, the series, the shape,
  the size and the resolution:

  <\scm-code>
    (set-font-rules

    \ \ '(((roman rm medium right $s $d) (ec ecrm $s $d))

    \ \ \ \ ((roman mr medium $a $s $d) (math-std ecrm cmr cmmi $s $d))))
  </scm-code>

  The variables of the pattern, written with a dollar sign, are bound to
  whatever they match. The right-hand side may name a physical font, such as
  <verbatim|(truetype luximr 10 600)>, a <TeX> font, a virtual font, or
  a <em|compound> font which is made of several of them, which is how the
  traditional <TeXmacs> mathematics font is assembled from
  <verbatim|cmr>, <verbatim|cmmi>, <verbatim|cmsy> and the rest.

  Rules are ordinary <scheme>, so your personal initialization file may
  add its own; they are tried in the order in which they were declared,
  so a rule added later takes precedence.

  <paragraph*|The fonts themselves>

  The directories <verbatim|fonts/truetype>, <verbatim|fonts/type1> and
  <verbatim|fonts/tfm> hold the fonts shipped with <TeXmacs>, and
  <verbatim|fonts/pk> in your home directory holds the bitmaps generated
  from <name|Metafont> sources. Fonts installed on your system are found in
  the usual places of the operating system and in the <TeX>
  distribution, if there is one.

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
