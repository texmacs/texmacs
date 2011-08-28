<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|<TeXmacs> fonts>

  <section|Classical conceptions of fonts>

  The way <apply|TeXmacs> handles fonts is quite different from classical
  text editors and even from <apply|TeX>. Let us first analyze some classical
  ways of conceiving fonts.

  <\itemize>
    <item>Physical fonts are just given by the name of a file, which contains
    a character set, i.e. a list of bitmaps. Usually the size of a character
    set is limited by 256 (or 65536).

    <item>True type fonts essentially work in the same way, except that the
    bitmaps can now be computed for any desired size.

    <item>In the X-window system, the name of the font is replaced by a more
    systematic name, which explicitly contains a certain number of font
    parameters, such as its size, series and shape. This makes it easier for
    applications to select an appropriate font. However, character sets are
    still limited in size.

    <item>In <apply|TeX>, symbols are seen as commands, which select an
    appropriate physical font (which corresponds to a <verbatim|.tfm> and a
    <verbatim|.pk> file), based on symbol font declarations and environment
    variables (such as size, series and shape).
  </itemize>

  Clearly, among all these methods, <apply|TeX> provides the largest
  flexibility. However, philosophically speaking, we think that it also has
  some drawbacks:

  <\itemize>
    <item>There is no distinction between usual commands and commands to make
    symbols: the current time might be considered as a symbol.

    <item>The encoding of the font is fixed by the names of the commands. For
    instance, for mathematical symbols, no clean general encoding scheme is
    provided, except the default naming of symbols by commands.

    <item>For beginners, it remains extremely hard to use non standard fonts.
  </itemize>

  Actually, in <apply|TeX>, the notion of ``the current font'' is
  ill-defined: it is merely the superposition of all character generating
  commands.

  <section|The conception of a font in TeXmacs>

  Philosophically speaking, we think that a font should be characterized by
  the following two essential properties:

  <\enumerate>
    <item>A font associates graphical meanings to <with|font
    shape|italic|words>. The words can always be represented by strings.

    <item>The way this association takes place is coherent as a function of
    the word.
  </enumerate>

  By a word, we either mean a word in a natural language, or a sequence of
  mathematical, technical or artistic symbols.

  This way of viewing fonts has several advantages:

  <\enumerate>
    <item>A font may take care of kerning and ligatures.

    <item>A font may consist of several ``physical fonts'', which are somehow
    merged together.

    <item>A font might in principle automatically build very complicated
    glyphs like hieroglyphs or large delimiters from words in a well chosen
    encoding.

    <item>A font is an irreductable and persistent entity, not a bunch of
    commands whose actions may depend on some environement.
  </enumerate>

  Notice finally that the ``graphical meaning'' of a word might be more than
  just a bitmap: it might also contain some information about a logical
  bounding box, appropriate places for scripts, etc. Similarly, the
  ``coherence of the association'' should be interpreted in its broadest
  sense: the font might contain additional information for the global
  typesetting of the words on a page, like the recommended distance between
  lines, the height of a fraction bar, etc.

  <section|String encodings>

  All text strings in <apply|TeXmacs> consist of sequences of either specific
  or universal symbols. A specific symbol is a character, different from
  <verbatim|'\\0'>, <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>. Its
  meaning may depend on the particular font which is being used. A universal
  symbol is a string starting with <verbatim|'\<less\>'>, followed by an
  arbitrary sequence of characters different from <verbatim|'\\0'>,
  <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>, and ending with
  <verbatim|'\<gtr\>'>. The meaning of universal characters does not depend
  on the particular font which is used, but different fonts may render them
  in a different way.

  Universal symbols can also be used to represent mathematical symbols of
  variable sizes like large brackets. The point here is that the shapes of
  such symbols depend on certain size parameters, which can not conveniently
  be thought of as font parameters. This problem is solved by letting the
  extra parameters be part of the symbol. For instance,
  <verbatim|"\<less\>left-(-1\<gtr\>"> would be usual bracket and
  <verbatim|"\<less\>left-(-2\<gtr\>"> a slightly larger one.

  <section|The abstract font class>

  The main abstract <verbatim|font> class is defined in <verbatim|font.hpp>:\ 

  <\verbatim>
    \ \ \ \ struct font_rep: rep\<less\>font\<gtr\> {<format|next line>
    \ \ \ \ \ display \ dis; \ \ \ \ \ \ \ \ \ \ \ \ \ // underlying
    display<format|next line> \ \ \ \ \ encoding enc;
    \ \ \ \ \ \ \ \ \ \ \ \ \ // underlying encoding of the font<format|next
    line> \ \ \ \ \ SI \ \ \ \ \ \ design_size; \ \ \ \ \ // design size in
    points/256<format|next line> \ \ \ \ \ SI \ \ \ \ \ \ display_size;
    \ \ \ \ // display size in points/PIXEL<format|next line>
    \ \ \ \ \ double \ \ slope; \ \ \ \ \ \ \ \ \ \ \ // italic
    slope<format|next line> \ \ \ \ \ space \ \ \ spc;
    \ \ \ \ \ \ \ \ \ \ \ \ \ // usual space between words<format|next line>
    \ \ \ \ \ space \ \ \ extra; \ \ \ \ \ \ \ \ \ \ \ // extra space at end
    of words<format|next line><format|next line> \ \ \ \ \ SI \ \ \ \ \ \ y1;
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ // bottom y position<format|next line>
    \ \ \ \ \ SI \ \ \ \ \ \ y2; \ \ \ \ \ \ \ \ \ \ \ \ \ \ // top y
    position<format|next line> \ \ \ \ \ SI \ \ \ \ \ \ yfrac;
    \ \ \ \ \ \ \ \ \ \ \ // vertical position fraction bar<format|next line>
    \ \ \ \ \ SI \ \ \ \ \ \ ysub; \ \ \ \ \ \ \ \ \ \ \ \ // base line for
    subscripts<format|next line> \ \ \ \ \ SI \ \ \ \ \ \ ysup;
    \ \ \ \ \ \ \ \ \ \ \ \ // base line for superscripts<format|next
    line><format|next line> \ \ \ \ \ SI \ \ \ \ \ \ wpt;
    \ \ \ \ \ \ \ \ \ \ \ \ \ // width of one point in font<format|next line>
    \ \ \ \ \ SI \ \ \ \ \ \ wquad; \ \ \ \ \ \ \ \ \ \ \ // wpt * design
    size in points<format|next line> \ \ \ \ \ SI \ \ \ \ \ \ wunit;
    \ \ \ \ \ \ \ \ \ \ \ // unit width for extendable fonts<format|next
    line> \ \ \ \ \ SI \ \ \ \ \ \ wfrac; \ \ \ \ \ \ \ \ \ \ \ // width
    fraction bar<format|next line> \ \ \ \ \ SI \ \ \ \ \ \ wsqrt;
    \ \ \ \ \ \ \ \ \ \ \ // width horzontal line in square root<format|next
    line> \ \ \ \ \ SI \ \ \ \ \ \ wneg; \ \ \ \ \ \ \ \ \ \ \ \ // width of
    negation line<format|next line><format|next line> \ \ \ \ \ font_rep
    (display dis, string name);<format|next line> \ \ \ \ \ font_rep (display
    dis, string name, font fn);<format|next line> \ \ \ \ \ void
    copy_math_pars (font fn);<format|next line><format|next line>
    \ \ \ \ \ virtual void \ \ get_extents (string s, text_extents& ex) =
    0;<format|next line> \ \ \ \ \ virtual void \ \ draw (ps_device dev,
    string s, SI x, SI y) = 0;<format|next line><format|next line>
    \ \ \ \ \ virtual SI \ \ \ \ get_sub_base (string s);<format|next line>
    \ \ \ \ \ virtual SI \ \ \ \ get_sup_base (string s);<format|next line>
    \ \ \ \ \ virtual double get_left_slope \ (string s);<format|next line>
    \ \ \ \ \ virtual double get_right_slope (string s);<format|next line>
    \ \ \ \ \ virtual SI \ \ \ \ get_left_correction \ (string
    s);<format|next line> \ \ \ \ \ virtual SI \ \ \ \ get_right_correction
    (string s);<format|next line> \ \ \ \ \ virtual SI
    \ \ \ \ get_lsub_correction (string s, double level);<format|next line>
    \ \ \ \ \ virtual SI \ \ \ \ get_lsup_correction (string s, double
    level);<format|next line> \ \ \ \ \ virtual SI
    \ \ \ \ get_rsub_correction (string s, double level);<format|next line>
    \ \ \ \ \ virtual SI \ \ \ \ get_rsup_correction (string s, double
    level);<format|next line><format|next line> \ \ \ \ void var_get_extents
    (string s, text_extents& ex);<format|next line> \ \ \ \ void var_draw
    (ps_device dev, string s, SI x, SI y);<format|next line> \ \ \ \ virtual
    bitmap_char get_bitmap (string s);<format|next line> \ \ };
  </verbatim>

  The main abstract routines are <verbatim|get_extents> and <verbatim|draw>.
  The first routine determines the logical and physical bounding boxes of a
  graphical representation of a word, the second one draws the string on the
  the screen.

  The additional data are used for global typesetting using the font. The
  other virtual routines are used for determening additional properties of
  typesetted strings.

  <section|Implementation of concrete fonts>

  Several types of concrete fonts have been implemented in <apply|TeXmacs>:

  <\description>
    <expand|item*|TeX text fonts.>See <verbatim|src/Resource/Fonts/tex_font.c\
    c>.

    <expand|item*|TeX rubber fonts.>See <verbatim|src/Resource/Fonts/tex_rubb\
    er_font.cpp>.

    <expand|item*|X fonts.>See <verbatim|src/Resource/Fonts/ps_font.cpp>.

    <expand|item*|Mathematical fonts.>See
    <verbatim|src/Resource/Fonts/math_font.cpp>.

    <expand|item*|Virtual fonts.>See <verbatim|src/Resource/Fonts/virtual_fon\
    t.cpp>.
  </description>

  In most cases, the lowest layer of the implementation consists of a
  collection of bitmaps, together with some font metric information. The font
  is responsable for putting these bitmaps together on the screen using some
  appropriate spacing. The <verbatim|ps_device> class comes with a method to
  display bitmaps in a nice, anti-aliased way, or to print them out.

  <section|Font selection>

  After having implemented fonts themselves, an important remaining issue is
  the selection of the appropriate font as a function of a certain number of
  parameters, such as its name, series, shape and size. For optimal
  flexibility, <apply|TeXmacs> comes with a powerful macro-based
  font-selection scheme (using the <apply|scheme> syntax), which allows the
  user to decide which parameters should be considered meaningful.

  At the lowest level, we provide a fixed number of macros which directly
  correspond to the above types of concrete fonts. For instance, the macro\ 

  <\verbatim>
    \ \ \ \ (tex $name $size $dpi)
  </verbatim>

  corresponds to the constructor\ 

  <\verbatim>
    \ \ \ \ font tex_font (display dis, string fam, int size, int dpi, int
    dsize=10);
  </verbatim>

  of a <apply|TeX> text font.

  At the middle level, it is possible to specify some rewriting rules like\ 

  <\verbatim>
    \ \ \ \ ((roman rm medium right $s $d) (ec ecrm $s $d))<format|next line>
    \ \ \ ((avant-garde rm medium right $s $d) (tex rpagk $s $d
    0))<format|next line> \ \ \ ((x-times rm medium right $s $d) (ps
    adobe-times-medium-r-normal $s $d))
  </verbatim>

  When a left hand pattern is matched, it is recursively substituted by the
  right hand side. The files in the directory <verbatim|progs/fonts> contain
  a large number of rewriting rules.

  At the top level, <apply|TeXmacs> calls a macro of the form\ 

  <\verbatim>
    \ \ \ \ ($name $family $series $shape $size $dpi)
  </verbatim>

  as a function of the current environment in the text. In the future, the
  top level macro call might change in order to enable the user to let the
  font depend on other environment variables.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|4.|?>>
    <associate|toc-4|<tuple|4.|?>>
    <associate|toc-5|<tuple|4.|?>>
    <associate|toc-6|<tuple|4.|?>>
    <associate|toc-7|<tuple|4.|?>>
  </collection>
</references>
