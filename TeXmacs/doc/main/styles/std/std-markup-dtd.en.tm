<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Standard markup>

  Various standard markup is defined in <tmdtd|std-markup>. The following
  textual content tags all take one argument. Most can be found in the
  <menu|Insert|Content tag> menu.

  <\explain|<explain-macro|strong|content>>
    Indicates an <strong|important> region of text. You can enter this tag
    via <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<explain-macro|em|content>>
    Emphasizes a region of text like in ``the <em|real> thing''. This tag
    corresponds to the menu entry \ <menu|Insert|Content tag|Emphasize>.
  </explain>

  <\explain|<explain-macro|dfn|content>>
    For definitions like ``a <dfn|gnu> is a horny beast''. This tag
    corresponds to <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<explain-macro|samp|content>>
    A sequence of literal characters like the <samp|ae> ligature æ. You can
    get this tag via <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<explain-macro|name|content>>
    The name of a particular thing or concept like the <name|Linux> system.
    This tag is obtained using <menu|Insert|Content tag|Name>.
  </explain>

  <\explain|<explain-macro|person|content>>
    The name of a person like <name|Joris>. This tag corresponds to
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<explain-macro|cite*|content>>
    A bibliographic citation like a book or magazine. Example: Melville's
    <cite*|Moby Dick>. This tag, which is obtained using <menu|Insert|Content
    tag|Cite>, should not be confused with <markup|cite>. The latter tag is
    also used for citations, but where the argument refers to an entry in a
    database with bibliographic references.
  </explain>

  <\explain|<explain-macro|abbr|content>>
    An abbreviation. Example: I work at the <abbr|C.N.R.S.> An abbreviation
    is created using <menu|Insert|Content tag|Abbreviation> or the <key|text
    a> keyboard shortcut.
  </explain>

  <\explain|<explain-macro|acronym|content>>
    An acronym is an abbreviation formed from the first letter of each word
    in a name or a phrase, such as <acronym|HTML> or <acronym|IBM>. In
    particular, the letters are not separated by dots. You may enter an
    acronym using <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<explain-macro|verbatim|content>>
    Verbatim text like output from a computer program. Example: the program
    said <verbatim|hello>. You may enter verbatim text via
    <menu|Insert|Content tag|Verbatim>. The tag may also be used as an
    environment for multi-paragraph text.
  </explain>

  <\explain|<explain-macro|kbd|content>>
    Text which should be entered on a keyboard. Example: please type
    <kbd|return>. This tag corresponds to the menu entry <menu|Insert|Content
    tag|Keyboard>.
  </explain>

  <\explain|<explain-macro|code*|content>>
    Code of a computer program like in ``<code*|cout \<less\>\<less\> 1+1;>
    yields <verbatim|2>''. This is entered using <menu|Insert|Content
    tag|Code>. For longer pieces of code, you should use the <markup|code>
    environment.
  </explain>

  <\explain|<explain-macro|var|content>>
    Variables in a computer program like in <verbatim|cp <var|src-file>
    <var|dest-file>>. This tag corresponds to the menu entry
    <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<explain-macro|math|content>>
    This tag is used for mathematics inside regular text. Example: the
    formula <math|sin<rsup|2> x+cos<rsup|2> x=1> is well-known.
  </explain>

  <\explain|<explain-macro|op|content>>
    This is a tag which can be used inside mathematics for specifying that an
    operator should be considered on itself, without any arguments. Example:
    the operation <math|<op|+>> is a function from <math|\<bbb-R\><rsup|2>>
    to <math|\<bbb-R\>>. This tag may become depreciated.
  </explain>

  <\explain|<explain-macro|tt|content>>
    This is a physical tag for typewriter phase. It is used for compatibility
    with <name|HTML>, but we do not recommend its use.
  </explain>

  Most of the following logical size tags can be found in <menu|Insert|Size
  tag> (or <menu|Insert|Size tag>):

  <\explain>
    <explain-macro|really-tiny|content>, <explain-macro|tiny|content>

    <explain-macro|really-small|content>, <explain-macro|very-small|content>,
    <explain-macro|smaller|content>, <explain-macro|small|content>

    <explain-macro|normal-size|content>

    <explain-macro|large|content>, <explain-macro|larger|content>,
    <explain-macro|very-large|content>, <explain-macro|really-large|content>

    <explain-macro|huge|content>, <explain-macro|really-huge|content>
  <|explain>
    These logical size tags should be used by preference when typesetting
    parts of your document in a larger or smaller font. Environments like
    footnotes or captions of tables may also be based on logical size tags.
    Document styles from professional publishers often assign very precise
    font settings to each of the logical size tags. By default, the size tags
    are rendered as follows:

    <\tm-fragment>
      <really-tiny|Really tiny>

      <tiny|Tiny>

      <really-small|Really small>

      <very-small|Very small>

      <smaller|Smaller>

      <small|Small>

      <normal-size|Normal size>

      <large|Large>

      <larger|Larger>

      <very-large|Very large>

      <really-large|Really large>

      <huge|Huge>

      <really-huge|Really huge>
    </tm-fragment>
  </explain>

  The following are standard environments:

  <\explain|<explain-macro|verbatim|body>>
    Described above.
  </explain>

  <\explain|<explain-macro|code|body>>
    Similar to <markup|code*>, but for pieces of code of several lines.
  </explain>

  <\explain|<explain-macro|quote-env|body>>
    Environment for short (one paragraph) quotations.
  </explain>

  <\explain|<explain-macro|quotation|body>>
    Environment for long (multi-paragraph) quotations.
  </explain>

  <\explain|<explain-macro|verse|body>>
    Environment for poetry.
  </explain>

  <\explain|<explain-macro|center|body>>
    This is a physical tag for centering one or several lines of text. It is
    used for compatibility with <name|HTML>, but we do not recommend its use.
  </explain>

  Some standard tabular environments are

  <\explain|<explain-macro|tabular*|table>>
    Centered tables.
  </explain>

  <\explain|<explain-macro|block|table>>
    Left aligned tables with a border of standard <verbatim|1ln> width.
  </explain>

  <\explain|<explain-macro|block*|table>>
    Centered tables with a border of standard <verbatim|1ln> width.
  </explain>

  The following tags are used to adjust the typesetting of content whenever
  necessary:

  <\explain>
    <explain-macro|smash|body>

    <explain-macro|smash-top|body>, <explain-macro|smash-bottom|body><explain-synopsis|smash
    vertical size to the size of an 'x'>
  <|explain>
    These macros can be used to adjust the vertical extents of the
    <src-arg|body> to those of the character<nbsp>'x'. In the case of
    <markup|smash-top> and <markup|smash-bottom>, only the top <abbr|resp.>
    bottom are changed.
  </explain>

  <\explain>
    <explain-macro|swell|body>

    <explain-macro|swell-top|body>, <explain-macro|swell-bottom|body><explain-synopsis|increase
    vertical size to the largest character in font>
  <|explain>
    These macros can be used to increase the vertical extents of the
    <src-arg|body> to those of the largest character in the current font. In
    the case of <markup|swell-top> and <markup|swell-bottom>, only the top
    <abbr|resp.> bottom are changed. This kind of adjustments may for
    instance be used in order to ensure that <math|2\<times\>2><nbsp>matrices
    with simple textual contents always have the same size:

    <\equation*>
      <matrix|<tformat|<table|<row|<cell|f>|<cell|0>>|<row|<cell|0>|<cell|f>>>>>,<matrix|<tformat|<table|<row|<cell|a>|<cell|x>>|<row|<cell|0>|<cell|a>>>>><with|mode|text|
      versus ><matrix|<tformat|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|f>|<cell|0>>|<row|<cell|0>|<cell|f>>>>>,<matrix|<tformat|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|a>|<cell|x>>|<row|<cell|0>|<cell|a>>>>>
    </equation*>

    In fact, for a more uniform appearance, swelling is activated by default
    inside matrices.
  </explain>

  <\explain>
    <explain-macro|extend|content|left-lim|bot-lim|right-lim|top-lim><explain-synopsis|extend
    the size>
  <|explain>
    This primitive is similar to <markup|resize>, except that the new size of
    the <src-arg|content> is always larger than the original size.
  </explain>

  The following miscellaneous tags don't take arguments:

  <explain|<explain-macro|TeXmacs>|The <TeXmacs> logo.>

  <explain|<explain-macro|TeXmacs-version>|The current version of <TeXmacs>
  (<TeXmacs-version>).>

  <explain|<explain-macro|made-by-TeXmacs>|A macro which may be used to
  indicate that your document was written using <TeXmacs>.>

  <explain|<explain-macro|TeX>|The <TeX> logo.>

  <explain|<explain-macro|LaTeX>|The <LaTeX> logo.>

  <\explain|<explain-macro|hrule>>
    A horizontal rule like the one you see below:

    <hrule>
  </explain>

  The following miscellaneous tags all take one or more arguments:

  <\explain|<explain-macro|phantom|content>>
    This tag takes as much space as the typeset argument <src-arg|content>
    would take, but <src-arg|content> is not displayed. For instance,
    <inactive*|<phantom|phantom>> yields ``<phantom|phantom>''.
  </explain>

  <\explain|<explain-macro|overline|content>>
    For <overline|overlined text>, which can be wrapped across several lines.
  </explain>

  <\explain|<explain-macro|underline|content>>
    For <underline|underlined text>, which can be wrapped across several
    lines.
  </explain>

  <\explain|<explain-macro|folded|summary|body>>
    The <src-arg|summary> is displayed and the <src-arg|body> ignored: the
    macro corresponds to the folded presentation of a piece of content
    associated to a short title or abstract. The second argument can be made
    visible using <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<explain-macro|unfolded|summary|body>>
    Unfolded presentation of a piece of content <src-arg|body> associated to
    a short title or abstract <src-arg|summary>. The second argument can be
    made invisible using <menu|Insert|Switch|Fold>.
  </explain>

  <\explain|<explain-macro|switch|current|alternatives>>
    Content which admits a finite number of alternative representation among
    which the user can switch using the function keys <key|F9>, <key|F10>,
    <key|F11> and <key|F12>. This may for instance be used in interactive
    presentations. The argument <src-arg|current> correspond to the currently
    visible presentation and <src-arg|alternative> to the set of
    alternatives.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>