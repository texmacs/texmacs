<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Standard markup>

  Various standard markup is defined in <tmdtd|std-markup>. The following
  textual content tags all take one argument. Most can be found in the
  <apply|menu|Text|Content tag> menu.

  <\description>
    <expand|item*|<markup|strong>>Indicates an <strong|important> region of
    text. You can enter this tag via <apply|menu|Text|Content tag|Strong>.

    <expand|item*|<markup|em>>Emphasizes a region of text like in ``the
    <em|real> thing''. This tag corresponds to the menu entry
    \ <apply|menu|Text|Content tag|Emphasize>.

    <expand|item*|<markup|dfn>>For definitions like ``a <dfn|gnu> is a horny
    beast''. This tag corresponds to <apply|menu|Text|Content
    tag|Definition>.

    <expand|item*|<markup|samp>>A sequence of literal characters like the
    <samp|ae> ligature æ. You can get this tag via <apply|menu|Text|Content
    tag|Sample>.

    <expand|item*|<markup|name>>The name of a particular thing or concept
    like the <name|Linux> system. This tag is obtained using
    <apply|menu|Text|Content tag|Name>.

    <expand|item*|<markup|person>>The name of a person like <name|Joris>.
    This tag corresponds to <apply|menu|Text|Content tag|Person>.

    <expand|item*|<markup|cite*>>A bibliographic citation like a book or
    magazine. Example: Melville's <expand|cite*|Moby Dick>. This tag, which
    is obtained using <apply|menu|Text|Content tag|Cite>, should not be
    confused with <markup|cite>. The latter tag is also used for citations,
    but where the argument refers to an entry in a database with
    bibliographic references.

    <expand|item*|<markup|abbr>>An abbreviation. Example: I work at the
    <abbr|C.N.R.S.> An abbreviation is created using <apply|menu|Text|Content
    tag|Abbreviation> or the <expand|kbd-text|a> keyboard shortcut.

    <expand|item*|<markup|acronym>>An acronym is an abbreviation formed from
    the first letter of each word in a name or a phrase, such as
    <acronym|HTML> or <acronym|IBM>. In particular, the letters are not
    separated by dots. You may enter an acronym using
    <apply|menu|Text|Content tag|Acronym>.

    <expand|item*|<markup|verbatim>>Verbatim text like output from a computer
    program. Example: the program said <verbatim|hello>. You may enter
    verbatim text via <apply|menu|Text|Content tag|Verbatim>. The tag may
    also be used as an environment for multi-paragraph text.

    <expand|item*|<markup|kbd>>Text which should be entered on a keyboard.
    Example: please type <kbd|return>. This tag corresponds to the menu entry
    <apply|menu|Text|Content tag|Keyboard>.

    <expand|item*|<markup|code*>>Code of a computer program like in
    ``<expand|code*|cout \<less\>\<less\> 1+1;> yields <verbatim|2>''. This
    is entered using <apply|menu|Text|Content tag|Code>. For longer pieces of
    code, you should use the <markup|code> environment.

    <expand|item*|<markup|var>>Variables in a computer program like in
    <verbatim|cp <var|src-file> <var|dest-file>>. This tag corresponds to the
    menu entry <apply|menu|Text|Content tag|Variable>.

    <expand|item*|<markup|math>>This is a tag which will be used in the
    future for mathematics inside regular text. Example: the formula
    <math|sin<rsup|2> x+cos<rsup|2> x=1> is well-known.

    <expand|item*|<markup|op>>This is a tag which can be used inside
    mathematics for specifying that an operator should be considered on
    itself, without any arguments. Example: the operation <math|<op|+>> is a
    function from <with|mode|math|\<bbb-R\><rsup|2>> to
    <with|mode|math|\<bbb-R\>>. This tag may become depreciated.

    <expand|item*|<markup|tt>>This is a physical tag for typewriter phase. It
    is used for compatability with <name|HTML>, but we do not recommend its
    use.
  </description>

  The following are standard environments:

  <\description>
    <expand|item*|<markup|verbatim>>Described above.

    <expand|item*|<markup|code>>Similar to <markup|code*>, but for pieces of
    code of several lines.

    <expand|item*|<markup|quote>>Environment for short (one paragraph)
    quotations.

    <expand|item*|<markup|quotation>>Environment for long (multi-paragraph)
    quotations.

    <expand|item*|<markup|verse>>Environment for poetry.

    <expand|item*|<markup|center>>This is a physical tag for centering one or
    several lines of text. It is used for compatability with <name|HTML>, but
    we do not recommend its use.
  </description>

  Some standard tabular environments are

  <\description>
    <expand|item*|<markup|tabular*>>Centered tables.

    <expand|item*|<markup|block>>Left aligned tables with a border of
    standard <verbatim|1ln> width.

    <expand|item*|<markup|block*>>Centered tables with a border of standard
    <verbatim|1ln> width.
  </description>

  The following miscellaneous tags don't take arguments:

  <\description>
    <expand|item*|<markup|TeXmacs>>The <TeXmacs> logo.

    <expand|item*|<markup|TeX>>The <TeX> logo.

    <expand|item*|<markup|LaTeX>>The <LaTeX> logo.

    <expand|item*|<markup|hflush>>Used by developers for flushing to the
    right in the definition of environments.

    <expand|item*|<markup|hrule>>A horizontal rule like the one you see
    below:

    <value|hrule>
  </description>

  The following miscellaneous tags all take one or more arguments:

  <\description>
    <expand|item*|<markup|overline>>For <overline|overlined text>, which can
    be wrapped across several lines.

    <expand|item*|<markup|underline>>For <underline|underlined text>, which
    can be wrapped across several lines.

    <expand|item*|<markup|fold>>Macro with two arguments. The first argument
    is displayed and the second one ignored: the macro corresponds to the
    folded presentation of a piece of content associated to a short title or
    abstract. The second argument can be made visible using
    <apply|menu|Insert|Switch|Unfold>.

    <expand|item*|<markup|unfold>>Macro with two arguments <var|x> and
    <var|y>, which yields the unfolded presentation of a piece of content
    <var|y> associated to a short title or abstract <var|x>. The second
    argument can be made invisible using <apply|menu|Insert|Switch|Fold>.

    <expand|item*|<markup|switch>>Macro with two arguments <var|x> and
    <var|y>, where <var|y> is a set of possible representations of the switch
    and <var|x> the current representation. The function keys <key|F9>,
    <key|F10>, <key|F11> and <key|F12> can be used to switch between
    different representations.

    <expand|item*|<markup|phantom>>Function with one argument <var|x>. This
    tag takes as much space as the typesetted argument <var|x> would take,
    but <var|x> is not displayed. For instance, the text ``phantom'' as an
    argument of <markup|phantom> yields ``<apply|phantom|phantom>''.

    <expand|item*|<markup|set-header>>Function with one argument for
    permanently changing the header. Notice that certain tags in the style
    file, like sectional tags, may override such manual changes.

    <expand|item*|<markup|set-footer>>Function with one argument for
    permanently changing the footer.
  </description>

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
