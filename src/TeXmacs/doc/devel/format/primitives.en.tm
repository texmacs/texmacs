<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|The primitive <TeXmacs> constructs>

  This page is far from up to date. Also, we need one file for each category
  and one page for each primitive, with an example.

  <section|Main formatting constructs>

  <\description>
    <expand|item*|<verbatim|(document p1 ...
    pn)>><with|mode|math|mode|text|><format|next line>The general form of the
    edit tree, which is a document consisting of the paragraphs <verbatim|p1,
    ..., pn>. Can also be used in subtrees to create a multiparagraph block
    in contexts which would be otherwise restricted line content; for example
    multiparagraph table cells use nested <verbatim|document> nodes.

    <expand|item*|<verbatim|(concat t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Horizontal
    concatenation of the trees <verbatim|t1, ..., tn>. Often, but not always,
    the paragraphs of the main edit tree are concatenations.

    <expand|item*|<verbatim|(surround left right
    body)>><with|mode|math|mode|text|><format|next line>Surround
    multiparagraph text <verbatim|body> by <verbatim|left> and
    <verbatim|right>.

    <expand|item*|<verbatim|(hspace spc)> or <verbatim|(hspace spc min
    max)>><with|mode|math|mode|text|><format|next line>Horizontal space of
    length <verbatim|spc>. The optional <verbatim|min> and <verbatim|max>
    parameters indicate that the space may be shrinked or extended to these
    values. We notice that certain units of length, like <verbatim|fn>,
    automatically provide shrinking and extension information.

    <expand|item*|<verbatim|(vspace* spc)> or <verbatim|(vspace* spc min
    max)>><with|mode|math|mode|text|><format|next line>Insert vertical space
    of length <verbatim|spc> before this paragraph.

    <expand|item*|<verbatim|(vspace spc)> or <verbatim|(vspace spc min
    max)>><with|mode|math|mode|text|><format|next line>Insert vertical space
    of length <verbatim|spc> after this paragraph.

    <expand|item*|<verbatim|(space spc bot
    top)>><with|mode|math|mode|text|><format|next line>Insert horizontal
    space of length <verbatim|spc>, with bottom and top lines at heights
    <verbatim|bot> resp. <verbatim|top>.

    <expand|item*|<verbatim|(htab spc)>><with|mode|math|mode|text|><format|next
    line>Tabbing space of minimal length <verbatim|spc>.

    <expand|item*|<verbatim|(split pos1 ...
    posn)>><with|mode|math|mode|text|><format|next line>Split the current
    paragraph as a table of <verbatim|n> columns, which are alligned
    following <verbatim|pos1, ..., posn>.

    <expand|item*|<verbatim|(move t x y)>><with|mode|math|mode|text|><format|next
    line>Move the tree <verbatim|n> horzontally by length <verbatim|x> and
    vertically by length <verbatim|y>.

    <expand|item*|<verbatim|(resize t "normal" x1 y1 x2
    y2)>><with|mode|math|mode|text|><format|next line>New logical bounding
    box of <verbatim|t> has <verbatim|(x1,y1)> as its lower left corner and
    <verbatim|(x2,y2)> as its upper right corner.

    <expand|item*|<verbatim|(resize t "extend" x1 y1 x2
    y2)>><with|mode|math|mode|text|><format|next line>As above, but the new
    bounding box is forced to include the old one.

    <expand|item*|<verbatim|(format "line
    break")>><with|mode|math|mode|text|><format|next line>Forces a line
    break.

    <expand|item*|<verbatim|(format "new line")>><with|mode|math|mode|text|><format|next
    line>Forces a new line.

    <expand|item*|<verbatim|(format "line
    separator")>><with|mode|math|mode|text|><format|next line>Like the
    <apply|TeX> <verbatim|&> command.

    <expand|item*|<verbatim|(format "next
    line")>><with|mode|math|mode|text|><format|next line>Like the <apply|TeX>
    <verbatim|\\\\> command.

    <expand|item*|<verbatim|(format "no line
    break")>><with|mode|math|mode|text|><format|next line>Prevents a line
    break.

    <expand|item*|<verbatim|(format "no first
    indentation")>><with|mode|math|mode|text|><format|next line>Prevent left
    indentation for this paragraph.

    <expand|item*|<verbatim|(format "no last
    indentation")>><with|mode|math|mode|text|><format|next line>Prevent right
    indentation for this paragraph.

    <expand|item*|<verbatim|(format "enable first
    indentation")>><with|mode|math|mode|text|><format|next line>Force left
    indentation for this paragraph.

    <expand|item*|<verbatim|(format "enable last
    indentation")>><with|mode|math|mode|text|><format|next line>Force right
    indentation for this paragraph.

    <expand|item*|<verbatim|(format "page
    break")>><with|mode|math|mode|text|><format|next line>Forces a page
    break.

    <expand|item*|<verbatim|(format "new page")>><with|mode|math|mode|text|><format|next
    line>Forces a new page.

    <expand|item*|<verbatim|(format "no page break
    before")>><with|mode|math|mode|text|><format|next line>Prevents a page
    break before this line.

    <expand|item*|<verbatim|(format "no page break
    after")>><with|mode|math|mode|text|><format|next line>Prevents a page
    break after this line.

    <expand|item*|<verbatim|(format "with
    limits")>><with|mode|math|mode|text|><format|next line>Notify that the
    preceding text has limits. This results subscripts and superscripts to be
    placed under and over the preceding text.
  </description>

  <section|Mathematical constructs>

  <\description>
    <expand|item*|<verbatim|(group t)>><with|mode|math|mode|text|><format|next
    line>Consider <verbatim|t> as an entity (a bit like enclosing
    <verbatim|t> within accolades in <apply|TeX>).

    <expand|item*|<verbatim|(left s)>><with|mode|math|mode|text|><format|next
    line>A large left delimiter <verbatim|s>.

    <expand|item*|<verbatim|(middle s)>><with|mode|math|mode|text|><format|next
    line>A large separator <verbatim|s>.

    <expand|item*|<verbatim|(right s)>><with|mode|math|mode|text|><format|next
    line>A large right delimiter <verbatim|s>.

    <expand|item*|<verbatim|(big s)>><with|mode|math|mode|text|><format|next
    line>A big operator <verbatim|s>.

    <expand|item*|<verbatim|(lprime s)>><with|mode|math|mode|text|><format|next
    line>A left prime <verbatim|s>.

    <expand|item*|<verbatim|(rprime s)>><with|mode|math|mode|text|><format|next
    line>A right prime <verbatim|s>.

    <expand|item*|<verbatim|(below t sub)>><with|mode|math|mode|text|><format|next
    line>Subscript <verbatim|sub> below <verbatim|t>.

    <expand|item*|<verbatim|(above t sup)>><with|mode|math|mode|text|><format|next
    line>Superscript <verbatim|sup> above <verbatim|t>.

    <expand|item*|<verbatim|(lsub script)>><with|mode|math|mode|text|><format|next
    line>Left subscript <verbatim|script> for text which follows.

    <expand|item*|<verbatim|(lsup script)>><with|mode|math|mode|text|><format|next
    line>Left superscript <verbatim|script> for text which follows.

    <expand|item*|<verbatim|(rsub script)>><with|mode|math|mode|text|><format|next
    line>Right subscript <verbatim|script> for preceding text.

    <expand|item*|<verbatim|(rsup script)>><with|mode|math|mode|text|><format|next
    line>Right superscript <verbatim|script> for preceding text.

    <expand|item*|<verbatim|(frac num den)>><with|mode|math|mode|text|><format|next
    line>Fraction with numerator <verbatim|num> and denominator
    <verbatim|den>.

    <expand|item*|<verbatim|(sqrt t)> or <verbatim|(sqrt t
    n)>><with|mode|math|mode|text|><format|next line>Square root of
    <verbatim|t> or <verbatim|n>-th root of <verbatim|t>.

    <expand|item*|<verbatim|(wide t accent)>><with|mode|math|mode|text|><format|next
    line>Wide <verbatim|accent> above <verbatim|t>.

    <expand|item*|<verbatim|(neg t)>><with|mode|math|mode|text|><format|next
    line>Wipe out <verbatim|t> with a negation slash.

    <expand|item*|<verbatim|(tree root t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Tree with root
    <verbatim|root> and children <verbatim|t1, ..., tn>.

    <expand|item*|<verbatim|(matrix t11 ... t1m ...... tn1 ... tnm n
    m)>><with|mode|math|mode|text|><format|next line><verbatim|n> by
    <verbatim|m> matrix with entries <verbatim|tij>.

    <expand|item*|<verbatim|(table t11 ... t1m ...... tn1 ... tnm n
    m)>><with|mode|math|mode|text|><format|next line><verbatim|n> by
    <verbatim|m> table with entries <verbatim|tij>.

    <expand|item*|<verbatim|(mosaic t11 ... t1m ...... tn1 ... tnm n
    m)>><with|mode|math|mode|text|><format|next line><verbatim|n> by
    <verbatim|m> table whose entries <verbatim|tij> can be aligned and
    merged.

    <expand|item*|<verbatim|(mosaic item t pos x y
    bg)>><with|mode|math|mode|text|><format|next line>This field in a mosaic
    contains <verbatim|t>, is positioned at <verbatim|pos>, spans over
    <verbatim|x> columns and <verbatim|y> rows, and has background
    <verbatim|bg>.
  </description>

  <section|Environment variables, macros, functions, etc.>

  <\description>
    <expand|item*|<verbatim|(assign var t)>><with|mode|math|mode|text|><format|next
    line>Assign the environment variable <verbatim|var> with <verbatim|t>.

    <expand|item*|<verbatim|(with var1 val1 ... varn valn
    body)>><with|mode|math|mode|text|><format|next line>Locally assign the
    environment variables <verbatim|var1, ..., varn> with <verbatim|val1,
    ..., valn> inside <verbatim|body>.

    <expand|item*|<verbatim|(expand f t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Expand the macro
    <verbatim|f> with arguments <verbatim|t1, ..., tn>.

    <expand|item*|<verbatim|(apply f t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Apply the function
    <verbatim|f> to <verbatim|t1, ..., tn>.

    <expand|item*|<verbatim|(func arg1 ... argn
    body)>><with|mode|math|mode|text|><format|next line>Function with
    arguments <verbatim|arg1, ..., argn> and body <verbatim|body>.

    <expand|item*|<verbatim|(argument var)>><with|mode|math|mode|text|><format|next
    line>Macro argument <verbatim|var>.

    <expand|item*|<verbatim|(value var)>><with|mode|math|mode|text|><format|next
    line>Value of <verbatim|var>.

    <expand|item*|<verbatim|(symbol s)>><with|mode|math|mode|text|><format|next
    line>For entering a universal symbol.

    <expand|item*|<verbatim|(latex cmd)>><with|mode|math|mode|text|><format|next
    line>For entering a <apply|LaTeX> command.

    <expand|item*|<verbatim|(hybrid cmd)>><with|mode|math|mode|text|><format|next
    line>For entering a <apply|TeXmacs> function call, a <apply|LaTeX>
    command, or a universal symbol.

    <expand|item*|<verbatim|(quote t)>><with|mode|math|mode|text|><format|next
    line>Evaluates to <verbatim|t>.

    <expand|item*|<verbatim|(eval t)>><with|mode|math|mode|text|><format|next
    line>Evaluate <verbatim|t>.

    <expand|item*|<verbatim|(delay (cmd t1 ...
    tn))>><with|mode|math|mode|text|><format|next line>Evaluates arguments
    <verbatim|t1, ..., tn> to <verbatim|u1, ..., un> and returns
    <verbatim|(cmd u1 ... un)>.
  </description>

  <section|Functional operators>

  <\description>
    <expand|item*|<verbatim|(plus t u)>><with|mode|math|mode|text|><format|next
    line>Add the numbers or lengths <verbatim|t> and <verbatim|u>.

    <expand|item*|<verbatim|(minus t u)>><with|mode|math|mode|text|><format|next
    line>Subtract the numbers or lengths <verbatim|t> and <verbatim|u>.

    <expand|item*|<verbatim|(times t u)>><with|mode|math|mode|text|><format|next
    line>Multiply the numbers <verbatim|t> and <verbatim|u>.

    <expand|item*|<verbatim|(merge t u)>><with|mode|math|mode|text|><format|next
    line>Concatenate the strings <verbatim|t> and <verbatim|u>.

    <expand|item*|<verbatim|(number t what)>><with|mode|math|mode|text|><format|next
    line>The number <verbatim|t> in <verbatim|what> (roman, alpha, etc.).

    <expand|item*|<verbatim|(translate t from
    into)>><with|mode|math|mode|text|><format|next line>Translate
    <verbatim|t> from <verbatim|from> into <verbatim|into>.
  </description>

  <section|Other dynamic content>

  <\description>
    <expand|item*|<verbatim|(inactive t)>><with|mode|math|mode|text|><format|next
    line>Deactivate an environmental or invisible operator for editing.

    <expand|item*|<verbatim|(label name)>><with|mode|math|mode|text|><format|next
    line>Label with name <verbatim|name>.

    <expand|item*|<verbatim|(reference name)>><with|mode|math|mode|text|><format|next
    line>Reference to label with name <verbatim|name>.

    <expand|item*|<verbatim|(write t aux)>><with|mode|math|mode|text|><format|next
    line>Write <verbatim|t> to auxiliairy data buffer <verbatim|aux>. This
    construct is used for creating tables of contents, bibliographies,
    glossaries, etc.

    <expand|item*|<verbatim|(specific what
    t)>><with|mode|math|mode|text|><format|next line>Specifies that
    <verbatim|t> should only appear when the document is converted to
    <verbatim|what>. If <verbatim|what> is <verbatim|"TeXmacs">, then
    <verbatim|t> only appears in <apply|TeXmacs> itself.

    <expand|item*|<verbatim|(postscript file width height x1 y1 x2
    y2)>><with|mode|math|mode|text|><format|next line>Postscript (or other
    kind of) figure in <verbatim|file>, of width <verbatim|width>, length
    <verbatim|length>, lower left clipping corner <verbatim|(x1,y1)> and
    upper right clipping corner <verbatim|(x2,y2)>. The parameters
    <verbatim|width, height, x1, y1, x2, y2> may be empty strings, in which
    case the default settings of the image are used. The width and the height
    may also be of the form <verbatim|*mag> or <verbatim|/schrink>, in which
    case the default lengths are magnified resp. shrinked by a factor
    <verbatim|mag> resp. <verbatim|schrink>.
  </description>

  <section|For private use>

  <\description>
    <expand|item*|<verbatim|(tuple t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>The tuple
    <verbatim|(t1, ..., tn)>.

    <expand|item*|<verbatim|(collection t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>The set <verbatim|{t1,
    ..., tn}>.

    <expand|item*|<verbatim|(associate t u)>><with|mode|math|mode|text|><format|next
    line>The association <verbatim|t -\<gtr\> u>.
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

<\references>
  <\collection>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Main formatting
      constructs><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Mathematical
      constructs><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Environment variables, macros,
      functions, etc.><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Functional
      operators><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Other dynamic
      content><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>For private
      use><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
