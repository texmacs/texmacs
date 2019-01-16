<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Common errors and syntax correction>

  By default, the semantic editing mode \Punderstands\Q most classical
  mathematical notations. This is achieved through the use of a carefully
  designed grammar for mainstream mathematics. Obviously, the use of a fixed
  grammar may cause the following problems:

  <\itemize>
    <item>Mathematical formulas frequently contain <em|ad hoc> notations. For
    instance, the formulas might contain some text or meaningful whitespace.
    Another example of an <em|ad hoc> notation is the sign sequence
    <math|++-+-+>. In such cases, the user should <hlink|explicitly
    annotate|man-semantic-annotation.en.tm> the appropriate parts of the
    formula in order to make them semantically meaningful.

    <item>The <TeXmacs> grammar used for the interpretation of mathematical
    formulas may be incomplete or inadequate for certain situations. It is
    possible to customize or extend the grammar using the standard <TeXmacs>
    macro mechanism. Notations for specific areas may be grouped together in
    dedicated style packages.
  </itemize>

  Besides these intrinsically hard to avoid problems, the following common
  and \Peasy-to-make\Q mistakes are a further source of trouble for
  associating semantics to mathematical formulas:

  <\itemize>
    <item>Since <TeXmacs> is a wysiwyg editor, some of the structure of the
    document is invisible for the user. For instance, the presence of a
    mathematical formula <math|x+y> is indicated through the use of an italic
    slant and special spacing. However, in the formula
    <math|f<around*|(|x|)>> it is easy to type the closing bracket outside
    the formula, with no visual difference.

    <item>Various mathematical notations are visually ambiguous. For
    instance, <math|a*<around*|(|b+c|)>> would usually be understood as
    <math|a\<cdot\><around*|(|b+c|)>>, whereas <math|f<around*|(|x+y|)>>
    rather corresponds to a function application. In the semantic editing
    mode, the user is expected to resolve this ambiguity by hand by entering
    multiplications using <key|*> and spaces using <key|space>. The
    multiply/apply ambiguity is one of the main sources of syntax errors,
    since many users do not pay attention to invisible differences.
    Similarly, the <math|\<wedge\>> glyph could be the \Plogical and\Q or the
    \Pwedge product\Q. This \Phomoglyph\Q issue will be addressed in more
    detail in the section on the <hlink|semantics of mathematical
    symbols|man-semantics-symbols.en.tm>.\ 

    <item>It could be that a text was originally written in <LaTeX> or an old
    version of <TeXmacs>. In that case, the document contains no special
    indication on matching brackets or the scopes of big operators. For
    instance, in the formula <math|<around*|[|x,y|[>>, should we interpret
    the second bracket as a closing bracket? This is indeed the standard
    french notation for an interval with an open right end. More generally,
    all problems that we have mentioned so far tend to be present
    simultaneously when trying to associate semantics to existing documents.
  </itemize>

  After activation of the semantic editing mode, you may check whether a
  formula is correct by positioning your cursor inside it and looking at the
  color of the bounding box of the <hlink|semantic
  focus|man-semantics.en.tm#semantic-focus>: a<nbsp>green color corresponds
  to a correct formula and a<nbsp>red color indicates an error in the
  formula. Alternatively, assuming that the focus is on a mathematical
  formula, you may select <menu|Focus|Preferences|Highlight incorrect
  formulas>, in which all incorrect formulas are highlighted inside red
  boxes.

  For the second kind of \Peasy-to-make\Q errors, <TeXmacs> includes an
  automatic syntax corrector. Assuming that your cursor is inside a formula,
  you may use <menu|Edit|Correct|Correct all> for the correction of all
  formulas in your document, or the correction of the current selection. If
  the versioning tool is activated, then you may use
  <menu|Edit|Correct|Correct manually> to show the differences between the
  original and the corrected versions. You may then use the versioning tool
  to go through these differences and select the preferred versions.

  The precise algorithms which are used for the correction may be enabled or
  disabled from <menu|Edit|Preferences|Mathematics|Manual correction>:

  <\description>
    <item*|<menu|Remove superfluous invisible operators>>This algorithm is
    used in order to remove any superfluous function applications or
    multiplications. For instance, users who are accustomed to editing ASCII
    files often type spaces around binary infixes such as addition. Such
    \Pfunction applications\Q will be removed by this algorithm.

    <item*|<menu|Insert missing invisible operators>>In <LaTeX>,
    multiplications and function applications are never entered explicitly.
    When importing a <LaTeX> document, it is therefore important to detect
    and insert missing multiplications and function applications.

    <item*|<menu|Homoglyph substitutions>>This algorithm may perform some
    other useful substitutions of symbols by visually similar, but
    semantically distinct symbols. For instance, the backslash
    symbol<nbsp><math|\\> is replaced by the binary set differences infix (as
    in <math|X\<setminus\>Y>), whenever appropriate.
  </description>

  From the <menu|Edit|Preferences|Mathematics|Automatic correction>, you may
  also select those corrections algorithms which should be applied
  automatically whenever you open a file. The various corrections are always
  carried out when importing a <LaTeX> file.

  After syntax correction, the remaining errors indicate genuine typos at
  worst or non standard or non supported notations at best. We also notice
  that \Pcorrect\Q formulas do not necessarily have the intended meaning. In
  order to check whether the operators indeed apply to the intended
  arguments, you should keep an eye on the current focus while typing your
  formulas.\ 

  <tmdoc-copyright|2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>