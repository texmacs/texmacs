<TeXmacs|1.0.3.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Table primitives>

  <\big-table>
    <tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|tformat>>|<cell|<with|mode|math|n+1>>|<cell|No>|<cell|Last>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|twith>>|<cell|<with|mode|math|2>>|<cell|Yes>|<cell|All>|<cell|Display>|<cell|Physical>>|<row|<cell|<markup|cwith>>|<cell|<with|mode|math|6>>|<cell|Yes>|<cell|All>|<cell|Display>|<cell|Physical>>|<row|<cell|<markup|table>>|<cell|<with|mode|math|n\<gtr\>0>>|<cell|No>|<cell|All>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|row>>|<cell|<with|mode|math|n\<gtr\>0>>|<cell|No>|<cell|All>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|cell>>|<cell|1>|<cell|No>|<cell|All>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|subtable>>|<cell|1>|<cell|No>|<cell|All>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|tmarker>>|<cell|0>|<cell|Yes>|<cell|->|<cell|Display>|<cell|Physical>>>>>
  </big-table|Table primitives>

  Tables are always present in documents inside evaluable tags which take a
  <markup|tformat> operand. All fundamental table structures have
  inaccessible borders. The basic top-level table tag is <markup|tabular>.

  <\description-dash>
    <item*|<markup|tformat>>Table formatting container.

    Every tabular structure in a document contains a <markup|tformat> tag.

    <verbatim|(tformat <var|table>)> means the table and cell variables
    defined in the top-level table tag are not modified. The <var|table>
    argument may be a <markup|table> or a nested <markup|tformat> tag, the
    latter does not appear in documents but is produced by the evaluation of
    the top-level tag.

    <verbatim|(tformat <var|w1> ... <var|wn> <var|table>)> is used when the
    table contains specific formatting information. The <var|w1> to <var|wn>
    operands must all be <markup|twith> or <markup|cwith> tags.

    <item*|<markup|twith>>Set a table variable.

    The formatting of the table as a whole is specified by a number of
    <em|table variables>, which are used internally and do not appear in the
    environment like regular typesetter variables.

    <verbatim|(twith <var|var> <var|val>)> sets the table variable <var|var>
    (literal string) to the value <var|val> (evaluated).

    <item*|<markup|cwith>>Set a cell variable for a range.

    The formatting of cells is specified by a number of <em|cell variables>,
    which are used internally and do not appear in the environment like
    regular typesetter variables. Rows, columns, and generally any
    rectangular range of cells can associated to a cell variable setting by a
    single <markup|cwith> tag.

    <verbatim|(cwith <var|i1> <var|i2> <var|j1> <var|j2> <var|var>
    <var|val>)> sets the cell variable <var|var> (literal string) to the
    value <var|val> (evaluated) for the range of cells spanning rows <var|i1>
    to <var|i2> and columns <var|j1> to <var|j2> (literal non-zero integers).

    Range coordinates must be non-zero literal integers, positive values are
    counted left to right and top to bottom, negative values are counted
    right to left and bottom to top. For example, 2 means the second row or
    column and -1 means the last row or column.

    Typical values for <with|mode|math|(i<rsub|1>, i<rsub|2>, j<rsub|1>,
    j<rsub|2>)> are <with|mode|math|(r,r,<op|->1,1)> for ``row
    <with|mode|math|r>'', <with|mode|math|(<op|->1,1,c,c)> for ``column
    <with|mode|math|c>'', and <with|mode|math|(r,r,c,c)> for ``the cell at
    row <with|mode|math|r>, column <with|mode|math|c>''. Note that, with
    <with|mode|math|m> is the number of rows and <with|mode|math|n> the
    number of columns, <with|mode|math|r> is the same row as
    <with|mode|math|r-m-1> and <with|mode|math|c> is the same column as
    <with|mode|math|c-n-1>, but the former are relative to the top and left
    borders while the latter are relative the bottom and right borders, this
    makes a difference when new cells are inserted.

    <item*|<markup|table>>Row container.

    The only purpose of the <markup|table> tag is to contain <markup|row>
    tags. The number of rows in a table is the number of subtrees in its
    <markup|table> tag.

    <item*|<markup|row>>Cell container.

    The only purpose of the <markup|row> tag is to contain <markup|cell>
    tags. All <markup|row> tags in a given <markup|table> must have exactly
    as many subtrees, all <markup|cell> tags, as there are columns in the
    table.

    <item*|<markup|cell>>Cell data container.

    Table cells can contain any document fragment. A <markup|cell> may
    directly contain an <re-index|inline content> tag or a <markup|concat>,
    if it has <re-index|block content> it must always contain a
    <markup|document> tree.

    A <markup|cell> whose operand is a <markup|document> is a
    <def-index|multiparagraph cell>. Since tables are allowed in
    <re-index|line context>, this is the only construct which allows,
    indirectly, the nesting of a <re-index|block context> within a
    <re-index|line context>. Note that most block content can only be typeset
    correctly within an hyphenated cell, this is controlled by the
    <verbatim|cell-hyphen> table variable.

    <item*|<markup|subtable>>Subtable cell data.

    In addition to regular markup, cells can accept
    <markup|><markup|subtable> as an operand. The operand of
    <markup|subtable> is a <markup|tformat> tree containing regular table
    data.

    A similar effect can be obtained with normal table by setting the cell's
    padding to zero in all directions, the extra twist of a <markup|subtable>
    is its inaccessible border positions.

    <item*|<markup|tmarker>>Decoration origin marker.

    This tag is used in the definition of cell decorations, see the
    documentation of the <verbatim|cell-decoration> environment variable.

    It is also used outside tables, in the <markup|switch> tag to mark the
    currently displayed position.<verbatim|<htab|5mm>>
  </description-dash>

  <tmdoc-copyright|2004|David Allouche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>