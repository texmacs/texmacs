<TeXmacs|1.0.3.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Table primitives>

  Tables are always present in documents inside evaluable tags which take a
  <markup|tformat> operand. All fundamental table structures have
  inaccessible borders. The basic top-level table tag is <markup|tabular>.

  <\explain>
    <explain-macro|tformat|with-1|<with|mode|math|\<cdots\>>|with-n|table><explain-synopsis|table
    formatting container>
  <|explain>
    Every tabular structure in a document contains a <markup|tformat> tag.

    <explain-macro|tformat|table> means the table and cell variables defined
    in the top-level table tag are not modified. The <src-arg|table> argument
    may be a <markup|table> or a nested <markup|tformat> tag, the latter does
    not appear in documents but is produced by the evaluation of the
    top-level tag.

    <explain-macro|tformat|with-1|<with|mode|math|\<cdots\>>|with-n|table> is
    used when the table contains specific formatting information. The
    <src-arg|with-1> to <src-arg|with-n> arguments must all be <markup|twith>
    or <markup|cwith> tags.
  </explain>

  <\explain>
    <label|table-twith><explain-macro|twith|var|val><explain-synopsis|set a
    table variable>
  <|explain>
    The formatting of the table as a whole is specified by a number of
    <em|table variables>, which are used internally and do not appear in the
    environment like regular typesetter variables.

    The <markup|twith> primitive<verbatim|> sets the table variable
    <src-arg|var> (literal string) to the value <src-arg|val> (evaluated).
  </explain>

  <\explain>
    <label|table-cwith><explain-macro|cwith|top-row|bot-row|left-col|right-col|var|val><explain-synopsis|set
    a cell variable for a range>
  <|explain>
    The formatting of cells is specified by a number of <em|cell variables>,
    which are used internally and do not appear in the environment like
    regular typesetter variables. Rows, columns, and generally any
    rectangular range of cells can associated to a cell variable setting by a
    single <markup|cwith> tag.

    The <markup|cwith> primitive sets the cell variable <src-arg|var>
    (literal string) to the value <src-arg|val> (evaluated) for the range of
    cells spanning rows <src-arg|top-row> to <src-arg|bot-row> and columns
    <src-arg|left-col> to <src-arg|right-col> (literal non-zero integers).

    Range coordinates must be non-zero literal integers, positive values are
    counted left to right and top to bottom, negative values are counted
    right to left and bottom to top. For example, 2 means the second row or
    column and -1 means the last row or column.

    Typical values for <with|mode|math|(<with|mode|text|<src-arg|top-row>>,<with|mode|text|<src-arg|bot-row>>,<with|mode|text|<src-arg|left-col>>,<with|mode|text|<src-arg|right-col>>)>
    are <with|mode|math|(r,r,<op|>1,<op|->1)> for ``row <with|mode|math|r>'',
    <with|mode|math|(<op|>1,<op|->1,c,c)> for ``column <with|mode|math|c>'',
    and <with|mode|math|(r,r,c,c)> for ``the cell at row <with|mode|math|r>,
    column <with|mode|math|c>''. When new cells are inserted, it makes a
    difference whether the rows are counted from the top or bottom, and the
    columns are counted from the left or right. If <with|mode|math|m> is the
    number of rows and <with|mode|math|n> the number of columns, then
    <with|mode|math|r> and <with|mode|math|r-m-1> represent the same
    row<emdash>the former is relative to the top border while the latter is
    relative the bottom border. Similarly, <with|mode|math|c> and
    <with|mode|math|c-n-1> represent the same column.
  </explain>

  <\explain>
    <explain-macro|table|row-1|<with|mode|math|\<cdots\>>|row-n><explain-synopsis|row
    container>
  <|explain>
    The only purpose of the <markup|table> tag is to contain <markup|row>
    tags. The number of rows in a table is the number of subtrees in its
    <markup|table> tag.
  </explain>

  <\explain>
    <explain-macro|row|cell-1|<with|mode|math|\<cdots\>>|cell-k><explain-synopsis|cell
    container>
  <|explain>
    The only purpose of the <markup|row> tag is to contain <markup|cell>
    tags. All <markup|row> tags in a given <markup|table> must have exactly
    as many subtrees, all <markup|cell> tags, as there are columns in the
    table.
  </explain>

  <\explain>
    <explain-macro|cell|content><explain-synopsis|cell data container>
  <|explain>
    Table cells can contain any document fragment. A <markup|cell> may
    directly contain an <re-index|inline content> tag or a <markup|concat>,
    if it has <re-index|block content> it must always contain a
    <markup|document> tree.

    A <markup|cell> whose operand is a <markup|document> is a
    <def-index|multi-paragraph cell>. Since tables are allowed in
    <re-index|line context>, this is the only construct which allows,
    indirectly, the nesting of a <re-index|block context> within a
    <re-index|line context>. Note that most block content can only be typeset
    correctly within an hyphenated cell, this is controlled by the
    <src-var|cell-hyphen> table variable.
  </explain>

  <\explain>
    <explain-macro|subtable|table><explain-synopsis|subtable cell data>
  <|explain>
    In addition to regular markup, cells can accept
    <markup|><markup|subtable> as an operand. The operand of
    <markup|subtable> is a <markup|tformat> tree containing regular table
    data.

    A similar effect can be obtained with normal table by setting the cell's
    padding to zero in all directions, the extra twist of a <markup|subtable>
    is its inaccessible border positions.
  </explain>

  <\explain>
    <explain-macro|tmarker|table><explain-synopsis|decoration origin marker>
  <|explain>
    This tag is used in the definition of cell decorations, see the
    documentation of the <src-var|cell-decoration> environment variable.

    It is also used outside tables, in the <markup|switch> tag to mark the
    currently displayed position.
  </explain>

  <\explain>
    <explain-macro|tabular|table><explain-synopsis|built-in tabular macro>
  <|explain>
    This macro implements standard left aligned tables without borders.
    Although the <markup|tabular> macro is built-in into <TeXmacs>, it should
    not really be considered as a primitive. However, it is not part of any
    style file either.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>