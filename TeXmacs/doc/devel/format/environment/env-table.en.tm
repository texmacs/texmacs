<TeXmacs|1.0.3.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Table layout>

  The environment variables for tables can be subdivided in variables
  (prefixed by <src-var|table->) which apply to the whole table and those
  (prefixed by <src-var|cell->) which apply to individual cells. Whereas
  usual environment variables are set with <markup|assign> and <markup|with>,
  the tabular environment variables are rather set with the
  <hyper-link|<markup|tformat> primitive|../regular/prim-table.en.tm>. This
  makes it possible to apply certain settings to any rectangular subtable of
  the entire table and in particular to rows or columns. For more details,
  see the <hyper-link|documentation|../regular/prim-table.en.tm#table-twith>
  of the <markup|twith> and <markup|cwith> primitives.

  <paragraph*|Layout of the table as a whole>

  <\explain>
    <var-val|table-width|>

    <var-val|table-height|><explain-synopsis|hint for table dimensions>
  <|explain>
    These parameters indicate a hint for the dimensions of the table. The
    <src-var|table-hmode> and <src-var|table-vmode> variables determine how
    to take into account these settings.
  </explain>

  <\explain>
    <var-val|table-hmode|>

    <var-val|table-vmode|><explain-synopsis|determination of table
    dimensions>
  <|explain>
    These parameters specify how to determine the dimensions of the table. At
    the moment, the values of <src-var|table-hmode> and <src-var|table-vmode>
    are actually ignored and <src-var|table-width> and <src-var|table-height>
    are interpreted as the minimal width and height of the table.
  </explain>

  <\explain>
    <var-val|table-halign|l>

    <var-val|table-valign|f><explain-synopsis|alignment inside text>
  <|explain>
    These parameters determine how the table should be aligned in the
    surrounding text. Possible values for <src-var|table-halign> are
    <verbatim|l> (left), <verbatim|c> (center) and <verbatim|r> (right), and
    possible values for <src-var|table-valign> are <verbatim|t> (top),
    <verbatim|f> (centered at fraction bar height), <verbatim|c> (center) and
    <verbatim|b> (bottom).

    In addition to the above values, the alignment can take place with
    respect to the baselines of particular cells. Such values for
    <src-var|table-halign> are <verbatim|L> (align <abbr|w.r.t.> the left
    column), <verbatim|C> (align <abbr|w.r.t.> the middle column),
    <verbatim|R> (align <abbr|w.r.t.> the right column) and <verbatim|O>
    (align <abbr|w.r.t.> the priviledged origin column
    <src-var|table-col-origin>). Similarly, <src-var|table-halign> may take
    the additional values <verbatim|T> (align <abbr|w.r.t.> the top row),
    <verbatim|C> (align <abbr|w.r.t.> the middle row), <verbatim|B> (align
    <abbr|w.r.t.> the bottom row) and <verbatim|O> (align <abbr|w.r.t.> the
    priviledged origin row <src-var|table-row-origin>).
  </explain>

  <\explain>
    <var-val|table-row-origin|0>

    <var-val|table-col-origin|0><explain-synopsis|priviledged cell>
  <|explain>
    Table coordinates of an priviledged ``origin cell'' which may be used for
    aligning the table in the surrounding text (see above).
  </explain>

  <\explain>
    <var-val|table-lsep|0fn>

    <var-val|table-rsep|0fn>

    <var-val|table-bsep|0fn>

    <var-val|table-tsep|0fn><explain-synopsis|padding around table>
  <|explain>
    Padding around the table (in addition to the padding of individual
    cells).
  </explain>

  <\explain>
    <var-val|table-lborder|0ln>

    <var-val|table-rborder|0ln>

    <var-val|table-bborder|0ln>

    <var-val|table-tborder|0ln><explain-synopsis|border around table>
  <|explain>
    Border width for the table (in addition to borders of the individual
    cells).
  </explain>

  <\explain>
    <var-val|table-hyphen|n><explain-synopsis|allow for hyphenation?>
  <|explain>
    A flag which specifies whether page breaks may occur at the middle of
    rows in the table. When <src-var|table-hyphen> is set to <verbatim|y>,
    then such page breaks may only occur when

    <\enumerate>
      <item> The table is not surrounded by other markup in the same
      paragraph.

      <item>The rows whether the page break occurs has no borders.
    </enumerate>

    An example of a tabular environment which allows for page breaks is
    <markup|eqnarray*>.
  </explain>

  <\explain>
    <var-val|table-min-rows|>

    <var-val|table-min-cols|>

    <var-val|table-max-rows|>

    <var-val|table-max-cols|><explain-synopsis|constraints on the table's
    size>
  <|explain>
    It is possible to specify a minimal and maximal numbers of rows or
    columns for the table. Such settings constraint the behaviour of the
    editor for operations which may modify the size of the table (like the
    insertion and deletion of rows and columns). This is particularly useful
    for tabular macros. For instance, <src-var|table-min-columns> and
    <src-var|table-max-columns> are both set to <with|mode|math|3> for the
    <markup|eqnarray*> environment.
  </explain>

  <paragraph*|Layout of the individual cells>

  <\explain>
    <var-val|cell-background|><explain-synopsis|background color>
  <|explain>
    A background color for the cell.
  </explain>

  <\explain>
    <var-val|cell-width|>

    <var-val|cell-height|><explain-synopsis|hint for cell dimensions>
  <|explain>
    Hints for the width and the height of the cell. The real width and height
    also depend on the modes <src-var|cell-hmode> and <src-var|cell-vmode>,
    possible filling (see <src-var|cell-hpart> and <src-var|cell-vpart>
    below), and, of course, on the dimensions of other cells in the same row
    or column.
  </explain>

  <\explain>
    <var-val|cell-hpart|>

    <var-val|cell-vpart|><explain-synopsis|fill part of unused space>
  <|explain>
    When the sum <with|mode|math|s> of the widths of all columns in a table
    is smaller than the width <with|mode|math|w> of the table itself, then it
    should be specified what should be done with the unused space. The
    <src-var|cell-hpart> parameter specifies a part in the unusued space
    which will be taken by a particular cell. The horizontal part taken by a
    column is the maximum of the horizontal parts of its composing cells. Now
    let <with|mode|math|p<rsub|i>> the so determined part for each column
    (<with|mode|math|i\<in\>{1,\<ldots\>,n}>). Then the extra horizontal
    space which will be distributed to this column is
    <with|mode|math|p<rsub|i>*(w-s)/(p<rsub|1>+\<cdots\>+p<rsub|n>)>. A
    similar computation determines the extra vertical space which is
    distributed to each row.
  </explain>

  <\explain>
    <var-val|cell-hmode|exact>

    <var-val|cell-vmode|exact><explain-synopsis|determination of cell
    dimensions>
  <|explain>
    These parameters specify how to determine the width and the height of the
    cell. If <src-var|cell-hmode> is <verbatim|exact>, then the width is
    given by <src-var|cell-width>. If <src-var|cell-hmode> is <verbatim|min>
    or <verbatim|max>, then the width is the minimul <abbr|resp.> maximum of
    <src-var|cell-width> and the width of the content. The height is
    determined similarly.
  </explain>

  <\explain>
    <var-val|cell-halign|l>

    <var-val|cell-valign|B><explain-synopsis|cell alignment>
  <|explain>
    These parameters determine the horizontal and vertical alignment of the
    cell. Possible values of <src-var|cell-halign> are <verbatim|l> (left),
    <verbatim|c> (center), <verbatim|r> (right), <verbatim|.> (decimal dot),
    <verbatim|,> (decimal comma) and <verbatim|R> (vertical baseline).
    Possible values of <src-var|cell-valign> are <verbatim|t> (top),
    <verbatim|c> (center), <verbatim|b> (bottom) and <verbatim|B> (baseline).
  </explain>

  <\explain>
    <var-val|cell-lsep|0fn>

    <var-val|cell-rsep|0fn>

    <var-val|cell-bsep|0fn>

    <var-val|cell-tsep|0fn><explain-synopsis|cell padding>
  <|explain>
    The amount of padding around the cell (at the left, right, bottom and
    top).
  </explain>

  <\explain>
    <var-val|cell-lborder|0ln>

    <var-val|cell-rborder|0ln>

    <var-val|cell-bborder|0ln>

    <var-val|cell-tborder|0ln><explain-synopsis|cell borders>
  <|explain>
    The borders of the cell (at the left, right, bottom and top). The
    displayed border between cells <with|mode|math|T<rsub|i,j>> and
    <with|mode|math|T<rsub|i,j+1>> at positions <with|mode|math|(i,j)> and
    <with|mode|math|(i,j+1)> is the maximum of the borders between the right
    border of <with|mode|math|T<rsub|i,j>> and the left border of
    <with|mode|math|T<rsub|i,j+1>>. Similarly, the displayed border between
    cells <with|mode|math|T<rsub|i,j>> and <with|mode|math|T<rsub|i+1,j>> is
    the maximum of the bottom border of <with|mode|math|T<rsub|i,j>> and the
    top border of <with|mode|math|T<rsub|i+1,j>>.
  </explain>

  <\explain>
    <var-val|cell-vcorrect|a><explain-synopsis|vertical correction of text>
  <|explain>
    As described above, the dimensions and the alignment of a cell may depend
    on the dimensions of its content. When cells contain text boxes, the
    vertical bounding boxes of such text may vary as a function of the text
    (the letter ``k'' <abbr|resp.> ``y'' ascends <abbr|resp.> descends
    further than ``x''). Such differences sometimes leads to unwanted,
    non-uniform results. The vertical cell correction allows for a more
    uniform treatment of text of the same font, by descending and/or
    ascending the bounding boxes to a level which only depends on the font.
    Possible values for <src-var|cell-vcorrect> are <verbatim|n> (no vertical
    correction), <verbatim|b> (vertical correction of the bottom),
    <verbatim|t> (vertical correction of the top), <verbatim|a> (vertical
    correction of bottom and the top).
  </explain>

  <\explain>
    <var-val|cell-hyphen|n><explain-synopsis|allow for hyphenation inside
    cells>
  <|explain>
    By default, the cells contain inline content which is not hyphenated. By
    selecting <menu|Table|Special cell properties|Hyphenation|Multi-paragraph>,
    the cell contents becomes multi-paragraph. In that case,
    <src-var|cell-hyphen> determines how this content is hyphenated. Possible
    values are <verbatim|n> (disable line breaking) and <verbatim|b>,
    <verbatim|c> and <verbatim|t> (enable line breaking and align at the
    bottom, center <abbr|resp.> top line).
  </explain>

  <\explain>
    <var-val|cell-row-span|1>

    <var-val|cell-col-span|1><explain-synopsis|span of a cell>
  <|explain>
    Certain cells in a table are allowed to span over other cells at their
    right or below them. The <src-var|cell-row-span> and
    <src-var|cell-col-span> specify the row span and column span of the cell.
  </explain>

  <\explain>
    <var-val|cell-decoration|><explain-synopsis|decorating table for cell>
  <|explain>
    This environment variable may contain a decorating table for the cell.
    Such a decoration enlarges the table with extra columns and cells. The
    <markup|tmarker> primitive determines the location of the original
    decorated cell and its surroundings in the enlarged table are filled up
    with the decorations. Cell decorations are not really used at present and
    may disappear in future versions of <TeXmacs>.
  </explain>

  <\explain>
    <var-val|cell-orientation|portrait><explain-synopsis|orientation of cell>
  <|explain>
    Other orientations for cells than <verbatim|portrait> have not yet been
    implemented.
  </explain>

  <\explain>
    <var-val|cell-row-nr|1>

    <var-val|cell-col-nr|1><explain-synopsis|current cell position>
  <|explain>
    In the future, these environment variables should contain the current
    cell position during the typesetting process.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>