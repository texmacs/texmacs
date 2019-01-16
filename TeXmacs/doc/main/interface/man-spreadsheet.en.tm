<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Spreadsheets>

  <TeXmacs> provides rudimentary spreadsheet-like facilities with the
  advantage that the computations can be carried out using any of the
  plug-ins that can be used as a scripting language. In order to use the
  spreadsheet facilities, you should therefore start with the selection of a
  scripting language in the menu <menu|Document|Scripts>.

  As soon as you have selecting a scripting language, such as <name|Maxima>,
  then you may enter a new spreadsheet using <menu|Insert|Table|Textual
  spreadsheet> or <menu|Insert|Table|Numeric spreadsheet>. You may edit the
  spreadsheet as an ordinary table, except that the <key|return> key will
  attempt to reevaluate the cells of the table.

  In addition, when preceding the contents of a cell by =, then cell will be
  considered as an input-output switch. More precisely, the input is a
  formula which will be evaluated using the current scripting language. After
  the evaluation, only the result of the evaluation is shown in the cell.
  After pressing <key|return> a second time in the cell, it will be possible
  switch back and edit the input. In the formulas, one may refer to the
  others using names such as <samp|c5> for the third row and the fifth
  column.

  <\example>
    On the left-hand side of the figure below, we have displayed a simple
    table with formulas for evaluating the sums of the first two items of
    each row. On the right-hand side, we have shown the result after
    evaluation.

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|simple1|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|1>>|<cell|<cell-inert|b1|10>>|<cell|<cell-inert|c1|=a1+b1>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|1000>>|<cell|<cell-inert|c2|=a2+b2>>>>>>><space|2em><calc-table|simple2|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|1>>|<cell|<cell-inert|b1|10>>|<cell|<cell-output|c1|=a1+b1|<math|11>>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|1000>>|<cell|<cell-output|c2|=a2+b2|<math|1100>>>>>>>>>>>
      Evaluation of a simple spreadsheet.
    </big-figure>
  </example>

  <\example>
    The cells may contain mathematical formulas and the spreadsheet may take
    advantage of any of the capacities of the scripting language. For
    instance, the figure below demonstrates another possible use of
    <name|Maxima>.

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|derivatives1|<textual-table|<tformat|<cwith|1|-1|1|1|cell-width|15em>|<cwith|1|-1|1|1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|<math|sin<around*|(|x<rsup|2>|)>>>>>|<row|<cell|<cell-inert|a2|=diff(a1,x)>>>|<row|<cell|<cell-inert|a3|=diff(a2,x)>>>|<row|<cell|<cell-inert|a4|=diff(a3,x)>>>>>>><space|2em><calc-table|derivatives2|<textual-table|<tformat|<cwith|1|-1|1|1|cell-width|15em>|<cwith|1|-1|1|1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|<math|sin<around*|(|x<rsup|2>|)>>>>>|<row|<cell|<cell-output|a2|=diff(a1,x)|<math|2*x*cos
    <around*|(|x<rsup|2>|)>>>>>|<row|<cell|<cell-output|a3|=diff(a2,x)|<math|2*cos
    <around*|(|x<rsup|2>|)>-4*x<rsup|2>*sin
    <around*|(|x<rsup|2>|)>>>>>|<row|<cell|<cell-output|a4|=diff(a3,x)|<math|-12*x*sin
    <around*|(|x<rsup|2>|)>-8*x<rsup|3>*cos
    <around*|(|x<rsup|2>|)>>>>>>>>>>>>
      Computation of successive derivatives using <name|Maxima>.
    </big-figure>
  </example>

  <TeXmacs> supports a few special notations for applying operations on all
  cells in a subtable. For instance, as in <name|Excel>, one may use the
  notation <samp|c3:d5> for indicating all cells <samp|c3>, <samp|c4>,
  <samp|c5>, <samp|d3>, <samp|d4>, <samp|d5> in the block from <samp|c3> to
  <samp|d5>. An alternative notation <cell-commas> for <samp|:> can be
  entered by typing <key|, ,>. In a similar way, one may enter the special
  notation <cell-plusses> by typing <key|+ +>. For instance,
  <samp|c3<cell-plusses>d5> stands for the sum of all cells between <samp|c3>
  and <samp|d5>.

  <\example>
    The figure below shows an example on how to use taking sums of cells.
    Notice that empty cells count for zero.

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|sum1|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|15.10>>|<cell|<cell-inert|b1|15.10>>|<cell|<cell-inert|c1|30.20>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|125>>|<cell|<cell-inert|c2|75>>>|<row|<cell|<cell-inert|a3|28.50>>|<cell|<cell-inert|b3|>>|<cell|<cell-inert|c3|14.25>>>|<row|<cell|<cell-inert|a4|12>>|<cell|<cell-inert|b4|16>>|<cell|<cell-inert|c4|20>>>|<row|<cell|<cell-inert|a5|=a1<cell-plusses>a4>>|<cell|<cell-inert|b5|=b1<cell-plusses>b4>>|<cell|<cell-inert|c5|=c1<cell-plusses>c4>>>>>>><space|2em><calc-table|sum2|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<cwith|5|5|1|3|cell-width|5em>|<cwith|5|5|1|3|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|15.10>>|<cell|<cell-inert|b1|15.10>>|<cell|<cell-inert|c1|30.20>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|125>>|<cell|<cell-inert|c2|75>>>|<row|<cell|<cell-inert|a3|28.50>>|<cell|<cell-inert|b3|>>|<cell|<cell-inert|c3|14.25>>>|<row|<cell|<cell-inert|a4|12>>|<cell|<cell-inert|b4|16>>|<cell|<cell-inert|c4|20>>>|<row|<cell|<cell-output|a5|=a1<cell-plusses>a4|<math|155.6>>>|<cell|<cell-output|b5|=b1<cell-plusses>b4|<math|156.1>>>|<cell|<cell-output|c5|=c1<cell-plusses>c4|<math|139.45>>>>>>>>>>>
      Evaluation of a simple spreadsheet.
    </big-figure>
  </example>

  Notice that copying and pasting of subtables works in the same way as for
  ordinary tables, with the additional features that the names of the cells
  and references to cells in the formulas are renumbered automatically.
  Similarly, automatic renumbering is used when inserting new columns or
  rows, or when removing existing columns or rows.

  We also notice that field references can be used inside spreadsheet cells
  in order to refer to some computational markup outside the table.
  Inversely, each spreadsheet also carries an invisible <samp|Ref> field
  which can be edited by deactivating the spreadsheet or from the focus bar
  (when selecting the entire spreadsheet). The <samp|Ref> field of the
  spreadsheet is used as a prefix for referring to the contents of cells
  outside the table or from within other spreadsheets. For instance, if
  <samp|Ref> equals <samp|sheet>, then <samp|sheet-c4> will refer to the
  field <samp|c4> inside the spreadsheet.

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>