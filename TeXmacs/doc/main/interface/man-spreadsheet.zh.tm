<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|电子表格>

  <TeXmacs> provides rudimentary spreadsheet-like facilities with the
  advantage that the computations can be carried out using any of the
  plug-ins that can be used as a scripting language. In order to use the
  spreadsheet facilities, you should therefore start with the selection of a
  scripting language in the menu <menu|Document|Scripts>.

  <TeXmacs> 提供了基本的、类似电子表格的工具，它的优点是可以使用任何可用作脚本语言的插件来执行计算。因此，为了使用电子表格工具，你应该先在菜单<menu|Document|Scripts>中选择脚本语言。

  As soon as you have selecting a scripting language, such as <name|Maxima>,
  then you may enter a new spreadsheet using <menu|Insert|Table|Textual
  spreadsheet> or <menu|Insert|Table|Numeric spreadsheet>. You may edit the
  spreadsheet as an ordinary table, except that the <key|return> key will
  attempt to reevaluate the cells of the table.

  一旦你选择了一种脚本语言，例如
  <name|Maxima>，你就可以使用<menu|Insert|Table|Textual
  spreadsheet>或<menu|Insert|Table|Numeric
  spreadsheet>来创建新的电子表格。你可以把电子表格当成普通表格来编辑，但按下
  <key|return> 键就会尝试重新对表格中光标所在的单元格进行求值。

  In addition, when preceding the contents of a cell by =, then cell will be
  considered as an input-output switch. More precisely, the input is a
  formula which will be evaluated using the current scripting language. After
  the evaluation, only the result of the evaluation is shown in the cell.
  After pressing <key|return> a second time in the cell, it will be possible
  switch back and edit the input. In the formulas, one may refer to the
  others using names such as <samp|c5> for the third row and the fifth
  column.

  此外，当单元格内容前面加上 =
  时，单元格将被视为输入输出开关。更准确地说，输入会被视为一个计算式，将使用当前的脚本语言进行求值。求值后，单元格中仅显示计算结果。在单元格中再次按
  <key|return> 后，可以切换回未求值的输入，并允许进一步编辑。在计算式中，我们可以引用其他单元格，例如，c5
  代表引用第三行、第五列的单元格。

  <\example>
    On the left-hand side of the figure below, we have displayed a simple
    table with formulas for evaluating the sums of the first two items of
    each row. On the right-hand side, we have shown the result after
    evaluation.

    如下图所示，在左侧，我们展示了一个简单的表格，其中包含用于计算每行前两项之和的计算式。在右侧，我们展示了计算后的结果。

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|simple1|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|1>>|<cell|<cell-inert|b1|10>>|<cell|<cell-inert|c1|=a1+b1>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|1000>>|<cell|<cell-inert|c2|=a2+b2>>>>>>><space|2em><calc-table|simple2|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|1>>|<cell|<cell-inert|b1|10>>|<cell|<cell-output|c1|=a1+b1|<math|11>>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|1000>>|<cell|<cell-output|c2|=a2+b2|<math|1100>>>>>>>>>>>
      Evaluation of a simple spreadsheet.
    </big-figure>
  </example>

  <\example>
    The cells may contain mathematical formulas and the spreadsheet may take
    advantage of any of the capacities of the scripting language. For
    instance, the figure below demonstrates another possible use of
    <name|Maxima>.

    单元格可以包含数学公式，并且电子表格可以使用脚本语言的全部功能。例如，下图展示了
    <name|Maxima> 另一种可能的用法。

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

  <TeXmacs> 支持一些特殊的符号来对子表中的所有单元格应用操作。例如，在
  <name|Excel> 中，可以使用符号 c3:d5 来表示从 c3 到 d5
  的所有单元格 c3、c4、c5、d3、d4、d5。在 <TeXmacs>
  中，可以通过键入 <key|, ,> 来输入 : 的替代符号
  <cell-commas> 。与此类似，可以通过键入 <key|+ +>
  \ 来输入特殊符号<cell-plusses>。举个例子，
  <samp|c3<cell-plusses>d5>代表对 <samp|c3> 和 <samp|d5>
  之间所有单元格进行求和。

  <\example>
    The figure below shows an example on how to use taking sums of cells.
    Notice that empty cells count for zero.

    下图展示了一个如何使用单元格求和的例子。请注意，空单元格会被当成
    0 计数。

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|sum1|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|15.10>>|<cell|<cell-inert|b1|15.10>>|<cell|<cell-inert|c1|30.20>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|125>>|<cell|<cell-inert|c2|75>>>|<row|<cell|<cell-inert|a3|28.50>>|<cell|<cell-inert|b3|>>|<cell|<cell-inert|c3|14.25>>>|<row|<cell|<cell-inert|a4|12>>|<cell|<cell-inert|b4|16>>|<cell|<cell-inert|c4|20>>>|<row|<cell|<cell-inert|a5|=a1<cell-plusses>a4>>|<cell|<cell-inert|b5|=b1<cell-plusses>b4>>|<cell|<cell-inert|c5|=c1<cell-plusses>c4>>>>>>><space|2em><calc-table|sum2|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<cwith|5|5|1|3|cell-width|5em>|<cwith|5|5|1|3|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|15.10>>|<cell|<cell-inert|b1|15.10>>|<cell|<cell-inert|c1|30.20>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|125>>|<cell|<cell-inert|c2|75>>>|<row|<cell|<cell-inert|a3|28.50>>|<cell|<cell-inert|b3|>>|<cell|<cell-inert|c3|14.25>>>|<row|<cell|<cell-inert|a4|12>>|<cell|<cell-inert|b4|16>>|<cell|<cell-inert|c4|20>>>|<row|<cell|<cell-output|a5|=a1<cell-plusses>a4|<math|155.6>>>|<cell|<cell-output|b5|=b1<cell-plusses>b4|<math|156.1>>>|<cell|<cell-output|c5|=c1<cell-plusses>c4|<math|139.45>>>>>>>>>>>
      Evaluation of a simple spreadsheet.
    </big-figure>
  </example>

  Notice that copying and pasting of subtables works in the same way as for
  ordinary tables, with the additional features that the names of the cells
  and references to cells in the formulas are renumbered automatically.
  Similarly, automatic renumbering is used when inserting new columns or
  rows, or when removing existing columns or rows.

  请注意，在电子表格中，复制和粘贴子表的方式与普通表相同，此外，公式中的单元格名称和对单元格的引用会被自动重新编号。与此类似，在插入新列或行或删除现有列或行时，也会自动进行重新编号。

  We also notice that field references can be used inside spreadsheet cells
  in order to refer to some computational markup outside the table.
  Inversely, each spreadsheet also carries an invisible <samp|Ref> field
  which can be edited by deactivating the spreadsheet or from the focus bar
  (when selecting the entire spreadsheet). The <samp|Ref> field of the
  spreadsheet is used as a prefix for referring to the contents of cells
  outside the table or from within other spreadsheets. For instance, if
  <samp|Ref> equals <samp|sheet>, then <samp|sheet-c4> will refer to the
  field <samp|c4> inside the spreadsheet.

  （这一段的\ 

  The <samp|Ref> field of the spreadsheet is used as a prefix for referring
  to the contents of cells outside the table or from within other
  spreadsheets.

  我反复读了几遍没能理解意思，除此之外，我也没能找到
  deactivating 对应的是什么操作。）

  我们还注意到，可以在电子表格单元格内使用字段引用，以便引用表外的一些计算标记。另一方面，每个电子表格也带有一个不可见的
  <samp|Ref> 字段，可以通过禁用电子表格或从焦点栏（选择整个电子表格时）编辑该字段。电子表格的
  <samp|Ref> 字段用作引用表格外或其他电子表格内的单元格内容的前缀。例如，如果一个电子表格的
  <samp|Ref> 为 <samp|sheet>，则 <samp|sheet-c4>
  将引用这个电子表格中的单元格 <samp|c4>。

  <tmdoc-copyright|1998\U2022|Joris van der Hoeven|詹旭弘>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>