<TeXmacs|2.1.1>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|\<#7535\>\<#5B50\>\<#8868\>\<#683C\>>

  <TeXmacs> provides rudimentary spreadsheet-like facilities with the
  advantage that the computations can be carried out using any of the
  plug-ins that can be used as a scripting language. In order to use the
  spreadsheet facilities, you should therefore start with the selection of a
  scripting language in the menu <menu|Document|Scripts>.

  <TeXmacs> \<#63D0\>\<#4F9B\>\<#4E86\>\<#57FA\>\<#672C\>\<#7684\>\<#3001\>\<#7C7B\>\<#4F3C\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#7684\>\<#5DE5\>\<#5177\>\<#FF0C\>\<#5B83\>\<#7684\>\<#4F18\>\<#70B9\>\<#662F\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>\<#4EFB\>\<#4F55\>\<#53EF\>\<#7528\>\<#4F5C\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#7684\>\<#63D2\>\<#4EF6\>\<#6765\>\<#6267\>\<#884C\>\<#8BA1\>\<#7B97\>\<#3002\>\<#56E0\>\<#6B64\>\<#FF0C\>\<#4E3A\>\<#4E86\>\<#4F7F\>\<#7528\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#5DE5\>\<#5177\>\<#FF0C\>\<#4F60\>\<#5E94\>\<#8BE5\>\<#5148\>\<#5728\>\<#83DC\>\<#5355\><menu|Document|Scripts>\<#4E2D\>\<#9009\>\<#62E9\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#3002\>

  As soon as you have selecting a scripting language, such as <name|Maxima>,
  then you may enter a new spreadsheet using <menu|Insert|Table|Textual
  spreadsheet> or <menu|Insert|Table|Numeric spreadsheet>. You may edit the
  spreadsheet as an ordinary table, except that the <key|return> key will
  attempt to reevaluate the cells of the table.

  \<#4E00\>\<#65E6\>\<#4F60\>\<#9009\>\<#62E9\>\<#4E86\>\<#4E00\>\<#79CD\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#FF0C\>\<#4F8B\>\<#5982\>
  <name|Maxima>\<#FF0C\>\<#4F60\>\<#5C31\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\><menu|Insert|Table|Textual
  spreadsheet>\<#6216\><menu|Insert|Table|Numeric
  spreadsheet>\<#6765\>\<#521B\>\<#5EFA\>\<#65B0\>\<#7684\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#3002\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#628A\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#5F53\>\<#6210\>\<#666E\>\<#901A\>\<#8868\>\<#683C\>\<#6765\>\<#7F16\>\<#8F91\>\<#FF0C\>\<#4F46\>\<#6309\>\<#4E0B\>
  <key|return> \<#952E\>\<#5C31\>\<#4F1A\>\<#5C1D\>\<#8BD5\>\<#91CD\>\<#65B0\>\<#5BF9\>\<#8868\>\<#683C\>\<#4E2D\>\<#5149\>\<#6807\>\<#6240\>\<#5728\>\<#7684\>\<#5355\>\<#5143\>\<#683C\>\<#8FDB\>\<#884C\>\<#6C42\>\<#503C\>\<#3002\>

  In addition, when preceding the contents of a cell by =, then cell will be
  considered as an input-output switch. More precisely, the input is a
  formula which will be evaluated using the current scripting language. After
  the evaluation, only the result of the evaluation is shown in the cell.
  After pressing <key|return> a second time in the cell, it will be possible
  switch back and edit the input. In the formulas, one may refer to the
  others using names such as <samp|c5> for the third row and the fifth
  column.

  \<#6B64\>\<#5916\>\<#FF0C\>\<#5F53\>\<#5355\>\<#5143\>\<#683C\>\<#5185\>\<#5BB9\>\<#524D\>\<#9762\>\<#52A0\>\<#4E0A\>
  = \<#65F6\>\<#FF0C\>\<#5355\>\<#5143\>\<#683C\>\<#5C06\>\<#88AB\>\<#89C6\>\<#4E3A\>\<#8F93\>\<#5165\>\<#8F93\>\<#51FA\>\<#5F00\>\<#5173\>\<#3002\>\<#66F4\>\<#51C6\>\<#786E\>\<#5730\>\<#8BF4\>\<#FF0C\>\<#8F93\>\<#5165\>\<#4F1A\>\<#88AB\>\<#89C6\>\<#4E3A\>\<#4E00\>\<#4E2A\>\<#8BA1\>\<#7B97\>\<#5F0F\>\<#FF0C\>\<#5C06\>\<#4F7F\>\<#7528\>\<#5F53\>\<#524D\>\<#7684\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#8FDB\>\<#884C\>\<#6C42\>\<#503C\>\<#3002\>\<#6C42\>\<#503C\>\<#540E\>\<#FF0C\>\<#5355\>\<#5143\>\<#683C\>\<#4E2D\>\<#4EC5\>\<#663E\>\<#793A\>\<#8BA1\>\<#7B97\>\<#7ED3\>\<#679C\>\<#3002\>\<#5728\>\<#5355\>\<#5143\>\<#683C\>\<#4E2D\>\<#518D\>\<#6B21\>\<#6309\>
  <key|return> \<#540E\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#5207\>\<#6362\>\<#56DE\>\<#672A\>\<#6C42\>\<#503C\>\<#7684\>\<#8F93\>\<#5165\>\<#FF0C\>\<#5E76\>\<#5141\>\<#8BB8\>\<#8FDB\>\<#4E00\>\<#6B65\>\<#7F16\>\<#8F91\>\<#3002\>\<#5728\>\<#8BA1\>\<#7B97\>\<#5F0F\>\<#4E2D\>\<#FF0C\>\<#6211\>\<#4EEC\>\<#53EF\>\<#4EE5\>\<#5F15\>\<#7528\>\<#5176\>\<#4ED6\>\<#5355\>\<#5143\>\<#683C\>\<#FF0C\>\<#4F8B\>\<#5982\>\<#FF0C\>c5
  \<#4EE3\>\<#8868\>\<#5F15\>\<#7528\>\<#7B2C\>\<#4E09\>\<#884C\>\<#3001\>\<#7B2C\>\<#4E94\>\<#5217\>\<#7684\>\<#5355\>\<#5143\>\<#683C\>\<#3002\>

  <\example>
    On the left-hand side of the figure below, we have displayed a simple
    table with formulas for evaluating the sums of the first two items of
    each row. On the right-hand side, we have shown the result after
    evaluation.

    \<#5982\>\<#4E0B\>\<#56FE\>\<#6240\>\<#793A\>\<#FF0C\>\<#5728\>\<#5DE6\>\<#4FA7\>\<#FF0C\>\<#6211\>\<#4EEC\>\<#5C55\>\<#793A\>\<#4E86\>\<#4E00\>\<#4E2A\>\<#7B80\>\<#5355\>\<#7684\>\<#8868\>\<#683C\>\<#FF0C\>\<#5176\>\<#4E2D\>\<#5305\>\<#542B\>\<#7528\>\<#4E8E\>\<#8BA1\>\<#7B97\>\<#6BCF\>\<#884C\>\<#524D\>\<#4E24\>\<#9879\>\<#4E4B\>\<#548C\>\<#7684\>\<#8BA1\>\<#7B97\>\<#5F0F\>\<#3002\>\<#5728\>\<#53F3\>\<#4FA7\>\<#FF0C\>\<#6211\>\<#4EEC\>\<#5C55\>\<#793A\>\<#4E86\>\<#8BA1\>\<#7B97\>\<#540E\>\<#7684\>\<#7ED3\>\<#679C\>\<#3002\>

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|simple1|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|1>>|<cell|<cell-inert|b1|10>>|<cell|<cell-inert|c1|=a1+b1>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|1000>>|<cell|<cell-inert|c2|=a2+b2>>>>>>><space|2em><calc-table|simple2|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|1>>|<cell|<cell-inert|b1|10>>|<cell|<cell-output|c1|=a1+b1|<math|11>>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|1000>>|<cell|<cell-output|c2|=a2+b2|<math|1100>>>>>>>>>>>
      Evaluation of a simple spreadsheet.
    </big-figure>
  </example>

  <\example>
    The cells may contain mathematical formulas and the spreadsheet may take
    advantage of any of the capacities of the scripting language. For
    instance, the figure below demonstrates another possible use of
    <name|Maxima>.

    \<#5355\>\<#5143\>\<#683C\>\<#53EF\>\<#4EE5\>\<#5305\>\<#542B\>\<#6570\>\<#5B66\>\<#516C\>\<#5F0F\>\<#FF0C\>\<#5E76\>\<#4E14\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#7684\>\<#5168\>\<#90E8\>\<#529F\>\<#80FD\>\<#3002\>\<#4F8B\>\<#5982\>\<#FF0C\>\<#4E0B\>\<#56FE\>\<#5C55\>\<#793A\>\<#4E86\>
    <name|Maxima> \<#53E6\>\<#4E00\>\<#79CD\>\<#53EF\>\<#80FD\>\<#7684\>\<#7528\>\<#6CD5\>\<#3002\>

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

  <TeXmacs> \<#652F\>\<#6301\>\<#4E00\>\<#4E9B\>\<#7279\>\<#6B8A\>\<#7684\>\<#7B26\>\<#53F7\>\<#6765\>\<#5BF9\>\<#5B50\>\<#8868\>\<#4E2D\>\<#7684\>\<#6240\>\<#6709\>\<#5355\>\<#5143\>\<#683C\>\<#5E94\>\<#7528\>\<#64CD\>\<#4F5C\>\<#3002\>\<#4F8B\>\<#5982\>\<#FF0C\>\<#5728\>
  <name|Excel> \<#4E2D\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>\<#7B26\>\<#53F7\>
  c3:d5 \<#6765\>\<#8868\>\<#793A\>\<#4ECE\> c3 \<#5230\> d5
  \<#7684\>\<#6240\>\<#6709\>\<#5355\>\<#5143\>\<#683C\>
  c3\<#3001\>c4\<#3001\>c5\<#3001\>d3\<#3001\>d4\<#3001\>d5\<#3002\>\<#5728\>
  <TeXmacs> \<#4E2D\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#952E\>\<#5165\>
  <key|, ,> \<#6765\>\<#8F93\>\<#5165\> :
  \<#7684\>\<#66FF\>\<#4EE3\>\<#7B26\>\<#53F7\> <cell-commas>
  \<#3002\>\<#4E0E\>\<#6B64\>\<#7C7B\>\<#4F3C\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#952E\>\<#5165\>
  <key|+ +> \ \<#6765\>\<#8F93\>\<#5165\>\<#7279\>\<#6B8A\>\<#7B26\>\<#53F7\><cell-plusses>\<#3002\>\<#4E3E\>\<#4E2A\>\<#4F8B\>\<#5B50\>\<#FF0C\>
  <samp|c3<cell-plusses>d5>\<#4EE3\>\<#8868\>\<#5BF9\> <samp|c3> \<#548C\>
  <samp|d5> \<#4E4B\>\<#95F4\>\<#6240\>\<#6709\>\<#5355\>\<#5143\>\<#683C\>\<#8FDB\>\<#884C\>\<#6C42\>\<#548C\>\<#3002\>

  <\example>
    The figure below shows an example on how to use taking sums of cells.
    Notice that empty cells count for zero.

    \<#4E0B\>\<#56FE\>\<#5C55\>\<#793A\>\<#4E86\>\<#4E00\>\<#4E2A\>\<#5982\>\<#4F55\>\<#4F7F\>\<#7528\>\<#5355\>\<#5143\>\<#683C\>\<#6C42\>\<#548C\>\<#7684\>\<#4F8B\>\<#5B50\>\<#3002\>\<#8BF7\>\<#6CE8\>\<#610F\>\<#FF0C\>\<#7A7A\>\<#5355\>\<#5143\>\<#683C\>\<#4F1A\>\<#88AB\>\<#5F53\>\<#6210\>
    0 \<#8BA1\>\<#6570\>\<#3002\>

    <\big-figure|<small|<with|prog-scripts|maxima|<calc-table|sum1|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|15.10>>|<cell|<cell-inert|b1|15.10>>|<cell|<cell-inert|c1|30.20>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|125>>|<cell|<cell-inert|c2|75>>>|<row|<cell|<cell-inert|a3|28.50>>|<cell|<cell-inert|b3|>>|<cell|<cell-inert|c3|14.25>>>|<row|<cell|<cell-inert|a4|12>>|<cell|<cell-inert|b4|16>>|<cell|<cell-inert|c4|20>>>|<row|<cell|<cell-inert|a5|=a1<cell-plusses>a4>>|<cell|<cell-inert|b5|=b1<cell-plusses>b4>>|<cell|<cell-inert|c5|=c1<cell-plusses>c4>>>>>>><space|2em><calc-table|sum2|<numeric-dot-table|<tformat|<cwith|1|-1|1|-1|cell-width|5em>|<cwith|1|-1|1|-1|cell-hmode|max>|<cwith|5|5|1|3|cell-width|5em>|<cwith|5|5|1|3|cell-hmode|max>|<table|<row|<cell|<cell-inert|a1|15.10>>|<cell|<cell-inert|b1|15.10>>|<cell|<cell-inert|c1|30.20>>>|<row|<cell|<cell-inert|a2|100>>|<cell|<cell-inert|b2|125>>|<cell|<cell-inert|c2|75>>>|<row|<cell|<cell-inert|a3|28.50>>|<cell|<cell-inert|b3|>>|<cell|<cell-inert|c3|14.25>>>|<row|<cell|<cell-inert|a4|12>>|<cell|<cell-inert|b4|16>>|<cell|<cell-inert|c4|20>>>|<row|<cell|<cell-output|a5|=a1<cell-plusses>a4|<math|155.6>>>|<cell|<cell-output|b5|=b1<cell-plusses>b4|<math|156.1>>>|<cell|<cell-output|c5|=c1<cell-plusses>c4|<math|139.45>>>>>>>>>>>
      Evaluation of a simple spreadsheet.
    </big-figure>
  </example>

  Notice that copying and pasting of subtables works in the same way as for
  ordinary tables, with the additional features that the names of the cells
  and references to cells in the formulas are renumbered automatically.
  Similarly, automatic renumbering is used when inserting new columns or
  rows, or when removing existing columns or rows.

  \<#8BF7\>\<#6CE8\>\<#610F\>\<#FF0C\>\<#5728\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#4E2D\>\<#FF0C\>\<#590D\>\<#5236\>\<#548C\>\<#7C98\>\<#8D34\>\<#5B50\>\<#8868\>\<#7684\>\<#65B9\>\<#5F0F\>\<#4E0E\>\<#666E\>\<#901A\>\<#8868\>\<#76F8\>\<#540C\>\<#FF0C\>\<#6B64\>\<#5916\>\<#FF0C\>\<#516C\>\<#5F0F\>\<#4E2D\>\<#7684\>\<#5355\>\<#5143\>\<#683C\>\<#540D\>\<#79F0\>\<#548C\>\<#5BF9\>\<#5355\>\<#5143\>\<#683C\>\<#7684\>\<#5F15\>\<#7528\>\<#4F1A\>\<#88AB\>\<#81EA\>\<#52A8\>\<#91CD\>\<#65B0\>\<#7F16\>\<#53F7\>\<#3002\>\<#4E0E\>\<#6B64\>\<#7C7B\>\<#4F3C\>\<#FF0C\>\<#5728\>\<#63D2\>\<#5165\>\<#65B0\>\<#5217\>\<#6216\>\<#884C\>\<#6216\>\<#5220\>\<#9664\>\<#73B0\>\<#6709\>\<#5217\>\<#6216\>\<#884C\>\<#65F6\>\<#FF0C\>\<#4E5F\>\<#4F1A\>\<#81EA\>\<#52A8\>\<#8FDB\>\<#884C\>\<#91CD\>\<#65B0\>\<#7F16\>\<#53F7\>\<#3002\>

  We also notice that field references can be used inside spreadsheet cells
  in order to refer to some computational markup outside the table.
  Inversely, each spreadsheet also carries an invisible <samp|Ref> field
  which can be edited by deactivating the spreadsheet or from the focus bar
  (when selecting the entire spreadsheet). The <samp|Ref> field of the
  spreadsheet is used as a prefix for referring to the contents of cells
  outside the table or from within other spreadsheets. For instance, if
  <samp|Ref> equals <samp|sheet>, then <samp|sheet-c4> will refer to the
  field <samp|c4> inside the spreadsheet.

  \<#FF08\>\<#8FD9\>\<#4E00\>\<#6BB5\>\<#7684\>\ 

  The <samp|Ref> field of the spreadsheet is used as a prefix for referring
  to the contents of cells outside the table or from within other
  spreadsheets.

  \<#6211\>\<#53CD\>\<#590D\>\<#8BFB\>\<#4E86\>\<#51E0\>\<#904D\>\<#6CA1\>\<#80FD\>\<#7406\>\<#89E3\>\<#610F\>\<#601D\>\<#FF0C\>\<#9664\>\<#6B64\>\<#4E4B\>\<#5916\>\<#FF0C\>\<#6211\>\<#4E5F\>\<#6CA1\>\<#80FD\>\<#627E\>\<#5230\>
  deactivating \<#5BF9\>\<#5E94\>\<#7684\>\<#662F\>\<#4EC0\>\<#4E48\>\<#64CD\>\<#4F5C\>\<#3002\>\<#FF09\>

  \<#6211\>\<#4EEC\>\<#8FD8\>\<#6CE8\>\<#610F\>\<#5230\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#5728\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#5355\>\<#5143\>\<#683C\>\<#5185\>\<#4F7F\>\<#7528\>\<#5B57\>\<#6BB5\>\<#5F15\>\<#7528\>\<#FF0C\>\<#4EE5\>\<#4FBF\>\<#5F15\>\<#7528\>\<#8868\>\<#5916\>\<#7684\>\<#4E00\>\<#4E9B\>\<#8BA1\>\<#7B97\>\<#6807\>\<#8BB0\>\<#3002\>\<#53E6\>\<#4E00\>\<#65B9\>\<#9762\>\<#FF0C\>\<#6BCF\>\<#4E2A\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#4E5F\>\<#5E26\>\<#6709\>\<#4E00\>\<#4E2A\>\<#4E0D\>\<#53EF\>\<#89C1\>\<#7684\>
  <samp|Ref> \<#5B57\>\<#6BB5\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#7981\>\<#7528\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#6216\>\<#4ECE\>\<#7126\>\<#70B9\>\<#680F\>\<#FF08\>\<#9009\>\<#62E9\>\<#6574\>\<#4E2A\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#65F6\>\<#FF09\>\<#7F16\>\<#8F91\>\<#8BE5\>\<#5B57\>\<#6BB5\>\<#3002\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#7684\>
  <samp|Ref> \<#5B57\>\<#6BB5\>\<#7528\>\<#4F5C\>\<#5F15\>\<#7528\>\<#8868\>\<#683C\>\<#5916\>\<#6216\>\<#5176\>\<#4ED6\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#5185\>\<#7684\>\<#5355\>\<#5143\>\<#683C\>\<#5185\>\<#5BB9\>\<#7684\>\<#524D\>\<#7F00\>\<#3002\>\<#4F8B\>\<#5982\>\<#FF0C\>\<#5982\>\<#679C\>\<#4E00\>\<#4E2A\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#7684\>
  <samp|Ref> \<#4E3A\> <samp|sheet>\<#FF0C\>\<#5219\> <samp|sheet-c4>
  \<#5C06\>\<#5F15\>\<#7528\>\<#8FD9\>\<#4E2A\>\<#7535\>\<#5B50\>\<#8868\>\<#683C\>\<#4E2D\>\<#7684\>\<#5355\>\<#5143\>\<#683C\>
  <samp|c4>\<#3002\>

  \;

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