<TeXmacs|1.0.0.5>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Specifying the cell and table size>

  Using <subsubmenu|Table|cell width|set width> resp. <subsubmenu|Table|cell
  height|set height> you may specify the width or height of a cell. In fact,
  the specified width (or height) may be taken into account in three
  different ways:

  <\description>
    <expand|item*|Minimum mode.>The actual width of the cell will be the
    minimum of the specified width and the width of the box inside the cell.

    <expand|item*|Exact mode.>The width of the cell will be precisely the
    specified one.

    <expand|item*|Maximum mode.>The actual width of the cell will be the
    maximum of the specified width and the width of the box inside the cell.
  </description>

  The border width and the cell padding (to be explained below) are taken
  into account in the size of the box inside the cell.

  You may also specify the width and the height of the entire table in
  <submenu|Table|special table properties>. In particular, you may specify
  the table to run over the entire width of a paragraph. When specifying a
  width (or height) for the entire table, you may specify how the unused
  space is distributed over the cells using <subsubmenu|Table|special cell
  properties|distribute unused space>. By default, the unused space is
  equally distributed.

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
