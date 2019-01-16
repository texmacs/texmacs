<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Specifying the cell and table size>

  Using <menu|Table|Cell width|Set width> <abbr|resp.> <menu|Table|Cell
  height|Set height> you may specify the width or height of a cell. In fact,
  the specified width (or height) may be taken into account in three
  different ways:

  <\description>
    <item*|Minimum mode>The actual width of the cell will be the minimum of
    the specified width and the width of the box inside the cell.

    <item*|Exact mode>The width of the cell will be precisely the specified
    one.

    <item*|Maximum mode>The actual width of the cell will be the maximum of
    the specified width and the width of the box inside the cell.
  </description>

  The border width and the cell padding (to be explained below) are taken
  into account in the size of the box inside the cell.

  You may also specify the width and the height of the entire table in
  <menu|Table|Special table properties>. In particular, you may specify the
  table to run over the entire width of a paragraph. When specifying a width
  (or height) for the entire table, you may specify how the unused space is
  distributed over the cells using <menu|Table|Special cell
  properties|Distribute unused space>. By default, the unused space is
  equally distributed.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>