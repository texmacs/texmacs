<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Borders, padding and background color>

  You may specify the border widths and padding spaces of a cell in all
  possible four directions: on the left, on the right, at the bottom and at
  the top (see <submenu|Table|cell border>). You have keyboard shorthands of
  the forms <key|table b><render-key|<with|mode|math|x>> and <key|table p
 ><render-key|<with|mode|math|x>> in order to specify border widths and cell padding.

  The default border width for cells in the block environment is
  <verbatim|1ln>, i.e. the standard line width in the current font (like the
  width of a fraction bar). This width occurs at the right and the bottom of
  each cell (except when the cell is on the first row or column). The default
  horizontal cell padding is <verbatim|1spc>: the width of a white space in
  the current font. The default vertical cell padding is <verbatim|1sep>: the
  standard minimal separation between two close boxes.

  Cells may be given a background color via <submenu|Table|cell background
  color>.

  The entire table may also be given a border and a table padding in
  <subsubmenu|Table|special table properties|border>. In this case, the
  padding occurs outside the border.

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
    <associate|toc-1|<tuple|<uninit>|?>>
  </collection>
</references>
