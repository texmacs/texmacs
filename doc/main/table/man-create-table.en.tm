<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creating tables>

  In order to create a table, you may either use <apply|menu|Insert|Table> or
  one of the following keyboard shorthands:

  <\description>
    <expand|item*|<expand|kbd-table|N t>>Create a regular table.

    <expand|item*|<expand|kbd-table|N T>>Create a regular table whose cells
    are centered.

    <expand|item*|<expand|kbd-table|N b>>Create a regular ``block'', whose
    cells are separated by lines.

    <expand|item*|<expand|kbd-table|N B>>Create a block whose cells are
    centered.
  </description>

  In math mode, a few other table-like structures are provided:

  <\description>
    <expand|item*|<expand|kbd-table|N m>>Create a matrix.

    <expand|item*|<expand|kbd-table|N d>>Create a determinant.

    <expand|item*|<expand|kbd-table|N c>>Create a choice list.
  </description>

  The <verbatim|\\eqnarray*> environment is also a special kind of table-like
  structure, which extends over the entire line. You may start a list of
  equations using <apply|menu|Insert|Mathematics|Equations>.

  When starting a new table, its size is minimal (usually
  <with|mode|math|1\<times\>1>) and its cells are empty. New rows and columns
  are inserted using the <key|A-<key-left>>,
  <key|A-<key-right>>, <key|A-<key-up>> and
  <key|A-<key-down>> shorthands. For instance,
  <key|A-<key-right>> creates a new column at the right of the
  current cursor position. You may also start a new row below the current
  cursor position by hitting <key|<key-return>>.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Table>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Mathematics>|<with|font
      family|<quote|ss>|Equations>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
