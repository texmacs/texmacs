<TeXmacs|1.0.6.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating tables>

  In order to create a table, you may either use <menu|Insert|Table> or one
  of the following keyboard shorthands:

  <\description>
    <item*|<key|table N t>>Create a plain table.

    <item*|<key|table N T>>Create a table whose cells are centered.

    <item*|<key|table N b>>Create a ``block'', whose cells all have a small
    border.

    <item*|<key|table N B>>Create a block whose cells are centered.
  </description>

  In math mode, a few other table-like structures are provided:

  <\description>
    <item*|<key|table N m>>Create a matrix.

    <item*|<key|table N d>>Create a determinant.

    <item*|<key|table N c>>Create a choice list.
  </description>

  Examples of a plain table, a centered block and a matrix are shown below.
  Notice that the environments with the explanatory text below the tables
  were created using <menu|Insert|Table|Small table>. The use of ``small
  tables'' allows you to put several tables besides each other on the same
  line. For a single large table, one may use <menu|Insert|Table|Big
  table>.<vspace|0.5fn>

  <\with|par-mode|center>
    <small-table|<tabular|<tformat|<table|<row|<cell|boom>|<cell|tree>>|<row|<cell|hallo>|<cell|hello>>|<row|<cell|wiskunde>|<cell|mathematics>>>>>|A
    plain table.><small-table|<block|<tformat|<table|<row|<cell|boom>|<cell|tree>>|<row|<cell|hallo>|<cell|hello>>|<row|<cell|wiskunde>|<cell|mathematics>>>>>|A
    centered block.><small-table|<with|mode|math|<with|math-display|true|<matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>>>|A
    matrix.>
  </with>

  <vspace*|0.5fn>There are several other table-like environments and new ones
  may be created by the user. For instance, using
  <menu|Insert|Mathematics|Equations> or <key|math &>, you may insert an
  <markup|eqnarray*> environment, which allows mathematically oriented users
  to align a list of equations which span over entire lines. An example of
  such a list of equations is

  <\eqnarray*>
    <tformat|<table|<row|<cell|sin (f(x)*g(x))<rprime|'>>|<cell|=>|<cell|(f(x)*g(x))<rprime|'>*cos
    (f(x)*g(x))>>|<row|<cell|>|<cell|=>|<cell|(f<rprime|'>(x)*g(x)+f(x)*g<rprime|'>(x))*cos
    (f(x)*g(x))>>>>
  </eqnarray*>

  When starting a new table, its size is minimal (usually
  <with|mode|math|1\<times\>1>) and its cells are empty. New rows and columns
  are inserted using the <shortcut|(structured-insert-left)>, <shortcut|(structured-insert-right)>,
  <shortcut|(structured-insert-up)> and <shortcut|(structured-insert-down)> shorthands. For instance,
  <shortcut|(structured-insert-right)> creates a new column at the right of the current cursor
  position, as illustrated in the figure below. You may also start a new row
  below the current cursor position by hitting <shortcut|(kbd-return)>.

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a<value|cursor>>|<cell|b>>|<row|<cell|c>|<cell|d>>>>><space|2em>\<longrightarrow\><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>>|<cell|b>>|<row|<cell|c>|<cell|>|<cell|d>>>>>
    </equation*>
  <|big-figure>
    Example of the insertion of a new column in a matrix. Assuming that the
    cursor is at the position indicated in the left-hand matrix, insertion of
    a new column using <shortcut|(structured-insert-right)> results in the right-hand matrix.
  </big-figure>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>