<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Advanced table features>

  In the menus, you also find some other more special features for tables.
  Very briefly, these include the following:

  <\itemize>
    <item>Change the ``span'' of a cell and let it run over its neighbouring
    cells on its right and below.

    <item>Creation of entire subtables inside cells.

    <item>Correction of the depth and height of text, in order to let the
    baselines match.

    <item>Horizontal hyphenation of cell contents and vertical hyphenation of
    the entire table.

    <item>Gluing several rows and/or columns together, so that the glued
    cells become ``part of the borders'' of the remaining cells.

    <item>Disactivation of the table, in order to see its ``source code''.

    <item>Setting the ``extension center'' of a table. From now on, the
    formatting properties of this cell will be used for new cells created
    around this center.

    <item>Specification of the minimal and maximum size of a table, which
    will be respected during further editing. (this is mainly useful when
    creating table macros).
  </itemize>

  Currently, all tables come inside an environment like <markup|tabular>,
  <markup|block>, <markup|matrix>, etc. When creating your own table macros,
  you may use <apply|menu|Table|Special table properties|Extract format> to
  extract the format from a given table.

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
