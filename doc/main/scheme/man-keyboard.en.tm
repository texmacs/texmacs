<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creating your own keyboard shortcuts>

  Keymaps are specified using the command\ 

  <\verbatim>
    \ \ \ \ (kbd-map predicate . keymaps)
  </verbatim>

  The predicate specifies under which circumstances the keymaps are valid.
  Examples of predicates are <verbatim|always?>, <verbatim|in-math?> and
  <verbatim|in-french?>, but the user may define his own predicates. Each
  item in <verbatim|keymaps> is of one of the following forms:

  <\verbatim>
    \ \ \ \ (key-combination action_1 ... action_n)<format|next line>
    \ \ \ (key-combination result)<format|next line> \ \ \ (key-combination
    result help-message)
  </verbatim>

  In the first case, the <verbatim|action_i> are <apply|scheme> commands
  associated to the string <verbatim|key-combination>. In the second and
  third case, <verbatim|result> is a string which is to be inserted in the
  text when the <verbatim|key-combination> has been completed. An optional
  <verbatim|help-message> may be displayed when the
  <verbatim|key-combination> is finished.

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
