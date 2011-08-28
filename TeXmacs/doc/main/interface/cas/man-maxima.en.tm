<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Maxima>

  <name|Maxima> is not alone one of the oldest and best computer algebra
  systems around, it is also one of the only general purpose systems for
  which there is a free implementation. You can get it from\ 

  <\verbatim>
    \ \ \ \ http://www.ma.utexas.edu/users/wfs/maxima.html
  </verbatim>

  The supported version is <name|GCL>-based <name|Maxima> 5.6. For
  <name|CLisp>-based <name|Maxima> 5.6, edit your <verbatim|tm_maxima> and
  replace <verbatim|-load> by <verbatim|-i>. For <name|Maxima> 5.9-pre,
  replace <verbatim|-load> by <verbatim|-p>. Known problems:

  <\itemize>
    <item>If you press <shortcut|(kbd-return)> when a statement is not
    complete (typically, terminated by <verbatim|;> or <verbatim|$>), the
    interface will hang.

    <item>If you cause the Lisp break prompt to appear, the interface will
    hang.

    <item>The command <verbatim|info> is not supported (it is defined in the
    underlying Lisp, and difficult to support portably).

    <item>Some commands in the debugger work, but some (including
    <verbatim|:c>) don't work, nobody knows why.

    <item>The command <verbatim|load> sometimes behaves mysteriously.
  </itemize>

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
