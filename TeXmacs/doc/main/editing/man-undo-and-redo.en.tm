<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Undo and redo>

  It is possible to gradually undo the changes you made in a document from
  the moment that you launched <apply|TeXmacs>. This can be done via
  <submenu|Edit|undo> or using the keystrokes <shortcut|(undo 0)> or
  <shortcut|(undo 0)>. Undone changes can be ``redone'' using <submenu|Edit|redo> or
  <shortcut|(redo 0)>.

  In order to save memory, the number of successive actions which can be
  undone is limited to 100 (by default). It is possible to increase this
  number by adding a command like\ 

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  in our personal initialization file (see <submenu|Help|scheme>). When
  specifying a negative number as your maximal undo depth, any number of
  actions can be undone.

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
