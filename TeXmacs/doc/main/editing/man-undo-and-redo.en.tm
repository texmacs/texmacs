<TeXmacs|1.99.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Undo and redo>

  It is possible to gradually undo the changes you made in a document from
  the moment that you launched <TeXmacs>. This can be done via
  <menu|Edit|Undo> or using the keystrokes <shortcut|(undo 0)> or
  <shortcut|(undo 0)>. Undone changes can be \Predone\Q using
  <menu|Edit|Redo> or <shortcut|(redo 0)>.

  In order to save memory, the number of successive actions that can be
  undone is limited to 100 (by default). It is possible to increase this
  number by adding a command like\ 

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  in our personal initialization file (see <menu|Help|Scheme>). When
  specifying a negative number as your maximal undo depth, any number of
  actions can be undone.

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