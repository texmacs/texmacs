<TeXmacs|1.99.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Undo and redo>

  It is possible to gradually undo the changes you made in a document from
  the moment that you launched <TeXmacs>. This can be done via
  <menu|Edit|Undo> or using the keystroke <shortcut|(undo 0)>. Undone changes
  can be \Predone\Q using <menu|Edit|Redo> or <shortcut|(redo 0)>. <TeXmacs>
  maintains the entire \Phistory tree\Q of all your edits. Sometimes, after
  multiple undo and redo actions, this means that there can be several ways
  to redo some modification; in that case, <menu|Edit|Redo> becomes a menu in
  which you can select the appropriate branch.

  We notice that only changes in the document itself are taken into account
  by the undo system. In particular, modifications of most of the global
  document properties can not be undone. This includes modifications of the
  document style, the page size, the main font, etc. The same remark applies
  to any modifications outside <TeXmacs> that were triggered by your actions.
  For instance, in a computer algebra session, you can undo your edits inside
  <TeXmacs>, but not the computations in the external computer algebra
  system.

  <tmdoc-copyright|1998--2017|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>