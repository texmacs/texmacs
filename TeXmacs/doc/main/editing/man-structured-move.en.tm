<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Structured cursor movement>

  <TeXmacs> implements the three main mechanisms for <em|structured cursor
  movement>:

  <\enumerate>
    <item>Traversal of the entire structure of the document.

    <item>Traversal of tags that are similar to the innermost tag.

    <item>Movements inside the innermost tag.
  </enumerate>

  Most keyboard shortcuts for structured cursor movements can be used in
  combination with the<nbsp><prefix|S-><nbhyph>key so as to simultaneously
  select text while moving around.

  <paragraph*|Structured traversal of the document>

  The <shortcut|(kbd-select-if-active traverse-left)>,
  <shortcut|(kbd-select-if-active traverse-right)>,
  <shortcut|(kbd-select-if-active traverse-up)> and
  <shortcut|(kbd-select-if-active traverse-down)> keys are used for the
  structured traversal of the entire document. Inside plain text,
  <shortcut|(kbd-select-if-active traverse-left)> and
  <shortcut|(kbd-select-if-active traverse-right)> allow you to move in a
  word-by-word manner, whereas <shortcut|(kbd-select-if-active traverse-up)>
  and <shortcut|(kbd-select-if-active traverse-down)> correspond to
  paragraph-by-paragraph motion.

  In the presence of other markup, the <shortcut|(kbd-select-if-active
  traverse-left)> and <shortcut|(kbd-select-if-active traverse-right)> keys
  allow you to visit all accessible cursor positions in the document, except
  that we keep moving in a word-by-word manner inside plain text. The
  behaviour of the <shortcut|(kbd-select-if-active traverse-up)> and
  <shortcut|(kbd-select-if-active traverse-down)> keys is more
  context-dependent. Inside matrices, they typically allow you to move one
  row up or down.

  <paragraph*|Traversal of tags that are similar to the innermost tag>

  This type of cursor movement allows you to quickly visit all other tags in
  the document that are <em|similar> to the innermost tag. The
  <shortcut|(kbd-select-if-active traverse-previous)> and
  <shortcut|(kbd-select-if-active traverse-next)> keys allow you move to the
  previous or next similar tags, whereas <shortcut|(kbd-select-if-active
  traverse-first)> and <shortcut|(kbd-select-if-active traverse-last)>
  directly jump to the first or last similar tags.

  For instance, if you are inside a section title, then you may move to the
  previous sectional title using <shortcut|(kbd-select-if-active
  traverse-previous)>. Subsection and chapter titles are in particular
  understood to be \Psimilar\Q to section titles. Notice that you may use
  <shortcut|(traverse-previous-section-title)> to jump to the previous
  section title.

  <paragraph*|Movements inside the innermost tag>

  It is also possible to quickly move inside the innermost tag without
  quitting it. The shortcuts <shortcut|(structured-left)>,
  <shortcut|(structured-right)>, <shortcut|(structured-start)> and
  <shortcut|(structured-end)> provide a way to move to the previous, next,
  first or last argument of the innermost tag. Furthermore, the shortcuts
  <shortcut|(structured-exit-left)> and <shortcut|(structured-exit-right)>
  may be used to exit the innermost tag on the left or on the right.

  This default behaviour may be overridden in special contexts. For instance,
  inside tables or trees, they rather correspond to cell-by-cell or
  node-by-node cursor movement. In addition, these cases associate vertical
  cursor movements to <shortcut|(structured-up)>,
  <shortcut|(structured-down)>, <shortcut|(structured-top)>
  and<nbsp><shortcut|(structured-bottom)>.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>