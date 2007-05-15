<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Structured editing>

  As a general rule, the behaviour of most structured editing operations is
  conditioned by the <em|innermost tag> for which a particular behaviour has
  been defined.

  For instance, the <em|structured insertion> commands <key|A-<key-left>>,
  <key|A-<key-right>>, <key|A-<key-up>> and <key|A-<key-down>> have a
  particular meaning both inside tables and trees. Inside tables, they allow
  you to insert new rows and columns (see
  figure<nbsp><reference|matrix-insert-fig>). Inside trees, they correspond
  to the insertion of new nodes (see figure<nbsp><reference|tree-insert-fig>).
  Whenever you inside a tree inside a table, then the innermost tag is a
  tree, and node insertions will take precedence over the insertion of new
  rows and columns.

  In many cases, a ``default particular behaviour'' has been defined for all
  tags minus some exceptional ones. In our example of structured insertion,
  the default behaviour of <key|A-<key-left>> and <key|A-<key-right>> is to
  insert a new argument to the tag at the left or at the right (when
  allowed).

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|>|<cell|<value|cursor>>|<cell|>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|>|<cell|<value|cursor>>|<cell|>>|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>>
    </equation*>
  <|big-figure>
    <label|matrix-insert-fig>Assume that the cursor is at the position of
    <value|cursor> inside the left-most matrix. Then the four other matrices
    respectively correspond to the insertion of a new column at the
    left<nbsp>(<key|A-<key-left>>) or right<nbsp>(<key|A-<key-right>>),
    <abbr|resp.> a new row above<nbsp>(<key|A-<key-up>>) or
    below<nbsp>(<key|A-<key-down>>).
  </big-figure>

  <\big-figure|<tree|a|b|c<value|cursor>|d><space|3em><space|3em><tree|a|b|<value|cursor>|c|d><space|3em><tree|a|b|c|<value|cursor>|d><space|3em><tree|a|b|<tree|<value|cursor>|c>|d><space|3em><tree|a|b|<tree|c|<value|cursor>>|d>>
    <label|tree-insert-fig>Assume that the cursor is at the position of
    <value|cursor> inside the left-most tree. Then the four other trees
    respectively correspond to the insertion of a new node at the
    left<nbsp>(<key|A-<key-left>>), at the right<nbsp>(<key|A-<key-right>>),
    above<nbsp>(<key|A-<key-up>>) or below<nbsp>(<key|A-<key-down>>).
  </big-figure>

  Similarly, in the case of matrices, the keys <key|A-<key-home>>,
  <key|A-<key-end>>, <key|A-<key-pageup>> and <key|A-<key-pagedown>> can be
  used for inserting a new first or last column, <abbr|resp.> a new first or
  last row. The keys <key|A-<key-backspace>> and <key|A-<key-delete>> are
  mapped to the commands for backward <abbr|resp.> forward <em|structured
  deletion>. In the case of matrices, this will result in the removal of the
  column before or after the cursor (see figure<nbsp><reference|matrix-remove-fig>).
  In order to remove the enclosing environment you may use
  <key|C-<key-backspace>> or <key|C-<key-delete>>.

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>c>>|<row|<cell|d>|<cell|f>>>>><space|2em>b<value|cursor>
    </equation*>
  <|big-figure>
    <label|matrix-remove-fig>Assume that the cursor is at the position of
    <value|cursor> inside the left-most matrix. Then pressing the
    keys<nbsp><key|A-<key-backspace>> and <key|A-<key-delete>> respectively
    result in the next two matrices. Pressing
    either<nbsp><key|C-<key-backspace>> or <key|C-<key-delete>> replaces the
    matrix by the content of the cell in which you are, leaving you with the
    <with|mode|math|b> at the right-hand side.
  </big-figure>

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

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