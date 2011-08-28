<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Structured editing>

  As a general rule, the behaviour of most structured editing operations is
  conditioned by the <em|<rigid|current focus>>. By default, the innermost
  tag which contains the cursor. Whenever some selection is active, the
  current focus is the innermost tag which contains the selection. During
  structured operations, such as navigating among similar tags, the current
  focus may temporarily be set to something else. The current focus is
  visually indicated by the innermost cyan box around the cursor.

  For instance, the <em|structured insertion> commands
  <shortcut|(structured-insert-left)>, <shortcut|(structured-insert-right)>,
  <shortcut|(structured-insert-up)> and <shortcut|(structured-insert-down)>
  have a particular meaning both inside tables and trees. Inside tables, they
  allow you to insert new rows and columns (see
  figure<nbsp><reference|matrix-insert-fig>). Inside trees, they correspond
  to the insertion of new nodes (see figure<nbsp><reference|tree-insert-fig>).
  Whenever you inside a tree inside a table, then the innermost tag is a
  tree, and node insertions will take precedence over the insertion of new
  rows and columns.

  In many cases, a ``default particular behaviour'' has been defined for all
  tags minus some exceptional ones. In our example of structured insertion,
  the default behaviour of <shortcut|(structured-insert-left)> and
  <shortcut|(structured-insert-right)> is to insert a new argument to the tag
  at the left or at the right (when allowed).

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|>|<cell|<value|cursor>>|<cell|>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|>|<cell|<value|cursor>>|<cell|>>|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>>
    </equation*>
  <|big-figure>
    <label|matrix-insert-fig>Assume that the cursor is at the position of
    <value|cursor> inside the left-most matrix. Then the four other matrices
    respectively correspond to the insertion of a new column at the
    left<nbsp>(<shortcut|(structured-insert-left)>) or
    right<nbsp>(<shortcut|(structured-insert-right)>), <abbr|resp.> a new row
    above<nbsp>(<shortcut|(structured-insert-up)>) or
    below<nbsp>(<shortcut|(structured-insert-down)>).
  </big-figure>

  <\big-figure|<tree|a|b|c<value|cursor>|d><space|3em><space|3em><tree|a|b|<value|cursor>|c|d><space|3em><tree|a|b|c|<value|cursor>|d><space|3em><tree|a|b|<tree|<value|cursor>|c>|d><space|3em><tree|a|b|<tree|c|<value|cursor>>|d>>
    <label|tree-insert-fig>Assume that the cursor is at the position of
    <value|cursor> inside the left-most tree. Then the four other trees
    respectively correspond to the insertion of a new node at the
    left<nbsp>(<shortcut|(structured-insert-left)>), at the
    right<nbsp>(<shortcut|(structured-insert-right)>),
    above<nbsp>(<shortcut|(structured-insert-up)>) or
    below<nbsp>(<shortcut|(structured-insert-down)>).
  </big-figure>

  Similarly, in the case of matrices, the keys
  <shortcut|(structured-insert-start)>, <shortcut|(structured-insert-end)>,
  <shortcut|(structured-insert-top)> and <shortcut|(structured-insert-bottom)>
  can be used for inserting a new first or last column, <abbr|resp.> a new
  first or last row. The keys <shortcut|(structured-remove-left)> and
  <shortcut|(structured-remove-right)> are mapped to the commands for
  backward <abbr|resp.> forward <em|structured deletion>. In the case of
  matrices, this will result in the removal of the column before or after the
  cursor (see figure<nbsp><reference|matrix-remove-fig>). In order to remove
  the enclosing environment you may use <shortcut|(remove-structure-upwards)>
  or <shortcut|(remove-structure-upwards)>.

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|b<value|cursor>>|<cell|c>>|<row|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<value|cursor>c>>|<row|<cell|d>|<cell|f>>>>><space|2em>b<value|cursor>
    </equation*>
  <|big-figure>
    <label|matrix-remove-fig>Assume that the cursor is at the position of
    <value|cursor> inside the left-most matrix. Then pressing the
    keys<nbsp><shortcut|(structured-remove-left)> and
    <shortcut|(structured-remove-right)> respectively result in the next two
    matrices. Pressing either<nbsp><shortcut|(remove-structure-upwards)> or
    <shortcut|(remove-structure-upwards)> replaces the matrix by the content
    of the cell in which you are, leaving you with the <math|b> at the
    right-hand side.
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