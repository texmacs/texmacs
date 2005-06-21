<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Fundamental tree modification routines>

  From an internal point of view, all modifications to the <TeXmacs> edit
  tree are decomposed into atomic modifications of eight different types. In
  this section, we describe the <value|scheme> interface to these fundamental
  modification routines. Even though it is usually more convenient to use
  higher level modification routines, as described in the <hlink|next
  section|edit-modify.en.tm>, the fundamental tree modification routines may
  occasionally be useful as well.

  It should be emphasized that the fundamental tree modification routines are
  <em|not> checked for their correctness. It is the responsability of the
  user to verify that the operations are valid and that they lead to a
  correct new edit tree. Although it is sometimes possible to leave the edit
  tree in a temporarily incorrect or ``unsimplified'' state (for instance, by
  allowing subtrees of the form <group|<scm|(concat "")>>), this practice is
  not generally recommended, and may lead to severe bugs.

  <\explain>
    <explain-scm-fun|tree-assign|<scm-arg|which>|<scm-arg|new-value>>

    <explain-scm-macro|tree-assign!|<scm-arg|which>|<scm-arg|new-value>><explain-synopsis|tree
    assignment>
  <|explain>
    This routine replaces the tree <scm-arg|which> by a new content value
    <scm-arg|new-value>.

    If <scm-arg|which> is also a <value|scheme> variable, then the macro
    <scm-macro|tree-assign!> should be used to set it to <scm-arg|new-value>.
  </explain>

  <\explain>
    <explain-scm-fun|tree-insert|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>>

    <explain-scm-macro|tree-insert!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>><explain-synopsis|insertion
    of new nodes or characters>
  <|explain>
    If <scm-arg|which> is a compound tree, then <scm-arg|ins> should be of
    compound content type, say with children
    <with|mode|math|u<rsub|0>,\<ldots\>,u<rsub|l-1>>. In that case, the
    routine inserts <with|mode|math|u<rsub|0>,\<ldots\>,u<rsub|l-1>> into the
    children of <scm-arg|which>, at position<nbsp><scm-arg|pos> (see figure
    <reference|insert-remove-fig>). If <scm-arg|which> is a string tree, then
    <scm-arg|ins> should be of string content type, and the string
    <scm-arg|ins> is inserted into <scm-arg|which> at
    position<nbsp><scm-arg|pos>.

    If <scm-arg|which> is also a <value|scheme> variable, then both the
    function <scm-fun|tree-insert> and the macro <scm-macro|tree-insert!>
    update the value of <scm-arg|which>.
  </explain>

  <\explain>
    <explain-scm-fun|tree-remove|<scm-arg|which>|<scm-arg|pos>|<scm-arg|nr>>

    <explain-scm-macro|tree-remove!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|nr>><explain-synopsis|removal
    of nodes or characters>
  <|explain>
    If <scm-arg|which> is a compound tree, then <scm-arg|nr> of its children
    are removed, starting at position <scm-arg|pos> (see figure
    <reference|insert-remove-fig>). If <scm-arg|which> is a string tree, then
    <scm-arg|nr> characters are removed, starting at
    position<nbsp><scm-arg|pos>.

    If <scm-arg|which> is also a <value|scheme> variable, then both the
    function <scm-fun|tree-remove> and the macro <scm-macro|tree-remove!>
    update the value of <scm-arg|which>.
  </explain>

  <\big-figure>
    <\equation*>
      <tabular*|<tformat|<table|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|insert(t,i,u)>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|u<rsub|0>|\<cdots\>|u<rsub|l-1>|t<rsub|i>|\<cdots\>|t<rsub|n-1>>>>>>>>>|<row|<cell|>>|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|remove(t,i,l)>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|t<rsub|i+l>|\<cdots\>|t<rsub|n-1>>>>>>>>>>>>
    </equation*>

    \;
  <|big-figure>
    <label|insert-remove-fig>Illustration of the operations
    <scm|(<scm-fun|tree-insert> t i u)> and <scm|(<scm-fun|tree-remove> t i
    l)>. If <scm|u> has arity<nbsp><scm|l>, then we notice that
    <scm|(<scm-fun|tree-remove> t i l)> undos the insertion
    <scm|(<scm-fun|tree-insert> t i u)>.
  </big-figure>

  \;

  <\explain>
    <explain-scm-fun|tree-split|<scm-arg|which>|<scm-arg|pos>|<scm-arg|at>>

    <explain-scm-macro|tree-split!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|at>><explain-synopsis|split
    the children into two parts>
  <|explain>
    Given a tree <scm-arg|which>, this routine is used to split its child
    <with|mode|math|u> at position <scm-arg|pos> into two parts. If
    <with|mode|math|u> is a compound tree, then the first part consists of
    the first <scm-arg|at> children and the second part of the remaining
    ones. Both parts carry the same label as <with|mode|math|u> and
    <with|mode|math|u> is replaced by the two parts inside <scm-arg|which>
    (see figure <reference|split-join-fig>). If <with|mode|math|u> is string
    tree, then it is rather split into two strings at position <scm-arg|at>.

    If <scm-arg|which> is also a <value|scheme> variable, then both the
    function <scm-fun|tree-split> and the macro <scm-macro|tree-split!>
    update the value of <scm-arg|which>.
  </explain>

  <\explain>
    <explain-scm-fun|tree-join|<scm-arg|which>|<scm-arg|pos>>

    <explain-scm-macro|tree-join!|<scm-arg|which>|<scm-arg|pos>><explain-synopsis|join
    two adjacent nodes>
  <|explain>
    Given a tree <scm-arg|which>, this routine is used to join its child
    <with|mode|math|u> at position <scm-arg|pos> with the child
    <with|mode|math|v> at position <scm-arg|pos>+1. If <with|mode|math|u> and
    <with|mode|math|v> are trees, then they are removed from <scm-arg|which>
    and replaced by a single tree which has the same label as
    <with|mode|math|u> and whose children are those of <with|mode|math|u>,
    followed by the children of <with|mode|math|v> (see figure
    <reference|split-join-fig>). If <with|mode|math|u> and <with|mode|math|v>
    are strings, then they are replaced by their concatenation.

    If <scm-arg|which> is also a <value|scheme> variable, then both the
    function <scm-fun|tree-join> and the macro <scm-macro|tree-join!> update
    the value of <scm-arg|which>.
  </explain>

  <\big-figure>
    <\equation*>
      <tabular*|<tformat|<table|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,k-1>>|l+1fn||r-1fn||>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|split(t,i,j)>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,j-1>>|l+1fn||||>|<resize|<tree|t<rsub|i>|t<rsub|i,j>|\<cdots\>|t<rsub|i,k-1>>|||r-1fn||>|\<cdots\>|t<rsub|n-1>>>>>>><space|1em><space|1em>>>|<row|<cell|>>|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,j-1>>|l+2fn||||>|<resize|<tree|t<rsub|i+1>|t<rsub|i+1,j>|\<cdots\>|t<rsub|i+1,k-1>>|||r-2fn||>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|join(t,i)>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,j-1>|t<rsub|i+1,0>|\<cdots\>|t<rsub|i+1,k-1>>|l+4fn||r-4fn||>|\<cdots\>|t<rsub|n-1>>>>>>>>>>>>
    </equation*>

    \;
  <|big-figure>
    <label|split-join-fig>Illustration of the operations
    <scm|(<scm-fun|tree-split> t i j)> and <scm|(<scm-fun|tree-join> t i)>.
    Notice that <scm|(<scm-fun|tree-join> t i)> undos
    <scm|(<scm-fun|tree-split> t i j)>.
  </big-figure>

  <\explain>
    <explain-scm-fun|tree-assign-node|<scm-arg|which>|<scm-arg|lab>>

    <explain-scm-macro|tree-assign-node!|<scm-arg|which>|<scm-arg|lab>><explain-synopsis|assign
    the label of a tree>
  <|explain>
    This routine replaces the label of a compound tree <scm-arg|which> by a
    new value <scm-arg|lab>.

    If <scm-arg|which> is also a <value|scheme> variable, then both the
    function <scm-fun|tree-assign-node> and the macro
    <scm-macro|tree-assign-node!> update the value of <scm-arg|which>.
  </explain>

  <\explain>
    <explain-scm-fun|tree-insert-node|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>>

    <explain-scm-macro|tree-insert-node!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>><explain-synopsis|insert
    the tree as a child of another one>
  <|explain>
    Given a tree <scm-arg|which> and a content tree <scm-arg|ins>, this
    routine replaces <scm-arg|which> by <scm-arg|ins>, with <scm-arg|which>
    inserted as a new child of <scm-arg|ins> at position <scm-arg|pos> (see
    figure <reference|insert-remove-node-fig>).

    If <scm-arg|which> is also a <value|scheme> variable, then the macro
    <scm-macro|tree-insert-node!> should be used to update the value of
    <scm-arg|which>.
  </explain>

  <\explain>
    <explain-scm-fun|tree-remove-node|<scm-arg|which>|<scm-arg|pos>>

    <explain-scm-macro|tree-remove-node!|<scm-arg|which>|<scm-arg|pos>><explain-synopsis|replace
    a tree by a child>
  <|explain>
    Given a compound tree <scm-arg|which>, this routine replaces it by its
    child at position <scm-arg|pos> (see figure
    <reference|insert-remove-node-fig>).

    If <scm-arg|which> is also a <value|scheme> variable, then the macro
    <scm-macro|tree-remove-node!> should be used to update the value of
    <scm-arg|which>.
  </explain>

  <\big-figure>
    <\equation*>
      <tabular*|<tformat|<table|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<cwith|1|1|1|1|cell-valign|c>|<table|<row|<cell|t>|<cell|>|<cell|\<longrightarrowlim\><rsup|insert-node(t,i,u)>>|<cell|>|<cell|<tree|u|u<rsub|0>|\<cdots\>|u<rsub|i-1>|t|u<rsub|i>|\<cdots\>|u<rsub|n-1>>>>>>>>>|<row|<cell|>>|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<cwith|1|1|5|5|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|remove-node(t,i)>>|<cell|>|<cell|t<rsub|i>>>>>>>>>>>
    </equation*>

    \;
  <|big-figure>
    <label|insert-remove-node-fig>Illustration of the operations
    <scm|(<scm-fun|tree-insert-node> t i u)> and
    <scm|(<scm-fun|tree-remove-node> t i)>. Notice that the second operation
    undos the first one.
  </big-figure>

  <tmdoc-copyright|2005|Joris van der Hoeven>

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