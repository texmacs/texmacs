<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Fundamental tree modification routines>

  From an internal point of view, all modifications to the <TeXmacs> edit
  tree are decomposed into atomic modifications of eight different types. In
  this section, we describe the <scheme> interface to these fundamental
  modification routines. Even though it is usually more convenient to use
  higher level modification routines, as described in the <hlink|next
  section|edit-modify.en.tm>, the fundamental tree modification routines may
  occasionally be useful as well.

  It should be emphasized that the fundamental tree modification routines are
  <em|not> checked for their correctness. It is the responsibility of the
  user to verify that the operations are valid and that they lead to a
  correct new edit tree. Although it is sometimes possible to leave the edit
  tree in a temporarily incorrect or \Punsimplified\Q state (for instance, by
  allowing subtrees of the form <rigid|<scm|(concat "")>>), this practice is
  not generally recommended, and may lead to severe bugs.

  <\explain>
    <scm|(tree-assign! <scm-arg|var> <scm-arg|new-value>)><explain-synopsis|tree
    assignment>
  <|explain>
    On input, we have a <scheme> variable <scm-arg|var> of type
    <verbatim|tree> and <scm-arg|new-value> of type <verbatim|content>. The
    macro replaces the tree by <scm-arg|new-value> and updates <scm-arg|var>
    accordingly. The new tree value of <scm-arg|var> is returned.
  </explain>

  <\explain>
    <scm|(tree-insert! <scm-arg|var> <scm-arg|pos>
    <scm-arg|ins>)><explain-synopsis|insertion of new nodes or characters>
  <|explain>
    The first parameter <scm-arg|var> is a <scheme> variable of type
    <verbatim|tree>. If <scm-arg|var> is a compound tree, then <scm-arg|ins>
    should be a list <math|u<rsub|0>,\<ldots\>,u<rsub|l-1>> of new children
    of type <verbatim|content>. In that case, the routine inserts
    <math|u<rsub|0>,\<ldots\>,u<rsub|l-1>> into the children of
    <scm-arg|var>, at position <scm-arg|pos> (see figure
    <reference|insert-remove-fig>). If <scm-arg|var> is a string tree, then
    <scm-arg|ins> should be of string content type, and the string
    <scm-arg|ins> is inserted into <scm-arg|var> at position <scm-arg|pos>.
    The variable <scm-arg|var> is updated with the result of the insertion
    and the result is returned.
  </explain>

  <\explain>
    <scm|(tree-remove! <scm-arg|var> <scm-arg|pos>
    <scm-arg|nr>)><explain-synopsis|removal of nodes or characters>
  <|explain>
    The first parameter <scm-arg|var> is a <scheme> variable of type
    <verbatim|tree>. If <scm-arg|var> is a compound tree, then <scm-arg|nr>
    of its children are removed, starting at position <scm-arg|pos> (see
    figure <reference|insert-remove-fig>). If <scm-arg|var> is a string tree,
    then <scm-arg|nr> characters are removed, starting at position
    <scm-arg|pos>. The variable <scm-arg|var> is updated with the result of
    the removal and the result is returned.
  </explain>

  <\big-figure>
    <\equation*>
      <tabular*|<tformat|<table|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|insert<around|(|t,i,u|)>>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|u<rsub|0>|\<cdots\>|u<rsub|l-1>|t<rsub|i>|\<cdots\>|t<rsub|n-1>>>>>>>>>|<row|<cell|>>|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|remove<around|(|t,i,l|)>>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|t<rsub|i+l>|\<cdots\>|t<rsub|n-1>>>>>>>>>>>>
    </equation*>

    \;
  <|big-figure>
    <label|insert-remove-fig>Illustration of the operations
    <scm|(tree-insert! t i u)> and <scm|(tree-remove! t i l)>. If <scm|u> has
    length<nbsp><scm|l>, then we notice that <scm|(tree-remove! t i l)> undos
    the insertion <scm|(tree-insert! t i u)>.
  </big-figure>

  \;

  <\explain>
    <scm|(tree-split! <scm-arg|var> <scm-arg|pos>
    <scm-arg|at>)><explain-synopsis|split the children into two parts>
  <|explain>
    The first parameter <scm-arg|var> is a <scheme> variable of type
    <verbatim|tree>. The macro is used to split the child <math|u> of
    <scm-arg|var> at position <scm-arg|pos> into two parts. If <math|u> is a
    compound tree, then the first part consists of the first <scm-arg|at>
    children and the second part of the remaining ones. Both parts carry the
    same label as <math|u> and <math|u> is replaced by the two parts inside
    <scm-arg|var> (see figure <reference|split-join-fig>). If <math|u> is
    string tree, then it is rather split into two strings at position
    <scm-arg|at>. The variable <scm-arg|var> is updated with the result of
    the split command and the result is returned.
  </explain>

  <\explain>
    <scm|(tree-join! <scm-arg|var> <scm-arg|pos>)><explain-synopsis|join two
    adjacent nodes>
  <|explain>
    The first parameter <scm-arg|var> is a <scheme> variable of type
    <verbatim|tree>. This macro is used to join the child <math|u> of
    <scm-arg|var> at position <scm-arg|pos> with the child <math|v> at
    position <scm-arg|pos>+1. If <math|u> and <math|v> are trees, then they
    are removed from <scm-arg|var> and replaced by a single tree which has
    the same label as <math|u> and whose children are those of <math|u>,
    followed by the children of <math|v> (see figure
    <reference|split-join-fig>). If <math|u> and <math|v> are strings, then
    they are replaced by their concatenation. The variable <scm-arg|var> is
    updated with the result of the join command and the result is returned.
  </explain>

  <\big-figure>
    <\equation*>
      <tabular*|<tformat|<table|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,k-1>>|<plus|1l|1fn>||<minus|1r|1fn>|>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|split<around|(|t,i,j|)>>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,j-1>>|<plus|1l|1fn>|||>|<resize|<tree|t<rsub|i>|t<rsub|i,j>|\<cdots\>|t<rsub|i,k-1>>|||<minus|1r|1fn>|>|\<cdots\>|t<rsub|n-1>>>>>>><space|1em><space|1em>>>|<row|<cell|>>|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,j-1>>|<plus|1l|2fn>|||>|<resize|<tree|t<rsub|i+1>|t<rsub|i+1,j>|\<cdots\>|t<rsub|i+1,k-1>>|||<minus|1r|2fn>|>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|join<around|(|t,i|)>>>|<cell|>|<cell|<tree|t|t<rsub|0>|\<cdots\>|<resize|<tree|t<rsub|i>|t<rsub|i,0>|\<cdots\>|t<rsub|i,j-1>|t<rsub|i+1,0>|\<cdots\>|t<rsub|i+1,k-1>>|<plus|1l|4fn>||<minus|1r|4fn>|>|\<cdots\>|t<rsub|n-1>>>>>>>>>>>>
    </equation*>

    \;
  <|big-figure>
    <label|split-join-fig>Illustration of the operations <scm|(tree-split! t
    i j)> and <scm|(tree-join! t i)>. Notice that <scm|(tree-join! t i)>
    undos <scm|(tree-split! t i j)>.
  </big-figure>

  <\explain>
    <scm|(tree-assign-node! <scm-arg|var>
    <scm-arg|lab>)><explain-synopsis|assign the label of a tree>
  <|explain>
    This macro replaces the label of a compound tree stored in a <scheme>
    variable <scm-arg|var> by a new value <scm-arg|lab>. The result of the
    substitution is returned.
  </explain>

  <\explain>
    <scm|(tree-insert-node! <scm-arg|var> <scm-arg|pos>
    <scm-arg|ins>)><explain-synopsis|insert the tree as a child of another
    one>
  <|explain>
    Given a <scheme> variable <scm-arg|var>, containing a tree, and a content
    tree <scm-arg|ins>, this macro replaces <scm-arg|var> by <scm-arg|ins>,
    with <scm-arg|var> inserted as a new child of <scm-arg|ins> at position
    <scm-arg|pos> (see figure <reference|insert-remove-node-fig>). The result
    of the insertion is returned.
  </explain>

  <\explain>
    <scm|(tree-remove-node! <scm-arg|var>
    <scm-arg|pos>)><explain-synopsis|replace a tree by a child>
  <|explain>
    Given a <scheme> variable <scm-arg|var>, containing a compound tree, this
    macro replaces <scm-arg|var> by its child at position <scm-arg|pos> (see
    figure <reference|insert-remove-node-fig>). The value of this child is
    returned.
  </explain>

  <\big-figure>
    <\equation*>
      <tabular*|<tformat|<table|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<cwith|1|1|1|1|cell-valign|c>|<table|<row|<cell|t>|<cell|>|<cell|\<longrightarrowlim\><rsup|insert-node<around|(|t,i,u|)>>>|<cell|>|<cell|<tree|u|u<rsub|0>|\<cdots\>|u<rsub|i-1>|t|u<rsub|i>|\<cdots\>|u<rsub|n-1>>>>>>>>>|<row|<cell|>>|<row|<cell|<tabular*|<tformat|<cwith|1|1|3|3|cell-valign|c>|<cwith|1|1|5|5|cell-valign|c>|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|>|<cell|\<longrightarrowlim\><rsup|remove-node<around|(|t,i|)>>>|<cell|>|<cell|t<rsub|i>>>>>>>>>>>
    </equation*>

    \;
  <|big-figure>
    <label|insert-remove-node-fig>Illustration of the operations
    <scm|(tree-insert-node! t i u)> and <scm|(tree-remove-node! t i)>. Notice
    that the second operation undoes the first one.
  </big-figure>

  <\remark>
    Each of the macros <scm|tree-assign!>, <scm|tree-insert!>, <abbr|etc.>
    has a functional counterpart <verbatim|<scm|tree-assign>>,
    <verbatim|<scm|tree-insert>>, etc. The first parameter of these
    counterparts can be an arbitrary \Pl-<no-break>value\Q and does not have
    to be a scheme variable. However, in the case when a <scheme> variable is
    passed as the first parameter, these variants do not necessarily update
    its contents with the return value.
  </remark>

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>