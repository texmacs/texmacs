<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Fundamental tree modification routines>

  From an internal point of view, all modifications to the <TeXmacs> edit
  tree are decomposed into atomic modifications of eight different types. In
  this section, we describe the <value|scheme> interface to these fundamental
  modification routines. Even though it is usually more convenient to use
  higher level modification routines, as described in the next section, the
  fundamental modification routines may occasionally be useful as well.

  All fundamental modification routines come in two blends: ...

  <\explain>
    <explain-scm-fun|tree-assign|<scm-arg|which>|<scm-arg|new-value>>

    <explain-scm-macro|tree-assign!|<scm-arg|which>|<scm-arg|new-value>><explain-synopsis|tree
    assignment>
  <|explain>
    .
  </explain>

  <\explain>
    <explain-scm-fun|tree-insert|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>>

    <explain-scm-macro|tree-insert!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>><explain-synopsis|insertion
    of new nodes or characters>
  <|explain>
    .
  </explain>

  <\explain>
    <explain-scm-fun|tree-remove|<scm-arg|which>|<scm-arg|pos>|<scm-arg|nr>>

    <explain-scm-macro|tree-remove!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|nr>><explain-synopsis|removal
    of nodes or characters>
  <|explain>
    .
  </explain>

  <\big-figure>
    <\eqnarray*>
      <tformat|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|\<longrightarrow\>>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|u<rsub|0>|\<cdots\>|u<rsub|l-1>|t<rsub|i>|\<cdots\>|t<rsub|n-1>>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|\<longrightarrow\>>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|t<rsub|i+l>|\<cdots\>|t<rsub|n-1>>>>>>
    </eqnarray*>
  <|big-figure>
    Illustration of the functions <scm-fun|tree-insert> and
    <scm-fun|tree-remove>.
  </big-figure>

  \;

  <\explain>
    <explain-scm-fun|tree-split|<scm-arg|which>|<scm-arg|pos>|<scm-arg|at>>

    <explain-scm-macro|tree-split!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|at>><explain-synopsis|split
    the children into two parts>
  <|explain>
    .
  </explain>

  <\explain>
    <explain-scm-fun|tree-join|<scm-arg|which>|<scm-arg|pos>>

    <explain-scm-macro|tree-join!|<scm-arg|which>|<scm-arg|pos>><explain-synopsis|join
    two adjacent nodes>
  <|explain>
    .
  </explain>

  <\big-figure>
    <\eqnarray*>
      <tformat|<table|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|<tree|t<rsub|i>|t<rsub|i,1>|\<cdots\>|t<rsub|i,k>>|\<cdots\>|t<rsub|n-1>>>|<cell|\<longrightarrow\>>|<cell|<tree|t|t<rsub|0>|\<cdots\>|<tree|t<rsub|i>|t<rsub|i,1>|\<cdots\>|t<rsub|i,k>>|\<cdots\>|t<rsub|n-1>>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|\<longrightarrow\>>|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|i-1>|t<rsub|i+l>|\<cdots\>|t<rsub|n-1>>>>>>
    </eqnarray*>
  <|big-figure>
    Illustration of the functions <scm-fun|tree-split> and
    <scm-fun|tree-join>.
  </big-figure>

  <\explain>
    <explain-scm-fun|tree-assign-node|<scm-arg|which>|<scm-arg|lab>>

    <explain-scm-macro|tree-assign-node!|<scm-arg|which>|<scm-arg|lab>><explain-synopsis|assign
    the label of a tree>
  <|explain>
    .
  </explain>

  <\explain>
    <explain-scm-fun|tree-insert-node|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>>

    <explain-scm-macro|tree-insert-node!|<scm-arg|which>|<scm-arg|pos>|<scm-arg|ins>><explain-synopsis|insert
    the tree as a child of another one>
  <|explain>
    .
  </explain>

  <\explain>
    <explain-scm-fun|tree-remove-node|<scm-arg|which>|<scm-arg|pos>>

    <explain-scm-macro|tree-remove-node!|<scm-arg|which>|<scm-arg|pos>><explain-synopsis|replace
    a tree by a child>
  <|explain>
    .
  </explain>

  <\big-figure>
    <\eqnarray*>
      <tformat|<table|<row|<cell|t>|<cell|\<longrightarrow\>>|<cell|<tree|u|u<rsub|0>|\<cdots\>|u<rsub|i-1>|t|u<rsub|i-1>|\<cdots\>|u<rsub|n-1>>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<tree|t|t<rsub|0>|\<cdots\>|t<rsub|n-1>>>|<cell|\<longrightarrow\>>|<cell|t<rsub|i>>>>>
    </eqnarray*>
  <|big-figure>
    Illustration of the functions <scm-fun|tree-insert-node> and
    <scm-fun|tree-remove-node>.
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