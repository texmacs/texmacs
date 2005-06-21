<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|High level modification routines>

  The routine <scm-fun|tree-set> and the corresponding macro
  <scm-macro|tree-set!> can be used as a higher level interface to the
  fundamental routines for modifying trees as described in the
  <hlink|previous section|edit-fundamental.en.tm>. However, the it is still
  up to the user to verify that the resulting edit tree is still correct.

  <\explain>
    <explain-scm-fun|tree-set|<scm-arg|which>
    <scm-args|accessors>|<scm-arg|new-value>>

    <explain-scm-macro|tree-set!|<scm-arg|which>|<scm-args|accessors>|<scm-arg|new-value>><explain-synopsis|smart
    tree assignment>
  <|explain>
    This routine replaces the tree <scm|(<scm-fun|tree-ref> <scm-arg|which>
    <scm-args|accessors>)> by a new content value <scm-arg|new-value>.
    Besides the fact that the routine <scm-fun|tree-set> supports additional
    accessors for <scm-arg|which> (see the description of <scm-fun|tree-ref>
    below), <scm-fun|tree-set> differs from <scm-fun|tree-assign> in this
    respect that <scm-fun|tree-set> tries to cleverly decompose the
    assignment into fundamental modification routines. The objective of this
    decomposition is to make a less intrusive modifications in the document,
    so as to preserve as many tree positions and cursor positions.

    For instance, the operation <scm|(<scm-fun|tree-set> t t)> is a
    no-operation for all trees <verbatim|t>. A more complex operations like

    <verbatim| \ \ \ <scm|(<scm-macro|tree-set!> t `(foo "Hop"
    ,(<scm-fun|tree-ref> t 2)))>>

    is decomposed into the following fundamental modifications:

    <\with|par-par-sep|0fn>
      <\verbatim>
        \ \ \ \ (<scm-macro|tree-remove-node!> t 2)

        \ \ \ \ (<scm-macro|tree-insert-node!> t 1 '(foo "Hop"))
      </verbatim>
    </with>

    Like in the case <scm-fun|tree-assign> and <scm-macro|tree-assign!>, you
    should use the macro <scm-macro|tree-set!> in order to update the value
    of <scm-arg|which> if <scm-arg|which> is a <value|scheme> variable
    <scm-arg|accessors> is the empty list.
  </explain>

  <\explain>
    <explain-scm-fun|tree-ref|<scm-arg|which>
    <scm-args|accessors>><explain-synopsis|enhanced tree access>
  <|explain>
    In its simplest form, this routine allows for the quick access of a
    subtree of <scm-arg|which> via a list of integers <scm-arg|accessors>.
    For instance, if <scm-arg|which> contains the tree <scm|(frac "a" (sqrt
    "b")))>, then <scm|(<scm-fun|tree-ref> <scm-arg|which> 1 0)> returns the
    tree <scm|"b">.

    In its general form, <scm-fun|tree-ref> relies on the routine
    <scm-fun|select> in order to compute the desired subtree. With
    <scm-arg|which> as in the above example, this makes it possible to
    retrieve the subtree <group|<scm|(sqrt "b")>> using
    <scm|(<scm-fun|tree-ref> t 'frac)>. In the case when there are several
    matches, the first match is returned. For instance, if <scm-arg|which>
    contains the tree <group|<scm|(frac (sqrt "a") (sqrt "b")))>>, then
    <scm|(<scm-fun|tree-ref> t 'frac)> returns <group|<scm|(sqrt "a")>>.

    In fact, the result of <scm-fun|tree-ref> is not necessarily a subtree of
    <scm-arg|which>: the <scm-fun|select> utility also accepts the accessors
    <scm|:up>, <scm|:down>, <scm|:next>, <scm|:previous>, <abbr|etc.> for
    navigating inside the edit tree starting with <scm-arg|which>. For
    instance, <verbatim|(<scm-fun|tree-ref> (cursor-tree) :up)> returns the
    parent of the cursor tree. For more details, we refer to the
    documentation of <scm-fun|select>.
  </explain>

  Besides the above routine for the direct modification of a subtree of the
  document, <TeXmacs> also provides several routines for inserting content at
  the current cursor position.

  <\explain>
    <explain-scm-fun|insert|<scm-arg|what>><explain-synopsis|insertion of
    content>
  <|explain>
    Insert the content <scm-arg|what> at the current cursor position.
    <TeXmacs> does some additional checking whether it is allowed to perform
    the insertion. For instance, it is disallowed to insert multi-paragraph
    content inside a mathematical formula. Whenever the user attempts to make
    an invalid insertion, then <scm-fun|insert> is equivalent to a
    no-operation.
  </explain>

  <\explain>
    <explain-scm-fun|make|<scm-arg|lab>><explain-synopsis|insertion of a tag>
  <|explain>
    This routine may be used to insert a valid tag with label <scm-arg|lab>.
    As many empty arguments as necessary are inserted in order to make the
    tag valid. Similarly, if <scm-arg|lab> is a multi-paragraph tag, then the
    necessary operations are performed to put the tag in a separate
    paragraph.
  </explain>

  <scm-fun|make-with>, <scm-fun|make-return>, etc.

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