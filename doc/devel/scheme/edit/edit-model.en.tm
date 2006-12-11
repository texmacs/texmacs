<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|The <TeXmacs> editing model>

  Routines for editing documents are usually based on one or several of the
  following ingredients:

  <\enumerate>
    <item>Identification of the document fragments which have to be edited.

    <item>Modification of one or several document fragments.

    <item>Moving the cursor to a new place.
  </enumerate>

  Before going into the precise API which allows you to carry out these
  tasks, let us first describe the fundamental underlying data types, and go
  through an example.

  <paragraph|Document fragments>

  All <TeXmacs> documents or document fragments can be thought of as
  <em|trees>, as explained in more detail in the chapter about the
  <hyper-link|<TeXmacs> document format|../../format/basics/basics.en.tm>.
  For instance, the mathematical formula

  <\equation>
    <label|example-edit-formula>a<rsub|1>+\<cdots\>+a<rsub|n>
  </equation>

  corresponds to the tree

  <\equation>
    <label|example-edit-tree><tree|<markup|concat>|a|<tree|<markup|rsub>|1>|+\<cdots\>+a|<tree|<markup|rsub>|n>>
  </equation>

  Trees which are part of a document which is effectively being edited are
  said to be <hlink|active|../overview/overview-content.en.tm#tree-active>,
  and they correspond to the <value|scheme> data type<nbsp><verbatim|tree>.

  Besides this representation format, which is preferred when editing
  document fragments, <TeXmacs> also allows you to represent
  <hlink|passive|../overview/overview-content.en.tm#tree-passive> document
  fragments by <value|scheme> trees. This alternative representation, which
  corresponds to the <value|scheme> type <verbatim|stree>, is more convenient
  when writing routines for processing documents (such as conversions to
  another format). Finally, <TeXmacs> provides a
  <hlink|hybrid|../overview/overview-content.en.tm#tree-hybrid>
  representation, which corresponds to the <value|scheme> type
  <verbatim|content>. The <verbatim|content> type is typically used for
  writing abstract utility routines for trees, which can then be applied
  indistinctly to objects of type <verbatim|tree> or <verbatim|stree>.

  One major advantage of active trees (of type <verbatim|tree>) is that they
  are aware of their own location in the document. As a consequence,
  <TeXmacs> provides editing routines which allow you to modify the document
  simply by assigning a tree to a different value. For instance, assume that
  the <value|scheme> variable <verbatim|t> contains the subscript
  <with|mode|math|1> in formula (<reference|example-edit-formula>). Then the
  instruction

  <\scheme-fragment>
    (tree-set! t "2")
  </scheme-fragment>

  will simultaneously change the subscript into a <with|mode|math|2> and
  update the <value|scheme> variable <verbatim|t>. Another nicety is that the
  value of <verbatim|t> is <em|persistent> during changes of other parts of
  the document. For instance, if we change the <with|mode|math|a>'s into
  <with|mode|math|b>'s in the formula (<reference|example-edit-formula>),
  then <verbatim|t> keeps its value <em|and> its location. Of course, the
  location of <verbatim|t> may be lost when <verbatim|t> or one of its
  parents is modified. Nevertheless, the modification routines are designed
  in such a way that we try hard to remember locations. For instance, when
  insert ``<with|mode|math|a<rsub|0>+>'' in front of the formula
  (<reference|example-edit-formula>) using the routine
  <verbatim|tree-insert!>, then <verbatim|t> keeps its value <em|and> its
  location, even though one of its ancestors was altered.

  <paragraph|Positions inside document fragments>

  The main way to address positions inside a tree is via a list of positive
  integers, called a <em|path>, and corresponding to the <value|scheme> type
  <verbatim|path>. For instance, assume that <verbatim|x> corresponds to the
  expression<nbsp>(<reference|example-edit-formula>). Then the subscript
  <with|mode|math|1> is identified uniquely by the
  path<nbsp><group|<verbatim|(1 0)>>. Similarly the cursor position just
  behind the subscript<nbsp><with|mode|math|1> corresponds to the
  path<nbsp><group|<verbatim|(1 0 1)>>. More generally, if <verbatim|p> is a
  path to a string leaf, then the path <group|<verbatim|(rcons p i)>>
  corresponds to the cursor position just behind the <verbatim|i>-th
  character in the string (we notice that <verbatim|rcons> is used to append
  a new element at the end of a list). If <verbatim|p> is a path to a
  non-string subtree, then <verbatim|(rcons p 0)> and <verbatim|(rcons p 1>)
  correspond to the cursor positions before and behind this subtree.

  It should be noticed that paths do not necessarily correspond to <em|valid>
  subtrees or cursor positions. Clearly, some of the elements in the path may
  be ``out of range''. However, certain <em|a priori> possible cursor
  positions may correspond to invisible parts of the document (like a cursor
  position inside a folded argument or an attribute of <markup|with>).
  Moreover, two possible cursor positions may actually coincide, like the
  paths <verbatim|(0)> and <verbatim|(0 0)> inside the
  expression<nbsp>(<reference|example-edit-formula>). In this example, only
  the second cursor path is valid. Usually, the validity of a cursor path may
  be quickly detected using DRD (Data Relation Definition) information, which
  is determined from the style file. In execeptional cases, the validity may
  only be available after typesetting the document.

  It should also be noticed that all active trees are a subtree of the global
  <em|<TeXmacs> edit tree> or <em|root tree>, which can be retrieved using
  <verbatim|(root-tree)>. The routines <verbatim|tree-\<gtr\>path> and
  <verbatim|path-\<gtr\>tree> can be used in order to get the location of an
  active tree and the active tree at a given location.

  A simple way to address subtrees of a tree in a more persistent way is
  using object of type <verbatim|tree>, <abbr|i.e.> by considering the
  subtrees themselves. The persistent analogue of a cursor path is a
  <em|persistent position>, which corresponds to an object of <value|scheme>
  type <verbatim|position>. One particularity of persitent positions is that,
  even when a tree into which they point is removed, they keep indicating a
  valid close position in the remaining document. For instance, assume that
  <verbatim|pos> stands for the cursor position <verbatim|(1 0 1)> in the
  expression<nbsp>(<reference|example-edit-formula>). If we remove
  <with|mode|math|a<rsub|1>+\<cdots\>+>, then the tree corresponding to the
  remaining expression <with|mode|math|a<rsub|n>> is given by

  <\equation*>
    <tree|<markup|concat>|a|<tree|<markup|rsub>|n>>
  </equation*>

  and the position associated to <verbatim|pos> becomes <verbatim|(0 0)>.
  <TeXmacs> provides the routines <verbatim|position-new>,
  <verbatim|position-delete>, <verbatim|position-set> and
  <verbatim|position-get> to create, delete, set and get persistent cursor
  positions.

  <paragraph|Semantic navigation and further utilities>

  Because accessing subtrees using paths may become quite cumbersome,
  <TeXmacs> provides some additional functionality to simplify this task. As
  a general rule, the routines <verbatim|select> and <verbatim|match?> may be
  used to select all subtrees of a given tree which match a certain pattern.
  For instance, if<nbsp><verbatim|x> corresponds to the
  expression<nbsp>(<reference|example-edit-formula>), then

  <verbatim| \ \ \ (select x '(rsub :%1))>

  returns a list with the two subscripts <with|mode|math|1> and
  <with|mode|math|n>. In fact, <verbatim|select> may also be used in order to
  navigate through a tree. For instance, if <verbatim|t> corresponds to the
  subscript <with|mode|math|1> in<nbsp>(<reference|example-edit-formula>),
  then

  <verbatim| \ \ \ (select t '(:up :next))>

  returns the list with one element ``<with|mode|math|+\<cdots\>+a>''. The
  routine <verbatim|select> is implicitly called by many routines which
  operate on trees. For instance, with <verbatim|t> as above,

  <verbatim| \ \ \ (tree-ref t :up :next)>

  directly returns the tree ``<with|mode|math|+\<cdots\>+a>''.

  Besides simpler access to subtrees of a tree or other ``close trees'',
  <TeXmacs> also provides several other useful mechanisms for writing editing
  routines. For instance, the routine <verbatim|tree-innermost> and the macro
  <verbatim|with-innermost> may be used to retrieve the innermost supertree
  of a certain type at the current cursor position. Since many editing
  routines operate at the current cursor position, two other useful macros
  are <verbatim|with-cursor> and <verbatim|cursor-after>, which allow you to
  perform some operations at a temporarily distinct cursor position
  <abbr|resp.> to compute the cursor position after some operations, without
  actually changing the current cursor position.

  <paragraph|A worked example>

  In order to illustrate the <TeXmacs> API for editing documents on a simple
  example, assume that we wish to write a function
  <verbatim|swap-numerator-denominator> which allows us to swap the numerator
  and the denominator of the innermost fraction at the current cursor
  position.

  The innermost fraction may simply be retrieved using the macro
  <verbatim|with-innermost>. Together with the routine <verbatim|tree-set!>
  for modifying a tree, this yields a first simple implementation:

  <\scheme-fragment>
    (define (swap-numerator-denominator)

    \ \ (with-innermost t 'frac

    \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))))
  </scheme-fragment>

  It should be noticed that the macro <verbatim|with-innermost> ignores its
  body whenever no innermost fraction is found.

  The above implementation has the disadvantage that we loose the current
  cursor position inside the numerator or denominator (whereever we were).
  The following refined implementation allows us to remain at the ``same
  position'' modulo the exchange numerator/denominator:

  <\scheme-fragment>
    (define (swap-numerator-denominator)

    \ \ (with-innermost t 'frac

    \ \ \ \ (with p (tree-cursor-path t)

    \ \ \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))

    \ \ \ \ \ \ (tree-go-to t (cons (- 1 (car p)) (cdr p))))))
  </scheme-fragment>

  Here we used the routines <verbatim|tree-cursor-path> and
  <verbatim|tree-go-to>, which allow us to manipulate the cursor position
  relative to a given tree. As the icing on the cake, we may make our routine
  available through the mechanism of structured variants:

  <\scheme-fragment>
    (define (variant-circulate forward?)

    \ \ (:inside frac)

    \ \ (swap-numerator-denominator))
  </scheme-fragment>

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
