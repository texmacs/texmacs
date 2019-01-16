<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

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

  <paragraph*|Document fragments>

  All <TeXmacs> documents or document fragments can be thought of as
  <em|trees>, as explained in more detail in the chapter about the
  <hlink|<TeXmacs> document format|../../format/basics/basics.en.tm>. For
  instance, the mathematical formula

  <\equation>
    <label|example-edit-formula>a<rsub|1>+\<cdots\>+a<rsub|n>
  </equation>

  corresponds to the tree

  <\equation>
    <label|example-edit-tree><tree|<markup|concat>|a|<tree|<markup|rsub>|1>|+\<cdots\>+a|<tree|<markup|rsub>|n>>
  </equation>

  Trees which are part of a document which is effectively being edited are
  said to be <hlink|active|../overview/overview-content.en.tm#tree-active>,
  and they are implemented using the <scheme> type<nbsp><verbatim|tree>.

  Besides this representation format, which is preferred when editing
  document fragments, <TeXmacs> also allows you to represent
  <hlink|passive|../overview/overview-content.en.tm#tree-passive> document
  fragments by <scheme> trees. This alternative representation, which
  corresponds to the <scheme> type <verbatim|stree>, is more convenient when
  writing routines for processing documents (such as conversions to another
  format). Finally, <TeXmacs> provides a <hlink|hybrid|../overview/overview-content.en.tm#tree-hybrid>
  representation, which corresponds to the <scheme> type <verbatim|content>.
  The <verbatim|content> type (corresponding to the prefix <verbatim|tm->,
  for simplicity) is typically used for writing abstract utility routines for
  trees, which can then be applied indistinctly to objects of type
  <verbatim|tree> or <verbatim|stree>.

  One major advantage of active trees (of type <verbatim|tree>) is that they
  are aware of their own location in the document. As a consequence,
  <TeXmacs> provides editing routines which allow you to modify the document
  simply by assigning a tree to a different value. For instance, assume that
  the <scheme> variable <scm|t> contains the subscript <math|1> in formula
  (<reference|example-edit-formula>). Then the instruction

  <\scm-code>
    (tree-set! t "2")
  </scm-code>

  will simultaneously change the subscript into a <math|2> and update the
  <scheme> variable <scm|t>. Another nicety is that the value of <scm|t> is
  <em|persistent> during changes of other parts of the document. For
  instance, if we change the <math|a>'s into <math|b>'s in the formula
  (<reference|example-edit-formula>), then <scm|t> keeps its value <em|and>
  its location. Of course, the location of <scm|t> may be lost when <scm|t>
  or one of its parents is modified. Nevertheless, the modification routines
  are designed in such a way that we try hard to remember locations. For
  instance, if \P<math|a<rsub|0>+>\Q is inserted in front of the formula
  (<reference|example-edit-formula>) using the routine <scm|tree-insert!>,
  then <scm|t> keeps its value <em|and> its location, even though one of its
  ancestors was altered.

  Some further precisions and terminology will be useful. First of all, we
  have seen a distinction between <em|active> and <em|passive> trees,
  according to whether a tree is part of a document or not. Secondly,
  <TeXmacs> both supports <em|native trees> (of type <verbatim|tree>), which
  are implemented in C++, and <em|scheme trees> (of type <verbatim|stree>),
  which have a more familiar <scheme> syntax. Finally, <em|hybrid trees>
  unify native and scheme trees. Formally speaking, a hybrid tree is either a
  string, a native tree or a list whose first element is a symbol and whose
  other elements are again hybrid trees. We notice that active trees are
  necessarily native, but native trees may both be active or passive.
  Furthermore, certain descendants of an inactive tree may be active, but we
  never have the contrary.

  <paragraph*|Positions inside document fragments>

  The main way to address positions inside a tree is via a list of positive
  integers, called a <em|path>, and corresponding to the <scheme> type
  <verbatim|path>. For instance, assume that <scm|x> corresponds to the
  expression<nbsp>(<reference|example-edit-formula>). Then the subscript
  <math|1> is identified uniquely by the path<nbsp><rigid|<scm|(1 0)>>.
  Similarly the cursor position just behind the subscript<nbsp><math|1>
  corresponds to the path<nbsp><rigid|<scm|(1 0 1)>>. More generally, if
  <scm|p> is a path to a string leaf, then the path <scm|(rcons p i)>
  corresponds to the cursor position just behind the <scm|i>-th character in
  the string (we notice that <scm|rcons> is used to append a new element at
  the end of a list). If <scm|p> is a path to a non-string subtree, then
  <scm|(rcons p 0)> and <scm|(rcons p 1>) correspond to the cursor positions
  before and behind this subtree.

  It should be noticed that paths do not necessarily correspond to <em|valid>
  subtrees or cursor positions. Clearly, some of the elements in the path may
  be \Pout of range\Q. However, certain <em|a priori> possible cursor
  positions may correspond to invisible parts of the document (like a cursor
  position inside a folded argument or an attribute of <markup|with>).
  Moreover, two possible cursor positions may actually coincide, like the
  paths <scm|(0)> and <scm|(0 0)> inside the
  expression<nbsp>(<reference|example-edit-formula>). In this example, only
  the second cursor path is valid. Usually, the validity of a cursor path may
  be quickly detected using DRD (Data Relation Definition) information, which
  is determined from the style file. In exceptional cases, the validity may
  only be available after typesetting the document.

  It should also be noticed that all active trees are a subtree of the global
  <em|<TeXmacs> edit tree> or <em|root tree>, which can be retrieved using
  <scm|(root-tree)>. The routines <scm|tree-\<gtr\>path> and
  <scm|path-\<gtr\>tree> can be used in order to get the location of an
  active tree and the active tree at a given location.

  A simple way to address subtrees of a tree in a more persistent way is
  using object of type <verbatim|tree>, <abbr|i.e.> by considering the
  subtrees themselves. The persistent analogue of a cursor path is a
  <em|persistent position>, which corresponds to an object of <scheme> type
  <verbatim|position>. One particularity of persistent positions is that,
  even when a tree into which they point is removed, they keep indicating a
  valid close position in the remaining document. For instance, assume that
  <scm|pos> stands for the cursor position <scm|(1 0 1)> in the
  expression<nbsp>(<reference|example-edit-formula>). If we remove
  <math|a<rsub|1>+\<cdots\>+>, then the tree corresponding to the remaining
  expression <math|a<rsub|n>> is given by

  <\equation*>
    <tree|<markup|concat>|a|<tree|<markup|rsub>|n>>
  </equation*>

  and the position associated to <scm|pos> becomes <scm|(0 0)>. <TeXmacs>
  provides the routines <scm|position-new>, <scm|position-delete>,
  <scm|position-set> and <scm|position-get> to create, delete, set and get
  persistent cursor positions.

  <paragraph*|Semantic navigation and further utilities>

  Because accessing subtrees using paths may become quite cumbersome,
  <TeXmacs> provides some additional functionality to simplify this task. As
  a general rule, the routines <scm|select> and <scm|match?> may be used to
  select all subtrees of a given tree which match a certain pattern. For
  instance, if<nbsp><scm|x> corresponds to the
  expression<nbsp>(<reference|example-edit-formula>), then

  <\scm-code>
    (select x '(rsub :%1))
  </scm-code>

  returns a list with the two subscripts <math|1> and <math|n>. In fact,
  <scm|select> may also be used in order to navigate through a tree. For
  instance, if <scm|t> corresponds to the subscript <math|1>
  in<nbsp>(<reference|example-edit-formula>), then

  <\scm-code>
    (select t '(:up :next))
  </scm-code>

  returns the list with one element \P<math|+\<cdots\>+a>\Q. The routine
  <scm|select> is implicitly called by many routines which operate on trees.
  For instance, with <scm|t> as above,

  <\scm-code>
    (tree-ref t :up :next)
  </scm-code>

  directly returns the tree \P<math|+\<cdots\>+a>\Q.

  Besides simpler access to subtrees of a tree or other \Pclose trees\Q,
  <TeXmacs> also provides several other useful mechanisms for writing editing
  routines. For instance, the routine <scm|tree-innermost> and the macro
  <scm|with-innermost> may be used to retrieve the innermost supertree of a
  certain type at the current cursor position. Since many editing routines
  operate at the current cursor position, two other useful macros are
  <scm|with-cursor> and <scm|cursor-after>, which allow you to perform some
  operations at a temporarily distinct cursor position <abbr|resp.> to
  compute the cursor position after some operations, without actually
  changing the current cursor position.

  <paragraph*|A worked example>

  In order to illustrate the <TeXmacs> API for editing documents on a simple
  example, assume that we wish to write a function
  <scm|swap-numerator-denominator> which allows us to swap the numerator and
  the denominator of the innermost fraction at the current cursor position.

  The innermost fraction may simply be retrieved using the macro
  <scm|with-innermost>. Together with the routine <scm|tree-set!> for
  modifying a tree, this yields a first simple implementation:

  <\scm-code>
    (define (swap-numerator-denominator)

    \ \ (with-innermost t 'frac

    \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))))
  </scm-code>

  It should be noticed that the macro <scm|with-innermost> ignores its body
  whenever no innermost fraction is found.

  The above implementation has the disadvantage that we loose the current
  cursor position inside the numerator or denominator (wherever we were). The
  following refined implementation allows us to remain at the \Psame
  position\Q modulo the exchange numerator/denominator:

  <\scm-code>
    (define (swap-numerator-denominator)

    \ \ (with-innermost t 'frac

    \ \ \ \ (with p (tree-cursor-path t)

    \ \ \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))

    \ \ \ \ \ \ (tree-go-to t (cons (- 1 (car p)) (cdr p))))))
  </scm-code>

  Here we used the routines <scm|tree-cursor-path> and <scm|tree-go-to>,
  which allow us to manipulate the cursor position relative to a given tree.

  As the icing on the cake, we may make our routine available through the
  mechanism of structured variants:

  <\scm-code>
    (define (variant-circulate t forward?)

    \ \ (:require (tree-is? t 'frac))

    \ \ (swap-numerator-denominator))
  </scm-code>

  Notice that this implementation can be incorrect when operating on nested
  fractions. The implementation can be further improved by letting
  <scm|swap-numerator-denominator> operate on a specific<nbsp>tree:

  <\scm-code>
    (define (swap-numerator-denominator t)

    \ \ (:require (tree-is? t 'frac))

    \ \ (with p (tree-cursor-path t)

    \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))

    \ \ \ \ (tree-go-to t (cons (- 1 (car p)) (cdr p)))))
  </scm-code>

  The corresponding generic routine could be defined as

  <\scm-code>
    (define (swap-numerator-denominator t)

    \ \ (and-with p (tree-outer t)

    \ \ \ \ (swap-numerator-denominator p)))
  </scm-code>

  This piece of code will perform an outward recursion until a specific
  handler is found. We may now replace the call
  <scm|(swap-numerator-denominator)> by <scm|(swap-numerator-denominator
  (cursor-tree))>.

  The new implementation also allows us to toggle the numerator and
  denominator of a<nbsp>selected fraction using
  <scm|(swap-numerator-denominator (focus-tree))>. However, the focus is not
  necessarily conserved during the operation, thereby disallowing to restore
  the original state by toggling a second time. We may explicitly conserve
  the focus as follows:

  <\scm-code>
    (define (swap-numerator-denominator t)

    \ \ (:require (tree-is? t 'frac))

    \ \ (with p (tree-cursor-path t)

    \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))

    \ \ \ \ (tree-go-to t (cons (- 1 (car p)) (cdr p)))

    \ \ \ \ (tree-focus t)))
  </scm-code>

  This routine will even work when we are inside a nested fraction and
  operating on the outer fraction.

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