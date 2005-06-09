<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|The <TeXmacs> content model>

  All <TeXmacs> documents or document fragments can be thought of as
  <em|trees>, as explained in more detail in the chapter about the
  <hyper-link|<TeXmacs> document format|../../format/basics/basics.en.tm>.
  Inside <value|scheme> programs, there are two main ways to represent such
  trees, depending on whether one manipulates active or passive documents:

  <paragraph|Passive documents and <value|scheme> trees>

  Passive documents, like those which are processed by a<nbsp>conversion
  tool, are usually represented by <em|scheme trees>. For instance, the
  fraction

  <\equation*>
    <frac|a<rsup|2>|b+c>
  </equation*>

  is typically represented by

  <\scheme-fragment>
    (frac (concat "a" (rsup "2")) "b+c")
  </scheme-fragment>

  This representation is convenient in the sense that they can be manipulated
  directly using standard <value|scheme> routines on lists.

  <paragraph|Active documents and C++ trees>

  Active documents, like ones which are visible in one of the editors
  windows, are rather represented using the internal C++ type
  <verbatim|tree>, which has been exported to <value|scheme> via the glue.
  When a tree is part of a real document inside the editor, the tree is aware
  about its position inside the document. Using routines from the tree API,
  you may then make changes in the document simply by assigning new values to
  the tree.

  For instance, consider the following experiment: open two windows and start
  a <value|scheme> session in each window. In the second window, enter the
  line

  <with|prog-language|scheme|prog-session|default|<\session>
    <\input|scheme] >
      (define t (buffer-tree))
    </input>
  </session>>

  In the first window, you may now modify the document in the second window
  using commands like

  <with|prog-language|scheme|prog-session|default|<\session>
    <\input|scheme] >
      (tree-set! t (tree 'document (string-\<gtr\>tree "First line.")

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-\<gtr\>tree
      "Second line.")))
    </input>

    <\input|scheme] >
      (tree-set t 1 (string-\<gtr\>tree "New second line."))
    </input>

    <\input|scheme] >
      (tree-set t 0 (tree 'strong (tree-ref t 0)))
    </input>
  </session>>

  <paragraph|A common framework>

  From the last three lines in above experiment, it becomes apparent that it
  is quite cumbersome to manipulate trees using the standard tree
  constructors. For this reason, <TeXmacs> provides a hybrid type
  <verbatim|content> for manipulating scheme trees and C++ trees in a common
  framework. For instance, the last three lines in the above experiment may
  be replaced by

  <with|prog-language|scheme|prog-session|default|<\session>
    <\input|scheme] >
      (tree-set! t '(document "First line." "Second line."))
    </input>

    <\input|scheme] >
      (tree-set t 1 "New second line.")
    </input>

    <\input|scheme] >
      (tree-set t 0 `(strong ,(tree-ref t 0)))
    </input>
  </session>>

  More precisely, a scheme expression of the type <verbatim|content> is
  either a string, a tree or a list whose first element is a symbol and whose
  remaining elements are other expressions of type <verbatim|content>.
  <TeXmacs> provides several routines (usually prefixed by <verbatim|tm->)
  for basic operations on content, like <verbatim|tm-car>,
  <verbatim|tm-arity>, <verbatim|tm-\<gtr\>list>, <verbatim|tm-equal?>, etc.
  Most higher level routines are built on top of these routines, so as to
  accept arguments of type <verbatim|content> whenever appropriate.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>