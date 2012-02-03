<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Traversing the <TeXmacs> documentation>

  As a general rule, you should avoid the use of sectioning commands inside
  the <TeXmacs> documentation and try to write small help pages on well
  identified topics. At a second stage, you should write recursive ``meta
  help files'' which indicate how to traverse the documentation in an
  automatic way. This allows the reuse of a help page for different purposes
  (a printed manual, a web-oriented tutorial, etc.).

  The <tmstyle|tmdoc> style provides three markup macros for indicating how
  to traverse documentation. The <markup|traverse> macro is used to
  encapsulate regions with traversal information. It can be inserted using
  the <subsubmenu|Manual|Traversal|Traverse> entry in the
  <menu|Manual|Traversal> or <icon|tm_traverse.xpm> menu. The <markup|branch>
  and <markup|extra-branch> macros indicate help pages which should be
  considered as a subsection and an appendix respectively, whereas the
  <markup|continue> macro indicates a follow-up page. Each of these macros
  should be used inside a <markup|traverse> environment and each of these
  macros takes two arguments. The first argument describes the link and the
  second argument gives the physical relative address of the linked file.

  Typically, at the end of a meta help file you will find several
  <markup|branch> or <markup|continue> macros, inside one <markup|traverse>
  macro. At the top of the document, you should also specify a title for your
  document using the <markup|tmdoc-title> macro, as <hlink|described
  before|copyright.en.tm>. When generating a printed manual from the
  documentation, a chapter-section-subsection structure will automatically be
  generated from this information and the document titles. Alternatively, one
  might automatically generate additional buttons for navigating inside the
  documentation using a browser.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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