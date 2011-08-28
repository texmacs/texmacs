<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Traversing the <TeXmacs> documentation>

  As a general rule, you should avoid the use of sectioning commands inside
  the <TeXmacs> documentation and try to write small help pages on well
  identified topics. At a second stage, you should write recursive ``meta
  help files'' which indicate how to traverse the documentation in an
  automatic way. This allows the reuse of a help page for different purposes
  (a printed manual, a web-oriented tutorial, etc.).

  The <tmstyle|tmdoc> style provides three markup macros for indicating how
  to traverse documentation. The <markup|traverse> macro is used to
  encapsulate regions with traversal information. The <markup|branch> macro
  indicates a help page which should be considered as a subsection and the
  <markup|continue> macro indicates a follow-up page. Both the
  <markup|branch> and the <markup|continue> macro take two arguments. The
  first argument describes the link and the second argument gives the
  physical relative address of the linked file.

  Typically, at the end of a meta help file you will find several
  <markup|branch> or <markup|continue> macros, inside one <markup|traverse>
  macro. At the top of the document, you should also specify a title for your
  document using the <markup|tmdoc-title> macro. When generating a printed
  manual from the documentation, a chapter-section-subsection structure will
  automatically be generated from this information and the document titles.
  Alternatively, one might automatically generate additional buttons for
  navigating inside the documentation using a browser.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
