<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|<TeXmacs> trees>

  All <TeXmacs> documents or document fragments can be thought of as
  <em|trees>. For instance, the tree

  <\equation*>
    <tree|<text|<markup|with>>|mode|math|<tree|<text|<markup|concat>>|x+y+|<tree|<text|<markup|frac>>|1|2>|+|<tree|<text|<markup|sqrt>>|y+z>>>
  </equation*>

  typically represents the formula

  <\equation>
    <label|tm-tree-ex>x+y+<frac|1|2>+<sqrt|y+z>
  </equation>

  <paragraph*|Internal nodes of <TeXmacs> trees>

  Each of the internal nodes of a <TeXmacs> tree is a string symbol and each
  of the leafs is an ordinary string. A string symbol is different from a
  usual string only from the efficiency point of view: <TeXmacs> represents
  each symbol by a unique number, so that it is extremely fast to test
  weather two symbols are equal.

  <paragraph*|Leafs of <TeXmacs> trees>

  Currently, all strings are represented using the <em|universal <TeXmacs>
  encoding>. This encoding coincides with the Cork font encoding for all
  characters except \P<verbatim|\<less\>>\Q and \P<verbatim|\<gtr\>>\Q.
  Character sequences starting with \P<verbatim|\<less\>>\Q and ending with
  \P<verbatim|\<gtr\>>\Q are interpreted as special extension characters. For
  example, <verbatim|\<less\>alpha\<gtr\>> stands for the letter
  <math|\<alpha\>>. The semantics of characters in the universal <TeXmacs>
  encoding does not depend on the context (currently, cyrillic characters are
  an exception, but this should change soon). In other words, the universal
  <TeXmacs> encoding may be seen as an analogue of Unicode. In the future, we
  might actually switch to Unicode.

  The string leafs either contain ordinary text or special data. <TeXmacs>
  supports the following atomic data types:

  <\description>
    <item*|Boolean numbers>Either <verbatim|true> or <verbatim|false>.

    <item*|Integers>Sequences of digits which may be preceded by a minus
    sign.

    <item*|Floating point numbers>Specified using the usual scientific
    notation.

    <item*|Lengths>Floating point numbers followed by a <hlink|length
    unit|lengths.en.tm>, like <verbatim|29.7cm> or <verbatim|2fn>.
  </description>

  <paragraph*|Serialization and preferred syntax for editing>

  When storing a document as a file on your hard disk or when copying a
  document fragment to the clipboard, <TeXmacs> trees have to be represented
  as strings. The conversion without loss of information of abstract
  <TeXmacs> trees into strings is called <em|serialization> and the inverse
  process <em|parsing>. <TeXmacs> provides three ways to serialize trees,
  which correspond to the standard <hlink|<TeXmacs> format|tm-tm.en.tm>, the
  <hlink|XML format|tm-tmml.en.tm> and the <hlink|<scheme>
  format|tm-scm.en.tm>.

  However, it should be emphasized that the preferred syntax for modifying
  <TeXmacs> documents is the screen display inside the editor. If that seems
  surprising to you, consider that a syntax is a way to represent information
  in a form suitable to understanding and modification. The on-screen typeset
  representation of a document, together with its interactive behaviour, is a
  particularly concrete syntax. Moreover, in the <menu|Document|Source> menu,
  you may find different ways to customize the way documents are viewed, such
  as different levels of informative flags and a <hlink|\Psource tree\Q
  mode|../../style/presentation/src-present.en.tm> for editing style files.

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>