<TeXmacs|1.0.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Concrete syntaxes>

  <TeXmacs> represents documents as a tree data structure which can be
  translated in plain text using different <em|concrete syntaxes>. Different
  syntaxes have merits and demerits and are best suited to different uses. We
  generally use the <value|scheme> syntax to document <TeXmacs> constructs,
  but it is not the mythical <em|document source code>, just one particular
  representation.

  Documents are generally written to disk using the <def-index|TeXmacs
  syntax>. This syntax is designed to be unobstrusive and easy to read, so
  the content of a document can be easily understood using a simple text
  editor:

  <framed-fragment|<verbatim|\<less\>with\|mode\|math\|x+y+\<less\>frac\|1\|2\<gtr\>+\<less\>sqrt\|y+z\<gtr\>\<gtr\>>>

  On the other hand, TeXmacs syntax makes style files difficult to read and
  is not designed to be hand-edited: whitespace has complex semantics and
  some internal structures are not obviously presented. Do not edit documents
  in the TeXmacs syntax unless you <em|really> know what you are doing.

  The preferred syntax for modification is the screen display in the
  <TeXmacs> typesetting editor. If that seems surprising to you, consider
  that a syntax is a way to represent information in a form suitable to
  understanding and modification. The on-screen typeset representation of a
  document, together with its interactive behaviour, is a particular concrete
  syntax:

  <\framed-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </framed-fragment>

  You should always use the <TeXmacs> editor itself to write style files,
  compose document fragments for use in programs, and of course, edit whole
  documents. Programmatic documents, like style files, should use ``preamble
  mode''.

  In contexts where a document cannot be represented and edited in its
  typeset form, like in <value|scheme> programs or in e-mails, or when the
  important information is the internal representation, the preferred syntax
  is <value|scheme>. This syntax was designed to be predictible, easy to
  hand-edit, and expose the complete internal structure of the document.

  <\tm-fragment>
    <verbatim|(with "mode" "math" (concat "x+y+" (frac "1" "2") "+" (sqrt
    "y+z")))>
  </tm-fragment>

  This syntax also represent documents as conventional s-expressions and is
  easy to process by <value|scheme> programs. You may also find it useful
  when debugging complex stylesheets.

  Documents can also be represented in <acronym|XML> syntax. This syntax was
  designed to be close to the internal tree structure and use convential
  <acronym|XML> notations which are well supported by standard tools.
  Whitespace generally <em|is> significant in <acronym|XML>, the example
  features some whitespace inserted at a location where it is ignored:

  <framed-fragment|<\verbatim>
    \<less\>with mode="math"\<gtr\>x+y+\<less\>frac\<gtr\><next-line>
    \ \<less\>tm-arg\<gtr\>1\<less\>/tm-arg\<gtr\>\<less\>tm-arg\<gtr\>2\<less\>/tm-arg\<gtr\>\<less\>/frac\<gtr\>+\<less\>sqrt\<gtr\>y+z\<less\>/sqrt\<gtr\>\<less\>/with\<gtr\>
  </verbatim>>

  You should only use the <acronym|XML> syntax for interoperability.

  The following section describes the <em|abstract syntax> of documents,
  using the <value|scheme> <em|concrete syntax> for reference.

  <tmdoc-copyright|2004|David Allouche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|gly-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|TeXmacs syntax>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>