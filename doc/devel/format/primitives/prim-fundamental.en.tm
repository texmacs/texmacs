<TeXmacs|1.0.3.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Fundamental structures>

  <\big-table>
    <assign|tag-info-table|<macro|x|<descriptive-table|<tformat|<cwith|1|2|1|-1|cell-halign|c>|<cwith|1|1|1|-1|cell-row-span|2>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|3|3|cell-bborder|0ln>|<cwith|1|2|1|-1|cell-lborder|1ln>|<cwith|2|2|1|-1|cell-background|pastel
    blue>|<cwith|1|1|3|3|cell-col-span|2>|<cwith|1|1|3|3|cell-row-span|1>|<twith|table-min-rows|3>|<twith|table-min-cols|6>|<twith|table-max-cols|6>|<arg|x>>>>><tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|document>>|<cell|<with|mode|math|1+n>>|<cell|<with|mode|math|>No>|<cell|All>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|concat>>|<cell|<with|mode|math|1+n>>|<cell|No>|<cell|All>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|error>>|<cell|1>|<cell|Yes>|<cell|None>|<cell|Display>|<cell|Never>>|<row|<cell|<markup|raw-data>>|<cell|1>|<cell|Yes>|<cell|None>|<cell|Display>|<cell|Logical>>>>>

    \;
  </big-table|Fundamental structures>

  The <markup|document> and <markup|concat> operators are fundamental and
  pervasive structures of <TeXmacs> documents. They are used to assemble
  lines and paragraphs out of all other structures.

  The <verbatim|texmacs> concrete syntax, used to save documents to disk, is
  designed to make these operators implicit, so they do not get in the way of
  reading the document content, that is one import reason why this concrete
  syntax is inappropriate for hand-editing.

  <\description-dash>
    <item*|<markup|document>>Sequence of logical paragraphs.

    A simple, plain text, document is made of a simple sequence of paragraphs
    (<em|i.e.> logical lines).

    <\tm-fragment>
      A simple document.

      Made of several paragraphs.
    </tm-fragment>

    Such a document is represented a simple <markup|document> tree whose
    subtrees are all strings.

    <\scheme-fragment>
      (document "A simple document." "Made of several paragraphs.")
    </scheme-fragment>

    Actually, the root of the <em|edit tree> is always a <markup|document>
    node. Document fragments use a root <markup|document> node only when they
    contain several top-level paragraphs.

    The <verbatim|document> operators are also found in inner subtrees
    wherever a multi-paragraph structure is needed, for example in list
    structures and multi-paragraph table cells.

    Some primitives (<em|e.g.> <markup|document>, <markup|include>) are
    <def-index|block content> and are not allowed in line context. A position
    is in <def-index|block context> if, after evaluation, it is immediately
    within:

    <\itemize>
      <item>a <markup|document> tree,

      <item>a <re-index|multiparagraph cell>, that is a <markup|cell> whose
      operand is a <markup|document>

      <item>the last operand of a <markup|with> in block context,

      <item>the last operand of a <markup|surround> in block context.
    </itemize>

    <item*|<markup|concat>>Sequence of line items.

    Since the interpretation of an operator is done according to the count
    and position its operands, we need an operator to glue compound operands
    into individual trees without introducing additional logical structure.

    <\tm-fragment>
      Some <em|emphasized> text.
    </tm-fragment>

    This fragment is made of several line items. The <markup|concat> node is
    used to assemble them into a single logical line.

    <\scheme-fragment>
      (concat "Some " (em "emphasized") " text.")
    </scheme-fragment>

    The <markup|concat> operator is essential to put compound structures in
    trees taking multiple parameters. For example, let us place the previous
    fragment in a multi-paragraph context:

    <\tm-fragment>
      Multiple paragraphs.

      Some <em|emphasized> text.
    </tm-fragment>

    We absolutely need <markup|concat> as a glue operator so the multiple
    components of the second paragraph are no considered multiple paragraphs.

    <\scheme-fragment>
      (document "Multiple paragraphs."

      \ \ \ \ \ \ \ \ \ \ (concat "Some " (em "emphasized") " text."))
    </scheme-fragment>

    Structures which are typeset inside a <markup|concat> or some operators
    which expect <def-index|inline content>, is said to be in <def-index|line
    context>. A correct document may not evaluate to a tree which contains
    block content within an inline context and, except for
    <re-index|multiparagraph cells>, it is not possible to nest a block
    context within a line context.
  </description-dash>

  The <markup|error> and <markup|raw-data> operators are much more rarely
  used. However, they are versatile, may be useful in numerous context, and
  do not fall neatly in any category, so we chose to describe them early.

  <\description-dash>
    <item*|<markup|error>>Error reporting.

    This primitive should never appear in documents. It is provided as aid in
    tracking down invalid constructs. It is produced at evaluation time by
    any kind of primitive which is given improper operands.

    <item*|<markup|raw-data>>Opaque data.

    In some contexts you need to embed uneditable data inside a document,
    most of the time this is uneditable binary data. The <markup|raw-data>
    primitive makes it impossible to view or modify its subtree from within
    the editor.
  </description-dash>

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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
    <associate|preamble|false>
  </collection>
</initial>