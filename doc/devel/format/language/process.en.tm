<TeXmacs|1.0.3.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Interpretation process>

  Interpretation of documents proceeds in several steps:

  <\enumerate>
    <item><strong|Evaluation> expands the <def-index|document tree>,
    containing markup tags, to the <def-index|object tree>, containing only
    primitive constructs.

    <item><strong|Typesetting> produce and position the boxes described by
    primitive constructs.

    <item><strong|Display> draws the boxes to screen or to
    PostScript<trademark>.
  </enumerate>

  We previously noted that a tree label can either be the name of a
  typesetter primitive or the name of markup tag. <hyper-link|Typesetter
  primitives|../primitives/primitives.en.tm> are the fundamental building
  blocks of the typesetting languages. Markup tags can be though of as the
  functions and procedures of the typesetting language. Tree leaves are
  character strings.

  Primitives and markup tags are collectively referred to as
  <def-index|typesetter operators>, or <def-index|operators> for short. Trees
  are expressions, tree labels are operators names and subtrees are operands.
  All expressions have a value and all values are trees. A trivial tree is
  only made of a string. In this respect, the typesetting language is similar
  to <value|scheme>: values and expressions are both represented as trees,
  atomic values are strings.

  However, an important difference with <value|scheme> is that most trees
  cannot be deconstructed <emdash>subtrees cannot be accessed,<emdash>
  strings and tuples are the most notable exceptions.

  Most operators are only meaningful either for evaluation or typesetting:
  <def-index|computational operators> only have a meaning for evaluation and
  are substituted before typesetting, <def-index|physical operators> only
  have a meaning for typesetting and are self-evaluating. Operands are
  evaluated in <em|applicative order>. The typesetting language does not use
  <em|normal order> lazy evaluation, but it provides mechanisms for partial
  and delayed evaluation.

  Operators may not display all their subtrees. Most computational operators
  and many physical operators consume at least one of their operands. Trees
  which are not associated to typeset boxes are invisible and inaccessible.
  Conversely, boxes which are not associated to subtrees of the document tree
  are uneditable. Trees which are produced during evaluation are called
  <def-index|synthetic trees> and are uneditable. Understanding which
  operators produce editable values and how they affect the accessibility of
  their operands is essential to designing good typesetter macros.

  What you should remember:

  <\itemize-dot>
    <item>Expressions and values are trees, labels are operators, subtrees
    are operands, and atomic values are strings.

    <item>Expressions and values both are trees, but most trees cannot be
    deconstructed in the typesetting language.

    <item>Expressions are evaluated in applicative order, when the operator
    is applied. If are not sure you understand this, it just means that
    evaluation is done in the usual way.

    <item>Operators may consume trees, produce uneditable trees, and include
    editable operands in their value.
  </itemize-dot>

  Now that you understand the basic rules of the language, we will describe
  its atomic data types and data storage mechanisms. Compound values are
  described in the chapter about <hyper-link|typesetter
  primitives|../primitives/primitives.en.tm>, along with the primitives used
  to create and operate on them.

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
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|3.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-7|<tuple|3.|?>>
    <associate|idx-1|<tuple|1.|?>>
    <associate|idx-2|<tuple|1.|?>>
    <associate|idx-3|<tuple|3.|?>>
    <associate|idx-4|<tuple|3.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|document tree>|<pageref|idx-1>>

      <tuple|<tuple|object tree>|<pageref|idx-2>>

      <tuple|<tuple|typesetter operators>|<pageref|idx-3>>

      <tuple|<tuple|operators>|<pageref|idx-4>>

      <tuple|<tuple|computational operators>|<pageref|idx-5>>

      <tuple|<tuple|physical operators>|<pageref|idx-6>>

      <tuple|<tuple|synthetic trees>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>