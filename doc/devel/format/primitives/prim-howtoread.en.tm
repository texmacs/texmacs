<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|How to read the primitives reference>

  This section is the reference documentation of all primitive tags defined
  by the typesetter. Each subsection documents several primitives and starts
  by a table summarizing their properties and intended usage.

  <big-table|<tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<verbatim|example>>|<cell|<with|mode|math|2+3*n>>|<cell|Yes>|<cell|First
  two>|<cell|Display>|<cell|Never>>>>>|Example tag description>

  <\description-dash>
    <item*|Operator>Primitives defined in this section.

    <item*|Arity>Allowed operand counts.

    <item*|Accessible border>Whether the tag borders are accessible.

    The border positions are ``just before'' and ``just after'' the tag.

    <item*|Accessible children>Children which are editable when the tag is
    active.

    Not all tags can be deactivated, only the accessible subtrees of such
    tags can be edited. Modifying the other subtrees is always possible by
    script.

    <\description>
      <item*|All>All subtrees are accessible.

      <item*|None>No subtree is accessible.

      <item*|First <with|mode|math|n>>Only a fixed number of subtrees at the
      start of the tag are accessible.

      <item*|Last <with|mode|math|n>>Only a fixed number of subtrees at the
      end of the tag are accessible.
    </description>

    <item*|Process>Steps of the typesetting process which are affected.

    <\description>
      <item*|Typesetting>Normally self-evaluating, affects the typesetting of
      operands. Some typesetting primitives may evaluate to error trees when
      given inappropriate operands.

      <item*|Functional>Substituted at evaluation, yields a synthetic tree,
      possibly empty.

      <item*|Macro>Substituted at evaluation, yields a tree which may have
      accessible parts.

      <item*|Display>Self-evaluating and explicitly displayed. Such
      primitives have no inherent semantics and are not normally visible in
      documents. They are used to report problems or in internal structures.
    </description>

    <item*|Usage>Documents where this primitive is allowed.

    <\description>
      <item*|Logical>Logically structured regular documents.

      <item*|Physical>Regular documents, but this primitive is not considered
      part of the logical subset of the typesetting language.

      <item*|Style>Style-sheet documents only.

      <item*|Never>This primitive should never appear in production
      documents. It may appear during the evaluation of incorrect document,
      occur temporarily during edition, or be useful for scripting.
    </description>
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
  </collection>
</initial>