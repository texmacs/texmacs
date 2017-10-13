<TeXmacs|1.0.7.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Semantic editing facilities>

  Starting with version 1.0.7.10, <TeXmacs> incorporates several features for
  the ``semantic'' editing of mathematical formulas. When used appropriately,
  this allows you to write documents in which all formulas are at least
  correct from a syntactical point of view. For instance, in the formulas
  <math|a+b>, the computer will understand that <math|+> is an operator which
  applies to the arguments <math|a> and <math|b>. Notice that our
  ``semantics'' does not go any further: <TeXmacs> is unaware of the
  mathematical nature of addition.

  Semantic editing does require additional efforts from the user, at least a
  little adaptation. For instance, it is the user's job to enter
  multiplications using the shortcut <key|*> and function applications using
  <key|space>. Indeed, from the graphical point of view, these operations
  cannot be distinguished, since they are both printed as invisible
  whitespace. However, the semantics of these operations is clearly very
  different.

  Although semantically correct documents are usually not very different from
  informal presentation-oriented documents as far as typesetting is
  concerned, the additional user effort may pay off for several reasons:

  <\itemize>
    <item>Documents with more semantics are for instance useful when using
    formulas as inputs for a computer algebra system.

    <item>Syntactically correct documents are less likely to contain
    ``typos'' or more intricate mathematical errors.

    <item>For certain editing operations, such as cut and paste, one may
    directly select subformulas which are meaningful from the syntactical
    point of view.

    <item>It reduces the risk of using non standard notations, which will be
    difficult to understand for potential readers of your work.
  </itemize>

  Furthermore, other semantic facilities might be integrated in the future,
  such as semantic search and replace, or semantic search on the web.

  In order to activate the semantic editing facilities, please toggle
  <menu|Edit|Preferences|Mathematics|Semantic editing>. In the semantic
  editing mode, several of the structured editing features of <TeXmacs> apply
  to the syntactic structure of the formula, rather than the visual structure
  of the document. For instance, the <em|semantic
  focus><label|semantic-focus> is usually a subformula of the <hlink|current
  focus|../../text/man-structure.en.tm>. Similarly, only syntactically
  meaningful subformulas can be selected when making a selection.

  The semantic focus is useful for several reasons. First of all, it is
  displayed in green if the formula is syntactically correct and in red if
  you made an error. This allows to quickly notice any typos while entering a
  formula. Secondly, if you have any doubt on the precedence of a
  mathematical operator or relation, then the semantic focus will inform you
  on the default interpretation: by putting your cursor right next to your
  operator, the subexpression to which the operator applies will be
  highlighted. In the case of an addition, or a more general associative
  operator, all summands are highlighted.

  <tmdoc-copyright|2010|Joris van der Hoeven>

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