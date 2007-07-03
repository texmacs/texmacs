<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Selection of subexpressions>

  Besides pattern matching on trees, <TeXmacs> provides the routine
  <scm|select> for pattern matching along paths. Given a tree, this mechanism
  typically allows the user to select all subtrees which are reached
  following a path which meets specific criteria. For instance, one might to
  select the second child of the last child or all square roots inside
  numerators of fractions. The syntax of the selection patterns is also used
  for high level tree accessors.

  <\explain>
    <scm|(select <scm-arg|expr> <scm-arg|pattern>)><explain-synopsis|select
    subexpressions following a pattern>
  <|explain>
    Select all subtrees inside a hybrid tree <scm-arg|expr> according to a
    specific path <scm-arg|pattern>.
  </explain>

  Patterns are lists of atomic patterns of one of the following forms:

  <\explain>
    <scm|0>, <scm|1>, <scm|2>, ...<explain-synopsis|select a specific child>
  <|explain>
    Given an integer <scm|n>, select the <scm|n>-th child of the input tree.
    For instance, <scm|(select '(frac "1" "2") '(0))> returns <scm|("1")>.
  </explain>

  <\explain>
    <scm|:first>, <scm|:last><explain-synopsis|select first or last child>
  <|explain>
    Select first or last child of the input tree.
  </explain>

  <\explain>
    <scm|(:range <scm-arg|start> <scm-arg|end>)><explain-synopsis|select
    children in a range>
  <|explain>
    Select all children in a specified range.
  </explain>

  <\explain>
    <scm-arg|label><explain-synopsis|select children with a given label>
  <|explain>
    Select all compound subtrees with the specified <scm-arg|label>. Example:

    <with|prog-language|scheme|prog-session|default|<\session>
      <\input|scheme] >
        (select '(document (strong "x") (math "a+b") (strong "y")) '(strong))
      </input>

      <\output>
        ((strong "x") (strong "y"))
      </output>
    </session>>
  </explain>

  <\explain>
    <scm|:#1>, <scm|:#2>, <scm|:#3>, ...<explain-synopsis|select descendants
    of a given generation>
  <|explain>
    The pattern <scm|:#n>, where <scm|n> is a number, selects all descendants
    of the <scm|n>-th generation. Example:

    <with|prog-language|scheme|prog-session|default|<\session>
      <\input|scheme] >
        (select '(foo (bar "x" "y") (slash (dot))) '(:#2))
      </input>

      <\output>
        ("x" "y" (dot))
      </output>
    </session>>
  </explain>

  <\explain>
    <scm|:*><explain-synopsis|select all descendants>
  <|explain>
    This pattern selects all descendants of the tree. For instance,
    <scm|(select t '(:* frac 0 :* sqrt))> selects all square roots inside
    numerators of fractions inside <scm|t>.
  </explain>

  <\explain>
    <scm|(:match <scm-arg|pattern>)><explain-synopsis|matching>
  <|explain>
    This pattern matches the input tree if and only the input tree matches
    the specified <scm-arg|pattern> according to <scm|match?>.
  </explain>

  <\explain>
    <scm|(:match <scm-arg|pattern>)><explain-synopsis|matching>
  <|explain>
    This pattern matches the input tree if and only the input tree matches
    the specified <scm-arg|pattern> according to <scm|match?>. Example:

    <with|prog-language|scheme|prog-session|default|<\session>
      <\input|scheme] >
        (select '(foo "x" (bar)) '(:#1 (:match :string?)))
      </input>

      <\output>
        ("x")
      </output>
    </session>>
  </explain>

  <\explain>
    <scm|(:or <scm-arg|pattern-1> ... <scm-arg|pattern-n>)>

    <scm|(:and <scm-arg|pattern-1> ... <scm-arg|pattern-n>)><explain-synopsis|boolean
    expressions>
  <|explain>
    These rules allow for the selection of all subtrees which satisfy one
    among or all patterns <scm-arg|pattern-1> until <scm-arg|pattern-n>.
  </explain>

  In the case when the input tree is active, the function <scm|select>
  supports some additional patterns which allow the user to navigate inside
  the tree.

  <\explain>
    <scm|:up><explain-synopsis|parent>
  <|explain>
    This pattern selects the parent of the input tree, if it exists.
  </explain>

  <\explain>
    <scm|:down><explain-synopsis|child containing the cursor>
  <|explain>
    If the cursor is inside some child of the input tree, then this pattern
    will select this child.
  </explain>

  <\explain>
    <scm|:next><explain-synopsis|next child>
  <|explain>
    If the input tree is the <math|i>-th child of its parent, then this
    pattern will select the <math|(i+1)>-th child.
  </explain>

  <\explain>
    <scm|:previous><explain-synopsis|previous child>
  <|explain>
    If the input tree is the <math|i>-th child of its parent, then this
    pattern will select the <math|(i-1)>-th child.
  </explain>

  <tmdoc-copyright|2007|Joris van der Hoeven>

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