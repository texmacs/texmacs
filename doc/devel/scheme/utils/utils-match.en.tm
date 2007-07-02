<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Matching regular expressions>

  Regular expressions naturally generalize from strings to trees and allow to
  test whether a given tree matches a given pattern. <TeXmacs> implements the
  primitives <scm-fun|match?> and <verbatim|<scm-fun|match>> for this
  purpose, which also provide support for wildcards, user-defined grammars
  and more.

  <\explain>
    <explain-scm-fun|match?|<scm-arg|expr>
    <scm-arg|pattern>><explain-synopsis|check whether a scheme expression
    satisfies a pattern>
  <|explain>
    This function determines whether a scheme expression <scm-arg|expr>
    satisfies a given <scm-arg|pattern>. It will be detailed below how to
    form valid patterns. The matching routines recursively understand that
    native trees match their scheme counterparts. For instance,
    <verbatim|(match? (tree "x") "x<name|">)> will return <verbatim|#t>.
  </explain>

  <\explain>
    <explain-scm-fun|match|<scm-arg|l> <scm-arg|pattern>
    <scm-arg|bindings>><explain-synopsis|solutions to a given pattern under
    bindings>
  <|explain>
    Given a list <scm-arg|l> of scheme expressions, a <scm-arg|pattern> with
    free variables and an association list of <scm-arg|bindings>, this
    routine determines all substitutions of free variables by values
    (extending the given <scm-arg|bindings>), for which <scm-arg|l> matches
    the <scm-arg|pattern>.
  </explain>

  <\explain>
    <explain-scm-fun|define-grammar|<scm-args|rules>><explain-synopsis|user
    defined matching grammars>
  <|explain>
    Given a list of rules of the form <verbatim|(:<scm-arg|var>
    <scm-arg|pattern-1> ... <scm-arg|pattern-n>)>, this instruction defines a
    new terminal symbol <verbatim|:<scm-arg|var>> for each such rule, which
    matches the disjunction of the patterns <scm-arg|pattern-1> until
    <scm-arg|pattern-n>. This terminal symbol can then be used as an
    abbreviation in matching patterns. Grammar rules may be interdependent.
  </explain>

  Valid patterns are formed in the following ways:

  <\explain>
    <verbatim|><scm-arg|leaf><explain-synopsis|symbols, strings, etc.>
  <|explain>
    A <scm-arg|leaf> is only matched against itself.
  </explain>

  <\explain>
    <verbatim|><verbatim|(<scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)><explain-synopsis|lists>
  <|explain>
    In the case when lists <verbatim|l-1> until <verbatim|l-n> match
    <scm-arg|pattern-1> until <scm-arg|pattern-n>, their concatenation
    matches the pattern <verbatim|(<scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)>.
  </explain>

  <\explain>
    <verbatim|><verbatim|:#1>, <verbatim|:#2>, <verbatim|:#3> ...,
    <verbatim|:*><explain-synopsis|wildcards>
  <|explain>
    The wildcard <verbatim|:#n>, where <verbatim|n> is a number matches any
    list of length <verbatim|n>. The wildcard <verbatim|:*> matches any list,
    including the empty list.
  </explain>

  <\explain>
    <verbatim|><verbatim|'<scm-arg|var>><explain-synopsis|variables>
  <|explain>
    This pattern attempts to bind the variable <scm-arg|var> against the
    expression. If <scm-arg|var> is used only once, then it essentially
    behaves as a wildcard. More generally, it can be used to form patterns
    with identical subexpressions. For instance, the pattern <verbatim|(frac
    'x 'x)> will match all fractions <math|<frac|x|x>>.
  </explain>

  <\explain>
    <verbatim|><verbatim|'<scm-arg|var>><explain-synopsis|variables>
  <|explain>
    This pattern attempts to bind the variable <scm-arg|var> against the
    expression. If <scm-arg|var> is used only once, then it essentially
    behaves as a wildcard. More generally, it can be used to form patterns
    with identical subexpressions. For instance, the pattern <verbatim|(frac
    'x 'x)> will match all fractions <math|<frac|x|x>>.
  </explain>

  <\explain>
    <verbatim|><verbatim|:<scm-arg|var>><explain-synopsis|user-provided
    grammar rules>
  <|explain>
    In the case when <verbatim|:<scm-arg|var>> is a user-provided terminal
    symbol (see <scm-fun|define-grammar> above), this pattern matches the
    corresponding grammar.
  </explain>

  <\explain>
    <verbatim|><verbatim|:<scm-arg|pred?>><explain-synopsis|arbitrary
    <value|scheme> predicates>
  <|explain>
    Given a <value|scheme> predicate <scm-arg|pred?>, such as
    <verbatim|string?>, this pattern matches any scheme expression which
    satisfies the predicate.
  </explain>

  <\explain>
    <verbatim|><verbatim|(:not <scm-arg|pattern>)>

    <verbatim|(:or <scm-arg|pattern-1> ... <scm-arg|pattern-n>)>

    <verbatim|><verbatim|(:and <scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)><explain-synopsis|logical operations>
  <|explain>
    Negation, disjunction and conjunction of patterns.
  </explain>

  <\explain>
    <verbatim|(:repeat <scm-arg|pattern>)><explain-synopsis|repetition>
  <|explain>
    Given lists <verbatim|l-1> until <verbatim|l-n> which match
    <scm-arg|pattern>, their concatenation matches the repetition
    <verbatim|(repeat <scm-arg|pattern>)>. In particular, the empty list is
    matched.
  </explain>

  <\explain>
    <verbatim|(:group <scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)><explain-synopsis|grouping>
  <|explain>
    Groups a concatenation of patterns into a new list patterns. For
    instance, all lists of the form <verbatim|(a b a b ... a b)> are matched
    by <verbatim|(:repeat (:group a b))>, whereas <verbatim|(:repeat (a b))>
    rather matches all lists of the form <verbatim|((a b) (a b) ... (a b))>.
  </explain>

  <\explain>
    <verbatim|(:quote <scm-arg|expr>)><explain-synopsis|quotation>
  <|explain>
    Only matches a given expression <scm-arg|expr>.
  </explain>

  <\example>
    The tree

    <\scheme-fragment>
      (define t '(foo (bar "x") (bar "y") (option "z")))
    </scheme-fragment>

    matches the pattern <verbatim|(foo (:repeat (bar :#1)) :*)>, but not
    <verbatim|(foo (:repeat (bar 'x)) :*)>. The call <verbatim|(match t '(foo
    'x 'y :*))> will return <verbatim|(((x . (bar "x")) (y . (bar "y"))))>.
  </example>

  <\example>
    Consider the grammar

    <\scheme-fragment>
      (define-grammar

      \ \ (:a a b c)

      \ \ (:b (:repeat :a)))
    </scheme-fragment>

    Then the list <verbatim|(a b x y c a a)> matches the pattern
    <verbatim|(:b :2 :b)>.
  </example>

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