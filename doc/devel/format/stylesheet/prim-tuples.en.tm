<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Operations on tuples>

  <\explain>
    <explain-macro|tuple|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|construct
    a tuple>
  <|explain>
    Forms a tuple from the expressions <src-arg|expr-1> until
    <src-arg|expr-n>.
  </explain>

  <\explain>
    <explain-macro|is-tuple|expr><explain-synopsis|tuple predicate>
  <|explain>
    Tests whether a given expression <src-arg|expr> evaluates to a tuple.
  </explain>

  <\explain>
    <explain-macro|length|expr><explain-synopsis|length of a tuple>
  <|explain>
    If <src-arg|expr> is a tuple, then we return its arity. For instance,
    <inactive*|<length|<tuple|hop|hola>>> evaluates to
    <length|<tuple|hop|hola>>.
  </explain>

  <\explain>
    <explain-macro|look-up|tuple|which><explain-synopsis|access an entry in a
    tuple>
  <|explain>
    Returns the element with index <src-arg|which> in <src-arg|tuple>. For
    instance, <inactive*|<look-up|<tuple|a|b|c>|1>> yields
    <look-up|<tuple|a|b|c>|1>.
  </explain>

  <\explain>
    <explain-macro|range|expr|start|end><explain-synopsis|extract a subtuple>
  <|explain>
    Return the subtuple of <src-arg|expr> starting at position
    <src-arg|start> and ending at position <src-arg|end> (not included). For
    instance, <inactive*|<range|<tuple|a|hola|hop|b|c>|2|4>> evaluates to
    <range|<tuple|a|hola|hop|b|c>|2|4>.
  </explain>

  <\explain>
    <explain-macro|merge|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|concatenate
    tuples>
  <|explain>
    This primitive may be used to concatenate several tuples <src-arg|expr-1>
    until <src-arg|expr-n>. For instance,
    <inactive*|<merge|<tuple|1|2>|<tuple|3|4|5>>> produces
    <merge|<tuple|1|2>|<tuple|3|4|5>>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>