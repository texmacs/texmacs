<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Environment primitives>

  A document <re-index|default initial environment> is the typesetter default
  environment modified by the execution of document style and packages. These
  primitives are used to access and modify environment variables.

  <\big-table>
    <tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|assign>>|<cell|<with|mode|math|2>>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|with>>|<cell|<with|mode|math|2*(n+1)+1>>|<cell|Yes>|<cell|Last>|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|value>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Functional>|<cell|Style>>|<row|<cell|<markup|provides>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Functional>|<cell|Style>>>>>
  </big-table|Environment primitives>

  <\description-dash>
    <item*|<markup|assign>>Variable mutation

    <verbatim|(assign <var|var> <var|val>)> sets the environment variable
    named <var|var> (string value) to the value of the <var|val> expression.
    This primitive is used to make non-scoped changes to the environment,
    like defining markup or increasing counters.

    This primitive affects the evaluation process <emdash>through
    <markup|value>, <markup|provides>, and macro definitions<emdash> and the
    typesetting process <emdash>through special typesetter variables.

    <\example>
      Enabling page breaking by style

      The <verbatim|page-medium> is used to enable page breaking. Since only
      the <re-index|initial environment> value for this variable is
      effective, this assignation must occur in a style file, not within a
      document.

      <\scheme-fragment>
        (assign "page-medium" "paper")
      </scheme-fragment>
    </example>

    <\example>
      Setting the chapter counter

      The following snippet will cause the immediately following chapter to
      be number 3. This is useful to get the the numbering right in
      <verbatim|book> style when working with projects and <markup|include>.

      <\scheme-fragment>
        (assign "chapternr" "2")
      </scheme-fragment>
    </example>

    <item*|<markup|with>>Variable scope

    <verbatim|(with <var|var1> <var|val1> ... <var|body>)> temporarily sets
    the environment variable <var|var1> (<abbr|resp.> <var|varN>, all string
    values) to the value of <var|val1> (<abbr|resp.> <var|valN>) and typesets
    <var|body> in the modified environment. All non-scoped change done with
    <markup|assign> to <var|var1> (<abbr|resp.> <var|varN>) within <var|body>
    are reverted at the end of the <markup|with>.

    This primitive is used extensively in style files to modify the
    typesetter environment. For example to locally set the text font, the
    paragraph style, or the mode for mathematics.

    <item*|<markup|value>>Variable value

    <verbatim|(value <var|var>)> evaluates to the current value of the
    environment variable <var|var> (literal string). This is useful to
    display counters and generally to implement environment-sensitive
    behavior.

    <item*|<markup|provides>>Definition predicate

    <verbatim|(provides <var|var>)> evaluates to true if the environment
    variable <var|var> (string value) is defined, and evaluates to false if
    it is not defined.

    That is useful for modular markup, like the <markup|session>
    environments, to fall back to a default appearance when a required
    package is not used in the document.
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