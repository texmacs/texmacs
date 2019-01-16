<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Evaluation control primitives>

  This section describes several primitives for controlling the way
  expressions in the style-sheet language are evaluated. The primitives are
  analogous to the <scheme> primitives <verbatim|eval>, <verbatim|quote>,
  <verbatim|quasiquote>, <abbr|etc.>, although the <TeXmacs> conventions are
  slightly <hlink|different|../../style/design/style-eval.en.tm> than those
  used by conventional functional languages like <name|Scheme>.

  <\explain>
    <explain-macro|eval|expr><explain-synopsis|force evaluation>
  <|explain>
    Typeset the result of the evaluation of <src-arg|expr>. This primitive is
    usually combined with a tag like <markup|quote> or <markup|quasiquote>
    for delaying the evaluation.
  </explain>

  <\explain>
    <explain-macro|quote|expr><explain-synopsis|delayed evaluation>
  <|explain>
    Evaluation of the expression <explain-macro|quote|expr> yields
    <src-arg|expr> itself. This kind of delayed evaluation may be useful in
    combination with the <markup|eval> primitive which forces evaluation.
  </explain>

  <\explain>
    <explain-macro|quasiquote|expr><explain-synopsis|delay evaluation and
    substitution>
  <|explain>
    This tag is a variant of the <markup|quote> tag, which returns the
    expression <src-arg|expr> in which all subexpressions of the form
    <explain-macro|unquote|subexpr> have been replaced by the evaluations of
    <src-arg|subexpr>. For instance,

    <\tm-fragment>
      <inactive*|<assign|hello|<quasiquote|<macro|name|<unquote|<localize|Hello>>
      <arg|name>.>>>>
    </tm-fragment>

    may be used to define a macro <markup|hello> whose value is localized for
    the current language. In a French document, the declaration would
    typically be equivalent to

    <\tm-fragment>
      <inactive*|<assign|hello|<macro|name|Bonjour <arg|name>.>>>
    </tm-fragment>

    Notice however that it is usually better not to use the
    <markup|quasiquote> primitive for such applications. When defining

    <\tm-fragment>
      <inactive*|<assign|hello|<macro|name|<localize|Hello> <arg|name>.>>>
    </tm-fragment>

    the typesetting of <inactive*|<hello|Name>> would naturally adapt itself
    to the current language, while the above version would always use the
    language at the moment of the definition of the macro. Nevertheless, the
    first form does have the advantage that the localization of the word
    ``Hello'' only has to be computed once, when the macro is defined.
    Therefore, the <markup|quasiquote> primitive may sometimes be used in
    order to improve performance.
  </explain>

  <\explain>
    <explain-macro|unquote|subexpr><explain-synopsis|mark substitutable
    subexpressions>
  <|explain>
    This tag is used in combination with <markup|quasiquote> and
    <markup|quasi> in order to mark the subexpressions which need to be
    evaluated.
  </explain>

  <\explain>
    <explain-macro|unquote*|subexprs><explain-synopsis|unquote splicing>
  <|explain>
    This tag is similar to <markup|unquote>, except that the argument
    <src-arg|subexprs> now evaluates to a list of subexpressions, which are
    inserted into the arguments of the parent node. For instance, consider
    the macro

    <\tm-fragment>
      <inactive*|<assign|fun|<xmacro|x|<style-with|src-compact|none|<quasi|<tree|dup|<unquote*|<quote-arg|x>>|<unquote*|<quote-arg|x>>>>>>>>
    </tm-fragment>

    Then <inactive*|<fun|a|b|c>> is typeset as

    <\equation*>
      <with|fun|<xmacro|x|<quasi|<tree|dup|<unquote*|<quote-arg|x>>|<unquote*|<quote-arg|x>>>>>|<fun|a|b|c>>
    </equation*>
  </explain>

  <\explain>
    <explain-macro|quasi|expr><explain-synopsis|substitution>
  <|explain>
    This tag is a shortcut for <explain-macro|eval|<with|font-shape|right|<explain-macro|quasiquote|expr>>>.
    This primitive is often used in the <TeXmacs> style files in order to
    write macros which define sets of other macros. For instance, the macro

    <\tm-fragment>
      <inactive*|<assign|new-theorem|<macro|name|text|<quasi|<assign|<unquote|name>|<macro|body|<surround|<no-indent><strong|<unquote|<arg|text>>.
      >|<right-flush>|<arg|body>>>>>>>>
    </tm-fragment>

    may be used in order to define new theorem-like environments.
  </explain>

  <\explain>
    <explain-macro|quote-value|var><explain-synopsis|retrieve a value but
    don't evaluate>
  <|explain>
    When retrieving an environment variable <src-arg|var>, one is usually
    interested in its typeset value, as given by <explain-macro|value|var>.
    In some cases, it may be useful to access the real, non-typeset value.
    This can be done with <explain-macro|quote-value|var>.
  </explain>

  <\explain>
    <explain-macro|quote-arg|var|index-1|<math|\<cdots\>>|index-n><explain-synopsis|retrieve
    an argument but don't evaluate>
  <|explain>
    When retrieving (a subexpression of) a macro argument <src-arg|var>, one
    is usually interested in its typeset value, as given by
    <explain-macro|arg|var|index-1|<math|\<cdots\>>|index-n>. In some cases,
    it may be useful to access the real, non-typeset value. This can be done
    with <explain-macro|quote-arg|var|index-1|<math|\<cdots\>>|index-n>.
  </explain>

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