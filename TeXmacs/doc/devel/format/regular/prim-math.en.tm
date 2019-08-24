<TeXmacs|1.99.11>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Mathematical primitives>

  <\explain>
    <explain-macro|left|large-delimiter>

    <explain-macro|left|large-delimiter|size>

    <explain-macro|left|large-delimiter|bottom|top>

    <explain-macro|mid|large-delimiter|<math|\<cdots\>>>

    <explain-macro|right|large-delimiter|<math|\<cdots\>>><explain-synopsis|large
    delimiters>
  <|explain>
    These primitives are used for producing large delimiters, like in the
    formula

    <\equation*>
      <around*|\<langle\>|<frac|1|a<rsub|1>><mid|\|><frac|1|a<rsub|2>><mid|\|>\<cdots\><mid|\|><frac|1|a<rsub|n>>|\<rangle\>>.
    </equation*>

    Matching left and right delimiters are automatically sized so as contain
    the enclosed expression. Between matching left and right delimiters, the
    formula may contain an arbitrary number of middle delimiters, which are
    sized in a similar way. Contrary to <TeX>, the depth of a large delimiter
    is not necessarily equal to its height, so as to correctly render
    formulas like

    <\equation*>
      f<around*|(|<frac|1|x+<frac|1|y+<frac|1|z>>>|)>
    </equation*>

    The user may override the automatically determined size by specifying
    additional length parameters <src-arg|size> or <src-arg|bottom> and
    <src-arg|top>. For instance,

    <\tm-fragment>
      <inactive*|f<left|(|-8mm|4mm>x<mid|\||8mm>y<right|)|-4mm|8mm>>
    </tm-fragment>

    is rendered as

    <\equation*>
      f<left|(|-8mm|4mm>x<mid|\||8mm>y<right|)|-4mm|8mm>
    </equation*>

    The <src-arg|size> may also be a number <math|n>, in which case the
    <math|n>-th available size for the delimiter is taken. For instance,

    <\tm-fragment>
      <inactive*|g<left|(|0><left|(|1><left|(|2><left|(|3>z<right|)|3><right|)|2><right|)|1><right|)|0>>
    </tm-fragment>

    is rendered as

    <\equation*>
      g<left|(|0><left|(|1><left|(|2><left|(|3>z<right|)|3><right|)|2><right|)|1><right|)|0>
    </equation*>
  </explain>

  \;

  <\explain>
    <explain-macro|big|big-symbol><explain-synopsis|big symbols>
  <|explain>
    This primitive is used in order to produce big operators as in

    <\equation>
      <label|big-example><big|sum><rsub|i=0><rsup|\<infty\>>a<rsub|i>*z<rsup|i>
    </equation>

    The size of the operator depends on whether the formula is rendered in
    ``display style'' or not. Formulas in separate equations, like
    (<reference|big-example>), are said to be rendered in display style,
    contrary to formulas which occur in the main text, like
    <math|<big|sum><rsub|i=0><rsup|\<infty\>>a<rsub|i>*z<rsup|i>>. The user
    may use <menu|Format|Display style> to override the current settings.

    Notice that the formula (<reference|big-example>) is internally
    represented as

    <\tm-fragment>
      <inactive*|<big|sum><rsub|i=0><rsup|\<infty\>>a<rsub|i>*z<rsup|i><big|.>>
    </tm-fragment>

    The invisible big operator <inactive*|<big|.>> is used to indicate the
    end of the scope of <inactive*|<big|sum>>.
  </explain>

  <\explain>
    <explain-macro|frac|num|den><explain-synopsis|fractions>
  <|explain>
    The <markup|frac> primitive is used in order to render fractions like
    <math|<frac|x|y>>. In display style, the numerator <src-arg|num> and
    denominator <src-arg|den> are rendered in the normal size, but display
    style is turned of when typesetting <src-arg|num> and <src-arg|den>. When
    the display style is turned of, then the arguments are rendered in script
    size. For instance, the content

    <\tm-fragment>
      <inactive*|<frac|1|a<rsub|0>+<frac|1|a<rsub|1>+<frac|1|a<rsub|2>+\<ddots\>>>>>
    </tm-fragment>

    is rendered in display style as

    <\equation*>
      <frac|1|a<rsub|0>+<frac|1|a<rsub|1>+<frac|1|a<rsub|2>+\<ddots\>>>>
    </equation*>
  </explain>

  <\explain>
    <explain-macro|sqrt|content>

    <explain-macro|sqrt|content|n><explain-synopsis|roots>
  <|explain>
    The <markup|sqrt> primitive is used in order to render square roots like
    <math|<sqrt|x>> or <src-arg|n>-th roots like <math|<sqrt|x|3>>. The root
    symbol is automatically sized so as to encapsulate the <src-arg|content>:

    <\equation*>
      <sqrt|<frac|f<around|(|x|)>|y<rsup|2>+z<rsup|2>>|i+j>
    </equation*>
  </explain>

  <\explain>
    <explain-macro|lsub|script>

    <explain-macro|lsup|script>

    <explain-macro|rsub|script>

    <explain-macro|rsup|script><explain-synopsis|scripts>
  <|explain>
    These primitives are used in order to attach a <src-arg|script> to the
    preceding box in a horizontal concatenation (in the case of right
    scripts) or the next one (in the case of left scripts). When there is no
    such box, then the script is attached to an empty box. Moreover, when
    both a subscript and a superscript are specified on the same side, then
    they are merged together. For instance, the expression

    <\tm-fragment>
      <inactive*|<rsub|a><rsup|b>+<lsub|1><lsup|2>x<rsub|3><rsup|4>=y<rsub|1>+<lsub|c>>
    </tm-fragment>

    is rendered as

    <\equation*>
      <rsub|a><rsup|b>+<lsub|1><lsup|2>x<rsub|3><rsup|4>=y<rsub|1>+<lsub|c>
    </equation*>

    When a right script is attached to an operator (or symbol) which accepts
    limits, then it is rendered below or above instead of beside the
    operator:

    <\equation*>
      lim<rsub|n\<rightarrow\>\<infty\>>a<rsub|n>
    </equation*>

    Scripts are rendered in a smaller font in non-display style.
    Nevertheless, in order to keep formulas readable, the size is not reduced
    below script-script-size.
  </explain>

  <\explain>
    <explain-macro|lprime|prime-symbols>

    <explain-macro|rprime|prime-symbols><explain-synopsis|primes>
  <|explain>
    Left and right primes are similar to left and right superscripts, except
    that they behave in a different way when being edited. For instance, when
    your cursor is behind the prime symbol in <math|f<rprime|'>> and you
    press backspace, then the prime is removed. If you are behind
    <math|f<rsup|n>> and you press backspace several times, then you first
    enter the superscript, next remove <math|n> and finally remove the
    superscript. Notice also that <src-arg|prime-symbols> is necessarily a
    string of concatenated prime symbols. For instance,
    <math|f<rprime|'\<dag\>>> is represented by
    <inactive*|f<rprime|'\<dag\>>>.
  </explain>

  <\explain>
    <explain-macro|below|content|script>

    <explain-macro|above|content|script><explain-synopsis|scripts above and
    below>
  <|explain>
    The <markup|below> and <markup|above> tags are used to explicitly attach
    a <src-arg|script> below or above a given <src-arg|content>. Both can be
    mixed in order to produce content with both a script below and above:

    <\equation*>
      <above|<below|xor|i=1>|\<infty\>> x<rsub|i>
    </equation*>

    can be produced using

    <\tm-fragment>
      <inactive*|<above|<below|xor|i=1>|\<infty\>> x<rsub|i>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|wide|content|wide-symbol>

    <explain-macro|wide*|content|wide-symbol><explain-synopsis|wide symbols>
  <|explain>
    These primitives can be used in order to produce wide accents above or
    below some mathematical <src-arg|content>. For instance
    <math|<wide|x+y|\<bar\>>> corresponds to the markup
    <inactive*|<wide|x+y|\<bar\>>>.
  </explain>

  <\explain>
    <explain-macro|neg|content><explain-synopsis|negations>
  <|explain>
    This primitive is mainly used for producing negated symbols or
    expressions, such as <math|<neg|\<rightarrowtail\>>> or <math|<neg|a>>.
  </explain>

  <\explain>
    <explain-macro|tree|root|child-1|<math|\<cdots\>>|child-n><explain-synopsis|trees>
  <|explain>
    This primitive is used to produce a tree with a given <src-arg|root> and
    children <src-arg|child-1> until <src-arg|child-n>. The primitive should
    be used recursively in order to produce trees. For instance,

    <\equation*>
      <tree|+|x|y|<tree|\<times\>|2|y|z>>
    </equation*>

    corresponds to the markup

    <\tm-fragment>
      <inactive*|<tree|+|x|y|<tree|\<times\>|2|y|z>>>
    </tm-fragment>

    In the future, we plan to provide further style parameters in order to
    control the rendering.
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