<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Evaluation control>

  The <menu|Source|Evaluation> menu contains several primitives to control
  the way expressions in the style-sheet language are evaluated. The most
  frequent use of these primitives is when you want to write a ``meta-macro''
  like <markup|new-theorem> which is used for defining or computing on other
  macros. For instance:

  <\tm-fragment>
    <inactive*|<assign|new-theorem|<macro|name|text|<quasi|<assign|<unquote|name>|<macro|body|<surround|<no-indent><strong|<unquote|<arg|text>>.
    >|<right-flush>|<arg|body>>>>>>>>
  </tm-fragment>

  When calling <inactive*|<new-theorem|theorem|Theorem>> in this example, we
  first evaluate all <markup|unquote> instructions inside the <markup|quasi>
  primitive, which yields the expression

  <\tm-fragment>
    <inactive*|<assign|theorem|<macro|body|<surround|<no-indent><strong|Theorem.
    >|<right-flush>|<arg|body>>>>>
  </tm-fragment>

  Next, this expression is evaluated, thereby defining a macro
  <markup|theorem>.

  It should be noticed that the <TeXmacs> conventions for evaluation are
  slightly different then those from conventional functional languages like
  <name|Scheme>. The subtle differences are motivated by our objective to
  make it as easy as possible for the user to write macros for typesetting
  purposes.

  For instance, when <TeXmacs> calls a macro
  <inactive*|<macro|<active*|x<rsub|1>>|<active*|<with|mode|math|\<cdots\>>>|<active*|x<rsub|n>>|body>>
  with arguments <verbatim|y<rsub|1>> until <verbatim|y<rsub|n>>, the
  argument variables <src-arg|x<rsub|1>> until <src-arg|x<rsub|n>> are bound
  to the unevaluated expressions <verbatim|y<rsub|1>> until
  <verbatim|y<rsub|n>>, and the body is evaluated with these bindings. The
  evaluation of <verbatim|y<rsub|i>> takes place each time we request for the
  argument <no-break><src-arg|x<rsub|i>>. In particular, when applying the
  macro <inactive*|<macro|x|<arg|x> and again <arg|x>>> to an expression
  <verbatim|y>, the expression <no-break><verbatim|y> is evaluated twice.

  In <name|Scheme>, the bodies of <name|Scheme> macros are evaluated twice,
  whereas the arguments of functions are evaluated. On the other hand, when
  retrieving a variable (whether it is an argument or an environment
  variable), the value is not evaluated. Consequently, a <TeXmacs> macro

  <\tm-fragment>
    <inactive*|<assign|foo|<macro|x|<blah|<arg|x>|<arg|x>>>>>
  </tm-fragment>

  would correspond to a <name|Scheme> macro

  <\scheme-fragment>
    (define-macro (foo x)

    \ \ `(let ((x (lambda () ,x)))

    \ \ \ \ \ (blah (x) (x)))
  </scheme-fragment>

  Conversely, the <name|Scheme> macro and function

  <\scheme-fragment>
    (define-macro (foo x) (blah x x))

    (define (fun x) (blah x x))
  </scheme-fragment>

  admit the following analogues in <TeXmacs>:

  <\tm-fragment>
    <\inactive*>
      <assign|foo|<macro|x|<eval|<blah|<quote-arg|x>|<quote-arg|x>>>>>
    </inactive*>

    <\inactive*>
      <assign|fun|<macro|x|<with|x*|<arg|x>|<blah|<quote-value|x*>|<quote-value|x*>>>>>
    </inactive*>
  </tm-fragment>

  Here the primitives <markup|quote-arg> and <markup|quote-value> are used to
  retrieve the value of an argument <abbr|resp.> an environment variable. The
  <TeXmacs> primitives <markup|eval>, <markup|quote>, <markup|quasiquote> and
  <markup|unquote> behave in the same way as their <name|Scheme> analogues.
  The <markup|quasi> primitive is a shortcut for quasi-quotation followed by
  evaluation.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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