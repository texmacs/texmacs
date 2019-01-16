<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Customized mathematical semantics>

  We have done our best to support most of the classical mathematical
  notations. Nevertheless, the user may sometimes want to define notations
  with a non standard semantics. Certain areas may also require special
  notations which are not supported by default.

  <TeXmacs> provides a very simple <markup|syntax> primitive, which allows
  the user to manually override the default syntactical semantics of a
  formula. Assuming that semantic editing was activated, you may insert the
  <markup|syntax> primitive using <shortcut|(make 'syntax)> or
  <menu|Insert|Semantics|Other>. The first argument contains the formula as
  it should be displayed, whereas the second argument contains the formula as
  it should be interpreted.

  For instance, if we enter <math|\<cal-R\>> as the first argument and
  <math|\<less\>> as the second one, then the <math|\<cal-R\>> will be
  interpreted as a binary relation, exactly in the same way as
  <math|\<less\>>. Moreover, the spacing around <math|\<cal-R\>> will be
  adapted, so as to mimic the spacing around <math|\<less\>>. In this
  particular example, we might have obtained the same result by using the
  <markup|math-relation> primitive, which is equivalent to <markup|syntax>
  with<nbsp><math|\<less\>> as its second argument. Most standard operator
  types are available from <menu|Insert|Semantics>, or using the
  <prefix|math:syntax> keyboard prefix. In particular, you may use
  <shortcut|(make 'math-ignore)> to simply ignore a formula and
  <shortcut|(make 'math-ordinary)> in order to make the formula behave as an
  ordinary symbol (such as the letter \Po\Q).

  The <markup|syntax> primitive is especially powerful when used in
  combination with the <TeXmacs> macro language. For instance, consider the
  formula <math|C=1/2*\<mathpi\>*\<mathi\>*<big|oint>f<around*|(|z|)>*\<mathd\>
  z>. It is likely that the intended interpretation of
  <math|1/2*\<mathpi\>*\<mathi\>> is <math|1/<around*|(|2*\<mathpi\>*\<mathi\>|)>>
  and not <math|<around*|(|1/2|)>*\<mathpi\>*\<mathi\>>. Therefore, if we
  often use the constant <math|2*\<mathpi\>*\<mathi\>>, then we might want to
  define a macro<nbsp><markup|twopii> by

  <\tm-fragment>
    <inactive|<assign|twopii|<inactive|<macro|<inactive|<syntax|<math|2*\<pi\>*\<mathi\>>|<math|(2*\<pi\>*\<mathi\>)>>>>>>>
  </tm-fragment>

  Such macros may be grouped together into a style package with the user's
  favourite notations. Future versions of <TeXmacs> might also provide style
  packages with notations dedicated to specific<nbsp>areas.

  Let us finally notice that there are usually several ways for redefining
  the semantics of a formula. For instance, an alternative way to define the
  macro <markup|twopii> is using

  <\tm-fragment>
    <inactive|<assign|twopii|<inactive|<macro|<math|<around*|\<nobracket\>|2*\<mathpi\>*\<mathi\>|\<nobracket\>>>>>>>
  </tm-fragment>

  where we inserted a pair of invisible brackets around
  <math|2*\<mathpi\>*\<mathi\>>. Similarly, in the formula

  <\equation*>
    \<mathe\><rsup|<sqrt|x>+\<mathe\><rsup|<sqrt|log
    x>+\<mathe\><rsup|<sqrt|log log x>+<math-ordinary|\<udots\>\<ddots\>>+log
    log log x>+log log x>+log x>,
  </equation*>

  we may either select the whole formula and give it the semantics of an
  ordinary symbol, by pressing<nbsp><shortcut|(make 'math-ordinary)>.
  However, a nicer solution is to only select the subformula
  <math|<math-ordinary|\<udots\>\<ddots\>>>, and give it the semantics of an
  ordinary symbol. Yet another example is the sign sequence <math|++-+-+>
  mentioned earlier. This sequence can be interpreted correctly by inserting
  invisible separators between the different signs using the <key|, space>
  shortcut.

  <tmdoc-copyright|2011|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>