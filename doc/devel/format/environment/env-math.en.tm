<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Typesetting mathematics>

  <\explain>
    <label|math-level><var-val|math-level|0><explain-synopsis|index level>
  <|explain>
    The <em|index level> increases inside certain mathematical constructs
    such as indices and fractions. When the index level is high, formulas are
    rendered in a smaller font. Nevertheless, index levels higher than
    <verbatim|2> are all rendered in the same way as index level
    <verbatim|2>; this ensures that formulas like

    <\equation*>
      \<mathe\><rsup|\<mathe\><rsup|\<mathe\><rsup|\<mathe\><rsup|x>>>>=<frac|1+<frac|1|x+\<mathe\><rsup|x>>|1+<frac|1|\<mathe\><rsup|x>+<frac|1|\<mathe\><rsup|\<mathe\><rsup|x>>>>>
    </equation*>

    remain readable. The index level may be manually changed in
    <menu|Format|Index level>, so as to produce formulas like

    <\equation*>
      x<rsup|<with|math-level|0|y<rsup|<with|math-level|0|z>>>>
    </equation*>

    <\tm-fragment>
      <with|mode|math|<inactive*|x<rsup|<with|math-level|0|y<rsup|<with|math-level|0|z>>>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|math-display|false><explain-synopsis|display style>
  <|explain>
    This environement variable controls whether we are in <em|display style>
    or not. Formulas which occur on separate lines like

    <\equation*>
      <frac|1|H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)>=<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>
    </equation*>

    are usually typeset in display style, contrary to inline formulas like
    <with|mode|math|<frac|1|H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)>=<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>>.
    As you notice, formulas in display style are rendered using a wider
    spacing. The display style is disabled in several mathematical constructs
    such as scripts, fractions, binomial coefficients, and so on. As a
    result, the double numerators in the formula

    <\equation*>
      H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)=<frac|1|<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>>
    </equation*>

    are typeset in a smaller font. You may override the default settings
    using <menu|Format|Display style>.
  </explain>

  <\explain>
    <var-val|math-condensed|false><explain-synopsis|condensed display style>
  <|explain>
    By default, formulas like <with|mode|math|a+\<cdots\>+z> are typeset
    using a nice, wide spacing around the <with|mode|math|<op|+>> symbol. In
    formulas with scripts like <with|mode|math|\<mathe\><rsup|a+\<cdots\>+z>+\<mathe\><rsup|\<alpha\>+\<cdots\>+\<zeta\>>>
    the readability is further enhanced by using a more condensed spacing
    inside the scripts: this helps the reader to distinguish symbols
    occurring in the scripts from symbols occurring at the ground level when
    the scripts are long. The default behaviour can be overridden using
    <menu|Format|Condensed>.
  </explain>

  <\explain>
    <var-val|math-vpos|0><explain-synopsis|position in fractions>
  <|explain>
    For a high quality typesetting of fraction, it is good to avoid
    subscripts in numerators to descend to low and superscripts in
    denominators to ascend to high. <TeXmacs> therefore provides an
    additional environment variable <src-var|math-vpos> which takes the value
    <with|mode|math|1> inside numerators, <with|mode|math|\<um\>1> inside
    denominators and <with|mode|math|0> otherwise. In order to see the effect
    of other settings, consider the following formulas:

    <\eqnarray*>
      <tformat|<table|<row|<cell|<with|math-vpos|-1|a<rsub|1><rsup|2>>+<with|math-vpos|0|a<rsub|2><rsup|2>>+<with|math-vpos|1|a<rsub|3><rsup|2>>>|<cell|\<longleftrightarrow\>>|<cell|<inactive*|<with|math-vpos|-1|<active*|a<rsub|1><rsup|2>>>+<with|math-vpos|0|<active*|a<rsub|2><rsup|2>>>+<with|math-vpos|1|<active*|a<rsub|3><rsup|2>>>>>>|<row|<cell|<frac|<with|math-vpos|1|a<rsub|1><rsup|2>+\<cdots\>+\<alpha\><rsub|n><rsup|2>>|<with|math-vpos|-1|b<rsub|tot><rsup|2>>>>|<cell|\<longleftrightarrow\>>|<cell|<inactive*|<frac|<with|math-vpos|1|<active*|a<rsub|1>+\<cdots\>+\<alpha\><rsub|n>>>|<with|math-vpos|-1|<active*|b<rsup|2>>>>>>>|<row|<cell|<frac|<with|math-vpos|0|a<rsub|1><rsup|2>+\<cdots\>+\<alpha\><rsub|n><rsup|2>>|<with|math-vpos|0|b<rsub|tot><rsup|2>>>>|<cell|\<longleftrightarrow\>>|<cell|<inactive*|<frac|<with|math-vpos|0|<active*|a<rsub|1>+\<cdots\>+\<alpha\><rsub|n>>>|<with|math-vpos|0|<active*|b<rsup|2>>>>>>>|<row|<cell|<frac|<with|math-vpos|-1|a<rsub|1><rsup|2>+\<cdots\>+\<alpha\><rsub|n><rsup|2>>|<with|math-vpos|1|b<rsub|tot><rsup|2>>>>|<cell|\<longleftrightarrow\>>|<cell|<inactive*|<frac|<with|math-vpos|-1|<active*|a<rsub|1>+\<cdots\>+\<alpha\><rsub|n>>>|<with|math-vpos|1|<active*|b<rsup|2>>>>>>>>>
    </eqnarray*>
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>