<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Mathematical environments>

  The <tmdtd|env-math> <abbr|d.t.d.> specifies which mathematical
  environments can be used inside text-mode. In other words, the environments
  should be used inside text-mode, but their bodies contain mathematical
  formulas or tables of mathematical formulas.

  <\explain|<explain-macro|equation|body>>
    A numbered equation. Use a <markup|label> to be able to
    <markup|reference> this equation elsewhere.
  </explain>

  <\explain|<explain-macro|equation*|body>>
    An unnumbered equation.
  </explain>

  <\explain|<explain-macro|equation-lab|body|lab>>
    An equation with an arbitrary text label to be displayed in references
    (e.g. <markup|reference> or <markup|eqref)>. Notice that the
    <markup|label> tag is not created automatically.
  </explain>

  <\explain|<explain-macro|eqnarray|table>>
    An array of numbered equations (not yet implemented).
  </explain>

  <\explain|<explain-macro|eqnarray*|table>>
    An array of unnumbered equations. You can use the <markup|eq-number> tag
    in order to number the equation.\ 
  </explain>

  <\warning>
    The numbering of equations inside tables is not yet as it should be. In
    particular, the <markup|eqnarray> tag is equivalent to <markup|eqnarray*>
    at the moment. Later on, when the <markup|eqnarray> tag will be
    implemented correctly, you will also have a <markup|no-number> tag in
    order to suppress the number of an equation, and a style package for
    numbering equations at the left hand side.
  </warning>

  <\warning>
    There is no option for numbering equations at the left hand side
    available yet. Nevertheless, you may use the manual tag
    <markup|leq-number> for this. You also have a tag <markup|next-number>
    which directly display the next number and increases the equation
    counter.
  </warning>

  <\warning>
    We do not encourage the use of the AMS-<TeX> environments
    <verbatim|align>, <verbatim|gather> and <verbatim|split>. Nevertheless,
    they are available under the names <markup|align>, <markup|gather>,
    <markup|eqsplit> together with their variants <markup|align*>,
    <markup|gather*> and <markup|eqsplit*>. In the future, we plan to provide
    more powerful environments.
  </warning>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>