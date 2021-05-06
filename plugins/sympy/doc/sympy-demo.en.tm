<TeXmacs|1.99.19>

<style|<tuple|tmdoc|maxima|english|old-spacing|old-dots|old-lengths>>

<\body>
  <\tmdoc-title>
    Example SymPy session
  </tmdoc-title>

  Assuming you have installed SymPy and launched GNU <TeXmacs> with the
  proper Python environment:

  <\session|sympy|default>
    <\output>
      SymPy 1.4 under Python 3.7.3

      Please see the documentation in Help -\<gtr\> Plugins -\<gtr\> SymPy
    </output>

    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      from sympy import *
    </input>

    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      x, t, z, nu = symbols("x t z nu")
    </input>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      diff(sin(x)*exp(x), x)
    <|unfolded-io>
      <math|e<rsup|x>*sin <around*|(|x|)>+e<rsup|x>*cos <around*|(|x|)>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      integrate(exp(x)*sin(x)+exp(x)*cos(x), x)
    <|unfolded-io>
      <math|e<rsup|x>*sin <around*|(|x|)>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      integrate(sin(x**2), (x,-oo,+oo))
    <|unfolded-io>
      <math|<frac|<sqrt|2>*<sqrt|\<pi\>>|2>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      limit(sin(x)/x, x, 0)
    <|unfolded-io>
      <math|1>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      solve(x**2-2, x)
    <|unfolded-io>
      [-sqrt(2), sqrt(2)]
    </unfolded-io>

    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <tmdoc-copyright|2021|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>