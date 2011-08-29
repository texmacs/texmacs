<TeXmacs|1.0.7.11>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\tmdoc-title>
    Example <name|Macaulay 2> session
  </tmdoc-title>

  Using <menu|Insert|Session|Macaulay2>, you may insert a new
  <name|Macaulay2> session. Here follows a sample session.

  <\session|macaulay2|default>
    <\output>
      Macaulay 2, version 0.9.2

      --Copyright 1993-2001, D. R. Grayson and M. E. Stillman

      --Singular-Factory 1.3b, copyright 1993-2001, G.-M. Greuel, et al.

      --Singular-Libfac 0.3.2, copyright 1996-2001, M. Messollen

      \ Macaulay 2 starting up\ 

      \;
    </output>

    <\unfolded-io|macaulay2] >
      R=QQ[x_1..x_4]
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o2>>|<cell|=>|<cell|R>>|<row|<cell|<text|o2>>|<cell|:>|<cell|<text|PolynomialRing>>>>>
      </leqnarray*>
    </unfolded-io>

    <\unfolded-io|macaulay2] >
      M=matrix{{x_1,x_2},{x_3,x_4}}
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o3>>|<cell|=>|<cell|<around*|(|<tabular*|<tformat|<table|<row|<cell|x<rsub|1>>|<cell|
        x<rsub|2>>>|<row|<cell|x<rsub|3>>|<cell|
        x<rsub|4>>>>>>|)>>>|<row|<cell|<text|o3>>|<cell|:>|<cell|<text|Matrix>>>>>
      </leqnarray*>
    </unfolded-io>

    <\unfolded-io|macaulay2] >
      D=det M
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o4>>|<cell|=>|<cell|-x<rsub|2>
        x<rsub|3>+x<rsub|1> x<rsub|4>>>|<row|<cell|<text|o4>>|<cell|:>|<cell|R>>>>
      </leqnarray*>
    </unfolded-io>

    <\unfolded-io|macaulay2] >
      T=trace M
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o5>>|<cell|=>|<cell|x<rsub|1>+x<rsub|4>>>|<row|<cell|<text|o5>>|<cell|:>|<cell|R>>>>
      </leqnarray*>
    </unfolded-io>

    <\unfolded-io|macaulay2] >
      I=ideal{D,T}
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o6>>|<cell|=>|<cell|<text|--Function-->
        (-x<rsub|2> x<rsub|3>+x<rsub|1> x<rsub|4>,x<rsub|1>+x<rsub|4>)>>|<row|<cell|<text|o6>>|<cell|:>|<cell|<text|Ideal>>>>>
      </leqnarray*>
    </unfolded-io>

    <\unfolded-io|macaulay2] >
      J=ideal M^2
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o7>>|<cell|=>|<cell|<text|--Function-->
        <around|(|x<rsub|1><rsup|2>+x<rsub|2> x<rsub|3>,x<rsub|1>
        x<rsub|3>+x<rsub|3> x<rsub|4>,x<rsub|1> x<rsub|2>+x<rsub|2>
        x<rsub|4>,x<rsub|2> x<rsub|3>+x<rsub|4><rsup|2>|)>>>|<row|<cell|<text|o7>>|<cell|:>|<cell|<text|Ideal>>>>>
      </leqnarray*>
    </unfolded-io>

    <\unfolded-io|macaulay2] >
      radical I== radical J
    <|unfolded-io>
      <\leqnarray*>
        <tformat|<table|<row|<cell|<text|o8>>|<cell|=>|<cell|<text|true>>>|<row|<cell|<text|o8>>|<cell|:>|<cell|<text|Boolean>>>>>
      </leqnarray*>
    </unfolded-io>

    <\input|macaulay2] >
      \;
    </input>
  </session>

  <tmdoc-copyright|2003|Chu-Ching Huang>

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