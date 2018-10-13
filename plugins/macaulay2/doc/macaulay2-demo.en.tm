<TeXmacs|1.99.8>

<style|<tuple|tmdoc|maxima|english|old-spacing|macaulay2>>

<\body>
  <\tmdoc-title>
    Example <name|Macaulay 2> session
  </tmdoc-title>

  Using <menu|Insert|Session|Macaulay2>, you may insert a new <name|Macaulay
  2> session. Here follows a sample session.

  <\session|macaulay2|default>
    <\output>
      <\errput>
        Macaulay2, version 1.12

        with packages: ConwayPolynomials, Elimination, IntegralClosure,
        InverseSystems,

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ LLLBases,
        PrimaryDecomposition, ReesAlgebra, TangentCone
      </errput>
    </output>

    <\unfolded-io>
      i1 :\ 
    <|unfolded-io>
      R=QQ[x_1..x_4]
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o1>>|<cell|<with|color|red|=>>|<cell|R>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o1>>|<cell|<with|color|red|:>>|<cell|PolynomialRing>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      i2 :\ 
    <|unfolded-io>
      M=matrix{{x_1,x_2},{x_3,x_4}}
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o2>>|<cell|<with|color|red|=>>|<cell|<around*|(|<tabular|<table|<row|<cell|x<rsub|1>>|<cell|x<rsub|2>>>|<row|<cell|x<rsub|3>>|<cell|x<rsub|4>>>>>|)>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o2>>|<cell|<with|color|red|:>>|<cell|Matrix>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      i3 :\ 
    <|unfolded-io>
      D=det M
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o3>>|<cell|<with|color|red|=>>|<cell|-x<rsub|2>x<rsub|3>+x<rsub|1>x<rsub|4>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o3>>|<cell|<with|color|red|:>>|<cell|R>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      i4 :\ 
    <|unfolded-io>
      T=trace M
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o4>>|<cell|<with|color|red|=>>|<cell|x<rsub|1>+x<rsub|4>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o4>>|<cell|<with|color|red|:>>|<cell|R>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      i5 :\ 
    <|unfolded-io>
      I=ideal{D,T}
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o5>>|<cell|<with|color|red|=>>|<cell|ideal<around*|(|-x<rsub|2>x<rsub|3>+x<rsub|1>x<rsub|4>,x<rsub|1>+x<rsub|4>|)>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o5>>|<cell|<with|color|red|:>>|<cell|Ideal>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      i6 :\ 
    <|unfolded-io>
      J=ideal M^2
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o6>>|<cell|<with|color|red|=>>|<cell|ideal<around*|(|x<rsub|1><rsup|2>+x<rsub|2>x<rsub|3>,x<rsub|1>x<rsub|3>+x<rsub|3>x<rsub|4>,x<rsub|1>x<rsub|2>+x<rsub|2>x<rsub|4>,x<rsub|2>x<rsub|3>+x<rsub|4><rsup|2>|)>>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o6>>|<cell|<with|color|red|:>>|<cell|Ideal>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      i7 :\ 
    <|unfolded-io>
      radical I== radical J
    <|unfolded-io>
      <html-text|<math|<tabular|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|l>|<table|<row|<cell|<with|color|red|o7>>|<cell|<with|color|red|=>>|<cell|true>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|color|red|o7>>|<cell|<with|color|red|:>>|<cell|Boolean>>>>>>>
    </unfolded-io>

    <\input>
      i8 :\ 
    <|input>
      \;
    </input>
  </session>

  <tmdoc-copyright|2018|Chu-Ching Huang, Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>