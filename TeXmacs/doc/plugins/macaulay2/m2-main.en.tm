<TeXmacs|1.0.1.20>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\expand|tmdoc-title>
    Using Macaulay 2 sessions inside <TeXmacs>
  </expand>

  <name|Macaulay 2> is a software system devoted to supporting research in
  algebraic geometry and commutative algebra. The software is available now
  in source code for porting, and in compiled form for <name|Linux>,
  <name|Sun OS>, <name|Solaris>, <name|Windows>, and a few other unix
  machines. You can get it from

  <\verbatim>
    \ \ \ \ http://www.math.uiuc.edu/Macaulay2
  </verbatim>

  Here follows a sample session, which was started using
  <apply|menu|Insert|Session|Macaulay 2>:

  <\session|macaulay2|default>
    <\output>
      Macaulay 2, version 0.9.2

      --Copyright 1993-2001, D. R. Grayson and M. E. Stillman

      --Singular-Factory 1.3b, copyright 1993-2001, G.-M. Greuel, et al.

      --Singular-Libfac 0.3.2, copyright 1996-2001, M. Messollen

      \ Macaulay 2 starting up\ 

      \;
    </output>

    <\input|macaulay2] >
      R=QQ[x_1..x_4]
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o2>>|<cell|=>|<cell|R>>|<row|<cell|<with|mode|text|o2>>|<cell|:>|<cell|<with|mode|text|PolynomialRing>>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      M=matrix{{x_1,x_2},{x_3,x_4}}
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o3>>|<cell|=>|<cell|<left|(><expand|tabular*|<tformat|<table|<row|<cell|x<rsub|1>>|<cell|
        x<rsub|2>>>|<row|<cell|x<rsub|3>>|<cell|
        x<rsub|4>>>>>><right|)>>>|<row|<cell|<with|mode|text|o3>>|<cell|:>|<cell|<with|mode|text|Matrix>>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      D=det M
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o4>>|<cell|=>|<cell|-x<rsub|2>
        x<rsub|3>+x<rsub|1> x<rsub|4>>>|<row|<cell|<with|mode|text|o4>>|<cell|:>|<cell|R>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      T=trace M
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o5>>|<cell|=>|<cell|x<rsub|1>+x<rsub|4>>>|<row|<cell|<with|mode|text|o5>>|<cell|:>|<cell|R>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      I=ideal{D,T}
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o6>>|<cell|=>|<cell|<with|mode|text|--Function-->
        (-x<rsub|2> x<rsub|3>+x<rsub|1> x<rsub|4>,x<rsub|1>+x<rsub|4>)>>|<row|<cell|<with|mode|text|o6>>|<cell|:>|<cell|<with|mode|text|Ideal>>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      J=ideal M^2
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o7>>|<cell|=>|<cell|<with|mode|text|--Function-->
        (x<rsub|1><rsup|2>+x<rsub|2> x<rsub|3>,x<rsub|1> x<rsub|3>+x<rsub|3>
        x<rsub|4>,x<rsub|1> x<rsub|2>+x<rsub|2> x<rsub|4>,x<rsub|2>
        x<rsub|3>+x<rsub|4><rsup|2>)>>|<row|<cell|<with|mode|text|o7>>|<cell|:>|<cell|<with|mode|text|Ideal>>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      radical I== radical J
    </input>

    <\output>
      <\expand|leqnarray*>
        <tformat|<table|<row|<cell|<with|mode|text|o8>>|<cell|=>|<cell|<with|mode|text|true>>>|<row|<cell|<with|mode|text|o8>>|<cell|:>|<cell|<with|mode|text|Boolean>>>>>
      </expand>
    </output>

    <\input|macaulay2] >
      \;
    </input>
  </session>

  <apply|tmdoc-copyright|2003|Chu-Ching Huang>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-10|<tuple|8.2|?>>
    <associate|toc-11|<tuple|8.3|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-12|<tuple|8.4|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|toc-13|<tuple|8.5|?>>
    <associate|idx-3|<tuple|3|?>>
    <associate|gly-4|<tuple|4|?>>
    <associate|toc-14|<tuple|8.6|?>>
    <associate|idx-4|<tuple|7|?>>
    <associate|gly-5|<tuple|5|?>>
    <associate|toc-15|<tuple|8.7|?>>
    <associate|idx-5|<tuple|8|?>>
    <associate|toc-16|<tuple|8.8|?>>
    <associate|gly-6|<tuple|6|?>>
    <associate|gly-7|<tuple|7|?>>
    <associate|gly-8|<tuple|8|?>>
    <associate|gly-9|<tuple|9|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|7|?>>
    <associate|toc-8|<tuple|8|?>>
    <associate|toc-9|<tuple|8.1|?>>
  </collection>
</references>
