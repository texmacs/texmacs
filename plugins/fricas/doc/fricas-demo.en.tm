<TeXmacs|1.0.7.16>

<style|tmdoc>

<\body>
  <tmdoc-title|Example of a <name|FriCAS> session>

  Here follows a sample session (with mathematical input mode).

  <\session|fricas|default>
    <\output>
      Checking for foreign routines

      AXIOM="/usr/lib/fricas/target/i686-pc-linux-gnu"

      spad-lib="/usr/lib/fricas/target/i686-pc-linux-gnu/lib/libspad.so"

      foreign routines found

      openServer result -2

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ FriCAS Computer Algebra
      System\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Version: FriCAS
      1.1.8

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ Timestamp: Friday September 21, 2012 at
      18:21:14\ 

      -----------------------------------------------------------------------------

      \ \ \ Issue )copyright to view copyright notices.

      \ \ \ Issue )summary for a summary of useful system commands.

      \ \ \ Issue )quit to leave FriCAS and return to shell.

      -----------------------------------------------------------------------------

      \ 

      Value = #\<less\>INTERPRETED-FUNCTION NIL {BBB3CFD}\<gtr\>
    </output>

    <\unfolded-io-math>
      (1) -\<gtr\>\ 
    <|unfolded-io-math>
      expand<around*|(|<around*|(|x+y-z|)><rsup|5>|)>
    <|unfolded-io-math>
      \;

      <with|mode|math|-z<rsup|5>+(5*y+5*x)*z<rsup|4>+(-10*y<rsup|2>-20*x*y-10*x<rsup|2>)*z<rsup|3>+(10*y<rsup|3>+30*x*y<rsup|2>+30*x<rsup|2>*y+10*x<rsup|3>)*z<rsup|2>+(-5*y<rsup|4>-20*x*y<rsup|3>-30*x<rsup|2>*y<rsup|2>-20*x<rsup|3>*y-5*x<rsup|4>)*z+y<rsup|5>+5*x*y<rsup|4>+10*x<rsup|2>*y<rsup|3>+10*x<rsup|3>*y<rsup|2>+5*x<rsup|4>*y+x<rsup|5>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Polynomial(Integer)
    </unfolded-io-math>

    <\unfolded-io-math>
      (2) -\<gtr\>\ 
    <|unfolded-io-math>
      <big|int><frac|1|x<rsup|4>-1>*\<mathd\>x
    <|unfolded-io-math>
      \;

      <with|mode|math|<frac|-log(x+1)+log(x-1)-2*atan(x)|4>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Union(Expression(Integer),...)
    </unfolded-io-math>

    <\unfolded-io-math>
      (3) -\<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|n=1><rsup|10><frac|1|n<rsup|2>>
    <|unfolded-io-math>
      \;

      <with|mode|math|<frac|1968329|1270080>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Union(Expression(Integer),...)
    </unfolded-io-math>

    <\unfolded-io-math>
      (4) -\<gtr\>\ 
    <|unfolded-io-math>
      sinx\<assign\>series<around*|(|sin<around*|(|x|)>,x=0|)>
    <|unfolded-io-math>
      \;

      <with|mode|math|x-<frac|1|6>*x<rsup|3>+<frac|1|120>*x<rsup|5>-<frac|1|5040>*x<rsup|7>+<frac|1|362880>*x<rsup|9>-<frac|1|39916800>*x<rsup|11>+O(x<rsup|12>)>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      UnivariatePuiseuxSeries(Expression(Integer),x,0)
    </unfolded-io-math>

    <\unfolded-io-math>
      (5) -\<gtr\>\ 
    <|unfolded-io-math>
      cosx\<assign\>series<around*|(|cos<around*|(|x|)>,x=0|)>
    <|unfolded-io-math>
      \;

      <with|mode|math|1-<frac|1|2>*x<rsup|2>+<frac|1|24>*x<rsup|4>-<frac|1|720>*x<rsup|6>+<frac|1|40320>*x<rsup|8>-<frac|1|3628800>*x<rsup|10>+O(x<rsup|11>)>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      UnivariatePuiseuxSeries(Expression(Integer),x,0)
    </unfolded-io-math>

    <\unfolded-io-math>
      (6) -\<gtr\>\ 
    <|unfolded-io-math>
      sinx<rsup|2>+cosx<rsup|2>
    <|unfolded-io-math>
      \;

      <with|mode|math|1+O(x<rsup|11>)>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      UnivariatePuiseuxSeries(Expression(Integer),x,0)
    </unfolded-io-math>

    <\unfolded-io-math>
      (7) -\<gtr\>\ 
    <|unfolded-io-math>
      m\<assign\><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    <|unfolded-io-math>
      \;

      <with|mode|math|<matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Matrix(Polynomial(Integer))
    </unfolded-io-math>

    <\unfolded-io-math>
      (8) -\<gtr\>\ 
    <|unfolded-io-math>
      mi\<assign\>m<rsup|-1>
    <|unfolded-io-math>
      \;

      <with|mode|math|<matrix|<tformat|<table|<row|<cell|<frac|d|a*d-b*c>>|<cell|-<frac|b|a*d-b*c>>>|<row|<cell|-<frac|c|a*d-b*c>>|<cell|<frac|a|a*d-b*c>>>>>>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Matrix(Fraction(Polynomial(Integer)))
    </unfolded-io-math>

    <\unfolded-io-math>
      (9) -\<gtr\>\ 
    <|unfolded-io-math>
      m*mi
    <|unfolded-io-math>
      \;

      <with|mode|math|<matrix|<tformat|<table|<row|<cell|1>|<cell|0>>|<row|<cell|0>|<cell|1>>>>>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Type:
      Matrix(Fraction(Polynomial(Integer)))
    </unfolded-io-math>

    <\unfolded-io-math>
      (10) -\<gtr\>\ 
    <|unfolded-io-math>
      <det|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    <|unfolded-io-math>
      \;

      <with|mode|math|a*d-b*c>
    </unfolded-io-math>

    <\unfolded-io-math>
      (11) -\<gtr\>\ 
    <|unfolded-io-math>
      )q
    <|unfolded-io-math>
      <script-dead>
    </unfolded-io-math>

    <\input-math>
      (12) -\<gtr\>\ 
    <|input-math>
      \;
    </input-math>
  </session>

  <tmdoc-copyright|2012|Andrey Grozin>

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
    <associate|preamble|false>
  </collection>
</initial>