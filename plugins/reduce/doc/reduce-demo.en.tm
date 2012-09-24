<TeXmacs|1.0.7.16>

<style|tmdoc>

<\body>
  <tmdoc-title|Example of a <name|Reduce> session>

  Here follows a sample session (with mathematical input mode).

  <\session|reduce|default>
    <\output>
      \;

      Reduce (Free PSL version), 31-Jan-2012 ...

      \;
    </output>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      a\<assign\><around*|(|x+y|)><rsup|2>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|a<with|math-font-family|rm|:=>x<rsup|2>+2*x*y+y<rsup|2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      x\<assign\>z+1$ a;
    <|unfolded-io-math>
      \;

      <with|color|black|mode|math|math-display|true|y<rsup|2>+2*y*z+2*y+z<rsup|2>+2*z+1>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      x\<assign\>z-1$ a;
    <|unfolded-io-math>
      \;

      <with|color|black|mode|math|math-display|true|y<rsup|2>+2*y*z-2*y+z<rsup|2>-2*z+1>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      clear x; a;
    <|unfolded-io-math>
      \;

      <with|color|black|mode|math|math-display|true|x<rsup|2>+2*x*y+y<rsup|2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <frac|x<rsup|2>+y<rsup|2>|x<rsup|2>-y<rsup|2>>-<frac|x<rsup|2>-y<rsup|2>|x<rsup|2>+y<rsup|2>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|4*x<rsup|2>*y<rsup|2>|x<rsup|4>-y<rsup|4>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <around*|(|<sqrt|x>-<sqrt|y>|)><rsup|4>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|-4*<sqrt|y>*<sqrt|x>*x-4*<sqrt|y>*<sqrt|x>*y+x<rsup|2>+6*x*y+y<rsup|2>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      load_package trigsimp
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      trigsimp<around*|(|sin<around*|(|\<alpha\>|)>*cos<around*|(|\<beta\>|)>,combine|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|sin<around*|(|\<alpha\>-\<beta\>|)>+sin<around*|(|\<alpha\>+\<beta\>|)>|2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      sin<around*|(|-x|)>; cos<around*|(|<frac|\<pi\>|6>|)>;
      log<around*|(|\<mathe\><rsup|x>|)>;
      <around*|(|2+3*\<mathi\>|)><rsup|2>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|-sin<around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<frac|<sqrt|3>|2>>

      <with|color|black|mode|math|math-display|true|x>

      <with|color|black|mode|math|math-display|true|<with|math-font-family|rm|12>*i-5>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      a\<assign\>\<mathe\><rsup|c*x>*cos<around*|(|x|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|a<with|math-font-family|rm|:=>e<rsup|c*x>*cos<around*|(|x|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      df<around*|(|a,x|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|e<rsup|c*x>*<around*|(|cos<around*|(|x|)>*c-sin<around*|(|x|)>|)>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      clear a
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <big|int><sqrt|x<rsup|2>+a>*\<mathd\>x
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|<sqrt|a+x<rsup|2>>*x+log<around*|(|<frac|<sqrt|a+x<rsup|2>>+x|<sqrt|a>>|)>*a|2>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      load_package defint
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <big|int><rsub|0><rsup|\<infty\>><frac|sin<around*|(|x|)>|x>*\<mathd\>x
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|\<pi\>|2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <big|sum><rsub|n=0><rsup|m>n<rsup|2>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|m*<around*|(|2*m<rsup|2>+3*m+1|)>|6>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <big|sum><rsub|n=1><rsup|10><frac|1|n<rsup|2>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|<with|math-font-family|rm|1968329>|<with|math-font-family|rm|1270080>>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      load_package sum
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|\<pi\><rsup|2>|6>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      load_package tps
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      sinx\<assign\>ps<around*|(|sin<around*|(|x|)>,x,0|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<with|math-font-family|rm|sinx:=>*x-<frac|1|6>*x<rsup|3>+<frac|1|<with|math-font-family|rm|120>>*x<rsup|5>+O<around*|(|x<rsup|7>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      cosx\<assign\>ps<around*|(|cos<around*|(|x|)>,x,0|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<with|math-font-family|rm|cosx:=>*1-<frac|1|2>*x<rsup|2>+<frac|1|<with|math-font-family|rm|24>>*x<rsup|4>-<frac|1|<with|math-font-family|rm|720>>*x<rsup|6>+O<around*|(|x<rsup|7>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      a\<assign\>sinx<rsup|2>+cosx<rsup|2>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|a<with|math-font-family|rm|:=>1+O<around*|(|x<rsup|7>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      psexplim 10$ a;
    <|unfolded-io-math>
      \;

      <with|color|black|mode|math|math-display|true|1+O<around*|(|x<rsup|<with|math-font-family|rm|11>>|)>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      clear a,sinx,cosx
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      solve<around*|(|a*x<rsup|2>+b*x+c=0,x|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|{|x=<frac|<sqrt|-4*a*c+b<rsup|2>>-b|2*a><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x=<frac|-<around*|(|<sqrt|-4*a*c+b<rsup|2>>+b|)>|2*a>|}>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      solve<around*|(|<around*|{|a*x+b*y=u,c*x+d*y=v|}>,<around*|{|x,y|}>|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|{|<around*|{|x=<frac|-b*v+d*u|a*d-b*c><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>y=<frac|a*v-c*u|a*d-b*c>|}>|}>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      m\<assign\><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|m<with|math-font-family|rm|:=><around*|(|<tabular*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      mi\<assign\>m<rsup|-1>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<with|math-font-family|rm|mi:=><around*|(|<tabular*|<tformat|<table|<row|<cell|<frac|d|a*d-b*c>>|<cell|<frac|-b|a*d-b*c>>>|<row|<cell|<frac|-c|a*d-b*c>>|<cell|<frac|a|a*d-b*c>>>>>>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      m*mi
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|1>|<cell|0>>|<row|<cell|0>|<cell|1>>>>>|)>>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      clear m,mi
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      <det|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|a*d-b*c>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      for all x,y let log<around*|(|x*y|)>=log<around*|(|x|)>+log<around*|(|y|)>,

      log<around*|(|<frac|x|y>|)>=log<around*|(|x|)>-log<around*|(|y|)>,

      log<around*|(|x<rsup|y>|)>=y*log<around*|(|x|)>
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      log<around*|(|x*\<mathe\><rsup|y>|)>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|log<around*|(|x|)>+y>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      for all x,y clear log<around*|(|x*y|)>,log<around*|(|<frac|x|y>|)>,log<around*|(|x<rsup|y>|)>
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      procedure fac<around*|(|n|)>;

      if n\<gtr\>1 then n*fac<around*|(|n-1|)> else 1;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|math-font-family|rm|fac>
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      load_package rdebug
    <|unfolded-io-math>
      *** fluid \0lispsystem!*' cannot become global

      *** \0break_prompt' has not been defined, because it is flagged LOSE

      \;
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      tr fac
    <|unfolded-io-math>
      \;

      (fac)

      \;
    </unfolded-io-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      fac<around*|(|5|)>
    <|unfolded-io-math>
      fac being entered

      \ \ \ n: \ \ 5

      \;

      \ \ fac (level 2) being entered

      \ \ \ \ \ n: 4

      \;

      \ \ \ \ fac (level 3) being entered

      \ \ \ \ \ \ \ n: \ \ \ \ \ \ 3

      \;

      \ \ \ \ \ \ fac (level 4) being entered

      \ \ \ \ \ \ \ \ \ n: \ \ \ \ 2

      \;

      \ \ \ \ \ \ \ \ fac (level 5) being entered

      \ \ \ \ \ \ \ \ \ \ \ n: \ \ 1

      \;

      \ \ \ \ \ \ \ \ fac (level 5) = 1

      \;

      \ \ \ \ \ \ fac (level 4) = 2

      \;

      \ \ \ \ fac (level 3) = 6

      \;

      \ \ fac (level 2) = 24

      \;

      fac = 120

      \;

      <with|color|black|mode|math|math-display|true|120>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      untr fac
    </input-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      load_package gnuplot
    </input-math>

    <\input-math>
      REDUCE]\ 
    <|input-math>
      plot<around*|(|<frac|sin<around*|(|x|)>|x>,x=<around*|(|-4*\<pi\>\<ldots\>4*\<pi\>|)>|)>
    </input-math>

    <\unfolded-io-math>
      REDUCE]\ 
    <|unfolded-io-math>
      bye
    <|unfolded-io-math>
      \;

      Quitting

      <script-dead>
    </unfolded-io-math>

    <\input-math>
      REDUCE]\ 
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