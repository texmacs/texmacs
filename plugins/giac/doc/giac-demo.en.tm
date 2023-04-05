<TeXmacs|2.1.1>

<style|<tuple|tmdoc|giac|old-lengths>>

<\body>
  <tmdoc-title|Example <name|Giac> session>

  Here follows a sample session, which was started using
  <menu|Insert|Session|Giac>. Note that you may also use <name|Giac> as a
  scripting language in ordinary documents.

  <\session|giac|default>
    <\output>
      <hrule>

      Giac 1.7.0 for TeXmacs, released under the GPL license (3.0)

      See www.gnu.org for license details

      May contain BSD licensed software parts (lapack, atlas, tinymt)

      \<copyright\> 2003\U2021 B. Parisse & al (giac), J. van der Hoeven
      (TeXmacs), L. Marohni¢ (interface)

      <hrule>

      Xcas (C-like) syntax mode

      Type ? for documentation or ?commandname for help on commandname

      Type tabulation key to complete a partial command
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      f(x):=x*sin(x)
    <|unfolded-io>
      <\equation*>
        x\<mapsto\>x*sin <around*|\<nobracket\>|x|\<nobracket\>>
      </equation*>

      <\errput>
        // Parsing f

        // Success

        // compiling f
      </errput>
    </unfolded-io>

    <\textput>
      Some basic calculus examples:
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      diff(f(x),x,2)
    <|unfolded-io>
      <\equation*>
        -x*sin <around*|\<nobracket\>|x|\<nobracket\>>+2*cos
        <around*|\<nobracket\>|x|\<nobracket\>>
      </equation*>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      integrate(f(x),x)
    <|unfolded-io>
      <\equation*>
        -x*cos <around*|\<nobracket\>|x|\<nobracket\>>+sin
        <around*|\<nobracket\>|x|\<nobracket\>>
      </equation*>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      integrate(f(x),x=0..pi)
    <|unfolded-io>
      <\equation*>
        \<mathpi\>
      </equation*>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      integrate(sqrt(tan(x)),x)
    <|unfolded-io>
      <\equation*>
        2*<around*|(|<frac|1|8>*<sqrt|2>*ln <around*|(|tan
        <around*|\<nobracket\>|x|\<nobracket\>>-<sqrt|2>*<sqrt|tan
        <around*|\<nobracket\>|x|\<nobracket\>>>+1|)>+<frac|1|4>*<sqrt|2>*arctan
        <around*|(|<frac|2*<around*|(|<sqrt|tan
        <around*|\<nobracket\>|x|\<nobracket\>>>-<frac|<sqrt|2>|2>|)>|<sqrt|2>>|)>-<frac|1|8>*<sqrt|2>*ln
        <around*|(|tan <around*|\<nobracket\>|x|\<nobracket\>>+<sqrt|2>*<sqrt|tan
        <around*|\<nobracket\>|x|\<nobracket\>>>+1|)>+<frac|1|4>*<sqrt|2>*arctan
        <around*|(|<frac|2*<around*|(|<sqrt|tan
        <around*|\<nobracket\>|x|\<nobracket\>>>+<frac|<sqrt|2>|2>|)>|<sqrt|2>>|)>|)>
      </equation*>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      g(x,y):=(1+x*y)/(1+sqrt(x))
    <|unfolded-io>
      <\equation*>
        <around*|(|x,y|)>\<mapsto\><frac|1+x*y|1+<sqrt|x>>
      </equation*>

      <\errput>
        // Parsing g

        // Success

        // compiling g
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      simplify(diff(g(x,y),x,x,y))
    <|unfolded-io>
      <\equation*>
        <frac|-<sqrt|x>-3|4*x<rsup|2>+12*x*<sqrt|x>+12*x+4*<sqrt|x>>
      </equation*>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      simplify(hessian(g(x,y),[x,y]))
    <|unfolded-io>
      <\equation*>
        <matrix|<tformat|<table|<row|<cell|<frac|-x*y*<sqrt|x>-3*x*y+3*<sqrt|x>+1|4*x<rsup|3>+12*x<rsup|2>*<sqrt|x>+12*x<rsup|2>+4*x*<sqrt|x>>>|<cell|<frac|<sqrt|x>+2|2*x+4*<sqrt|x>+2>>>|<row|<cell|<frac|<sqrt|x>+2|2*x+4*<sqrt|x>+2>>|<cell|0>>>>>
      </equation*>
    </unfolded-io>

    <\textput>
      Plots and charts:
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      plot(sin(x)/x,x=0..10)
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-1.pdf|0.618par|||><htab|>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      plot3d(sin(sqrt(x^2+y^2))/sqrt(x^2+y^2),x=-10..10,y=-10..10)
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-2.pdf|0.618par|||><htab|>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      bar_plot([[2,"Yesterday","Today"],["A",2,5],["B",5,6],["C",7,7]])
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-3.pdf|0.618par|||><htab|>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      camembert([["France",6],["Allemagne",12],["Suisse",5]])
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-4.pdf|0.618par|||><htab|>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      histogram(seq(rand(1000),k,0,100),0,100)
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-5.pdf|0.618par|||><htab|>
    </unfolded-io>

    <\textput>
      <abbr|2D> geometry example:
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      P1:=point(3,4); P2:=point(6,2); P3:=point(4,3);

      Sq:=square(P1,P2,color=blue);

      Cr:=circle(P3,2);

      Q:=inter(Sq,Cr);

      T1:=tangent(Cr,Q[0],color=magenta);

      T2:=tangent(Cr,Q[1],color=green)
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-6.pdf|0.618par|||><htab|>
    </unfolded-io>

    <\textput>
      Graph drawing example:
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      G:=graph("groetzsch")
    <|unfolded-io>
      <\equation*>
        <text|an undirected unweighted graph with 11 vertices and 20 edges>
      </equation*>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      draw_graph(highlight_vertex(G,vertices(G),greedy_color(G)))
    <|unfolded-io>
      <htab|><image|giac-demo.en-image-7.pdf|0.618par|||><htab|>
    </unfolded-io>
  </session>

  <name|Giac> plugin has a good support for mathematical input mode. Besides
  the usual algebraic expressions, the following standard notations are
  supported:

  <\itemize-dot>
    <item>(partial) derivatives

    <item>integrals, sums and products

    <item>limits

    <item>piecewise defined functions

    <item>simplified notation for powers of trigonometric and other
    elementary functions (e.g. <math|sin<rsup|2>
    <around*|\<nobracket\>|x|\<nobracket\>>>)

    <item>absolute values, floor, ceiling

    <item>sets, lists, and matrices

    <item>special functions

    <item>complex conjugates, real and imaginary parts
  </itemize-dot>

  Note that <name|Giac> output (or a part of it) can be selected and copied
  to an input field.

  Parentheses around function arguments are mandatory in <name|Giac>.
  However, in <TeXmacs> mathematical input mode one can enter hidden
  parentheses by pressing <key|(> <key|Tab>, which work the same as the
  ordinary ones. Therefore it is possible to enter e.g.<nbsp><math|sin
  <around*|\<nobracket\>|x|\<nobracket\>>> (note the hidden parentheses
  around <math|x>!) instead of <math|sin <around*|(|x|)>>.

  When entering derivatives, the function application symbol (entered by
  pressing <key|Space>) between the derivative operator and the argument is
  mandatory, as well as the (hidden) parentheses around the argument.

  In the following examples we switch to the mathematical input mode.

  <\session|giac|default>
    <\textput>
      Hidden parentheses example:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      sin <around*|\<nobracket\>|\<pi\>|\<nobracket\>>
    <|unfolded-io-math>
      <\equation*>
        0
      </equation*>
    </unfolded-io-math>

    <\textput>
      Derivatives:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|\<mathd\><rsup|2>|\<mathd\> x<rsup|2>> <around*|(|x*ln
      <around*|\<nobracket\>|x|\<nobracket\>>-<frac|1|1-x>|)>
    <|unfolded-io-math>
      <\equation*>
        <frac|x<rsup|3>-3*x<rsup|2>+5*x-1|x<rsup|4>-3*x<rsup|3>+3*x<rsup|2>-x>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      collect <around*|(|<frac|\<partial\><rsup|3>|\<partial\> x*\<partial\>
      y<rsup|2>> <around*|(|<frac|y*\<mathe\><rsup|-x>|x<rsup|2>+y<rsup|2>>|)>|)>
    <|unfolded-io-math>
      <\equation*>
        <frac|2*y*<around*|(|3*x<rsup|4>+12*x<rsup|3>+2*x<rsup|2>*y<rsup|2>-12*x*y<rsup|2>-y<rsup|4>|)>*\<mathe\><rsup|-x>|<around*|(|y<rsup|2>+x<rsup|2>|)><rsup|4>>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Limits:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|x\<rightarrow\>0> <around*|\<nobracket\>|<frac|1-<frac|1|2>*x<rsup|2>-cos
      <around*|(|<frac|x|1-x<rsup|2>>|)>|x<rsup|4>>|\<nobracket\>>
    <|unfolded-io-math>
      <\equation*>
        <frac|23|24>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|x\<rightarrow\>0<rsup|+>> <around*|\<nobracket\>|\<mathe\><rsup|-1/x>|\<nobracket\>>
    <|unfolded-io-math>
      <\equation*>
        0
      </equation*>
    </unfolded-io-math>

    <\textput>
      Integrals, sums, and products:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><frac|1|<around*|(|x<rsup|2>+9|)><rsup|3>>*\<mathd\> x
    <|unfolded-io-math>
      <\equation*>
        <frac|x<rsup|3>+15*x|216*<around*|(|x<rsup|2>+9|)><rsup|2>>+<frac|arctan
        <around*|(|<frac|x|3>|)>|648>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><rsub|-\<infty\>><rsup|+\<infty\>>\<mathe\><rsup|-x<rsup|2>>*\<mathd\>
      x
    <|unfolded-io-math>
      <\equation*>
        <sqrt|\<mathpi\>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=1><rsup|+\<infty\>><frac|1|1+\<mathpi\><rsup|2>*k<rsup|2>>
    <|unfolded-io-math>
      <\equation*>
        <frac|1|\<mathe\><rsup|2>-1>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      trigsimplify <around*|(|2<rsup|10>*sin
      <around*|\<nobracket\>|<frac|x|2<rsup|10>>|\<nobracket\>>*<big|prod><rsub|k=1><rsup|10><around*|\<nobracket\>|cos
      |\<nobracket\>><frac|x|2<rsup|k>>|)>
    <|unfolded-io-math>
      <\equation*>
        sin <around*|\<nobracket\>|x|\<nobracket\>>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Simplification:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|<sqrt|5+2*<sqrt|6>>+<sqrt|9-2*<sqrt|6>-4*<sqrt|5-2*<sqrt|6>>>|)>
    <|unfolded-io-math>
      <\equation*>
        2*<sqrt|2>+2
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|cot <around*|(|atan <around*|(|<frac|12|13>|)>+acos
      <around*|(|<frac|4|5>|)>|)>|)>
    <|unfolded-io-math>
      <\equation*>
        <frac|16|87>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      normal <around*|(|1+x-<frac|1-x|1-x<rsup|2>>|)>
    <|unfolded-io-math>
      <\equation*>
        <frac|x<rsup|2>+2*x|x+1>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      trigsimplify <around*|(|1-<frac|1|4>*sin<rsup|2>
      <around*|(|2*x|)>-sin<rsup|2> <around*|\<nobracket\>|y|\<nobracket\>>-cos<rsup|4>
      <around*|\<nobracket\>|x|\<nobracket\>>|)>
    <|unfolded-io-math>
      <\equation*>
        sin<rsup|2> <around*|\<nobracket\>|x|\<nobracket\>>-sin<rsup|2>
        <around*|\<nobracket\>|y|\<nobracket\>>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Equation solving:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      solve<around*|(|x<rsup|3>-x+1-<frac|1|2-x<rsup|2>>=0,x|)>
    <|unfolded-io-math>
      <\equation*>
        <around*|[|<frac|-<sqrt|5>-1|2>,-1,<frac|<sqrt|5>-1|2>,1|]>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      linsolve <around*|(|<around*|[|x+y=2,x-2*y=3|]>,<around*|[|x,y|]>|)>
    <|unfolded-io-math>
      <\equation*>
        <around*|[|<frac|7|3>,-<frac|1|3>|]>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      fsolve <around*|(|x=\<mathe\><rsup|-x>,x=0|)>
    <|unfolded-io-math>
      <\equation*>
        0.56714329041
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      csolve <around*|(|z<rsup|2>\<cdot\><wide|z|\<bar\>>=\<Re\>
      <around*|(|z|)>-8*\<mathi\>,z|)>
    <|unfolded-io-math>
      <\equation*>
        <around*|[|-2*\<mathi\>|]>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Solving ordinary differential equations (note that various notational
      formats are supported):
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|<frac|\<mathd\><rsup|2> y|\<mathd\>
      x<rsup|2>>-y=2*sin <around*|\<nobracket\>|x|\<nobracket\>>,x,y|)>
    <|unfolded-io-math>
      <\equation*>
        c\<nosymbol\><rsub|0>*\<mathe\><rsup|x>+c\<nosymbol\><rsub|1>*\<mathe\><rsup|-x>-sin
        <around*|\<nobracket\>|x|\<nobracket\>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|<wide|x|\<ddot\>>=x,t,x|)>
    <|unfolded-io-math>
      <\equation*>
        c\<nosymbol\><rsub|0>*\<mathe\><rsup|t>+c\<nosymbol\><rsub|1>*\<mathe\><rsup|-t>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|y<rprime|''>-y=2*sin
      <around*|\<nobracket\>|x|\<nobracket\>>\<wedge\>y<around*|(|0|)>=0\<wedge\>y<rprime|'><around*|(|0|)>=1,x,y|)>
    <|unfolded-io-math>
      <\equation*>
        \<mathe\><rsup|x>-\<mathe\><rsup|-x>-sin
        <around*|\<nobracket\>|x|\<nobracket\>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|dsolve <around*|(|y<rsup|<around*|(|3|)>>=8*y,t,y|)>|)>
    <|unfolded-io-math>
      <\equation*>
        <frac|c\<nosymbol\><rsub|0>*<around*|(|\<mathe\><rsup|t>|)><rsup|3>*tan<rsup|2>
        <around*|(|<frac|1|2>*t*<sqrt|3>|)>+c\<nosymbol\><rsub|0>*<around*|(|\<mathe\><rsup|t>|)><rsup|3>-c\<nosymbol\><rsub|1>*tan<rsup|2>
        <around*|(|<frac|1|2>*t*<sqrt|3>|)>+c\<nosymbol\><rsub|1>+2*c\<nosymbol\><rsub|2>*tan
        <around*|(|<frac|1|2>*t*<sqrt|3>|)>|\<mathe\><rsup|t>*tan<rsup|2>
        <around*|(|<frac|1|2>*t*<sqrt|3>|)>+\<mathe\><rsup|t>>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Solving inequalities in one variable:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      solve <around*|(|x-<around*|\||x-<around*|\||x<rsup|2>-3*x-2|\|>|\|>-1\<gtr\>0,x|)>
    <|unfolded-io-math>
      <\equation*>
        <around*|[|x\<gtr\><frac|<sqrt|13>+1|2>\<wedge\>x\<less\><frac|<sqrt|13>+3|2>,x\<gtr\><frac|<sqrt|21>+3|2>\<wedge\>x\<less\><frac|<sqrt|29>+5|2>,<frac|<sqrt|29>+5|2>|]>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      domain <around*|(|<sqrt|3-<sqrt|2-<sqrt|1-x>>>,x|)>
    <|unfolded-io-math>
      <\equation*>
        x\<geqslant\>-3\<wedge\>x\<leqslant\>1
      </equation*>
    </unfolded-io-math>

    <\textput>
      Vectors and matrices (note that double symbols such as <verbatim|AA>
      and <verbatim|vv> are interpreted as bold symbols in <TeXmacs> and vice
      versa):
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-A\>\<assign\>hilbert <around*|(|3|)>
    <|unfolded-io-math>
      <\equation*>
        <matrix|<tformat|<table|<row|<cell|1>|<cell|<frac|1|2>>|<cell|<frac|1|3>>>|<row|<cell|<frac|1|2>>|<cell|<frac|1|3>>|<cell|<frac|1|4>>>|<row|<cell|<frac|1|3>>|<cell|<frac|1|4>>|<cell|<frac|1|5>>>>>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      det <around*|(|\<b-up-A\>|)>
    <|unfolded-io-math>
      <\equation*>
        <frac|1|2160>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-v\>\<assign\><around*|[|1,2,3|]>
    <|unfolded-io-math>
      <\equation*>
        <around*|[|1,2,3|]>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\<\|\|\>|\<b-up-v\>|\<\|\|\>>
    <|unfolded-io-math>
      <\equation*>
        <sqrt|14>
      </equation*>
    </unfolded-io-math>

    <\textput>
      If <math|A> is a matrix, then its element <math|a<rsub|i\<nocomma\>j>>
      can be obtained by using the usual notation. Note that the indices in
      the subscript must be enclosed within invisible parentheses and
      separated by (invisible) comma.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-A\><rsub|<around*|\<nobracket\>|2\<nocomma\>1|\<nobracket\>>>
    <|unfolded-io-math>
      <\equation*>
        <frac|1|4>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-v\><rsub|<around*|\<nobracket\>|1|\<nobracket\>>>+\<b-up-v\><rsub|<around*|\<nobracket\>|2|\<nobracket\>>>
    <|unfolded-io-math>
      <\equation*>
        5
      </equation*>
    </unfolded-io-math>

    <\textput>
      Finite sequences, lists, and sets can be generated as in the examples
      below. The symbol <math|\<barsuchthat\>> is entered by pressing
      <key|\|><key|Tab><key|Tab><key|Tab><key|Tab> and it corresponds to the
      <verbatim|$> operator in Giac, which has a very high priority.
      Therefore the compound expressions on both sides of
      <math|\<barsuchthat\>> should be surrounded by hidden parentheses.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      euler <around*|(|k|)>\<barsuchthat\><around*|\<nobracket\>|k=1\<ldots\>20|\<nobracket\>>
    <|unfolded-io-math>
      <\equation*>
        1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|ithprime <around*|(|j|)>\<barsuchthat\><around*|\<nobracket\>|j=1\<ldots\>20|\<nobracket\>>|]>
    <|unfolded-io-math>
      <\equation*>
        <around*|[|2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71|]>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      A\<assign\><around*|{|k<rsup|2>\<barsuchthat\><around*|\<nobracket\>|k=1\<ldots\>10|\<nobracket\>>|}>
    <|unfolded-io-math>
      <\equation*>
        <around*|{|1,4,9,16,25,36,49,64,81,100|}>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      B\<assign\><around*|{|<around*|\<nobracket\>|1+8*k|\<nobracket\>>\<barsuchthat\><around*|\<nobracket\>|k=1\<ldots\>10|\<nobracket\>>|}>
    <|unfolded-io-math>
      <\equation*>
        <around*|{|9,17,25,33,41,49,57,65,73,81|}>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      C\<assign\><around*|{|9,18,27|}>
    <|unfolded-io-math>
      <\equation*>
        <around*|{|9,18,27|}>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Set operations:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|A\<cap\> B|)>\<cup\> C
    <|unfolded-io-math>
      <\equation*>
        <around*|{|9,25,49,81,18,27|}>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      A\<setminus\>B
    <|unfolded-io-math>
      <\equation*>
        <around*|{|1,4,16,36,64,100|}>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Using the notation <math|a\<in\>A> we get the 1-based index of the
      element <math|a> in the set <math|A>, or 0 if <math|a\<nin\>A>:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      19\<in\> A
    <|unfolded-io-math>
      <\equation*>
        0
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      57\<in\> B
    <|unfolded-io-math>
      <\equation*>
        7
      </equation*>
    </unfolded-io-math>

    <\textput>
      Binomial coefficients are entered using the <markup|binom> tag:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|49|6>
    <|unfolded-io-math>
      <\equation*>
        13983816
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      assume <around*|(|n,integer|)>;additionally
      <around*|(|n\<geqslant\>2|)>
    <|unfolded-io-math>
      <\equation*>
        \<bbb-Z\>,n
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|n|2>
    <|unfolded-io-math>
      <\equation*>
        <frac|n!|2\<cdot\><around*|(|n-2|)>!>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Composition of functions:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|cos\<circ\>sin|)><around*|(|\<mathpi\>|)>
    <|unfolded-io-math>
      <\equation*>
        1
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      f\<assign\>x\<mapsto\>x<rsup|2>+1
    <|unfolded-io-math>
      <\equation*>
        x\<mapsto\>x<rsup|2>+1
      </equation*>

      <\errput>
        // Success

        // End defining f
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      g\<assign\>y\<mapsto\>y-1
    <|unfolded-io-math>
      <\equation*>
        y\<mapsto\>y-1
      </equation*>

      <\errput>
        // Success

        // End defining g
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|f\<circ\>g|)><around*|(|u|)>
    <|unfolded-io-math>
      <\equation*>
        <around*|(|u-1|)><rsup|2>+1
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|g\<circ\>f|)><around*|(|v|)>
    <|unfolded-io-math>
      <\equation*>
        v<rsup|2>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Conditionals:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<longequal\>2
    <|unfolded-io-math>
      <\equation*>
        false
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<neq\>2
    <|unfolded-io-math>
      <\equation*>
        true
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<less\>2
    <|unfolded-io-math>
      <\equation*>
        true
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<geqslant\>2
    <|unfolded-io-math>
      <\equation*>
        false
      </equation*>
    </unfolded-io-math>

    <\textput>
      Polynomials may be entered as lists of coefficients with double-struck
      brackets:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      p\<assign\><around*|\<llbracket\>|-1,3,2|\<rrbracket\>>
    <|unfolded-io-math>
      <\equation*>
        <around*|\<llbracket\>|-1,3,2|\<rrbracket\>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      expand <around*|(|poly2symb <around*|(|p,x|)>|)>
    <|unfolded-io-math>
      <\equation*>
        -x<rsup|2>+3*x+2
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      q\<assign\><around*|\<llbracket\>|2,0,-2,1|\<rrbracket\>>
    <|unfolded-io-math>
      <\equation*>
        <around*|\<llbracket\>|2,0,-2,1|\<rrbracket\>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      p\<cdot\>q
    <|unfolded-io-math>
      <\equation*>
        <around*|\<llbracket\>|-2,6,6,-7,-1,2|\<rrbracket\>>
      </equation*>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      p+q
    <|unfolded-io-math>
      <\equation*>
        <around*|\<llbracket\>|2,-1,1,3|\<rrbracket\>>
      </equation*>
    </unfolded-io-math>

    <\textput>
      Piecewise functions can be entered by using the <markup|choice> tag
      (note that any textual condition, i.e.<nbsp>a <markup|text> tag, is
      interpreted as \Potherwise\Q, no matter of its contents):
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      f <around*|(|x|)>\<assign\><choice|<tformat|<table|<row|<cell|0,>|<cell|x\<less\>0>>|<row|<cell|x,>|<cell|x\<less\>1>>|<row|<cell|1,>|<cell|x\<less\>3>>|<row|<cell|\<mathe\><rsup|3-x>,>|<cell|<text|in
      all other cases>>>>>>
    <|unfolded-io-math>
      <\equation*>
        x\<mapsto\><choice|<tformat|<table|<row|<cell|0,>|<cell|x\<less\>0>>|<row|<cell|x,>|<cell|x\<less\>1>>|<row|<cell|1,>|<cell|x\<less\>3>>|<row|<cell|\<mathe\><rsup|3-x>,>|<cell|<text|otherwise>>>>>>
      </equation*>

      <\errput>
        // Parsing f

        // Success

        // compiling f
      </errput>
    </unfolded-io-math>

    <\textput>
      Periodic functions can be entered as well:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      h\<assign\>periodic <around*|(|<around*|(|1-x<rsup|4>|)>*\<mathe\><rsup|1-x<rsup|3>>,x=-1\<ldots\>1|)>
    <|unfolded-io-math>
      <\equation*>
        <around*|(|1-<around*|(|x-2*<around*|\<lfloor\>|<frac|x+1|2>|\<rfloor\>>|)><rsup|4>|)>*\<mathe\><rsup|1-<around*|(|x-2*<around*|\<lfloor\>|<frac|x+1|2>|\<rfloor\>>|)><rsup|3>>
      </equation*>
    </unfolded-io-math>
  </session>
</body>

<initial|<\collection>
</collection>>