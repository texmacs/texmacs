<TeXmacs|1.99.20>

<style|<tuple|tmdoc|giac>>

<\body>
  <tmdoc-title|Example Giac session>

  Here follows a sample session, which was started using
  <menu|Insert|Session|Giac>.

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

      <\errput>
        // Using locale /usr/local/share/locale/

        // en_US.UTF-8

        // /usr/local/share/locale/

        // giac

        // UTF-8

        // Maximum number of parallel threads 6

        Added 0 synonyms
      </errput>
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      f(x):=sin(x)+x
    <|unfolded-io>
      <equation*|<math|x\<mapsto\>sin <around*|\<nobracket\>|x|\<nobracket\>>+x>>

      <\errput>
        // Parsing f

        // Success

        // compiling f
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      diff(f(x),x)
    <|unfolded-io>
      <equation*|<math|cos <around*|\<nobracket\>|x|\<nobracket\>>+1>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      integrate(f(x),x=0..pi)
    <|unfolded-io>
      <equation*|<math|<frac|\<mathpi\><rsup|2>+2|2>+1>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      plot(f(x),x=0..2*pi)
    <|unfolded-io>
      <image|giac-demo.en-image-1.pdf|0.7par|||>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      plot([x^2*sin(x),f(x)],x=-pi..pi,color=[blue,magenta])
    <|unfolded-io>
      <image|giac-demo.en-image-2.pdf|0.7par|||>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      implicitplot(x^2=y^3-3y+1,x=-4..4,y=-4..4)
    <|unfolded-io>
      <image|giac-demo.en-image-3.pdf|0.7par|||>
    </unfolded-io>

    <\textput>
      Mathematical and physical constants, as well as physical units, are
      typeset using the conventional notation whenever possible, as in the
      example below.
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      e,i,pi,euler_gamma,inf,5_Angstrom,_NA_,_REarth_,_hbar_
    <|unfolded-io>
      <equation*|<math|\<mathe\>,\<mathi\>,\<mathpi\>,\<matheuler\>,+\<infty\>,5\<space\>\<space\>\<nosymbol\>\<AA\>,1\<space\>\<space\>\<nosymbol\>N<rsub|A>\<nosymbol\>,1\<space\>\<space\>\<nosymbol\>R<rsub|\<oplus\>>\<nosymbol\>,1\<space\>\<space\>\<nosymbol\>\<hbar\>\<nosymbol\>>>
    </unfolded-io>

    <\textput>
      Graphs can be constructed, manipulated with and drawn.
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      G:=graph("grotzsch")
    <|unfolded-io>
      <equation*|<math|<text|an undirected unweighted graph with 11 vertices
      and 20 edges>>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      draw_graph(highlight_vertex(G,vertices(G),greedy_color(G)))
    <|unfolded-io>
      <image|giac-demo.en-image-4.pdf|0.7par|||>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      draw_graph(random_tree(30),labels=false)
    <|unfolded-io>
      <image|giac-demo.en-image-5.pdf|0.7par|||>
    </unfolded-io>

    <\textput>
      In the following examples it is demonstrated how to create bar plots,
      histograms and pie charts.
    </textput>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      bar_plot([[2,"Yesterday","Today"],["A",2,5],["B",5,6],["C",7,7]])
    <|unfolded-io>
      <image|giac-demo.en-image-6.pdf|0.7par|||>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      histogram(seq(rand(1000),k,0,100),0,100)
    <|unfolded-io>
      <image|giac-demo.en-image-7.pdf|0.7par|||>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      camembert([["France",6],["Allemagne",12],["Suisse",5]])
    <|unfolded-io>
      <image|giac-demo.en-image-8.pdf|0.7par|||>
    </unfolded-io>

    <\textput>
      A <abbr|2D> geometry example:
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
      <image|giac-demo.en-image-9.pdf|0.7par|||>
    </unfolded-io>

    <\textput>
      The <name|Giac> plugin has a good support for mathematical input mode.
      Besides the usual algebraic expressions, the following standard
      notations are supported:

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

        <item>etc.
      </itemize-dot>
    </textput>

    Parentheses around function arguments are mandatory in <name|Giac>.
    However, in <TeXmacs> mathematical input mode one can enter hidden
    parentheses by pressing <key|(> <key|Tab>, which work the same as the
    ordinary ones. Therefore it is possible to enter e.g.<nbsp><math|sin
    <around*|\<nobracket\>|x|\<nobracket\>>> (note the hidden parentheses
    around <math|x>!) instead of <math|sin <around*|(|x|)>>.

    When entering derivatives, the function application symbol (entered by
    pressing <key|Space>) between the derivative operator and the argument is
    mandatory, as well as the (hidden) parentheses around the argument.

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|\<mathd\>|\<mathd\> x> <around*|(|x*ln
      <around*|\<nobracket\>|x|\<nobracket\>>-<frac|1|1-x>|)>
    <|unfolded-io-math>
      <equation*|<math|ln <around*|\<nobracket\>|x|\<nobracket\>>+1-<frac|1|<around*|(|1-x|)><rsup|2>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|\<mathd\><rsup|2>|\<mathd\> x<rsup|2>> <around*|(|x*ln
      <around*|\<nobracket\>|x|\<nobracket\>>-<frac|1|1-x>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|x<rsup|3>-3*x<rsup|2>+5*x-1|x<rsup|4>-3*x<rsup|3>+3*x<rsup|2>-x>>>
    </unfolded-io-math>

    <\textput>
      In the following example, the stationary points of the function
      <math|f> are computed.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      assume <around*|(|r\<gtr\>0|)>
    <|unfolded-io-math>
      <equation*|<math|r>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      f\<assign\>unapply <around*|(|<frac|ln
      <around*|\<nobracket\>|x|\<nobracket\>>|r>-<frac|r*x|x+1>,x|)>
    <|unfolded-io-math>
      <equation*|<math|x\<mapsto\><frac|ln
      <around*|\<nobracket\>|x|\<nobracket\>>|r>-<frac|r*x|x+1>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      solve <around*|(|<frac|\<mathd\>|\<mathd\> x>
      <around*|\<nobracket\>|f<around*|(|x|)>|\<nobracket\>>=0,x|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|[|<frac|r<rsup|2>+r*<sqrt|r<rsup|2>-4>-2|2>,<frac|r<rsup|2>-r*<sqrt|r<rsup|2>-4>-2|2>|]>>>
    </unfolded-io-math>

    <\textput>
      Various notations for deriatives in differential equations are
      supported.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|<frac|\<mathd\><rsup|2> y|\<mathd\>
      x<rsup|2>>-y=2*sin <around*|\<nobracket\>|x|\<nobracket\>>,x,y|)>
    <|unfolded-io-math>
      <equation*|<math|c\<nosymbol\><rsub|0>*\<mathe\><rsup|x>+c\<nosymbol\><rsub|1>*\<mathe\><rsup|-x>-sin
      <around*|\<nobracket\>|x|\<nobracket\>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|y<rprime|''>-y=2*sin
      <around*|\<nobracket\>|x|\<nobracket\>>\<wedge\>y<around*|(|0|)>=0\<wedge\>y<rprime|'><around*|(|0|)>=1,x,y|)>
    <|unfolded-io-math>
      <equation*|<math|\<mathe\><rsup|x>-\<mathe\><rsup|-x>-sin
      <around*|\<nobracket\>|x|\<nobracket\>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|<wide|x|\<ddot\>>=x,t,x|)>
    <|unfolded-io-math>
      <equation*|<math|c\<nosymbol\><rsub|0>*\<mathe\><rsup|t>+c\<nosymbol\><rsub|1>*\<mathe\><rsup|-t>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|dsolve <around*|(|y<rsup|<around*|(|3|)>>=y,t,y|)>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|c\<nosymbol\><rsub|0>*<around*|(|\<mathe\><rsup|<frac|1|2>*t>|)><rsup|3>*tan<rsup|2>
      <around*|(|<frac|1|4>*t*<sqrt|3>|)>+c\<nosymbol\><rsub|0>*<around*|(|\<mathe\><rsup|<frac|1|2>*t>|)><rsup|3>-c\<nosymbol\><rsub|1>*tan<rsup|2>
      <around*|(|<frac|1|4>*t*<sqrt|3>|)>+c\<nosymbol\><rsub|1>+2*c\<nosymbol\><rsub|2>*tan
      <around*|(|<frac|1|4>*t*<sqrt|3>|)>|\<mathe\><rsup|<frac|1|2>*t>*tan<rsup|2>
      <around*|(|<frac|1|4>*t*<sqrt|3>|)>+\<mathe\><rsup|<frac|1|2>*t>>>>
    </unfolded-io-math>

    <\textput>
      Partial derivatives are supported as well.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      collect <around*|(|<frac|\<partial\><rsup|3>|\<partial\> x*\<partial\>
      y<rsup|2>> <around*|(|<frac|y*\<mathe\><rsup|-x>|x<rsup|2>+y<rsup|2>>|)>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|2*y*<around*|(|3*x<rsup|4>+12*x<rsup|3>+2*x<rsup|2>*y<rsup|2>-12*x*y<rsup|2>-y<rsup|4>|)>*\<mathe\><rsup|-x>|<around*|(|y<rsup|2>+x<rsup|2>|)><rsup|4>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|\<partial\><rsup|2>|\<partial\> x*\<partial\> y>
      <around*|\<nobracket\>|g <around*|(|x,x*y|)>|\<nobracket\>>
    <|unfolded-io-math>
      <equation*|<math|x*y*g<rsup|<around*|(|2,2|)>>
      <around*|(|x,x*y|)>+x*g<rsup|<around*|(|1,2|)>>
      <around*|(|x,x*y|)>+g<rsup|<around*|(|2|)>> <around*|(|x,x*y|)>>>
    </unfolded-io-math>

    <\textput>
      Laplace and Fourier transform functions are available.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      F\<assign\>laplace <around*|(|t*sin <around*|(|3*t|)>,t,s|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|6*s|s<rsup|4>+18*s<rsup|2>+81>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      fourier <around*|(|rect <around*|(|x|)>,x,s|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|2*sin <around*|(|<frac|s|2>|)>|s>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      addtable <around*|(|fourier,y <around*|(|x|)>,Y <around*|(|s|)>,x,s|)>
    <|unfolded-io-math>
      <equation*|<math|1>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      T\<assign\>fourier <around*|(|y <around*|(|x+1|)>-<frac|\<mathd\><rsup|3>|\<mathd\>
      x<rsup|3>> <around*|\<nobracket\>|y
      <around*|(|x|)>|\<nobracket\>>,x,s|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|(|\<mathe\><rsup|\<mathi\>*s>+\<mathi\>*s<rsup|3>|)>*Y
      <around*|(|s|)>>>
    </unfolded-io-math>

    <\textput>
      Heaviside function and Dirac <math|\<delta\>>-distribution are
      associated with upright Greek symbols <math|<math-up|\<theta\>>> and
      <math|<math-up|\<delta\>>>, respectively.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      F\<assign\>exp2pow <around*|(|lin <around*|(|fourier
      <around*|(|<frac|x|x<rsup|2>-x+1>,x,\<omega\>|)>|)>|)>
    <|unfolded-io-math>
      <equation*|<math|\<mathpi\>*\<mathi\>*\<up-theta\>
      <around*|(|-\<omega\>|)>*\<mathe\><rsup|<frac|\<omega\>*<sqrt|3>-\<mathi\>*\<omega\>|2>>-\<mathpi\>*\<mathi\>*\<up-theta\>
      <around*|(|\<omega\>|)>*\<mathe\><rsup|-<frac|\<omega\>*<sqrt|3>+\<mathi\>*\<omega\>|2>>+\<mathpi\>*\<mathe\><rsup|<frac|-\<mathi\>*\<omega\>*<sqrt|3>-3*<around*|\||\<omega\>|\|>|2*<sqrt|3>>>*<sqrt|3><rsup|-1>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ifourier <around*|(|F,\<omega\>,x|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|x|x<rsup|2>-x+1>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      h\<assign\>fourier <around*|(|1|)>
    <|unfolded-io-math>
      <equation*|<math|2*\<mathpi\>*\<up-delta\> <around*|(|x|)>>>
    </unfolded-io-math>

    <\textput>
      <name|Giac> has basic support for variational calculus.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      eq\<assign\>euler_lagrange <around*|(|x<rsup|2>\<cdot\><around*|(|y<rprime|'>|)><rsup|2>+2*y<rsup|2>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|\<mathd\><rsup|2>|\<mathd\> x<rsup|2>>
      <around*|\<nobracket\>|y <around*|(|x|)>|\<nobracket\>>=<frac|-2*<frac|\<mathd\>|\<mathd\>
      x> <around*|\<nobracket\>|y <around*|(|x|)>|\<nobracket\>>*x+2*y
      <around*|(|x|)>|x<rsup|2>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      dsolve <around*|(|eq,x,y|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|<around*|(|-<frac|c\<nosymbol\><rsub|0>|3*x<rsup|3>>+c\<nosymbol\><rsub|1>|)>*x<rsup|3>|x<rsup|2>>>>
    </unfolded-io-math>

    <\textput>
      Integrals, sums and products are entered in the usual way.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><frac|1|<around*|(|x<rsup|2>+9|)><rsup|3>>*\<mathd\> x
    <|unfolded-io-math>
      <equation*|<math|<frac|x<rsup|3>+15*x|216*<around*|(|x<rsup|2>+9|)><rsup|2>>+<frac|arctan
      <around*|(|<frac|x|3>|)>|648>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><sqrt|tan <around*|\<nobracket\>|x|\<nobracket\>>>*\<mathd\> x
    <|unfolded-io-math>
      <equation*|<math|2*<around*|(|<frac|1|8>*<sqrt|2>*ln <around*|(|tan
      <around*|\<nobracket\>|x|\<nobracket\>>-<sqrt|2>*<sqrt|tan
      <around*|\<nobracket\>|x|\<nobracket\>>>+1|)>+<frac|1|4>*<sqrt|2>*arctan
      <around*|(|<frac|2*<around*|(|<sqrt|tan
      <around*|\<nobracket\>|x|\<nobracket\>>>-<frac|<sqrt|2>|2>|)>|<sqrt|2>>|)>-<frac|1|8>*<sqrt|2>*ln
      <around*|(|tan <around*|\<nobracket\>|x|\<nobracket\>>+<sqrt|2>*<sqrt|tan
      <around*|\<nobracket\>|x|\<nobracket\>>>+1|)>+<frac|1|4>*<sqrt|2>*arctan
      <around*|(|<frac|2*<around*|(|<sqrt|tan
      <around*|\<nobracket\>|x|\<nobracket\>>>+<frac|<sqrt|2>|2>|)>|<sqrt|2>>|)>|)>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><rsub|-\<infty\>><rsup|+\<infty\>>\<mathe\><rsup|-x<rsup|2>>*\<mathd\>
      x
    <|unfolded-io-math>
      <equation*|<math|<sqrt|\<mathpi\>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      assume <around*|(|\<alpha\>\<gtr\>0|)>
    <|unfolded-io-math>
      <equation*|<math|\<alpha\>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><rsub|0><rsup|+\<infty\>>ln
      <around*|(|1+<frac|\<alpha\><rsup|2>|x<rsup|2>>|)>*\<mathd\> x
    <|unfolded-io-math>
      <equation*|<math|\<mathpi\>*\<alpha\>>>

      <\errput>
        No checks were made for singular points of antiderivative
        x*ln(1+alpha^2/x^2)+2*alpha^2*2*1/2/alpha*atan(x/alpha) for definite
        integration in [0,+infinity]
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=1><rsup|+\<infty\>><frac|1|1+\<mathpi\><rsup|2>*k<rsup|2>>
    <|unfolded-io-math>
      <equation*|<math|<frac|1|\<mathe\><rsup|2>-1>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|prod><rsub|k=1><rsup|10><around*|(|1-<frac|1|2*k<rsup|2>>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|103376401778279|275188285440000>>>
    </unfolded-io-math>

    <\textput>
      <name|Giac> can determine domain of an univariate real function and
      solve inequalities.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      domain <around*|(|<sqrt|3-<sqrt|2-<sqrt|1-x>>>,x|)>
    <|unfolded-io-math>
      <equation*|<math|x\<geqslant\>-3\<wedge\>x\<leqslant\>1>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      solve <around*|(|<around*|\||2*x<rsup|2>-3|\|>\<leqslant\>5,x|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|[|x\<geqslant\>-2\<wedge\>x\<leqslant\>2|]>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      solve <around*|(|x-<around*|\||x-<around*|\||x<rsup|2>-3*x-2|\|>|\|>-1\<gtr\>0,x|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|[|x\<gtr\><frac|<sqrt|13>+1|2>\<wedge\>x\<less\><frac|<sqrt|13>+3|2>,x\<gtr\><frac|<sqrt|21>+3|2>\<wedge\>x\<less\><frac|<sqrt|29>+5|2>|]>>>
    </unfolded-io-math>

    <\textput>
      A partial fractions decomposition example:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      partfrac <around*|(|<frac|x<rsup|4>-44*x<rsup|3>+22*x<rsup|2>-11*x+1|x<rsup|5>+3*x<rsup|4>+x<rsup|3>-x<rsup|2>-4>,x|)>
    <|unfolded-io-math>
      <equation*|<math|-<frac|31|18*<around*|(|x-1|)>>-<frac|479|15*<around*|(|x+2|)><rsup|2>>+<frac|1742|225*<around*|(|x+2|)>>+<frac|-251*x+107|50*<around*|(|x<rsup|2>+1|)>>>>
    </unfolded-io-math>

    <\textput>
      Simplification and auto-simplification examples:
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|<sqrt|5+2*<sqrt|6>>+<sqrt|9-2*<sqrt|6>-4*<sqrt|5-2*<sqrt|6>>>|)>
    <|unfolded-io-math>
      <equation*|<math|2*<sqrt|2>+2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|cot <around*|(|atan <around*|(|<frac|12|13>|)>+acos
      <around*|(|<frac|4|5>|)>|)>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|16|87>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      trigsimplify <around*|(|1-<frac|1|4>*sin<rsup|2>
      <around*|(|2*x|)>-sin<rsup|2> <around*|\<nobracket\>|y|\<nobracket\>>-cos<rsup|4>
      <around*|\<nobracket\>|x|\<nobracket\>>|)>
    <|unfolded-io-math>
      <equation*|<math|sin<rsup|2> <around*|\<nobracket\>|x|\<nobracket\>>-sin<rsup|2>
      <around*|\<nobracket\>|y|\<nobracket\>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      trigsimplify <around*|(|<frac|<big|sum><rsub|n=1><rsup|5>sin
      <around*|(|n*x|)>|<big|sum><rsub|n=1><rsup|5>cos <around*|(|n*x|)>>|)>
    <|unfolded-io-math>
      <equation*|<math|tan <around*|(|3*x|)>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      assume <around*|(|n,integer|)>;additionally<around*|(|n\<geqslant\>0|)>
    <|unfolded-io-math>
      <equation*|<math|\<bbb-Z\>,n>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<Gamma\> <around*|(|n+1|)>
    <|unfolded-io-math>
      <equation*|<math|n!>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      cos <around*|(|n*\<mathpi\>|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|(|-1|)><rsup|n>>>
    </unfolded-io-math>

    <\textput>
      Binomial coefficients are entered using the <markup|binom> tag.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|49|7>
    <|unfolded-io-math>
      <equation*|<math|85900584>>
    </unfolded-io-math>

    <\textput>
      An expression with the wide bar accent is interpreted as the complex
      conjugate. Additionally, the usual notation for real and imaginary
      parts is supported.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      assume <around*|(|z,complex|)>;csolve
      <around*|(|z<rsup|2>\<cdot\><wide|z|\<bar\>>=\<Re\>
      <around*|(|z|)>-8*\<mathi\>,z|)>
    <|unfolded-io-math>
      <equation*|<math|\<bbb-C\>,<around*|[|-2*\<mathi\>|]>>>
    </unfolded-io-math>

    <\textput>
      Substitution of parameters in an expression can be executed\Vbesides
      using the <verbatim|subs> command\V by entering the symbol <math|\|>
      (obtained by pressing <key|\|><key|Tab>) between the expression and the
      sequence of equations in form <verbatim|parameter=value>.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      cos<rsup|2> <around*|\<nobracket\>|y|\<nobracket\>>+y*sin
      <around*|\<nobracket\>|x|\<nobracket\>>\|x=<frac|\<pi\>|3>,y=<frac|\<mathpi\>|4>
    <|unfolded-io-math>
      <equation*|<math|<around*|(|<frac|<sqrt|2>|2>|)><rsup|2>+<frac|\<mathpi\>*<sqrt|3>|4\<cdot\>2>>>

      <\errput>
        // Success
      </errput>
    </unfolded-io-math>

    <\textput>
      The invisible addition symbol, entered with
      <key|+><key|Tab><key|Tab><key|Tab><key|Tab>, translates to
      <verbatim|+>.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      7\<noplus\><frac*|3|4>
    <|unfolded-io-math>
      <equation*|<math|<frac|31|4>>>
    </unfolded-io-math>

    <\textput>
      The invisible symbol, entered with <shortcut|\<nosymbol\>>, is
      interpreted as the underscore (_). It is useful for
      e.g.<nbsp>differentiating between e.g.<nbsp><verbatim|x0> and
      <verbatim|x_0>, which are both typeset (and, in math input mode,
      entered) as <math|x<rsub|0>>. However, in the latter case the invisible
      symbol is appended to <math|x>, as in the example below. Since the
      subscript of a symbol is simply appended to the symbol for input in
      <name|Giac>, the concatenation yields <verbatim|x_0>. The invisible
      symbol is also used for entering physical units, which begin with _,
      and physical constants, which begin and end with _ in <name|Giac>.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|x\<nosymbol\><rsub|0>-x<rsub|0>|)>
    <|unfolded-io-math>
      <equation*|<math|-x<rsub|0>+x\<nosymbol\><rsub|0>>>
    </unfolded-io-math>

    <\textput>
      Bold symbols may be used, which is useful for denoting matrices and
      vectors. A bold symbol is input as a double symbol,
      e.g.<nbsp><math|\<b-up-G\>> corresponds to <verbatim|GG> in
      <name|Giac>. Note that indices in <name|Giac> are 0-based by default.
      For 1-based indices, switch to Maple mode.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-A\>\<assign\>matrix <around*|(|4,4,<around*|(|j,k|)>\<mapsto\>k+j<rsup|k+1>|)>
    <|unfolded-io-math>
      <equation*|<math|<matrix|<tformat|<table|<row|<cell|0>|<cell|1>|<cell|2>|<cell|3>>|<row|<cell|1>|<cell|2>|<cell|3>|<cell|4>>|<row|<cell|2>|<cell|5>|<cell|10>|<cell|19>>|<row|<cell|3>|<cell|10>|<cell|29>|<cell|84>>>>>>>

      <\errput>
        // Success
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      det <around*|(|\<b-up-A\>|)>
    <|unfolded-io-math>
      <equation*|<math|-24>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-A\><rsup|-1>
    <|unfolded-io-math>
      <equation*|<math|<matrix|<tformat|<table|<row|<cell|-<frac|17|6>>|<cell|-<frac|3|2>>|<cell|<frac|3|2>>|<cell|-<frac|1|6>>>|<row|<cell|<frac|17|6>>|<cell|<frac|23|4>>|<cell|-<frac|7|2>>|<cell|<frac|5|12>>>|<row|<cell|-<frac|7|6>>|<cell|-4>|<cell|<frac|5|2>>|<cell|-<frac|1|3>>>|<row|<cell|<frac|1|6>>|<cell|<frac|3|4>>|<cell|-<frac|1|2>>|<cell|<frac|1|12>>>>>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-v\>\<assign\><around*|[|-1,3,7|]>
    <|unfolded-io-math>
      <equation*|<math|<around*|[|-1,3,7|]>>>
    </unfolded-io-math>

    <\textput>
      The expression below is computed as <math|\<ell\><rsup|2>>-norm of
      <math|\<b-up-v\>>.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\<\|\|\>|\<b-up-v\>|\<\|\|\>>
    <|unfolded-io-math>
      <equation*|<math|<sqrt|59>>>
    </unfolded-io-math>

    <\textput>
      If <math|A> is a matrix, then its element <math|a<rsub|i\<nocomma\>j>>
      can be fetched using the common notation, as below. Note that the
      indices in the subscript must be enclosed within invisible parentheses
      and separated by (invisible) comma.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-A\><rsub|<around*|\<nobracket\>|3\<nocomma\>2|\<nobracket\>>>
    <|unfolded-io-math>
      <equation*|<math|29>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<b-up-v\><rsub|<around*|\<nobracket\>|1|\<nobracket\>>>+\<b-up-v\><rsub|<around*|\<nobracket\>|2|\<nobracket\>>>
    <|unfolded-io-math>
      <equation*|<math|10>>
    </unfolded-io-math>

    <\textput>
      Limits are entered like in the examples below. Note that the body of a
      limit must be parenthesed (use invisible parentheses when appropriate)
      and prepended by the function application symbol.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|x\<rightarrow\>0> <around*|\<nobracket\>|<frac|1-<frac|1|2>*x<rsup|2>-cos
      <around*|(|<frac|x|1-x<rsup|2>>|)>|x<rsup|4>>|\<nobracket\>>
    <|unfolded-io-math>
      <equation*|<math|<frac|23|24>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|x\<rightarrow\>0<rsup|+>> <around*|\<nobracket\>|\<mathe\><rsup|-1/x>|\<nobracket\>>
    <|unfolded-io-math>
      <equation*|<math|0>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|x\<rightarrow\>1<rsup|->> <around*|\<nobracket\>|sin
      <around*|(|\<pi\>*x|)><rsup|1/ln <around*|(|1-x|)>>|\<nobracket\>>
    <|unfolded-io-math>
      <equation*|<math|\<mathe\>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|n\<rightarrow\>+\<infty\>>
      <around*|(|<sqrt|n<rsup|3>-2*n<rsup|2>+n-1|3>-n|)>
    <|unfolded-io-math>
      <equation*|<math|-<frac|2|3>>>
    </unfolded-io-math>

    <\textput>
      <paragraph|Example.>It can be shown that be shown that the series
      <math|<big|sum><rsub|n=0><rsup|\<infty\>>s <around*|(|n|)>>, where
      <math|s> is defined below, converges to <math|<frac|1|\<mathpi\>>>
      (J.<nbsp>M.<nbsp>Borwein et al., 1989). We prove the convergence using
      the criterion of D'Alembert and compute the number of significant
      digits in the approximation of <math|\<mathpi\>> using the first 11
      terms.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      s<around*|(|n|)>\<assign\><binom|2*n|n><rsup|3>*<frac|42*n+5|2<rsup|12*n+4>>
    <|unfolded-io-math>
      <equation*|<math|n\<mapsto\><binom|2*n|n><rsup|3>*<frac|42*n+5|2<rsup|12*n+4>>>>

      <\errput>
        // Parsing s

        // Success

        // compiling s
      </errput>
    </unfolded-io-math>

    <\textput>
      The following limit is smaller than 1, hence the series converges.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|lim<rsub|n\<rightarrow\>+\<infty\>>
      <around*|\<nobracket\>|<around*|\||<frac|s<around*|(|n+1|)>|s<around*|(|n|)>>|\|>|\<nobracket\>>|)>
    <|unfolded-io-math>
      <equation*|<math|<frac|1|64>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      p\<assign\><around*|(|<big|sum><rsub|n=0><rsup|10>s
      <around*|(|n|)>|)><rsup|-1>
    <|unfolded-io-math>
      <equation*|<math|<frac|332306998946228968225951765070086144|105776603012651189498293061907704445>>>

      <\errput>
        Warning: solving in n equation 16*(2^n)^12*(n!)^6=0

        Unable to isolate function factorial
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|1+<around*|\<lfloor\>|-log<rsub|10>
      <around*|(|2*<around*|\||\<mathpi\>-p|\|>|)>|\<rfloor\>>|)>
    <|unfolded-io-math>
      <equation*|<math|20>>
    </unfolded-io-math>

    <\textput>
      The symbol <math|\<varepsilon\>> stands for <verbatim|epsilon()> in
      <name|Giac>, which is set to <math|10<rsup|-12>> by default.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      125.483*\<varepsilon\>
    <|unfolded-io-math>
      <equation*|<math|1.25483\<times\>10<rsup|-10>>>
    </unfolded-io-math>

    <\textput>
      Finite sequences, lists, and sets can be generated as in the examples
      below. The symbol <math|\<barsuchthat\>> is entered by pressing
      <key|\|><key|Tab><key|Tab><key|Tab><key|Tab>. Note that
      <math|\<barsuchthat\>> corresponds to the <verbatim|$> operator in
      Giac, which has a very high priority. Therefore the compound
      expressions on both sides of <math|\<barsuchthat\>> should be
      surrounded by hidden parentheses, as in the examples below.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      euler <around*|(|k|)>\<barsuchthat\><around*|\<nobracket\>|k=1\<ldots\>20|\<nobracket\>>
    <|unfolded-io-math>
      <equation*|<math|1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|ithprime <around*|(|j|)>\<barsuchthat\><around*|\<nobracket\>|j=1\<ldots\>20|\<nobracket\>>|]>
    <|unfolded-io-math>
      <equation*|<math|<around*|[|2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71|]>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      A\<assign\><around*|{|k<rsup|2>\<barsuchthat\><around*|\<nobracket\>|k=1\<ldots\>10|\<nobracket\>>|}>
    <|unfolded-io-math>
      <equation*|<math|<around*|{|1,4,9,16,25,36,49,64,81,100|}>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      B\<assign\><around*|{|<around*|\<nobracket\>|1+8*k|\<nobracket\>>\<barsuchthat\><around*|\<nobracket\>|k=1\<ldots\>10|\<nobracket\>>|}>
    <|unfolded-io-math>
      <equation*|<math|<around*|{|9,17,25,33,41,49,57,65,73,81|}>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      C\<assign\><around*|{|9,18,27|}>
    <|unfolded-io-math>
      <equation*|<math|<around*|{|9,18,27|}>>>
    </unfolded-io-math>

    <\textput>
      Set operations are entered in the usual way.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      A\<setminus\>B
    <|unfolded-io-math>
      <equation*|<math|<around*|{|1,4,16,36,64,100|}>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|A\<cap\>B|)>\<cup\>C
    <|unfolded-io-math>
      <equation*|<math|<around*|{|9,25,49,81,18,27|}>>>
    </unfolded-io-math>

    <\textput>
      Using the notation <math|a\<in\>A> we get the 1-based index of the
      element <math|a> in the set <math|A>, or 0 if <math|a\<nin\>A>.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      19\<in\>A,57\<in\>B
    <|unfolded-io-math>
      <equation*|<math|0,7>>
    </unfolded-io-math>

    <\textput>
      The usual notation for composition of functions is supported.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|cos\<circ\>sin|)><around*|(|\<mathpi\>|)>
    <|unfolded-io-math>
      <equation*|<math|1>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      f\<assign\>x\<mapsto\>x<rsup|2>+1;g\<assign\>y\<mapsto\>y-1
    <|unfolded-io-math>
      <equation*|<math|x\<mapsto\>x<rsup|2>+1,y\<mapsto\>y-1>>

      <\errput>
        // Success

        // End defining f

        // Success

        // End defining g
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|f\<circ\>g|)><around*|(|t|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|(|t-1|)><rsup|2>+1>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|g\<circ\>f|)><around*|(|t|)>
    <|unfolded-io-math>
      <equation*|<math|t<rsup|2>>>
    </unfolded-io-math>

    <\textput>
      Conditionals are entered like in the examples below.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<longequal\>2
    <|unfolded-io-math>
      <equation*|<math|false>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<neq\>0
    <|unfolded-io-math>
      <equation*|<math|true>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      assume <around*|(|h\<geqslant\>0|)>
    <|unfolded-io-math>
      <equation*|<math|h>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      h\<less\>h+1
    <|unfolded-io-math>
      <equation*|<math|true>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      sin <around*|(|h|)>-h\<leqslant\>2
    <|unfolded-io-math>
      <equation*|<math|true>>
    </unfolded-io-math>

    <\textput>
      Polynomials may be entered as lists of coefficients with double-struck
      brackets.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      p\<assign\><around*|\<llbracket\>|-1,3,2|\<rrbracket\>>;q\<assign\><around*|\<llbracket\>|2,0,-2,1|\<rrbracket\>>
    <|unfolded-io-math>
      <equation*|<math|<around*|\<llbracket\>|-1,3,2|\<rrbracket\>>,<around*|\<llbracket\>|2,0,-2,1|\<rrbracket\>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      p\<cdot\>q
    <|unfolded-io-math>
      <equation*|<math|<around*|\<llbracket\>|-2,6,6,-7,-1,2|\<rrbracket\>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      expand <around*|(|poly2symb <around*|(|p+q,x|)>|)>
    <|unfolded-io-math>
      <equation*|<math|2*x<rsup|3>-x<rsup|2>+x+3>>
    </unfolded-io-math>

    <\textput>
      Periodic functions can be defined, as demonstrated below.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      h\<assign\>periodic <around*|(|<around*|(|1-x<rsup|4>|)>*\<mathe\><rsup|1-x<rsup|3>>,x=-1\<ldots\>1|)>
    <|unfolded-io-math>
      <equation*|<math|<around*|(|1-<around*|(|x-2*<around*|\<lfloor\>|<frac|x+1|2>|\<rfloor\>>|)><rsup|4>|)>*\<mathe\><rsup|1-<around*|(|x-2*<around*|\<lfloor\>|<frac|x+1|2>|\<rfloor\>>|)><rsup|3>>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      plot <around*|(|h,x=-5\<ldots\>5|)>
    <|unfolded-io-math>
      <image|giac-demo.en-image-10.pdf|0.7par|||>
    </unfolded-io-math>

    <\textput>
      Piecewise functions can be entered by using the <markup|choice> tag, as
      in the example below. Note that any textual condition (a <markup|text>
      tag) is interpreted as \Potherwise\Q, no matter of its contents.
    </textput>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      f <around*|(|x|)>\<assign\><choice|<tformat|<table|<row|<cell|0,>|<cell|x\<less\>0>>|<row|<cell|x,>|<cell|x\<less\>1>>|<row|<cell|1,>|<cell|x\<less\>3>>|<row|<cell|\<mathe\><rsup|3-x>,>|<cell|<text|in
      all other cases>>>>>>
    <|unfolded-io-math>
      <equation*|<math|x\<mapsto\><choice|<tformat|<table|<row|<cell|0,>|<cell|x\<less\>0>>|<row|<cell|x,>|<cell|x\<less\>1>>|<row|<cell|1,>|<cell|x\<less\>3>>|<row|<cell|\<mathe\><rsup|3-x>,>|<cell|<text|otherwise>>>>>>>>

      <\errput>
        // Parsing f

        // Success

        // compiling f
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      plot <around*|(|f <around*|(|x|)>,x=-1\<ldots\>6|)>
    <|unfolded-io-math>
      <image|giac-demo.en-image-11.pdf|0.7par|||>
    </unfolded-io-math>
  </session>

  \;

  <tmdoc-copyright|2021|Luka Marohni¢>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>