<TeXmacs|2.1.4>

<style|<tuple|generic|british>>

<\body>
  <\hide-preamble>
    <assign|showcase|<\macro|fn>
      <\with|font|<arg|fn>>
        <section*|<arg|fn>>

        <strong|Alphabets.> Latin <math|a b c d e f g h i j k l m n o p q r
        s t u v w x y z>, <math|A B C D E F G H I J K L M N O P Q R S T U V W
        X Y Z>, digits <math|0 1 2 3 4 5 6 7 8 9>, Greek
        <math|\<alpha\> \<beta\> \<gamma\> \<delta\> \<epsilon\> \<zeta\>
        \<eta\> \<theta\> \<iota\> \<kappa\> \<lambda\> \<mu\> \<nu\> \<xi\>
        \<pi\> \<rho\> \<sigma\> \<tau\> \<upsilon\> \<phi\> \<chi\> \<psi\>
        \<omega\> \<varepsilon\> \<vartheta\> \<varphi\>>, <math|\<Gamma\>
        \<Delta\> \<Theta\> \<Lambda\> \<Xi\> \<Pi\> \<Sigma\> \<Phi\> \<Psi\>
        \<Omega\>>, bold <math|<math-bf|a b c x y z A B C X Y Z>>, upright
        <math|<math-up|a b c A B C>>, calligraphic <math|\<cal-A\>\<cal-B\>\<cal-C\>\<cal-D\>\<cal-E\>\<cal-F\>\<cal-G\>\<cal-H\>\<cal-I\>\<cal-J\>\<cal-K\>\<cal-L\>>, fraktur <math|\<frak-A\>\<frak-B\>\<frak-C\>\<frak-D\>\<frak-a\>\<frak-b\>\<frak-c\>\<frak-d\>>,
        blackboard bold <math|\<bbb-N\>\<bbb-Z\>\<bbb-Q\>\<bbb-R\>\<bbb-C\>\<bbb-H\>>, sans serif
        <math|<math-ss|A B C a b c>>, typewriter <math|<math-tt|A B C a b
        c>>.

        <strong|Scripts and kerning.> <math|V<rsub|a><rsup|2>*f<rsup|2>*A<rsup|T>*W<rsub|i>*T<rsub|j><rsup|3>*x<rsub|1><rsup|2>*e<rsup|x<rsup|2>>*y<rsub|k<rsub|1>>*f<rprime|'><around*|(|x|)>*<lsub|a>X<rsub|b>*P<rsub|i><rsup|j>*<around*|(|x+y|)><rsup|2>*<around*|(|<frac|a|b>|)><rsup|n>*<big|int><rsub|0><rsup|1>*<big|sum><rsub|k=1><rsup|n>*\<Gamma\><rsup|2>*\<gamma\><rsub|0>*7<rsup|2>*7<rsub|1>>
        and in display style

        <\equation*>
          V<rsub|a><rsup|2>+f<rsup|2>+A<rsup|T>+W<rsub|i>+T<rsub|j><rsup|3>+e<rsup|x<rsup|2>+y<rsup|2>>+<around*|(|<frac|a|b>|)><rsup|n>+<big|int><rsub|0><rsup|1>f<around*|(|x|)>*\<mathd\>x+<big|sum><rsub|k=1><rsup|n>a<rsub|k>+<big|prod><rsub|k=1><rsup|n>b<rsub|k>
        </equation*>

        <strong|Fractions.> <math|<frac|a|b>+<frac|a+b|c+d>+<frac|1|1+<frac|1|1+<frac|1|x>>>+<binom|n|k>+<frac|\<partial\>f|\<partial\>x>+<frac|1|2>*x<rsup|2>>
        and in display style

        <\equation*>
          <frac|a|b>+<frac|a+b|c+d>+<frac|1|1+<frac|1|1+<frac|1|x>>>+<binom|n|k>+<frac|\<partial\><rsup|2>f|\<partial\>x<rsup|2>>+<frac|<frac|a|b>|<frac|c|d>>+<frac|1|2>*x<rsup|2>
        </equation*>

        <strong|Radicals.> <math|<sqrt|2>+<sqrt|x<rsup|2>+y<rsup|2>>+<sqrt|<frac|a|b>|3>+<sqrt|<sqrt|<sqrt|x>>>+<sqrt|a<rsup|2>|n>+<sqrt|<frac|<frac|1|2>|<frac|3|4>>>>
        and in display style

        <\equation*>
          <sqrt|2>+<sqrt|x<rsup|2>+y<rsup|2>>+<sqrt|<frac|a|b>|3>+<sqrt|<sqrt|<sqrt|x>>>+<sqrt|a<rsup|2>|n>+<sqrt|<frac|<frac|1|2>|<frac|3|4>>>+<sqrt|<big|sum><rsub|k=1><rsup|n><frac|1|k<rsup|2>>|k+1>
        </equation*>

        <strong|Delimiters.> <math|<around*|(|x|)>*<around*|[|x|]>*<around*|{|x|}>*<around*|\<langle\>|x|\<rangle\>>*<around*|\<lfloor\>|x|\<rfloor\>>*<around*|\<lceil\>|x|\<rceil\>>*<around*|\||x|\|>*<around*|\<\|\|\>|x|\<\|\|\>>*<around*|(|<frac|a|b>|)>*<around*|[|<frac|<frac|a|b>|c>|]>*<around*|{|<frac|1|<frac|1|<frac|1|x>>>|}>>
        and in display style

        <\equation*>
          <around*|(|<frac|a|b>|)>+<around*|[|<frac|<frac|a|b>|c>|]>+<around*|{|<frac|1|<frac|1|<frac|1|x>>>|}>+<around*|\<langle\>|<frac|<frac|a|b>|<frac|c|d>>|\<rangle\>>+<around*|\<lfloor\>|<frac|<frac|a|b>|<frac|c|d>>|\<rfloor\>>+<around*|\||<frac|1|<frac|1|<frac|1|<frac|1|x>>>>|\|>+<around*|\<\|\|\>|<frac|1|<frac|1|<frac|1|<frac|1|x>>>>|\<\|\|\>>+<around*|(|<matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>|)>+<choice|<tformat|<table|<row|<cell|x>|<cell|x\<geq\>0>>|<row|<cell|-x>|<cell|x\<less\>0>>>>>
        </equation*>

        <strong|Big operators.> <math|<big|sum><rsub|k=1><rsup|n>k+<big|prod><rsub|p>p+<big|int><rsub|0><rsup|1>f+<big|iint><rsub|D>g+<big|oint><rsub|\<gamma\>>h+<big|cap><rsub|i>A<rsub|i>+<big|cup><rsub|i>B<rsub|i>+<big|wedge><rsub|i>\<omega\><rsub|i>+<big|oplus><rsub|i>V<rsub|i>>
        and in display style

        <\equation*>
          <big|sum><rsub|k=1><rsup|n>k+<big|prod><rsub|p<space|0.2spc>prime>p+<big|int><rsub|0><rsup|1>f<around*|(|x|)>*\<mathd\>x+<big|iint><rsub|D>g+<big|iiint><rsub|Q>h+<big|oint><rsub|\<gamma\>>\<omega\>+<big|cap><rsub|i\<in\>I>A<rsub|i>+<big|cup><rsub|i\<in\>I>B<rsub|i>+<big|wedge><rsub|i>\<omega\><rsub|i>+<big|oplus><rsub|i=1><rsup|\<infty\>>V<rsub|i>+<big|intlim><rsub|0><rsup|\<infty\>>e<rsup|-t>*\<mathd\>t
        </equation*>

        <strong|Accents.> <math|<wide|x|^>+<wide|i|^>+<wide|j|~>+<wide|x|~>+<wide|x|\<bar\>>+<wide|v|\<vect\>>+<wide|x|\<check\>>+<wide|x|\<breve\>>+<wide|x|\<dot\>>+<wide|x|\<ddot\>>+<wide|x|\<acute\>>+<wide|x|\<grave\>>+<wide|xyz|^>+<wide|abcdef|~>+<wide|x+y|\<bar\>>+<wide|abc|\<vect\>>+<wide|<around*|(|A|)>|^>+<wide|<frac|a|b>|~>>
        and braces

        <\equation*>
          <wide|a+b|\<overbrace\>>+<wide*|a<rsub|1>+\<cdots\>+a<rsub|n>|\<underbrace\>>+<wide|u+v+w|\<sqoverbrace\>>+<wide*|x|\<bar\>>+<wide*|x+y+z|\<bar\>>+<wide|f+g+h|\<vect\>>+<wide|\<alpha\>|^>+<wide|\<Gamma\>|~>
        </equation*>

        <strong|Arrows and relations.> <math|A<long-arrow|\<rubber-rightarrow\>|f>B<long-arrow|\<rubber-leftarrow\>|g|h>C<long-arrow|\<rubber-longleftrightarrow\>|\<sim\>>D<long-arrow|\<rubber-longmapsto\>|\<phi\>>E>,
        <math|x\<neq\>y>, <math|a\<nin\>A>, <math|<neg|\<subset\>>>,
        <math|<neg|\<rightarrow\>>>, <math|a\<leq\>b\<geq\>c\<ll\>d\<gg\>e\<approx\>f\<simeq\>g\<equiv\>h\<prec\>i\<succ\>j\<subset\>k\<supset\>l\<in\>m\<ni\>n>,
        <math|\<forall\>x\<exists\>y:x\<rightarrow\>y\<Rightarrow\>z\<leftrightarrow\>w\<mapsto\>v\<hookrightarrow\>u>,
        <math|\<partial\>\<nabla\>\<infty\>\<emptyset\>\<hbar\>\<ell\>\<wp\>\<Re\>\<Im\>\<aleph\>\<top\>\<bot\>\<angle\>\<triangle\>\<diamond\>\<star\>\<dag\>\<ddag\>>.

        <\theorem>
          <dueto|Residue theorem>Let <math|f> be analytic in the region
          <math|G> except for the isolated singularities
          <math|a<rsub|1>,a<rsub|2>,\<ldots\>,a<rsub|m>>. If <math|\<gamma\>>
          is a closed rectifiable curve in <math|G> which does not pass
          through any of the points <math|a<rsub|k>> and if
          <math|\<gamma\>\<approx\>0> in <math|G>, then

          <\equation*>
            <frac|1|2*\<pi\>*i>*<big|int><rsub|\<gamma\>>f<around*|(|z|)>*\<mathd\>z=<big|sum><rsub|k=1><rsup|m>n<around|(|\<gamma\>;a<rsub|k>|)>*<op|Res><around|(|f;a<rsub|k>|)><space|0.17em>.
          </equation*>
        </theorem>

        <new-page>
      </with>
    </macro>>
  </hide-preamble>

  <showcase|roman>

  <showcase|TeX Gyre Pagella>

  <showcase|TeX Gyre Termes>

  <showcase|TeX Gyre Bonum>

  <showcase|TeX Gyre Schola>

  <showcase|Stix>

  <showcase|Latin Modern Math>

  <showcase|Stix Two Math>

  <showcase|Asana Math>

  <showcase|Fira Math>

  <showcase|KpMath>

  <showcase|TeX Gyre DejaVu Math>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>
