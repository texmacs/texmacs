<TeXmacs|1.99.9>

<style|<tuple|letter|old-spacing>>

<\body>
  <chapter|TeXmacs overview>

  <section|Formatting primitives>

  This is a file which to test most of the TeXmacs constructs. First of all,
  collossoraspberrycoloredtyrannosaurodinosaure is a very long
  word.<htab|5mm>You agree?

  <\with|par-mode|center>
    You should!
  </with>

  Here comes a \Pline break\Q<line-break>, which does not do anything unless
  at the end of a line. The \Pno <no-break>line <no-break>break\Q symbol
  prevents from line breaking.<new-line>The new line symbol really starts a
  new paragraph, and the<next-line>\Pnext line\Q symbol just inserts a
  carriage return.

  The equantion array environment is based on the \Psplit\Q construct:

  <\eqnarray*>
    <tformat|<table|<row|<cell|a>|<cell|=>|<cell|b+c<space|0.6spc>;>>|<row|<cell|x>|<cell|=>|<cell|y<rsup|2>+z<rsup|2>.>>>>
  </eqnarray*>

  It is possible to insert<space|2cm|0fn|0.5fn>any amount of horizontal
  or<vspace|2cm>

  vertical white space. It is also possible to

  <surround||an object or to <block*|<tformat|<table|<row|<cell|<resize|resize|<minimum|1l|-1cm>|<minimum|1b|-1cm>|<maximum|1r|1cm>|<maximum|1t|1cm>>>>>>>
  it.|<with|par-mode|center|<move|move|0cm|1cm>>>

  The \Prigid group symbol\Q also prevents from line breaking. An empty group
  may be used to insert some invisible content, e.g.<rigid|> after a period
  in an abbreviation (so that less space is inserted).

  <section|Mathematics>

  Here is a formula with some of the most basic mathematical operators:

  <\equation*>
    sin z=<frac|1|x+y>+<sqrt|x>+<sqrt|y|3>+a<rsub|i>+b<rsup|2>.
  </equation*>

  Different types of indices, exponents and primes have been implemented:

  <\equation*>
    a<rsub|i>+b<rsup|2>+c<rsub|i><rsup|3>+\<rho\><rprime|'''>+<lprime|`>\<beta\>+<lsub|1><lsup|2>X<rsub|3><rsup|4>.
  </equation*>

  Here is a formula that demonstrates large delimiters and big operators:

  <\equation*>
    X=<around*|\<langle\>|<big|sum><rsub|i=1><rsup|\<infty\>><frac|a<rsub|i>|b<rsub|i>+c<rsub|i>><mid|\|><big|prod><rsub|i=1><rsup|\<infty\>><frac|1|1+\<alpha\>*z<rsup|2<rsup|i>>>|\<rangle\>>
  </equation*>

  Some other handy operators are \Pabove\Q and \Pbelow\Q

  <\equation*>
    <above|operator|script>+<below|operator|below>
  </equation*>

  and wide accents:

  <\equation*>
    <wide|a|^>+<wide|b|~>+<wide|c|\<bar\>>+<wide|d|\<vect\>>+<wide|e|\<check\>>+<wide|f|\<breve\>>+<wide|g|\<acute\>>+<wide|h|\<grave\>>+<wide|<wide|\<imath\>|^>|^>+<wide|\<alpha\>+\<beta\>|^>+<wide|\<gamma\>+\<delta\>|~>+<wide|\<varepsilon\>+\<varphi\>|\<bar\>>+<wide|\<zeta\>+\<eta\>|\<vect\>>+<wide|\<iota\>+\<kappa\>|\<check\>>+<wide|\<mu\>+\<nu\>|\<breve\>>.
  </equation*>

  Some mathematical symbols:

  <\equation*>
    \<alpha\>\<oplus\>b\<amalg\>c\<precprec\>x\<boxtimes\>y+\<nabla\><around|(|\<wp\><around|(|x<rsub|1>|)>,\<ldots\>,\<wp\><around|(|x<rsub|n>|)>|)>+<neg|x>+<neg|y>\<precprec\>S<rsub|a\<rightarrow\>b\<rightsquigarrow\>c>
  </equation*>

  We obtained <with|mode|math|<neg|x>> using the \Pnegation\Q construct. We
  conclude with a tree

  <\equation*>
    <tree|x|y|<tree|a|b|c|d>|<tree|a|x<rsup|2>|y<rsup|2>>|z|<tree|p|<tree|x|y|y>|<tree|y|x|x>>>
  </equation*>

  and some tabular material:

  <\equation*>
    <around*|(|<tabular*|<tformat|<table|<row|<cell|a<rsub|1,1>>|<cell|\<cdots\>>|<cell|a<rsub|1,n>>>|<row|<cell|\<vdots\>>|<cell|>|<cell|\<vdots\>>>|<row|<cell|a<rsub|n,1>>|<cell|\<cdots\>>|<cell|a<rsub|n,n>>>>>>|)><space|8spc><block*|<tformat|<table|<row|<cell|b<rsub|1,1>>|<cell|\<cdots\>>|<cell|b<rsub|1,n>>>|<row|<cell|\<vdots\>>|<cell|>|<cell|\<vdots\>>>|<row|<cell|b<rsub|n,1>>|<cell|\<cdots\>>|<cell|b<rsub|n,n>>>>>><space|8spc><tabular|<tformat|<cwith|1|-1|1|-1|cell
    mode|c>|<cwith|1|2|1|2|cell-halign|c>|<cwith|1|3|1|3|cell-halign|r>|<cwith|2|2|2|2|cell-halign|c>|<cwith|2|3|2|3|cell-halign|r>|<cwith|3|2|3|2|cell-halign|c>|<cwith|3|3|3|3|cell-halign|r>|<table|<row|<cell|links
    boven>|<cell|<with|color|blue|boven>>|<cell|rechts
    boven>>|<row|<cell|links>|<cell|<with|color|blue|centrum>>|<cell|rechts>>|<row|<cell|links
    onder>|<cell|<with|color|blue|onder>>|<cell|rechts onder>>>>>
  </equation*>

  <section|Dynamic constructs>

  Some examples of inactive constructs are <inactive|<symbol|symbol>>,
  <inactive|<hybrid|command>> and <inactive|<plus|1|1>>. Let us show some
  examples of how such constructs look like when activated:

  <\eqnarray*>
    <tformat|<table|<row|<cell|<inactive|<symbol|alpha>>>|<cell|\<longrightarrow\>>|<cell|\<alpha\>>>|<row|<cell|<inactive|<hybrid|sin>>>|<cell|\<longrightarrow\>>|<cell|sin>>|<row|<cell|<inactive|<hybrid|x>>>|<cell|\<longrightarrow\>>|<cell|<x>>>|<row|<cell|<inactive|<assign|x|sin
    y>><inactive|<hybrid|x>>>|<cell|\<longrightarrow\>>|<cell|<assign|x|sin
    y><x>>>|<row|<cell|<inactive|<assign|f|<inactive|<macro|a|<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>>>>><inactive|<f|b>>>|<cell|\<longrightarrow\>>|<cell|<assign|f|<macro|a|<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>>><f|b><space|25spc>>>|<row|<cell|<inactive|<plus|1|1>>>|<cell|\<longrightarrow\>>|<cell|<plus|1|1>>>|<row|<cell|<inactive|<minus|7|4>>>|<cell|\<longrightarrow\>>|<cell|<minus|7|4>>>|<row|<cell|<inactive|<times|3|3>>>|<cell|\<longrightarrow\>>|<cell|<times|3|3>>>|<row|<cell|<inactive|<merge|Hi|There>>>|<cell|\<longrightarrow\>>|<cell|<merge|Hi|There>>>|<row|<cell|<inactive|<number|1993|Roman>>>|<cell|\<longrightarrow\>>|<cell|<number|1993|Roman>>>|<row|<cell|<inactive|<translate|File|english|french>>>|<cell|\<longrightarrow\>>|<cell|<translate|File|english|french>>>>>
  </eqnarray*>

  Some examples of environments in the present \Pletter\Q style are:

  <\itemize>
    <item><with|color|red|First item> in red.

    <item><with|font-shape|italic|Second item> in italic.

    <item><with|font-series|bold|And so on>...
  </itemize>

  <surround||A numbered equation with label \Peq\Q:|<\theorem>
    The following conditions are equivalent:

    <\enumerate-alpha>
      <item><with|mode|math|P\<Longleftrightarrow\>Q>;

      <item><with|mode|math|Q\<Longleftrightarrow\>P>.
    </enumerate-alpha>

    \;
  </theorem>>

  <\equation>
    a<rsup|2>+b<rsup|2>=c<rsup|2><label|eq>
  </equation>

  A reference to the equation (<reference|eq>).

  Here comes an active hyperlink to <hlink|http://www.gnu.org|http://www.gnu.org>.
  Click <action|here|(lambda () (display "Hello\\n"))> to print hello to the
  standard output. <specific|texmacs|This text><specific|latex|some funny
  stuff> is only visible inside TeXmacs and will be translated to \Psome
  funny stuff\Q when converted to LaTeX. We conclude with a picture:

  <\with|par-mode|center>
    <image|../../misc/pixmaps/TeXmacs-solid.xpm||||>
  </with>

  \;
</body>

<\initial>
  <\collection>
    <associate|par-hyphen|normal>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1|?>>
    <associate|auto-3|<tuple|2|?>>
    <associate|auto-4|<tuple|3|?>>
    <associate|eq|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|1<space|2spc>TeXmacs
      overview> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|1fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Formatting
      primitives> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Mathematics>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Dynamic
      constructs> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>