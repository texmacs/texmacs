<TeXmacs|0.3.2-3>

<style|letter>

<\body>
  <chapter|TeXmacs overview>

  <section|Formatting primitives>

  This is a file which to test most of the TeXmacs constructs. First of all,
  collossoraspberrycoloredtyrannosaurodinosaure is a very long
  word.<htab|5mm>You agree?

  <set|paragraph mode|center>You should!<reset|paragraph mode>

  Here comes a ``line break''<format|line break>, which does not do anything
  unless at the end of a line. The ``no <format|no line break>line <format|no
  line break>break'' symbol prevents from line breaking.<format|new line>The
  new line symbol really starts a new paragraph, and the<format|next
  line>``next line'' symbol just inserts a carriage return.

  The equantion array environment is based on the ``split'' construct:

  <begin|eqnarray*>a<format|line separator>=<format|line
  separator>b+c<space|0.6spc>;<format|next line>x<format|line
  separator>=<format|line separator>y<rsup|2>+z<rsup|2>.<end|eqnarray*>

  It is possible to insert<space|2cm|0fn|0.5fn>any amount of horizontal
  or<vspace|2cm>

  vertical white space. It is also possible to

  <set|paragraph mode|center><move|move|0cm|1cm><reset|paragraph mode>an
  object or to <table|<resize|resize|extend|-1cm|-1cm|1cm|1cm>|1|1> it.

  The ``group symbol'' also prevents from line breaking. An empty group may
  be used to insert some invisible content, e.g.<group|> after a period in an
  abbreviation (so that less space is inserted).

  <section|Mathematics>

  Here is a formula with some of the most basic mathematical operators:

  <begin|equation*>sin z=<frac|1|x+y>+<sqrt|x>+<sqrt|y|3>+a<rsub|i>+b<rsup|2>\
  .<end|equation*>

  Different types of indices, exponents and primes have been implemented:

  <begin|equation*>a<rsub|i>+b<rsup|2>+c<rsub|i><rsup|3>+\<rho\><rprime|'''>+\
  <lprime|`>\<beta\>+<lsub|1><lsup|2>X<rsub|3><rsup|4>.<end|equation*>

  Here is a formula that demonstrates large delimiters and big operators:

  <begin|equation*>X=<left|langle><big|sum><rsub|i=1><rsup|\<infty\>><frac|a<\
  rsub|i>|b<rsub|i>+c<rsub|i>><mid|\|><big|prod><rsub|i=1><rsup|\<infty\>><fr\
  ac|1|1+\<alpha\>*z<rsup|2<rsup|i>>><right|rangle><end|equation*>

  Some other handy operators are ``above'' and ``below''

  <begin|equation*><above|operator|script>+<below|operator|below><end|equatio\
  n*>

  and wide accents:

  <begin|equation*><wide|a|^>+<wide|b|~>+<wide|c|\<bar\>>+<wide|d|\<vect\>>+<\
  wide|e|\<check\>>+<wide|f|\<breve\>>+<wide|g|\<acute\>>+<wide|h|\<grave\>>+\
  <wide|<wide|\<imath\>|^>|^>+<wide|\<alpha\>+\<beta\>|^>+<wide|\<gamma\>+\<d\
  elta\>|~>+<wide|\<varepsilon\>+\<varphi\>|\<bar\>>+<wide|\<zeta\>+\<eta\>|\\
  <vect\>>+<wide|\<iota\>+\<kappa\>|\<check\>>+<wide|\<mu\>+\<nu\>|\<breve\>>\
  .<end|equation*>

  Some mathematical symbols:

  <begin|equation*>\<alpha\>\<oplus\>b\<amalg\>c\<precprec\>x\<boxtimes\>y+\<\
  nabla\>(\<wp\>(x<rsub|1>),\<ldots\>,\<wp\>(x<rsub|n>))+<neg|x>+<neg|y>\<pre\
  cprec\>S<rsub|a\<rightarrow\>b\<rightsquigarrow\>c><end|equation*>

  We obtained <set|mode|math><neg|x><reset|mode> using the ``negation''
  construct. We conclude with a tree

  <begin|equation*><tree|x|y|<tree|a|b|c|d>|<tree|a|x<rsup|2>|y<rsup|2>>|z|<t\
  ree|p|<tree|x|y|y>|<tree|y|x|x>>><end|equation*>

  and some tabular material:

  <begin|equation*><left|(><matrix|a<rsub|1,1>|\<cdots\>|a<rsub|1,n>|\<vdots\\
  >||\<vdots\>|a<rsub|n,1>|\<cdots\>|a<rsub|n,n>|3|3><right|)><space|8spc><ta\
  ble|b<rsub|1,1>|\<cdots\>|b<rsub|1,n>|\<vdots\>||\<vdots\>|b<rsub|n,1>|\<cd\
  ots\>|b<rsub|n,n>|3|3><space|8spc><mosaic|<mosaic_item|links
  boven|0w|1|1|none>|<mosaic_item|<set|color|blue>boven<reset|color>|0c|1|1|n\
  one>|<mosaic_item|rechts boven|0e|1|1|none>|<mosaic_item|links|0w|1|1|none>\
  |<mosaic_item|<set|color|blue>centrum<reset|color>|0c|1|1|none>|<mosaic_ite\
  m|rechts|0e|1|1|none>|<mosaic_item|links
  onder|0w|1|1|none>|<mosaic_item|<set|color|blue>onder<reset|color>|0c|1|1|n\
  one>|<mosaic_item|rechts onder|0e|1|1|none>|3|3><end|equation*>

  <section|Dynamic constructs>

  Some examples of inactive constructs are <inactive|<symbol|symbol>>,
  <inactive|<hybrid|command>> and <inactive|<plus|1|1>>. Let us show some
  examples of how such constructs look like when activated:

  <begin|eqnarray*><inactive|<symbol|alpha>><format|line
  separator>\<longrightarrow\><format|line separator>\<alpha\><format|next
  line><inactive|<hybrid|sin>><format|line
  separator>\<longrightarrow\><format|line separator>sin<format|next
  line><inactive|<hybrid|x>><format|line separator>\<longrightarrow\><format|\
  line separator><x><format|next line><inactive|<assign|x|sin
  y>><inactive|<hybrid|x>><format|line separator>\<longrightarrow\><format|li\
  ne separator><assign|x|sin y><x><format|next
  line><inactive|<assign|f|<inactive|<func|a|<a><rsub|1>,\<ldots\>,<a><rsub|n\
  >>>>><inactive|<f|b>><format|line separator>\<longrightarrow\><format|line
  separator><assign|f|<func|a|<a><rsub|1>,\<ldots\>,<a><rsub|n>>><f|b><space|\
  25spc><format|next line><inactive|<plus|1|1>><format|line
  separator>\<longrightarrow\><format|line separator><plus|1|1><format|next
  line><inactive|<minus|7|4>><format|line
  separator>\<longrightarrow\><format|line separator><minus|7|4><format|next
  line><inactive|<times|3|3>><format|line
  separator>\<longrightarrow\><format|line separator><times|3|3><format|next
  line><inactive|<merge|Hi|There>><format|line
  separator>\<longrightarrow\><format|line
  separator><merge|Hi|There><format|next line><inactive|<number|1993|Roman>><\
  format|line separator>\<longrightarrow\><format|line
  separator><number|1993|Roman><format|next
  line><inactive|<translate|File|english|french>><format|line
  separator>\<longrightarrow\><format|line
  separator><translate|File|english|french><end|eqnarray*>

  Some examples of environments in the present ``letter'' style are:

  <begin|itemize><item><set|color|red>First item<reset|color> in red.

  <item><set|font shape|italic>Second item<reset|font shape> in italic.

  <item><set|font series|bold>And so on<reset|font series>...<end|itemize>

  <begin|theorem>The following conditions are equivalent:

  <begin|enumerate-alpha><item><set|mode|math>P\<Longleftrightarrow\>Q<reset|\
  mode>;

  <item><set|mode|math>Q\<Longleftrightarrow\>P<reset|mode>.<end|enumerate-al\
  pha>

  <end|theorem>A numbered equation with label ``eq'':

  <begin|equation>a<rsup|2>+b<rsup|2>=c<rsup|2><end|equation><label|eq>

  A reference to the equation (<reference|eq>).

  Here comes an active hyperlink to <hlink|http://www.gnu.org|http://www.gnu.\
  org>. Click <action|here|(display "Hello\\n")> to print hello to the
  standard output. <specific|texmacs|This text><specific|latex|some funny
  stuff> is only visible inside TeXmacs and will be translated to ``some
  funny stuff'' when converted to LaTeX. We conclude with a picture:

  <set|paragraph mode|center><postscript|../../misc/pixmaps/TeXmacs-solid.xpm\
  ||||||><reset|paragraph mode>

  \;
</body>

<\references>
  <\collection>
    <associate|eq|1>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate>
      toc
    <|associate>
      <vspace*|1fn><set|font series|<quote|bold>>TeXmacs overview<reset|font
      series><htab|5mm>?<vspace|0.5fn>

      Formatting primitives<htab|5mm>?

      Mathematics<htab|5mm>?

      Dynamic constructs<htab|5mm>?
    </associate>
  </collection>
</auxiliary>
