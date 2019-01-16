<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Semantics of mathematical symbols>

  The mathematical symbols in <TeXmacs> all come with a certain number of
  properties which correspond to their intended meaning. For instance,
  <TeXmacs> is aware that <math|<op|+>> is an infix operator, whereas ! is
  rather a postfix, and , a separator.

  <TeXmacs> has special symbols <math|\<mathe\>=2.71828\<cdots\>>,
  <math|\<mathpi\>=3.14159\<cdots\>> and <math|\<mathi\>> for important
  mathematical constants, which display differently from the mere characters
  <math|e>, <math|\<pi\>> and <math|i>, and which can be entered using the
  shortcuts <shortcut|\<mathe\>>, <shortcut|\<mathpi\>> and
  <shortcut|\<mathi\>>. We recommend to systematically use these shortcuts.

  Inversely, semantically distinct symbols may display in a similar way. For
  instance, the comma separator, as in <math|f<around|(|x,y|)>>, is different
  from the decimal comma, as in <math|3\<comma\>14159\<cdots\>>. Notice that
  the two symbols admit different spacing rules. Semantically distinct
  symbols which are rendered by the same glyph are called <em|homoglyphs>.
  Notice that our semantics is purely syntactic: for instance,
  the<nbsp><math|+> infix is commonly used for addition, but sometimes also
  for the concatenation of strings. Nevertheless, these two uses do not
  differ from a syntactical point of view, since the<nbsp><math|+> symbol
  remains a binary infix with the same precedence with respect to other
  symbols.

  The most confusing homoglyphs are the various invisible symbols supported
  by <TeXmacs>:

  <\itemize>
    <item>The multiplication, entered by <key|*>. Example: <math|a*b>.

    <item>Function application, entered by <key|space>. Example: <math|sin
    x>.

    <item>An invisible separator, entered by <shortcut|\<nocomma\>>. Example:
    the matrix <math|A=<around|(|a<rsub|i\<nocomma\>j>|)>>.

    <item>An invisible addition, entered by <shortcut|\<noplus\>>. Example:
    <math|17\<noplus\><frac*|3|8>>.

    <item>An invisible symbol, entered by <shortcut|\<nosymbol\>>. Example:
    the increment <math|\<nosymbol\>+1>.

    <item><label|nobracket>An invisible bracket (mainly for internal use). A
    matching pair of invisible brackets is entered using <key|( var>.
  </itemize>

  Again it is recommended that authors carefully enter these various
  invisible symbols when appropriate. It is particularly important to
  distinguish between multiplication and function application, since there is
  no 100% safe automatic way to make this distinction (we already mentioned
  the formulas <math|a<around|(|b+c|)>> and <math|f<around|(|x+y|)>> before).

  <TeXmacs> supports two quite general schemes for entering homoglyphs. On
  the one hand, we often rely on the standard variant system. For instance,
  <math|\<times\>> and <math|\<ast\>> are obtained using <shortcut|\<times\>>
  and <shortcut|\<ast\>>. In table<nbsp><reference|homoglyph-table> we have
  given the complete list of homoglyphs supported by <TeXmacs>.

  <big-table|<block|<tformat|<table|<row|<cell|Shortcut>|<cell|Glyph>|<cell|Example>|<cell|Semantics>>|<row|<cell|<key|*>>|<cell|>|<cell|<math|a*b>>|<cell|Multiplication>>|<row|<cell|<key|space>>|<cell|>|<cell|<math|sin
  x>>|<cell|Function application>>|<row|<cell|<shortcut|\<nocomma\>>>|<cell|>|<cell|<math|a<rsub|i\<nocomma\>j>=a<rsub|j\<nocomma\>i>>>|<cell|Invisible
  separator>>|<row|<cell|<shortcut|\<noplus\>>>|<cell|>|<cell|<with|mode|math|17\<noplus\><frac*|3|8>>>|<cell|Invisible
  addition>>|<row|<cell|<shortcut|\<nosymbol\>>>|<cell|>|<cell|<math|\<nosymbol\>+1>>|<cell|Invisible
  symbol>>|<row|<cell|<key|( var>>|<cell|>|<cell|<math|\<Phi\>\<equiv\><around*|\<nobracket\>|\<forall\>x,P<around*|(|x|)>|\<nobracket\>>>>|<cell|Invisible
  bracket(s)>>|<row|<cell|<key|\|>>|<cell|<math|\|>>|<cell|<math|<around*|\||-x|\|>=<around*|\||x|\|>>>|<cell|Absolute
  value>>|<row|<cell|<key|\| var>>|<cell|<math|\|>>|<cell|<math|<around*|{|x\<in\>\<bbb-R\>\|x\<gtr\>0|}>>>|<cell|Separating
  bar>>|<row|<cell|<key|\| var var>>|<cell|<math|\|>>|<cell|<math|<around*|\<langle\>|a<rsub|i><rsup|2><mid|\|>a<rsub|j><rsup|2>|\<rangle\>>>>|<cell|Extensible
  middle bar>>|<row|<cell|<key|\| var var var
  var>>|<cell|<math|\|>>|<cell|<math|11\<divides\>1001>>|<cell|Divides
  relation>>|<row|<cell|<key|,>>|<cell|,>|<cell|<math|f<around*|(|x,y|)>>>|<cell|Comma
  separator>>|<row|<cell|<key|, var>>|<cell|,>|<cell|<math|123\<comma\>456>>|<cell|Decimal
  comma>>|<row|<cell|<key|.>>|<cell|.>|<cell|<math|123.456>>|<cell|Decimal
  point>>|<row|<cell|<key|. var>>|<cell|.>|<cell|<math|\<mathlambda\>x\<point\>x<rsup|2>>>|<cell|Dot
  connector>>|<row|<cell|<key|* var var var>>|<cell|<math|\<cdot\>>>|<cell|<math|\<b-v\>\<cdot\>\<b-w\>>>|<cell|Dot
  multiplication>>|<row|<cell|<key|. var var>>|<cell|<math|\<cdummy\>>>|<cell|<math|\<cdummy\>+1>>|<cell|Dummy
  wildcard>>|<row|<cell|<key|:>>|<cell|<math|:>>|<cell|<math|<around*|{|x\<in\>E:P<around*|(|x|)>|}>>>|<cell|Separator>>|<row|<cell|<shortcut|\<of\>>>|<cell|<math|\<of\>>>|<cell|<math|x\<of\><math-ss|Int>>>|<cell|Type
  satisfaction>>|<row|<cell|<shortcut|\<over\>>>|<cell|<math|\<over\>>>|<cell|<math|121\<over\>11=11>>|<cell|Division>>|<row|<cell|<key|\\
  var>>|<cell|<math|\\>>|<cell|<math|\\x>>|<cell|Backslash>>|<row|<cell|<key|\\
  var var>>|<cell|<math|\\>>|<cell|<math|\<bbb-N\><rsup|\<gtr\>>=\<bbb-N\>\<setminus\><around*|{|0|}>>>|<cell|Set
  minus>>|<row|<cell|<key|&>>|<cell|<math|\<wedge\>>>|<cell|<math|1=1\<wedge\>2=2>>|<cell|Logical
  and>>|<row|<cell|<key|* &>>|<cell|<math|\<exterior\>>>|<cell|<math|\<mathd\>x\<exterior\>\<mathd\>y>>|<cell|Wedge
  product>>>>>|<label|homoglyph-table>Homoglyphs supported by <TeXmacs>.>

  <tmdoc-copyright|2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>