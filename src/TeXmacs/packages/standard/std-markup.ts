<TeXmacs|1.0.2.5>

<\body>
  <assign|std-markup-package|1.0>

  <assign|std-markup-dtd|1.0>

  \;

  <assign|TeXmacs|<macro|T<rsub|<space|-0.4spc><move|<resize|<with|index
  level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>>>

  <assign|made-by-TeXmacs|<macro|<float|footnote||<with|font
  size|0.84|paragraph mode|justify|left margin|0cm|right
  margin|0cm|<move|<postscript|local:$TEXMACS_PATH/misc/images/tm_gnu3.ps||1fn||||>|0fn|-0.2fn><space|2spc><translate|This
  document has been produced using|english|<apply|language>> GNU <TeXmacs>
  (<translate|see|english|<apply|language>> <with|font
  family|tt|http://www.texmacs.org>).<apply|rightflush>>>>>

  <assign|TeX|<macro|T<rsub|<space|-0.4spc><move|<resize|<with|index
  level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>

  <assign|LaTeX|<macro|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|index
  level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>

  <assign|hflush|<func|<htab|0fn|0>>>

  <assign|rightflush|<func|<htab|0fn|first>>>

  <assign|leftflush|<func|<htab|0fn|last>>>

  <assign|hrule|<no_first_indentation><tabular|<tformat|<cwith|1|-1|1|-1|cell
  tborder|1ln>|<twith|table width|1par>|<cwith|1|-1|1|-1|cell
  vmode|exact>|<cwith|1|-1|1|-1|cell height|1ln>|<cwith|1|-1|1|-1|cell
  lsep|0fn>|<cwith|1|-1|1|-1|cell rsep|0fn>|<cwith|1|-1|1|-1|cell
  bsep|0fn>|<cwith|1|-1|1|-1|cell tsep|0fn>|<cwith|1|-1|1|-1|cell
  vcorrect|n>|<table|<row|<cell|<space|1fn|0ln|1ln>>>>>>>

  \;

  <assign|localize|<func|x|<translate|<value|x>|english|<value|language>>>>

  <assign|overline|<macro|x|<hold|<datoms|<macro|x|<with|color|<release|<value|color>>|<wide|<arg|x>|\<wide-bar\>>>>|<arg|x>>>>>

  <drd_props|overline|accessible|all>

  <assign|underline|<macro|x|<hold|<datoms|<macro|x|<with|color|<release|<value|color>>|<wide*|<arg|x>|\<wide-bar\>>>>|<arg|x>>>>>

  <drd_props|underline|accessible|all>

  <assign|fold|<macro|x|y|<with|left margin|<plus|<value|left
  margin>|1.5fn>|<surround|<with|first indentation|-1.5fn|<enable_first_indentation>><action|<resize|<with|mode|math|<op|\<circ\>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>|<apply|hflush>|<arg|x>>>>>

  <assign|unfold|<\macro|x|y>
    <\with|left margin|<plus|<value|left margin>|1.5fn>>
      <surround|<with|first indentation|-1.5fn|<enable_first_indentation>><action|<resize|<with|mode|math|\<bullet\>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>|<apply|hflush>|<arg|x>>

      <surround||<apply|rightflush>|<arg|y>>
    </with>
  </macro>>

  <assign|switch|<macro|x|y|<surround||<apply|rightflush>|<arg|x>>>>

  <assign|phantom|<func|x|<var_if|false|<apply|x>>>>

  <assign|set-header|<func|s|<assign|odd page header|<apply|s>><assign|even
  page header|<apply|s>>>>

  <assign|set-footer|<func|s|<assign|odd page footer|<apply|s>><assign|even
  page footer|<apply|s>>>>

  \;

  <assign|strong|<macro|x|<with|font series|bold|math font
  series|bold|<arg|x>>>>

  <assign|em|<macro|x|<with|font shape|italic|<arg|x>>>>

  <assign|tt|<macro|x|<with|font family|tt|<arg|x>>>>

  <assign|name|<macro|x|<with|font shape|small-caps|<arg|x>>>>

  <assign|samp|<macro|x|<with|font family|ss|<arg|x>>>>

  <assign|abbr|<macro|x|<group|<arg|x>>>>

  <assign|math|<macro|x|<with|mode|math|<arg|x>>>>

  <assign|op|<macro|x|<with|math condensed|true|<arg|x>>>>

  <assign|cite*|<macro|x|<with|font shape|italic|<arg|x>>>>

  <assign|dfn|<macro|x|<with|font shape|italic|<arg|x>>>>

  <assign|code*|<macro|x|<with|font family|tt|<arg|x>>>>

  <assign|kbd|<macro|x|<with|font family|tt|<arg|x>>>>

  <assign|var|<macro|x|<with|font family|tt|font shape|italic|<arg|x>>>>

  <assign|acronym|<macro|x|<with|font shape|small-caps|<arg|x>>>>

  <assign|person|<macro|x|<with|font shape|small-caps|<arg|x>>>>

  \;

  <assign|verbatim|<macro|body|<with|font
  family|tt|language|verbatim|<arg|body>>>>

  <assign|code|<macro|body|<surround|<vspace*|1fn>|<apply|rightflush><htab|5mm><vspace|1fn><no_indentation_after>|<with|font
  family|tt|language|verbatim|first indentation|0fn|<arg|body>>>>>

  <assign|quote-env|<macro|body|<surround|<vspace*|0.5fn>|<apply|rightflush><vspace|0.5fn>|<with|left
  margin|<plus|<apply|left margin>|3fn>|right margin|<plus|<apply|right
  margin>|3fn>|first indentation|0fn|interparagraph
  space|0.25fn|<arg|body>>>>>

  <assign|quotation|<macro|body|<surround|<vspace*|0.5fn>|<apply|rightflush><vspace|0.5fn>|<with|left
  margin|<plus|<apply|left margin>|3fn>|right margin|<plus|<apply|right
  margin>|3fn>|<arg|body>>>>>

  <assign|verse|<macro|body|<surround|<vspace*|0.5fn>|<apply|rightflush><vspace|0.5fn>|<with|left
  margin|<plus|<apply|left margin>|4.5fn>|right margin|<plus|<apply|right
  margin>|3fn>|first indentation|-1.5fn|interparagraph
  space|0fn|<arg|body>>>>>

  <assign|center|<macro|body|<with|paragraph mode|center|<arg|body>>>>

  \;

  <assign|tabular*|<macro|x|<tformat|<cwith|1|-1|1|-1|cell
  halign|c>|<arg|x>>>>

  <assign|block|<macro|x|<tformat|<cwith|1|-1|1|-1|cell
  rborder|1ln>|<cwith|1|-1|1|-1|cell bborder|1ln>|<cwith|1|1|1|-1|cell
  tborder|1ln>|<cwith|1|-1|1|1|cell lborder|1ln>|<arg|x>>>>

  <assign|block*|<macro|x|<tformat|<cwith|1|-1|1|-1|cell
  rborder|1ln>|<cwith|1|-1|1|-1|cell bborder|1ln>|<cwith|1|1|0|0|cell
  tborder|1ln>|<cwith|1|-1|1|1|cell lborder|1ln>|<cwith|1|-1|1|-1|cell
  halign|c>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|reduction page bottom margin|15mm>
    <associate|page type|a4>
    <associate|reduction page left margin|25mm>
    <associate|even page margin|30mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
  </collection>
</initial>