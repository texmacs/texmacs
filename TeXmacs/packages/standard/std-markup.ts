<TeXmacs|1.0.3.4>

<style|<tuple|source|common-base>>

<\body>
  <assign|std-markup-package|1.0>

  <assign|std-markup-dtd|1.0>

  \;

  <assign|TeXmacs|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>>>>

  <assign|made-by-TeXmacs|<macro|<float|footnote||<with|font-size|0.84|par-mode|justify|par-left|0cm|par-right|0cm|<active*|<move|<postscript|local:$TEXMACS_PATH/misc/images/tm_gnu3.ps||1fn||||>|0fn|-0.2fn>><space|2spc><localize|This
  document has been produced using> GNU <TeXmacs> (<localize|see>
  <with|font-family|tt|http://www.texmacs.org>).<rightflush>>>>>

  <assign|TeX|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|LaTeX|<macro|<active*|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|hflush|<macro|<htab|0fn|0>>>

  <assign|rightflush|<macro|<htab|0fn|first>>>

  <assign|leftflush|<macro|<htab|0fn|last>>>

  <assign|hrule|<no-indent><tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|-1|1|-1|cell-height|1ln>|<cwith|1|-1|1|-1|cell-lsep|0fn>|<cwith|1|-1|1|-1|cell-rsep|0fn>|<cwith|1|-1|1|-1|cell-bsep|0fn>|<cwith|1|-1|1|-1|cell-tsep|0fn>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|<space|1fn|0ln|1ln>>>>>>>

  \;

  <assign|localize|<macro|x|<translate|<arg|x>|english|<value|language>>>>

  <assign|overline|<macro|x|<eval|<hold|<datoms|<macro|x|<with|color|<release|<value|color>>|<wide|<arg|x>|\<wide-bar\>>>>|<arg|x>>>>>>

  <drd-props|overline|arity|1|accessible|all>

  <assign|underline|<macro|x|<eval|<hold|<datoms|<macro|x|<with|color|<release|<value|color>>|<wide*|<arg|x>|\<wide-bar\>>>>|<arg|x>>>>>>

  <drd-props|underline|arity|1|accessible|all>

  <assign|fold|<macro|x|y|<with|par-left|<plus|<value|par-left>|1.5fn>|<surround|<with|par-first|-1.5fn|<yes-indent>><action|<resize|<active*|<with|mode|math|<op|\<circ\>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>|<hflush>|<arg|x>>>>>

  <assign|unfold|<\macro|x|y>
    <\with|par-left|<plus|<value|par-left>|1.5fn>>
      <surround|<with|par-first|-1.5fn|<yes-indent>><action|<resize|<active*|<with|mode|math|\<bullet\>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>|<hflush>|<arg|x>>

      <surround||<rightflush>|<arg|y>>
    </with>
  </macro>>

  <assign|switch|<macro|x|y|<surround||<rightflush>|<arg|x>>>>

  <assign|phantom|<macro|x|<if*|false|<arg|x>>>>

  <assign|set-header|<macro|s|<assign|page-odd-header|<arg|s>><assign|page-even-header|<arg|s>>>>

  <assign|set-footer|<macro|s|<assign|page-odd-footer|<arg|s>><assign|page-even-footer|<arg|s>>>>

  \;

  <assign|strong|<macro|x|<with|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|em|<macro|x|<with|font-shape|italic|<arg|x>>>>

  <assign|tt|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|name|<macro|x|<with|font-shape|small-caps|<arg|x>>>>

  <assign|samp|<macro|x|<with|font-family|ss|<arg|x>>>>

  <assign|abbr|<macro|x|<group|<arg|x>>>>

  <assign|math|<macro|x|<with|mode|math|<arg|x>>>>

  <assign|op|<macro|x|<with|math-condensed|true|<arg|x>>>>

  <assign|cite*|<macro|x|<with|font-shape|italic|<arg|x>>>>

  <assign|dfn|<macro|x|<with|font-shape|italic|<arg|x>>>>

  <assign|code*|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|kbd|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|var|<macro|x|<with|font-family|tt|font-shape|italic|<arg|x>>>>

  <assign|acronym|<macro|x|<with|font-shape|small-caps|<arg|x>>>>

  <assign|person|<macro|x|<with|font-shape|small-caps|<arg|x>>>>

  \;

  <assign|verbatim|<macro|body|<with|font-family|tt|language|verbatim|<arg|body>>>>

  <assign|code|<macro|body|<surround|<vspace*|1fn>|<rightflush><htab|5mm><vspace|1fn><no-indent*>|<with|font-family|tt|language|verbatim|par-first|0fn|<arg|body>>>>>

  <assign|quote-env|<macro|body|<surround|<vspace*|0.5fn>|<rightflush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|par-first|0fn|par-par-sep|0.25fn|<arg|body>>>>>

  <assign|quotation|<macro|body|<surround|<vspace*|0.5fn>|<rightflush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>

  <assign|verse|<macro|body|<surround|<vspace*|0.5fn>|<rightflush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|4.5fn>|par-right|<plus|<value|par-right>|3fn>|par-first|-1.5fn|par-par-sep|0fn|<arg|body>>>>>

  <assign|center|<macro|body|<with|par-mode|center|<arg|body>>>>

  \;

  <assign|tabular*|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>>>>

  <assign|block|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<arg|x>>>>

  <assign|block*|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|0|0|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>