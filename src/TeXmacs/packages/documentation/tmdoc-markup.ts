<TeXmacs|1.0.3.2>

<\body>
  <assign|tmdoc-markup-package|1.0>

  \;

  <assign|menu-item|<macro|x|<with|font-family|ss|<localize|<arg|x>>>>>

  <assign|menu-extra|<macro|x|<with|mode|math|\<rightarrow\>><menu-item|<arg|x>>>>

  <assign|menu|<xmacro|x|<menu-item|<arg|x|0>><map-args|menu-extra|concat|x|1><index-write|<map-args|menu-item|tuple|x>>>>

  \;

  <assign|markup|<macro|x|<with|font-family|tt|color|dark
  green|<arg|x>><index|<with|font-family|tt|color|dark green|<arg|x>>>>>

  <assign|tmstyle|<macro|x|<with|font-family|tt|color|brown|<arg|x>><index|<with|font-family|tt|color|brown|<arg|x>>>>>

  <assign|tmpackage|<macro|x|<with|font-family|tt|color|brown|<arg|x>><index|<with|font-family|tt|color|brown|<arg|x>>>>>

  <assign|tmdtd|<macro|x|<with|font-family|tt|color|dark
  magenta|<arg|x>><index|<with|font-family|tt|color|dark magenta|<arg|x>>>>>

  \;

  <assign|simple-link|<macro|x|<hlink|<with|font-family|tt|<arg|x>>|<arg|x>>>>

  <assign|hyper-link|<macro|x|y|<hlink|<arg|x>|<arg|y>>>>

  <assign|hyper-link|<macro|x|y|<hold|<datoms|<macro|x|<hlink|<arg|x>|<release|<arg|y>>>>|<with|color|blue|<arg|x>>>>>>

  <assign|concept-link|<macro|x|<with|color|magenta|<arg|x>>>>

  <assign|only-index|<macro|x|<index|<arg|x>>>>

  <assign|def-index|<macro|x|<em|<arg|x>><index|<arg|x>>>>

  <assign|re-index|<macro|x|<arg|x><index|<arg|x>>>>

  <assign|example-plugin-link|<macro|x|<hlink|<with|font-family|tt|<arg|x>>|<merge|$TEXMACS_PATH/examples/plugins/|<arg|x>>>>>

  \;

  <assign|scheme|<name|Scheme>>

  <assign|cpp|<name|C++>>

  <assign|framed-fragment|<macro|x|<with|color|dark
  grey|<block|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-bsep|1spc>|<cwith|1|1|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-background|pastel
  blue>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<table|<row|<\cell>
    <with|color|black|<arg|x>>
  </cell>>>>>>>>

  <assign|scheme-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <assign|cpp-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <assign|tm-fragment|<\macro|x>
    <quotation|<framed-fragment|<arg|x>>>
  </macro>>

  <assign|scheme-code|<macro|x|<verbatim|<arg|x>>>>

  <assign|cpp-code|<macro|x|<verbatim|<arg|x>>>>

  <assign|todo|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<table|<row|<cell|To
  do: <arg|x>>>>>>>>

  \;

  <assign|icon|<macro|name|<postscript|<find-file|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|http://www.gnu.org/software/texmacs-doc/images/pixmaps|<arg|name>>|/2|/2||||>>>

  <assign|screenshot|<macro|name|<postscript|<find-file|$TEXMACS_DOC_PATH/images/screenshots|$TEXMACS_HOME_PATH/doc/images/screenshots|$TEXMACS_PATH/doc/images/screenshots|http://www.gnu.org/software/texmacs-doc/images/screenshots|<arg|name>>|/2|/2||||>>>

  <assign|descriptive-table|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|-1|cell-background|pastel
  blue>|<twith|table-min-rows|2>|<twith|table-lborder|1ln>|<twith|table-rborder|1ln>|<twith|table-bborder|1ln>|<twith|table-tborder|1ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<twith|table-min-cols|2>|<arg|x>>>>

  <assign|tag-info-table|<macro|x|<descriptive-table|<tformat|<cwith|1|2|1|-1|cell-halign|c>|<cwith|1|1|1|-1|cell-row-span|2>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|2|1|-1|cell-lborder|1ln>|<cwith|2|2|4|4|cell-lborder|0.5ln>|<cwith|2|2|3|4|cell-bborder|1ln>|<cwith|1|1|3|4|cell-bborder|0.5ln>|<cwith|2|2|1|-1|cell-background|pastel
  blue>|<cwith|1|1|3|3|cell-col-span|2>|<cwith|1|1|3|3|cell-row-span|1>|<twith|table-min-rows|3>|<twith|table-min-cols|6>|<twith|table-max-cols|6>|<arg|x>>>>>

  <newtheorem|question|Question>

  <assign|answer|<macro|body|<quotation|<surround|<theoremname|<translate|Answer|english|<language>><theoremsep>>||<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|page-even|30mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-par-sep|0fn>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>