<TeXmacs|1.0.2.6>

<\body>
  <assign|tmdoc-markup-package|1.0>

  \;

  <assign|menu-item|<macro|x|<with|font family|ss|<apply|localize|<arg|x>>>>>

  <assign|menu-extra|<macro|x|<with|mode|math|\<rightarrow\>><menu-item|<arg|x>>>>

  <assign|menu|<xmacro|x|<menu-item|<arg|x|0>><map_args|menu-extra|concat|x|1><apply|index-write|<map_args|menu-item|tuple|x>>>>

  \;

  <assign|markup|<macro|x|<with|font family|tt|color|dark
  green|<arg|x>><apply|index|<with|font family|tt|color|dark
  green|<arg|x>>>>>

  <assign|tmstyle|<macro|x|<with|font family|tt|color|brown|<arg|x>><apply|index|<with|font
  family|tt|color|brown|<arg|x>>>>>

  <assign|tmpackage|<macro|x|<with|font family|tt|color|brown|<arg|x>><apply|index|<with|font
  family|tt|color|brown|<arg|x>>>>>

  <assign|tmdtd|<macro|x|<with|font family|tt|color|dark
  magenta|<arg|x>><apply|index|<with|font family|tt|color|dark
  magenta|<arg|x>>>>>

  \;

  <assign|simple-link|<macro|x|<hlink|<with|font
  family|tt|<arg|x>>|<arg|x>>>>

  <assign|hyper-link|<func|x|y|<hlink|<value|x>|<value|y>>>>

  <assign|hyper-link|<func|x|y|<hold|<datoms|<macro|x|<hlink|<arg|x>|<release|<value|y>>>>|<with|color|blue|<value|x>>>>>>

  <assign|concept-link|<macro|x|<with|color|magenta|<arg|x>>>>

  <assign|only-index|<func|x|<apply|index|<value|x>>>>

  <assign|def-index|<macro|x|<em|<arg|x>><apply|index|<arg|x>>>>

  <assign|re-index|<macro|x|<arg|x><apply|index|<arg|x>>>>

  <assign|example-plugin-link|<macro|x|<hlink|<with|font
  family|tt|<arg|x>>|<merge|$TEXMACS_PATH/examples/plugins/|<arg|x>>>>>

  \;

  <assign|scheme|<name|Scheme>>

  <assign|cpp|<name|C++>>

  <assign|framed-fragment|<macro|x|<with|color|dark
  grey|<block|<tformat|<twith|table width|1par>|<cwith|1|1|1|1|cell
  hyphen|t>|<cwith|1|1|1|1|cell bsep|1spc>|<cwith|1|1|1|1|cell
  tsep|1spc>|<cwith|1|1|1|1|cell background|pastel blue>|<cwith|1|1|1|1|cell
  lborder|0.5ln>|<cwith|1|1|1|1|cell rborder|0.5ln>|<cwith|1|1|1|1|cell
  bborder|0.5ln>|<cwith|1|1|1|1|cell tborder|0.5ln>|<table|<row|<\cell>
    <with|color|black|<arg|x>>
  </cell>>>>>>>>

  <assign|scheme-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|interparagraph
    space|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <assign|cpp-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|interparagraph
    space|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <assign|tm-fragment|<\macro|x>
    <quotation|<framed-fragment|<arg|x>>>
  </macro>>

  <assign|scheme-code|<macro|x|<verbatim|<arg|x>>>>

  <assign|cpp-code|<macro|x|<verbatim|<arg|x>>>>

  <assign|todo|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell background|pastel
  red>|<cwith|1|1|1|1|cell lborder|0.5ln>|<cwith|1|1|1|1|cell
  rborder|0.5ln>|<cwith|1|1|1|1|cell bborder|0.5ln>|<cwith|1|1|1|1|cell
  tborder|0.5ln>|<table|<row|<cell|To do: <arg|x>>>>>>>>

  \;

  <assign|icon|<func|name|<postscript|<find_file|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|http://www.gnu.org/software/texmacs-doc/images/pixmaps|<apply|name>>|/2|/2||||>>>

  <assign|screenshot|<func|name|<postscript|<find_file|$TEXMACS_DOC_PATH/images/screenshots|$TEXMACS_HOME_PATH/doc/images/screenshots|$TEXMACS_PATH/doc/images/screenshots|http://www.gnu.org/software/texmacs-doc/images/screenshots|<apply|name>>|/2|/2||||>>>

  <assign|descriptive-table|<macro|x|<tformat|<cwith|1|-1|1|-1|cell
  rborder|0.5ln>|<cwith|1|-1|1|-1|cell bborder|0.5ln>|<cwith|1|-1|1|1|cell
  lborder|0.5ln>|<cwith|1|1|1|-1|cell tborder|0.5ln>|<cwith|1|1|1|-1|cell
  background|pastel blue>|<twith|table min rows|2>|<twith|table
  lborder|1ln>|<twith|table rborder|1ln>|<twith|table
  bborder|1ln>|<twith|table tborder|1ln>|<cwith|1|1|1|-1|cell
  bborder|1ln>|<twith|table min cols|2>|<arg|x>>>>

  <apply|newtheorem|question|Question>

  <assign|answer|<macro|body|<quotation|<surround|<theoremname|<translate|Answer|english|<apply|language>><apply|theoremsep>>||<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|interparagraph space|0fn>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>