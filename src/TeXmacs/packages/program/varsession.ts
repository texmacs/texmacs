<TeXmacs|1.0.2.2>

<\body>
  <assign|fold|<macro|x|y|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<tabular|<tformat|<twith|table
  width|1par>|<cwith|1|1|1|1|cell lborder|0.5ln>|<cwith|1|1|1|1|cell
  rborder|0.5ln>|<cwith|1|1|1|1|cell bborder|0.5ln>|<cwith|1|1|1|1|cell
  tborder|0.5ln>|<cwith|1|1|2|2|cell lborder|0.5ln>|<cwith|1|1|2|2|cell
  rborder|0.5ln>|<cwith|1|1|2|2|cell bborder|0.5ln>|<cwith|1|1|2|2|cell
  tborder|0.5ln>|<cwith|1|1|2|2|cell hpart|1>|<cwith|1|1|2|2|cell
  background|pastel orange>|<cwith|1|1|1|1|cell background|pastel
  brown>|<cwith|1|1|2|2|cell hyphen|t>|<cwith|1|1|2|2|cell
  bsep|0.25fn>|<cwith|1|1|2|2|cell tsep|0.25fn>|<cwith|1|1|2|2|cell
  lsep|0.5fn>|<cwith|1|1|2|2|cell rsep|0.5fn>|<table|<row|<cell|<action||(mouse-unfold)|<arg|x>>>|<\cell>
    <arg|x>
  </cell>>>>>>>>

  <assign|unfold|<macro|x|y|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<tabular|<tformat|<twith|table
  width|1par>|<cwith|1|-1|2|2|cell hpart|1>|<cwith|1|1|2|2|cell
  lborder|0.5ln>|<cwith|1|1|2|2|cell rborder|0.5ln>|<cwith|1|1|2|2|cell
  bborder|0.5ln>|<cwith|1|1|2|2|cell tborder|0.5ln>|<cwith|1|-1|1|1|cell
  lborder|0.5ln>|<cwith|1|-1|1|1|cell rborder|0.5ln>|<cwith|1|-1|1|1|cell
  bborder|0.5ln>|<cwith|1|-1|1|1|cell tborder|0.5ln>|<cwith|1|1|1|1|cell
  bborder|0ln>|<cwith|2|2|1|1|cell tborder|0ln>|<cwith|2|2|2|2|cell
  lsep|0fn>|<cwith|2|2|2|2|cell rsep|0fn>|<cwith|2|2|2|2|cell
  hyphen|t>|<cwith|1|1|2|2|cell hyphen|t>|<cwith|1|1|2|2|cell
  bsep|0.25fn>|<cwith|1|1|2|2|cell tsep|0.25fn>|<cwith|2|2|2|2|cell
  tsep|0.5fn>|<cwith|1|-1|1|1|cell background|pastel
  brown>|<cwith|1|1|2|2|cell background|pastel orange>|<cwith|2|2|2|2|cell
  bsep|0fn>|<cwith|1|1|2|2|cell lsep|0.5fn>|<cwith|1|1|2|2|cell
  rsep|0.5fn>|<table|<row|<cell|<action||(mouse-fold)|<arg|x>>>|<\cell>
    <arg|x>
  </cell>>|<row|<cell|<action||(mouse-fold)|<arg|x>>>|<\cell>
    <arg|y>
  </cell>>>>>>>>

  \;

  <assign|generic-input|<macro|prompt|body|<tabular|<tformat|<twith|table
  width|1par>|<cwith|1|1|2|2|cell hpart|1>|<cwith|1|1|2|2|cell
  background|pastel yellow>|<cwith|1|1|1|1|cell background|pastel
  yellow>|<cwith|1|1|2|2|cell hyphen|t>|<cwith|1|1|1|1|cell
  lborder|0.5ln>|<cwith|1|1|2|2|cell rborder|0.5ln>|<cwith|1|1|2|2|cell
  tborder|0.5ln>|<cwith|1|1|1|1|cell tborder|0.5ln>|<cwith|1|1|1|1|cell
  bborder|0.5ln>|<cwith|1|1|2|2|cell bborder|0.5ln>|<cwith|1|1|1|1|cell
  lsep|0.5fn>|<cwith|1|1|2|2|cell rsep|0.5fn>|<cwith|1|1|1|1|cell
  rsep|0fn>|<cwith|1|1|2|2|cell lsep|0fn>|<cwith|1|1|2|2|cell
  tsep|0.25fn>|<cwith|1|1|2|2|cell bsep|0.25fn>|<table|<row|<cell|<apply|id-function|<arg|prompt>>>|<\cell>
    <with|formula style|true|<arg|body>>
  </cell>>>>>>>

  <assign|generic-output*|<macro|body|<with|left margin|<plus|<apply|left
  margin>|0.5fn>|right margin|<plus|<apply|right margin>|0.5fn>|paragraph
  mode|left|formula style|true|<arg|body>>>>

  <assign|generic-output|<macro|body|<surround|<vspace*|0.75fn>|<vspace|0.75fn>|<expand|generic-output*|<arg|body>>>>>

  <assign|generic-textput|<macro|body|<with|left margin|<plus|<apply|left
  margin>|0.5fn>|right margin|<plus|<apply|right margin>|0.5fn>|<arg|body>>>>

  <assign|generic-errput|<macro|body|<tabular|<tformat|<twith|table
  width|1par>|<cwith|1|1|1|1|cell lborder|0.5ln>|<cwith|1|1|1|1|cell
  rborder|0.5ln>|<cwith|1|1|1|1|cell bborder|0.5ln>|<cwith|1|1|1|1|cell
  tborder|0.5ln>|<cwith|1|1|1|1|cell background|pastel
  red>|<cwith|1|1|1|1|cell hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
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
