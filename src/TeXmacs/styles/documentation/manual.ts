<TeXmacs|1.0.0.4>

<style|book>

<\body>
  <assign|manual-style|1.0>

  \;

  <assign|paragraph hyphenation|professional>

  \;

  <assign|scheme|<with|font shape|small-caps|Scheme>>

  <assign|pari|<with|font shape|small-caps|Pari>>

  <assign|tmat|@>

  <assign|tmunsc|<with|font family|tt|_>>

  \;

  <assign|tmdef|<macro|concept|<with|font shape|italic|<apply|concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<expand|block*|<tformat|<table|<row|<cell|<arg|whi\
  ch>>>>>>>>

  <assign|menu|<macro|name|<with|font family|ss|<arg|name>>>>

  <assign|submenu|<macro|name|sub|<with|font
  family|ss|<arg|name><with|mode|math|\<rightarrow\>><arg|sub>>>>

  <assign|subsubmenu|<macro|name|sub|subsub|<with|font
  family|ss|<arg|name><with|mode|math|\<rightarrow\>><arg|sub><with|mode|math\
  |\<rightarrow\>><arg|subsub>>>>

  \;

  <assign|verbatim|<macro|body|<surround|<vspace*|0.5fn><format|no page break
  before>|<vspace|0.5fn><format|no indentation after>|<with|font
  family|tt|language|verbatim|<arg|body>>>>>

  <assign|big-figure|<macro|fig|cap|<surround|<vspace*|1fn><format|no first
  indentation><assign|figurenr|<plus|<apply|figurenr>|1>><assign|thelabel|<ap\
  ply|*prefix><apply|figurenr>>|<vspace|1fn><format|no indentation
  after>|<expand|tabular*|<tformat|<twith|table
  width|1par>|<cwith|3|3|1|1|cell hyphen|t>|<cwith|1|-1|1|-1|cell
  lsep|0spc>|<cwith|1|-1|1|-1|cell rsep|0spc>|<cwith|2|2|1|1|cell
  height|0.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
    <with|font size|0.84|<surround|<with|font
    series|bold|<translate|Figure|english|<apply|language>>
    <apply|*prefix><apply|figurenr>. >||<arg|cap>>>
  </cell>>>>>>>>

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
