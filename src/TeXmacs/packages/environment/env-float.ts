<TeXmacs|1.0.1.10>

<\body>
  <assign|env-float-package|1.0>

  <assign|env-float-dtd|1.0>

  \;

  <assign|figurename|<macro|name|<with|font series|bold|<arg|name>>>>

  <assign|figuresep|<func|. >>

  <assign|footnotesep|<func|. >>

  \;

  <assign|list-caption|<func|type|cap|<assign|glynr|<plus|<value|glynr>|1>><label|<apply|thegly>><write|<value|type>|<tuple|normal|<value|cap>|<pageref|<apply|thegly>>>>>>

  <assign|small-figure*|<macro|type|name|fig|cap|<expand|tabular*|<tformat|<cwith|3|3|1|1|cell
  hyphen|t>|<cwith|1|-1|1|-1|cell lsep|0spc>|<cwith|1|-1|1|-1|cell
  rsep|0spc>|<cwith|2|2|1|1|cell height|0.5fn>|<twith|table
  valign|B>|<table|<row|<cell|<resize|<arg|fig>|l-2fn||r+2fn||>>>|<row|<cell|>>|<row|<\cell>
    <with|font size|0.84|<surround|<figurename|<arg|name><apply|figuresep>><apply|list-caption|<arg|type>|<arg|cap>>||<arg|cap>>>
  </cell>>>>>>>

  <assign|big-figure*|<macro|type|name|fig|cap|<surround|<vspace*|1fn><format|no
  first indentation>|<vspace|1fn>|<expand|tabular*|<tformat|<twith|table
  width|1par>|<cwith|3|3|1|1|cell hyphen|t>|<cwith|1|-1|1|-1|cell
  lsep|0spc>|<cwith|1|-1|1|-1|cell rsep|0spc>|<cwith|2|2|1|1|cell
  height|0.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
    <with|font size|0.84|<surround|<figurename|<arg|name><apply|figuresep><apply|list-caption|<arg|type>|<arg|cap>>>||<arg|cap>>>
  </cell>>>>>>>>

  \;

  <assign|footnote|<macro|x|<assign|footnotenr|<plus|<apply|footnotenr>|1>><assign|thelabel|<apply|thefootnote>><float|footnote||<with|font
  size|0.84|paragraph mode|justify|left margin|0cm|right
  margin|0cm|<surround|<apply|thefootnote><apply|footnotesep><label|<merge|footnote-|<apply|thefootnote>>>|<apply|rightflush>|<arg|x>>>><space|0spc><rsup|<reference|<merge|footnote-|<apply|thefootnote>>>>>>

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
