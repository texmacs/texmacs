<TeXmacs|1.0.1.18>

<\body>
  <assign|env-theorem-package|1.0>

  <assign|env-theorem-dtd|1.0>

  \;

  <assign|theoremname|<macro|name|<with|font series|bold|<arg|name>>>>

  <assign|exercisename|<macro|name|<with|font series|bold|<arg|name>>>>

  <assign|theoremsep|<func|. >>

  <assign|exercisesep|<func|. >>

  \;

  <assign|theorem*|<macro|which|body|<surround|<vspace*|1fn><format|no first
  indentation><theoremname|<arg|which><apply|theoremsep>>|<apply|rightflush><vspace|1fn>|<with|font
  shape|italic|<arg|body>>>>>

  <assign|remark*|<macro|which|body|<expand|theorem*|<arg|which>|<with|font
  shape|right|<arg|body>>>>>

  <assign|exercise*|<macro|which|body|<surround|<vspace*|0.5fn><format|no
  first indentation>|<apply|rightflush><vspace|0.5fn>|<with|left
  margin|<plus|<apply|left margin>|1.5fn>|font
  size|0.84|<surround|<exercisename|<arg|which><apply|exercisesep>>||<arg|body>>>>>>

  <assign|proof*|<macro|which|body|<surround|<vspace*|1fn><format|no first
  indentation><theoremname|<arg|which><apply|theoremsep>>|<space|0.5fn><apply|rightflush><with|mode|math|\<box\>><vspace|1fn>|<arg|body>>>>

  \;

  <assign|dueto|<macro|name|<with|font shape|right|<theoremname|(<arg|name>)
  >>>>

  <assign|corollary*|<macro|body|<expand|theorem*|<translate|Corollary|english|<apply|language>>|<arg|body>>>>

  <assign|proof|<macro|body|<expand|proof*|<translate|Proof|english|<apply|language>>|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|odd page margin|30mm>
    <associate|paragraph width|150mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
  </collection>
</initial>
