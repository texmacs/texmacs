<TeXmacs|1.0.2.9>

<\body>
  <assign|env-theorem-package|1.0>

  <assign|env-theorem-dtd|1.0>

  \;

  <assign|theoremname|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|exercisename|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|theoremsep|<macro|. >>

  <assign|exercisesep|<macro|. >>

  \;

  <assign|theorem*|<macro|which|body|<surround|<vspace*|1fn><no-indent><theoremname|<arg|which><theoremsep>>|<rightflush><vspace|1fn>|<with|font-shape|italic|<arg|body>>>>>

  <assign|remark*|<macro|which|body|<theorem*|<arg|which>|<with|font-shape|right|<arg|body>>>>>

  <assign|exercise*|<macro|which|body|<surround|<vspace*|0.5fn><no-indent>|<rightflush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|1.5fn>|font-size|0.84|<surround|<exercisename|<arg|which><exercisesep>>||<arg|body>>>>>>

  <assign|proof*|<macro|which|body|<surround|<vspace*|1fn><no-indent><theoremname|<arg|which><theoremsep>>|<space|0.5fn><rightflush><with|mode|math|\<box\>><vspace|1fn>|<arg|body>>>>

  \;

  <assign|dueto|<macro|name|<with|font-shape|right|<theoremname|(<arg|name>)
  >>>>

  <assign|corollary*|<macro|body|<theorem*|<translate|Corollary|english|<language>>|<arg|body>>>>

  <assign|proof|<\macro|body>
    <proof*|<translate|Proof|english|<language>>|<arg|body>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>