<TeXmacs|1.0.0.24>

<\body>
  <assign|std-automatic-package|1.0>

  <assign|std-automatic-dtd|1.0>

  \;

  <assign|cite-sub|<func|what|<write|bib|<look_up|<value|what>|0>><reference|\
  <merge|bib-|<look_up|<value|what>|0>>><if|<is_tuple|<look_up|<value|what>|1\
  >>|, <apply|cite-sub|<look_up|<value|what>|1>>>>>

  <assign|cite|<func|what*|[<if|<is_tuple|<value|what>>|<apply|cite-sub|<valu\
  e|what>>|-->]>>

  <assign|nocite-sub|<func|what|<write|bib|<look_up|<value|what>|0>><if|<is_t\
  uple|<look_up|<value|what>|1>>|<apply|nocite-sub|<look_up|<value|what>|1>>>\
  >>

  <assign|nocite|<func|what*|<if|<is_tuple|<value|what>>|<apply|nocite-sub|<v\
  alue|what>>>>>

  <assign|bibitem|<func|text|<expand|item*|[<value|text>]><assign|thelabel|<v\
  alue|text>><label|<merge|bib-|<value|text>>>>>

  <assign|bibitem*|<func|text|<expand|item*|[<value|text>]><assign|thelabel|<\
  value|text>>>>

  <assign|protect|>

  <assign|newblock|>

  <assign|citeauthoryear|<func|author|year|<apply|author> <apply|year>>>

  \;

  <assign|tocnr|0>

  <assign|thetoc|<func|<merge|toc-|<value|tocnr>>>>

  <assign|toc-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font
  series|medium|<with|font size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|toc-main-1|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label|<\
  apply|thetoc>><write|toc|<vspace*|2fn><with|font series|bold|math font
  series|bold|font size|1.19|<value|what>><quote|<value|toc-dots>><pageref|<a\
  pply|thetoc>><vspace|1fn>>>>

  <assign|toc-main-2|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label|<\
  apply|thetoc>><write|toc|<vspace*|1fn><with|font series|bold|math font
  series|bold|<value|what>><quote|<value|toc-dots>><pageref|<apply|thetoc>><v\
  space|0.5fn>>>>

  <assign|toc-normal-1|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label\
  |<apply|thetoc>><write|toc|<value|what><quote|<value|toc-dots>><pageref|<ap\
  ply|thetoc>>>>>

  <assign|toc-normal-2|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label\
  |<apply|thetoc>><write|toc|<with|left margin|1.5fn|<value|what><quote|<valu\
  e|toc-dots>><pageref|<apply|thetoc>>>>>>

  <assign|toc-normal-3|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label\
  |<apply|thetoc>><write|toc|<with|left margin|3fn|<value|what><quote|<value|\
  toc-dots>><pageref|<apply|thetoc>>>>>>

  <assign|toc-small-1|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label|\
  <apply|thetoc>><write|toc|<with|left margin|6fn|font
  size|0.84|<value|what><quote|<value|toc-dots>><pageref|<apply|thetoc>>>>>>

  <assign|toc-small-2|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label|\
  <apply|thetoc>><write|toc|<with|left margin|7.5fn|font
  size|0.84|<value|what><quote|<value|toc-dots>><pageref|<apply|thetoc>>>>>>

  \;

  <assign|idxnr|0>

  <assign|theidx|<func|<merge|idx-|<value|idxnr>>>>

  <assign|index-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font
  series|medium|<with|font size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|index-line|<func|key|entry|<write|idx|<tuple|<value|key>||<value|en\
  try>>>>>

  <assign|index-write|<func|entry|<assign|idxnr|<plus|<value|idxnr>|1>><label\
  |<apply|theidx>><write|idx|<tuple|<value|entry>|<pageref|<apply|theidx>>>>>\
  >

  <assign|index|<func|x|<apply|index-write|<tuple|<value|x>>>>>

  <assign|subindex|<func|x|y|<apply|index-write|<tuple|<value|x>|<value|y>>>>\
  >

  <assign|subsubindex|<func|x|y|z|<apply|index-write|<tuple|<value|x>|<value|\
  y>|<value|z>>>>>

  <assign|index-complex|<func|key|how|range|entry|<assign|idxnr|<plus|<value|\
  idxnr>|1>><label|<apply|theidx>><write|idx|<tuple|<value|key>|<value|how>|<\
  value|range>|<value|entry>|<pageref|<apply|theidx>>>>>>

  <assign|index-1|<macro|left|right|<arg|left><value|index-dots><arg|right>>>

  <assign|index-1*|<macro|left|<arg|left><format|no page break after>>>

  <assign|index-2|<macro|left|right|<with|left
  margin|1.5fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-2*|<macro|left|<with|left margin|1.5fn|<arg|left><format|no
  page break after>>>>

  <assign|index-3|<macro|left|right|<with|left
  margin|3fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-3*|<macro|left|<with|left margin|3fn|<arg|left><format|no
  page break after>>>>

  <assign|index-4|<macro|left|right|<with|left
  margin|4.5fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-4*|<macro|left|<with|left margin|4.5fn|<arg|left><format|no
  page break after>>>>

  <assign|index-5|<macro|left|right|<with|left
  margin|6fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-5*|<macro|left|<with|left margin|6fn|<arg|left><format|no
  page break after>>>>

  \;

  <assign|glynr|0>

  <assign|thegly|<func|<merge|gly-|<value|glynr>>>>

  <assign|glossary-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font
  series|medium|<with|font size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|glossary-line|<func|entry|<write|gly|<tuple|<value|entry>>>>>

  <assign|glossary|<func|entry|<assign|glynr|<plus|<value|glynr>|1>><label|<a\
  pply|thegly>><write|gly|<tuple|normal|<value|entry>|<pageref|<apply|thegly>\
  >>>>>

  <assign|glossary-explain|<func|entry|explain|<assign|glynr|<plus|<value|gly\
  nr>|1>><label|<apply|thegly>><write|gly|<tuple|normal|<value|entry>|<value|\
  explain>|<pageref|<apply|thegly>>>>>>

  <assign|glossary-dup|<func|entry|<assign|glynr|<plus|<value|glynr>|1>><labe\
  l|<apply|thegly>><write|gly|<tuple|dup|<value|entry>|<pageref|<apply|thegly\
  >>>>>>

  <assign|glossary-1|<macro|left|right|<arg|left><value|glossary-dots><arg|ri\
  ght>>>

  <assign|glossary-2|<macro|entry|explain|right|<resize|<arg|entry>
  |||r]10fn|><arg|explain><value|glossary-dots><arg|right>>>

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
    <associate|page type|a4>
    <associate|reduction page left margin|25mm>
    <associate|even page margin|30mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
  </collection>
</initial>
