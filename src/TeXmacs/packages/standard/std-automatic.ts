<TeXmacs|1.0.2.9>

<\body>
  <assign|std-automatic-package|1.0>

  <assign|std-automatic-dtd|1.0>

  \;

  <assign|cite-arg|<macro|x|<write|bib|<arg|x>><reference|<merge|bib-|<arg|x>>>>>

  <assign|cite-arg-extra|<macro|x|, <cite-arg|<arg|x>>>>

  <assign|cite|<xmacro|x|[<cite-arg|<arg|x|0>><map-args|cite-arg-extra|concat|x|1>]>>

  <assign|cite-detail|<macro|x|y|[<cite-arg|<arg|x>>, <arg|y>]>>

  <assign|nocite-arg|<macro|x|<write|bib|<arg|x>>>>

  <assign|nocite|<xmacro|x|<flag|<localize|bibliography>|dark
  green|x><map-args|nocite-arg|concat|x|1>>>

  <assign|bibitem|<macro|text|<item*|[<arg|text>]><assign|thelabel|<arg|text>><label|<merge|bib-|<arg|text>>>>>

  <assign|bibitem*|<macro|text|<item*|[<arg|text>]><assign|thelabel|<arg|text>>>>

  <assign|protect|>

  <assign|newblock|>

  <assign|citeauthoryear|<macro|author|year|<arg|author> <arg|year>>>

  \;

  <assign|tocnr|0>

  <assign|thetoc|<macro|<merge|toc-|<value|tocnr>>>>

  <assign|toc-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|toc-main-1|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<vspace*|2fn><with|font-series|bold|math-font-series|bold|font-size|1.19|<arg|what>><quote|<value|toc-dots>><pageref|<thetoc>><vspace|1fn>>>>

  <assign|toc-main-2|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<vspace*|1fn><with|font-series|bold|math-font-series|bold|<arg|what>><quote|<value|toc-dots>><pageref|<thetoc>><vspace|0.5fn>>>>

  <assign|toc-normal-1|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<arg|what><quote|<value|toc-dots>><pageref|<thetoc>>>>>

  <assign|toc-normal-2|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<with|par-left|1.5fn|<arg|what><quote|<value|toc-dots>><pageref|<thetoc>>>>>>

  <assign|toc-normal-3|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<with|par-left|3fn|<arg|what><quote|<value|toc-dots>><pageref|<thetoc>>>>>>

  <assign|toc-small-1|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<with|par-left|6fn|font-size|0.84|<arg|what><quote|<value|toc-dots>><pageref|<thetoc>>>>>>

  <assign|toc-small-2|<macro|what|<flag|<localize|table of contents>|dark
  green|what><assign|tocnr|<plus|<value|tocnr>|1>><label|<thetoc>><write|toc|<with|par-left|7.5fn|font-size|0.84|<arg|what><quote|<value|toc-dots>><pageref|<thetoc>>>>>>

  \;

  <assign|idxnr|0>

  <assign|theidx|<macro|<merge|idx-|<value|idxnr>>>>

  <assign|index-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|index-line|<macro|key|entry|<flag|<localize|index>|dark
  green|key><write|idx|<tuple|<arg|key>||<arg|entry>>>>>

  <assign|index-write|<macro|entry|<assign|idxnr|<plus|<value|idxnr>|1>><label|<theidx>><write|idx|<tuple|<arg|entry>|<pageref|<theidx>>>>>>

  <assign|index|<macro|x|<flag|<localize|index>|dark
  green|x><index-write|<tuple|<arg|x>>>>>

  <assign|subindex|<macro|x|y|<flag|<localize|index>|dark
  green|x><index-write|<tuple|<arg|x>|<arg|y>>>>>

  <assign|subsubindex|<macro|x|y|z|<flag|<localize|index>|dark
  green|x><index-write|<tuple|<arg|x>|<arg|y>|<arg|z>>>>>

  <assign|index-complex|<macro|key|how|range|entry|<flag|<localize|index>|dark
  green|key><assign|idxnr|<plus|<value|idxnr>|1>><label|<theidx>><write|idx|<tuple|<arg|key>|<arg|how>|<arg|range>|<arg|entry>|<pageref|<theidx>>>>>>

  <assign|index-1|<macro|left|right|<arg|left><value|index-dots><arg|right>>>

  <assign|index-1*|<macro|left|<arg|left><no-page-break>>>

  <assign|index-2|<macro|left|right|<with|par-left|1.5fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-2*|<macro|left|<with|par-left|1.5fn|<arg|left><no-page-break>>>>

  <assign|index-3|<macro|left|right|<with|par-left|3fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-3*|<macro|left|<with|par-left|3fn|<arg|left><no-page-break>>>>

  <assign|index-4|<macro|left|right|<with|par-left|4.5fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-4*|<macro|left|<with|par-left|4.5fn|<arg|left><no-page-break>>>>

  <assign|index-5|<macro|left|right|<with|par-left|6fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-5*|<macro|left|<with|par-left|6fn|<arg|left><no-page-break>>>>

  \;

  <assign|glynr|0>

  <assign|thegly|<macro|<merge|gly-|<value|glynr>>>>

  <assign|glossary-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|glossary-line|<macro|entry|<flag|<localize|glossary>|dark
  green|entry><write|gly|<tuple|<arg|entry>>>>>

  <assign|glossary|<macro|entry|<flag|<localize|glossary>|dark
  green|entry><assign|glynr|<plus|<value|glynr>|1>><label|<thegly>><write|gly|<tuple|normal|<arg|entry>|<pageref|<thegly>>>>>>

  <assign|glossary-explain|<macro|entry|explain|<flag|<localize|glossary>|dark
  green|entry><assign|glynr|<plus|<value|glynr>|1>><label|<thegly>><write|gly|<tuple|normal|<arg|entry>|<arg|explain>|<pageref|<thegly>>>>>>

  <assign|glossary-dup|<macro|entry|<flag|<localize|glossary>|dark
  green|entry><assign|glynr|<plus|<value|glynr>|1>><label|<thegly>><write|gly|<tuple|dup|<arg|entry>|<pageref|<thegly>>>>>>

  <assign|glossary-1|<macro|left|right|<arg|left><value|glossary-dots><arg|right>>>

  <assign|glossary-2|<macro|entry|explain|right|<resize|<arg|entry>
  |||r]10fn|><arg|explain><value|glossary-dots><arg|right>>>

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
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>