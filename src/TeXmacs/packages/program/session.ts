<TeXmacs|1.0.1.21>

<\body>
  <assign|session-package|1.0>

  <assign|session-dtd|1.0>

  \;

  <assign|session|<macro|body|<expand|<if|<provides|<merge|<apply|prog
  language>|-session>>|<merge|<apply|prog
  language>|-session>|generic-session>|<arg|name>|<arg|body>>>>

  <assign|generic-session|<macro|name|body|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<with|first
  indentation|0fn|interparagraph space|0fn|<arg|body>>>>>

  <assign|input|<macro|prompt|body|<with|mode|prog|<expand|<if|<provides|<merge|<apply|prog
  language>|-input>>|<merge|<apply|prog language>|-input>|generic-input>|<arg|prompt>|<arg|body>>>>>

  <assign|id-function|<func|x|<apply|x>>>

  <assign|generic-input|<\macro|prompt|body>
    <tabular|<tformat|<twith|table width|1par>|<cwith|1|1|2|2|cell
    hpart|1>|<cwith|1|1|1|1|cell lsep|0fn>|<cwith|1|1|1|1|cell
    rsep|0fn>|<cwith|1|1|2|2|cell lsep|0fn>|<cwith|1|1|2|2|cell
    rsep|0fn>|<cwith|1|1|2|2|cell hyphen|t>|<twith|table
    hyphen|y>|<table|<row|<cell|<apply|id-function|<arg|prompt>>>|<\cell>
      <with|color|blue|formula style|true|<arg|body>>
    </cell>>>>>
  </macro>>

  <assign|output|<macro|body|<with|mode|prog|<expand|<if|<provides|<merge|<apply|prog
  language>|-output>>|<merge|<apply|prog language>|-output>|generic-output>|<arg|body>>>>>

  <assign|generic-output*|<macro|body|<with|paragraph mode|left|formula
  style|true|<arg|body>>>>

  <assign|generic-output|<macro|body|<surround|<vspace*|0.5fn>|<apply|rightflush><vspace|0.5fn>|<with|left
  margin|<plus|<apply|left margin>|1.5fn>|<expand|generic-output*|<arg|body>>>>>>

  <assign|textput|<macro|body|<expand|<if|<provides|<merge|<apply|prog
  language>|-textput>>|<merge|<apply|prog
  language>|-textput>|generic-textput>|<arg|body>>>>

  <assign|generic-textput|<macro|body|<surround||<apply|rightflush>|<arg|body>>>>

  <assign|errput|<macro|body|<expand|<if|<provides|<merge|<apply|prog
  language>|-errput>>|<merge|<apply|prog language>|-textput>|generic-errput>|<arg|body>>>>

  <assign|generic-errput|<macro|body|<surround||<apply|rightflush>|<with|color|red|<arg|body>>>>>

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
