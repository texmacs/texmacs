<TeXmacs|1.0.1.10>

<\body>
  <assign|title-generic-package|1.0>

  <assign|header-title-dtd|1.0>

  \;

  <assign|make-title|<macro|body|<surround|<apply|start-page|>|<vspace|2fn>|<with|paragraph
  mode|center|<arg|body>>>>>

  <assign|abstract|<macro|body|<surround|<vspace*|2fn>|<apply|rightflush><vspace|1fn>|<\with|left
  margin|15mm|right margin|15mm|font size|0.84>
    <format|no first indentation><apply|leftflush><with|font
    series|bold|<translate|Abstract|english|<apply|language>>><apply|rightflush><vspace|0.5fn><format|no
    page break after>

    <arg|body>
  </with>>>>

  \;

  <assign|title*|<macro|body|<with|math font series|bold|font
  series|bold|font size|1.68|<arg|body>>>>

  <assign|title|<macro|body|<expand|title*|<arg|body>><apply|header-title|<arg|body>>>>

  <assign|author*|<macro|body|<with|font shape|small-caps|<translate|by|english|<apply|language>>
  <arg|body>>>>

  <assign|author|<macro|body|<vspace*|1fn><expand|author*|<arg|body>><apply|header-author|<arg|body>>>>

  <assign|address*|<macro|body|<with|interparagraph space|0fn|<arg|body>>>>

  <assign|address|<macro|body|<surround|<vspace*|1fn>||<expand|address*|<arg|body>>>>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell
  lsep|1.5fn>|<cwith|1|-1|-1|-1|cell rsep|1.5fn>|<twith|table
  valign|T>|<arg|x>>>>

  <assign|title-email*|<macro|body|<with|font
  shape|small-caps|<translate|Email:|english|<apply|language>>
  ><verbatim|<arg|body>>>>

  <assign|title-email|<macro|body|<vspace*|1fn><expand|title-email*|<arg|body>>>>

  <assign|title-date*|<macro|body|<with|font shape|italic|<arg|body>>>>

  <assign|title-date|<macro|body|<vspace*|1.5fn><apply|leftflush><expand|title-date*|<arg|body>><apply|rightflush>>>

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
