<TeXmacs|1.0.0.25>

<\body>
  <assign|title-generic-package|1.0>

  <assign|header-title-dtd|1.0>

  \;

  <assign|make-title|<\macro|body>
    <format|no first indentation><assign|this page header|><assign|this page
    footer|><vspace|0.3pag>

    <\with|paragraph mode|center>
      <arg|body>
    </with>

    <format|new page>

    <assign|this page header|><assign|this page footer|><vspace|0.3pag>

    <surround||<apply|hflush>|<format|new page>>
  </macro>>

  <assign|abstract|<\macro|body>
    <surround|<format|no first indentation>||<expand|chapter*|<translate|Abst\
    ract|english|<apply|language>>>>

    <surround||<apply|hflush>|<arg|body>>
  </macro>>

  \;

  <assign|title*|<macro|name|<with|math font series|bold|font
  series|bold|font size|2.82|<arg|name>>>>

  <assign|title|<macro|body|<expand|title*|<arg|body>><apply|header-title|<ar\
  g|body>>>>

  <assign|author*|<macro|body|<with|font shape|small-caps|font
  size|1.54|<arg|body>>>>

  <assign|author|<macro|body|<vspace*|0.1pag><expand|author*|<arg|body>><appl\
  y|header-author|<arg|body>>>>

  <assign|address*|<macro|body|<with|interparagraph space|0fn|<arg|body>>>>

  <assign|address|<macro|body|<surround|<vspace*|0.1pag>||<expand|address*|<a\
  rg|body>>>>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell
  lsep|1.5fn>|<cwith|1|-1|-1|-1|cell rsep|1.5fn>|<twith|table
  valign|T>|<arg|x>>>>

  <assign|title-email*|<macro|body|<with|font
  shape|small-caps|<translate|Email:|english|<apply|language>>
  ><verbatim|<arg|body>>>>

  <assign|title-email|<macro|body|<vspace*|0.1pag><expand|title-email*|<arg|b\
  ody>>>>

  <assign|title-date*|<macro|body|<with|font shape|italic|font
  size|1.41|<arg|body>>>>

  <assign|title-date|<macro|body|<vspace*|0.1pag><apply|hflush><expand|title-\
  date*|<arg|body>><apply|hflush>>>

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
