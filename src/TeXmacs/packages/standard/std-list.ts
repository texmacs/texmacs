<TeXmacs|1.0.1.10>

<\body>
  <assign|std-list-package|1.0>

  <assign|std-list-dtd|1.0>

  \;

  <assign|itemname|<macro|name|<with|font series|bold|<arg|name>>>>

  <assign|item|<macro|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first
  indentation>><resize|<with|mode|math|\<ast\>> |||r]1.5fn|>>>

  <assign|item*|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first
  indentation>><resize|<itemname|<arg|theitem>><space|0.5fn>|||r]1.5fn|>>>

  \;

  <assign|thetag|<with|mode|math|\<ast\>>>

  <assign|itemize-level|0>

  <assign|itemize-base|<macro|body|<surround|<format|no page break
  before>|<apply|rightflush><vspace|0.5fn><format|no indentation
  after>|<with|left margin|<plus|<value|left
  margin>|3fn>|item|<macro|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first indentation>><resize|<value|thetag>
  |||r]1.5fn|>>|<arg|body>>>>>

  <assign|itemize|<macro|body|<with|itemize-level|<plus|<value|itemize-level>|1>|<with|thetag|<case|<equal|<mod|<value|itemize-level>|3>|1>|<with|mode|math|\<bullet\>>|<equal|<mod|<value|itemize-level>|3>|2>|<with|mode|math|\<circ\>>|<equal|<mod|<value|itemize-level>|3>|0>|<with|mode|math|->>|<expand|itemize-base|<arg|body>>>>>>

  \;

  <assign|newitemize|<func|name|tag|<assign|<apply|name>|<hold|<macro|body|<with|thetag|<release|<value|tag>>|<expand|itemize-base|<arg|body>>>>>>>>

  <apply|newitemize|itemize-minus|<with|mode|math|->>

  <apply|newitemize|itemize-dot|<with|mode|math|\<bullet\>>>

  <apply|newitemize|itemize-arrow|<with|mode|math|\<rightarrow\>>>

  \;

  <assign|theitem|<func|<apply|itemnr>.>>

  <assign|enumerate-level|0>

  <assign|enumerate-base|<macro|body|<surround|<format|no page break
  before>|<apply|rightflush><vspace|0.5fn><format|no indentation
  after>|<with|itemnr|0|left margin|<plus|<value|left
  margin>|3fn>|item|<macro|<vspace*|0.5fn><assign|itemnr|<plus|<value|itemnr>|1>><assign|thelabel|<apply|theitem>><with|first
  indentation|-3fn|<format|enable first indentation>><resize|<apply|theitem>|r-2.5fn||r+0.5fn|>>|<arg|body>>>>>

  <assign|enumerate|<macro|body|<with|enumerate-level|<plus|<value|enumerate-level>|1>|<with|theitem|<case|<equal|<mod|<value|enumerate-level>|3>|1>|<hold|<apply|itemnr>.>|<equal|<mod|<value|enumerate-level>|3>|2>|<func|<number|<apply|itemnr>|alpha><with|font
  shape|right|)>>|<equal|<mod|<value|enumerate-level>|3>|0>|<func|<number|<apply|itemnr>|roman>.>>|<expand|enumerate-base|<arg|body>>>>>>

  \;

  <assign|newenumerate|<func|name|num|<assign|<apply|name>|<hold|<macro|body|<with|theitem|<release|<value|num>>|<expand|enumerate-base|<arg|body>>>>>>>>

  <apply|newenumerate|enumerate-numeric|<func|<apply|itemnr>.>>

  <apply|newenumerate|enumerate-roman|<func|<number|<apply|itemnr>|roman>.>>

  <apply|newenumerate|enumerate-Roman|<func|<number|<apply|itemnr>|Roman>.>>

  <apply|newenumerate|enumerate-alpha|<func|<number|<apply|itemnr>|alpha><with|font
  shape|right|)>>>

  <apply|newenumerate|enumerate-Alpha|<func|<number|<apply|itemnr>|Alpha><with|font
  shape|right|)>>>

  \;

  <assign|newdescription|<func|name|item-macro|<assign|<apply|name>|<hold|<macro|body|<with|item*|<release|<value|item-macro>>|<expand|itemize-base|<arg|body>>>>>>>>

  <apply|newdescription|description-compact|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first
  indentation>><resize|<itemname|<arg|theitem>> \ |||r]1.5fn|>>>

  <apply|newdescription|description-aligned|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first
  indentation>><expand|tabular*|<tformat|<cwith|1|1|1|1|cell
  lsep|0fn>|<cwith|1|1|1|1|cell rsep|0fn>|<cwith|1|1|1|1|cell
  bsep|0fn>|<cwith|1|1|1|1|cell tsep|0fn>|<cwith|1|1|1|1|cell
  width|3fn>|<cwith|1|1|1|1|cell halign|r>|<table|<row|<cell|<itemname|<arg|theitem>>>>>>>
  \ >>

  <apply|newdescription|description-dash|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first
  indentation>><resize|<itemname|<arg|theitem>>  |||r]1.5fn|>>>

  <apply|newdescription|description-long|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<format|enable first
  indentation>><resize|<itemname|<arg|theitem>>|||r]1.5fn|><format|next
  line>>>

  <assign|description|<value|description-compact>>

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
