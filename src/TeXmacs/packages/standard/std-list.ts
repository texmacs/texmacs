<TeXmacs|1.0.2.7>

<\body>
  <assign|std-list-package|1.0>

  <assign|std-list-dtd|1.0>

  \;

  <assign|itemname|<macro|name|<with|font series|bold|<arg|name>>>>

  <assign|item|<macro|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><resize|<with|mode|math|\<ast\>>
  |||r]1.5fn|>>>

  <assign|item*|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><resize|<itemname|<arg|theitem>><space|0.5fn>|||r]1.5fn|>>>

  \;

  <assign|thetag|<with|mode|math|\<ast\>>>

  <assign|itemize-level|0>

  <assign|itemize-base|<macro|body|<surround|<no_page_break_before>|<rightflush><vspace|0.5fn><no_indentation_after>|<with|left
  margin|<plus|<value|left margin>|3fn>|item|<macro|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><resize|<value|thetag>
  |||r]1.5fn|>>|<arg|body>>>>>

  <assign|itemize|<\macro|body>
    <with|itemize-level|<plus|<value|itemize-level>|1>|<with|thetag|<case|<equal|<mod|<value|itemize-level>|3>|1>|<with|mode|math|\<bullet\>>|<equal|<mod|<value|itemize-level>|3>|2>|<with|mode|math|\<circ\>>|<equal|<mod|<value|itemize-level>|3>|0>|<with|mode|math|->>|<itemize-base|<arg|body>>>>
  </macro>>

  \;

  <assign|newitemize|<macro|name|tag|<assign|<arg|name>|<hold|<\macro|body>
    <with|thetag|<release|<arg|tag>>|<itemize-base|<arg|body>>>
  </macro>>>>>

  <newitemize|itemize-minus|<with|mode|math|->>

  <newitemize|itemize-dot|<with|mode|math|\<bullet\>>>

  <newitemize|itemize-arrow|<with|mode|math|\<rightarrow\>>>

  \;

  <assign|theitem|<macro|<itemnr>.>>

  <assign|enumerate-level|0>

  <assign|enumerate-base|<macro|body|<surround|<no_page_break_before>|<rightflush><vspace|0.5fn><no_indentation_after>|<with|itemnr|0|left
  margin|<plus|<value|left margin>|3fn>|item|<macro|<vspace*|0.5fn><assign|itemnr|<plus|<value|itemnr>|1>><assign|thelabel|<theitem>><with|first
  indentation|-3fn|<enable_first_indentation>><resize|<theitem>|r-2.5fn||r+0.5fn|>>|<arg|body>>>>>

  <assign|enumerate|<\macro|body>
    <with|enumerate-level|<plus|<value|enumerate-level>|1>|<with|theitem|<case|<equal|<mod|<value|enumerate-level>|3>|1>|<hold|<itemnr>.>|<equal|<mod|<value|enumerate-level>|3>|2>|<macro|<number|<itemnr>|alpha><with|font
    shape|right|)>>|<equal|<mod|<value|enumerate-level>|3>|0>|<macro|<number|<itemnr>|roman>.>>|<enumerate-base|<arg|body>>>>
  </macro>>

  \;

  <assign|newenumerate|<macro|name|num|<assign|<arg|name>|<hold|<\macro|body>
    <with|theitem|<release|<arg|num>>|<enumerate-base|<arg|body>>>
  </macro>>>>>

  <newenumerate|enumerate-numeric|<macro|<itemnr>.>>

  <newenumerate|enumerate-roman|<macro|<number|<itemnr>|roman>.>>

  <newenumerate|enumerate-Roman|<macro|<number|<itemnr>|Roman>.>>

  <newenumerate|enumerate-alpha|<macro|<number|<itemnr>|alpha><with|font
  shape|right|)>>>

  <newenumerate|enumerate-Alpha|<macro|<number|<itemnr>|Alpha><with|font
  shape|right|)>>>

  \;

  <assign|newdescription|<macro|name|item-macro|<assign|<arg|name>|<hold|<\macro|body>
    <with|item*|<release|<arg|item-macro>>|<itemize-base|<arg|body>>>
  </macro>>>>>

  <newdescription|description-compact|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><resize|<itemname|<arg|theitem>>
  \ |||r]1.5fn|>>>

  <newdescription|description-aligned|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><tabular*|<tformat|<cwith|1|1|1|1|cell
  lsep|0fn>|<cwith|1|1|1|1|cell rsep|0fn>|<cwith|1|1|1|1|cell
  bsep|0fn>|<cwith|1|1|1|1|cell tsep|0fn>|<cwith|1|1|1|1|cell
  width|3fn>|<cwith|1|1|1|1|cell halign|r>|<table|<row|<cell|<itemname|<arg|theitem>>>>>>>
  \ >>

  <newdescription|description-dash|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><resize|<itemname|<arg|theitem>>
   |||r]1.5fn|>>>

  <newdescription|description-long|<macro|theitem|<vspace*|0.5fn><with|first
  indentation|-1.5fn|<enable_first_indentation>><resize|<itemname|<arg|theitem>>|||r]1.5fn|><next_line>>>

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