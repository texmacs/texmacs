<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <assign|std-list-package|1.0>

  <assign|std-list-dtd|1.0>

  \;

  <assign|itemname|<macro|name|<with|font-series|bold|math-font-series|bold|<arg|name>>>>

  <assign|item|<macro|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<active*|<with|mode|math|\<ast\>>>
  |||r]1.5fn|>>>

  <assign|item*|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<itemname|<arg|theitem>><space|0.5fn>|||r]1.5fn|>>>

  \;

  <assign|thetag|<active*|<with|mode|math|\<ast\>>>>

  <assign|itemize-level|0>

  <assign|itemize-base|<macro|body|<surround|<no-page-break*>|<rightflush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|item|<macro|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<value|thetag>
  |||r]1.5fn|>>|<arg|body>>>>>

  <assign|itemize|<\macro|body>
    <with|itemize-level|<plus|<value|itemize-level>|1>|<with|thetag|<case|<equal|<mod|<value|itemize-level>|3>|1>|<active*|<with|mode|math|\<bullet\>>>|<equal|<mod|<value|itemize-level>|3>|2>|<active*|<with|mode|math|\<circ\>>>|<equal|<mod|<value|itemize-level>|3>|0>|<active*|<with|mode|math|->>>|<itemize-base|<arg|body>>>>
  </macro>>

  \;

  <assign|newitemize|<macro|name|tag|<assign|<arg|name>|<hold|<\macro|body>
    <with|thetag|<release|<arg|tag>>|<itemize-base|<arg|body>>>
  </macro>>>>>

  <newitemize|itemize-minus|<active*|<with|mode|math|->>>

  <newitemize|itemize-dot|<active*|<with|mode|math|\<bullet\>>>>

  <newitemize|itemize-arrow|<active*|<with|mode|math|\<rightarrow\>>>>

  \;

  <assign|theitem|<macro|<itemnr>.>>

  <assign|enumerate-level|0>

  <assign|enumerate-base|<macro|body|<surround|<no-page-break*>|<rightflush><vspace|0.5fn><no-indent*>|<with|itemnr|0|par-left|<plus|<value|par-left>|3fn>|item|<macro|<vspace*|0.5fn><assign|itemnr|<plus|<value|itemnr>|1>><assign|thelabel|<theitem>><with|par-first|-3fn|<yes-indent>><resize|<theitem>|r-2.5fn||r+0.5fn|>>|<arg|body>>>>>

  <assign|enumerate|<\macro|body>
    <with|enumerate-level|<plus|<value|enumerate-level>|1>|<with|theitem|<case|<equal|<mod|<value|enumerate-level>|3>|1>|<hold|<itemnr>.>|<equal|<mod|<value|enumerate-level>|3>|2>|<macro|<number|<itemnr>|alpha><with|font-shape|right|)>>|<equal|<mod|<value|enumerate-level>|3>|0>|<macro|<number|<itemnr>|roman>.>>|<enumerate-base|<arg|body>>>>
  </macro>>

  \;

  <assign|newenumerate|<macro|name|num|<assign|<arg|name>|<hold|<\macro|body>
    <with|theitem|<release|<arg|num>>|<enumerate-base|<arg|body>>>
  </macro>>>>>

  <newenumerate|enumerate-numeric|<macro|<itemnr>.>>

  <newenumerate|enumerate-roman|<macro|<number|<itemnr>|roman>.>>

  <newenumerate|enumerate-Roman|<macro|<number|<itemnr>|Roman>.>>

  <newenumerate|enumerate-alpha|<macro|<number|<itemnr>|alpha><with|font-shape|right|)>>>

  <newenumerate|enumerate-Alpha|<macro|<number|<itemnr>|Alpha><with|font-shape|right|)>>>

  \;

  <assign|newdescription|<macro|name|item-macro|<assign|<arg|name>|<hold|<\macro|body>
    <with|item*|<release|<arg|item-macro>>|<itemize-base|<arg|body>>>
  </macro>>>>>

  <newdescription|description-compact|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<itemname|<arg|theitem>>
  \ |||r]1.5fn|>>>

  <newdescription|description-aligned|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|1|1|cell-bsep|0fn>|<cwith|1|1|1|1|cell-tsep|0fn>|<cwith|1|1|1|1|cell-width|3fn>|<cwith|1|1|1|1|cell-halign|r>|<table|<row|<cell|<itemname|<arg|theitem>>>>>>>
  \ >>

  <newdescription|description-dash|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<itemname|<arg|theitem>>
   |||r]1.5fn|>>>

  <newdescription|description-long|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<itemname|<arg|theitem>>|||r]1.5fn|><next-line>>>

  <assign|description|<value|description-compact>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>