<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <assign|std-list-package|1.0>

  <assign|std-list-dtd|1.0>

  \;

  <assign|item-name|<macro|name|<with|font-series|bold|math-font-series|bold|<arg|name>>>>

  <assign|item|<macro|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<active*|<with|mode|math|\<ast\>>>
  |||r]1.5fn|>>>

  <assign|item*|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<item-name|<arg|theitem>><space|0.5fn>|||r]1.5fn|>>>

  \;

  <assign|thetag|<active*|<with|mode|math|\<ast\>>>>

  <assign|itemize-level|0>

  <assign|itemize-base|<macro|body|<surround|<no-page-break*>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|item|<macro|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<value|thetag>
  |||r]1.5fn|>>|<arg|body>>>>>

  <assign|itemize|<\macro|body>
    <with|itemize-level|<plus|<value|itemize-level>|1>|<with|thetag|<case|<equal|<mod|<value|itemize-level>|3>|1>|<active*|<with|mode|math|\<bullet\>>>|<equal|<mod|<value|itemize-level>|3>|2>|<active*|<with|mode|math|\<circ\>>>|<equal|<mod|<value|itemize-level>|3>|0>|<active*|<with|mode|math|->>>|<itemize-base|<arg|body>>>>
  </macro>>

  \;

  <assign|new-itemize|<macro|name|tag|<assign|<arg|name>|<quasiquote|<\macro|body>
    <with|thetag|<unquote|<arg|tag>>|<itemize-base|<arg|body>>>
  </macro>>>>>

  <new-itemize|itemize-minus|<active*|<with|mode|math|->>>

  <new-itemize|itemize-dot|<active*|<with|mode|math|\<bullet\>>>>

  <new-itemize|itemize-arrow|<active*|<with|mode|math|\<rightarrow\>>>>

  \;

  <assign|the-item|<macro|<item-nr>.>>

  <assign|enumerate-level|0>

  <assign|enumerate-base|<macro|body|<surround|<no-page-break*>|<right-flush><vspace|0.5fn><no-indent*>|<with|item-nr|0|par-left|<plus|<value|par-left>|3fn>|item|<macro|<vspace*|0.5fn><assign|item-nr|<plus|<value|item-nr>|1>><assign|the-label|<the-item>><with|par-first|-3fn|<yes-indent>><resize|<the-item>|r-2.5fn||r+0.5fn|>>|<arg|body>>>>>

  <assign|enumerate|<\macro|body>
    <with|enumerate-level|<plus|<value|enumerate-level>|1>|<with|the-item|<case|<equal|<mod|<value|enumerate-level>|3>|1>|<quasiquote|<item-nr>.>|<equal|<mod|<value|enumerate-level>|3>|2>|<macro|<number|<item-nr>|alpha><with|font-shape|right|)>>|<equal|<mod|<value|enumerate-level>|3>|0>|<macro|<number|<item-nr>|roman>.>>|<enumerate-base|<arg|body>>>>
  </macro>>

  \;

  <assign|new-enumerate|<macro|name|num|<assign|<arg|name>|<quasiquote|<\macro|body>
    <with|the-item|<unquote|<arg|num>>|<enumerate-base|<arg|body>>>
  </macro>>>>>

  <new-enumerate|enumerate-numeric|<macro|<item-nr>.>>

  <new-enumerate|enumerate-roman|<macro|<number|<item-nr>|roman>.>>

  <new-enumerate|enumerate-Roman|<macro|<number|<item-nr>|Roman>.>>

  <new-enumerate|enumerate-alpha|<macro|<number|<item-nr>|alpha><with|font-shape|right|)>>>

  <new-enumerate|enumerate-Alpha|<macro|<number|<item-nr>|Alpha><with|font-shape|right|)>>>

  \;

  <assign|new-description|<macro|name|item-macro|<assign|<arg|name>|<quasiquote|<\macro|body>
    <with|item*|<unquote|<arg|item-macro>>|<itemize-base|<arg|body>>>
  </macro>>>>>

  <new-description|description-compact|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<item-name|<arg|theitem>>
  \ |||r]1.5fn|>>>

  <new-description|description-aligned|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|1|1|cell-bsep|0fn>|<cwith|1|1|1|1|cell-tsep|0fn>|<cwith|1|1|1|1|cell-width|3fn>|<cwith|1|1|1|1|cell-halign|r>|<table|<row|<cell|<item-name|<arg|theitem>>>>>>>
  \ >>

  <new-description|description-dash|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<item-name|<arg|theitem>>
   |||r]1.5fn|>>>

  <new-description|description-long|<macro|theitem|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<item-name|<arg|theitem>>|||r]1.5fn|><next-line>>>

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