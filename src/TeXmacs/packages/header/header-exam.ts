<TeXmacs|1.0.1.10>

<style|header-generic>

<\body>
  <assign|header-exam-package|1.0>

  <assign|header-exam-dtd|1.0>

  \;

  <assign|class*|<macro|body|<arg|body>>>

  <assign|class|<macro|body|<format|no first
  indentation><expand|class*|<arg|body>><apply|leftflush>>>

  <assign|title-date*|<macro|body|<arg|body>>>

  <assign|title-date|<macro|body|<apply|rightflush><expand|title-date*|<arg|body>>>>

  <assign|title*|<macro|body|<with|math font series|bold|font
  series|bold|font size|1.30|font shape|small-caps|<arg|body>>>>

  <assign|title|<\macro|body>
    <\with|paragraph mode|center>
      <surround|<vspace*|0.5fn>|<vspace|2fn>|<expand|title*|<arg|body>>>
    </with>
  </macro>>

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
