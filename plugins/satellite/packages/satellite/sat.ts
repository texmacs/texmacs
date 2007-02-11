<TeXmacs|1.0.2.8>

<style|<tuple|common-base|env-us|section-sat>>

<\body>
  ;Macros définies par David Allouche

  <assign|sat-action|<macro|x|<datoms|<macro|y|<action|<with|color|black|<arg|y>>|(back-to-me-in-source)>>|<arg|x>>>>

  <assign|remark|<eval|<hold|<macro|x|<sat-action|<compound|<release|<value|remark>>|<arg|x>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
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