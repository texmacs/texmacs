<TeXmacs|1.0.1.20>

<\body>
  <assign|mupad-output|<macro|body|<surround|<vspace*|0.75fn>|<vspace|0.75fn>|<with|left
  margin|<plus|<apply|left margin>|1.5fn>|<with|paragraph mode|left|interline
  space|0.45fn|formula style|true|<arg|body>>>>>>

  <assign|mupad-input|<macro|prompt|body|<expand|generic-input|<resize|<space|0.5fn><arg|prompt>|0fn||1.5fn||>|<arg|body>>>>

  <assign|mupad-output|<macro|body|<surround|<vspace*|0.75fn>|<vspace|0.75fn>|<with|left
  margin|<plus|<apply|left margin>|1.5fn>|<expand|generic-output*|<with|interline
  space|0.45fn|<arg|body>>>>>>>

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
