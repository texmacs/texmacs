<TeXmacs|1.0.1.10>

<style|header-generic>

<\body>
  <assign|header-letter-package|1.0>

  <assign|header-letter-dtd|1.0>

  \;

  <assign|address*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|address|<macro|body|<surround|<vspace*|0.5fn><apply|leftflush>||<tabular|<tformat|<cwith|1|1|1|1|cell
  lsep|0spc>|<cwith|1|1|1|1|cell rsep|0spc>|<cwith|1|1|1|1|cell
  hyphen|n>|<twith|table valign|T>|<table|<row|<\cell>
    <expand|address*|<arg|body>>
  </cell>>>>>>>>

  <assign|destination|<macro|body|<surround|<vspace*|1.5fn>|<apply|rightflush>|<tabular|<tformat|<cwith|1|1|1|1|cell
  lsep|0spc>|<cwith|1|1|1|1|cell rsep|0spc>|<cwith|1|1|1|1|cell
  hyphen|n>|<twith|table valign|T>|<table|<row|<\cell>
    <expand|address*|<arg|body>>
  </cell>>>>>>>>

  <assign|letter-date*|<macro|body|<arg|body>>>

  <assign|letter-date|<macro|body|<surround|<vspace*|0.5fn><apply|leftflush>||<expand|letter-date*|<arg|body>>>>>

  <assign|letter-header|<macro|body|<surround||<vspace|5fn>|<arg|body>>>>

  \;

  <assign|opening*|<macro|body|<arg|body>>>

  <assign|opening|<macro|body|<surround||<apply|rightflush><vspace|1fn>|<expand|opening*|<arg|body>>>>>

  <assign|closing*|<macro|body|<arg|body>>>

  <assign|closing|<macro|body|<surround||<apply|rightflush><vspace|1fn>|<expand|closing*|<arg|body>>>>>

  <assign|signature*|<macro|body|<arg|body>>>

  <assign|signature|<macro|body|<surround|<vspace*|5fn><apply|leftflush>||<expand|signature*|<arg|body>>>>>

  <assign|cc*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|cc|<macro|body|<vspace*|0.5fn><translate|Cc|english|<apply|language>>:
  <tabular|<tformat|<cwith|1|1|1|1|cell lsep|0spc>|<cwith|1|1|1|1|cell
  rsep|0spc>|<cwith|1|1|1|1|cell hyphen|n>|<twith|table
  valign|T>|<table|<row|<\cell>
    <expand|cc*|<arg|body>>
  </cell>>>>><apply|rightflush>>>

  <assign|encl*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|encl|<macro|body|<vspace*|0.5fn><abbr|<translate|Encl|english|<apply|language>>>:
  <tabular|<tformat|<cwith|1|1|1|1|cell lsep|0spc>|<cwith|1|1|1|1|cell
  rsep|0spc>|<cwith|1|1|1|1|cell hyphen|n>|<twith|table
  valign|T>|<table|<row|<\cell>
    <expand|encl*|<arg|body>>
  </cell>>>>><apply|rightflush>>>

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
