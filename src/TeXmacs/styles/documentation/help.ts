<TeXmacs|1.0.0.12>

<style|generic>

<\body>
  <assign|help-style|1.0>

  \;

  <assign|scheme|<with|font shape|small-caps|Scheme>>

  <assign|pari|<with|font shape|small-caps|Pari>>

  <assign|tmat|@>

  <assign|tmunsc|<with|font family|tt|_>>

  \;

  <assign|tmdef|<macro|concept|<with|font shape|italic|<arg|concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<expand|block*|<tformat|<table|<row|<cell|<arg|whi\
  ch>>>>>>>>

  <assign|skey|<macro|x|<key|shift-<arg|x>>>>

  <assign|ckey|<macro|x|<key|ctrl-<arg|x>>>>

  <assign|akey|<macro|x|<key|alt-<arg|x>>>>

  <assign|mkey|<macro|x|<key|meta-<arg|x>>>>

  <assign|hkey|<macro|x|<key|hyper-<arg|x>>>>

  <assign|menu-sub|<func|what|<with|font family|ss|<translate|<look_up|<value\
  |what>|0>|english|<apply|language>>><if|<is_tuple|<look_up|<value|what>|1>>\
  |<with|mode|math|\<rightarrow\>><apply|menu-sub|<look_up|<value|what>|1>>>>\
  >

  <assign|menu|<func|what*|<if|<is_tuple|<value|what>>|<apply|menu-sub|<value\
  |what>>>>>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
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
