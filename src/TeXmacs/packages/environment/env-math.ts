<TeXmacs|1.0.1.22>

<\body>
  <assign|env-math-package|1.0>

  <assign|env-math-dtd|1.0>

  \;

  <assign|nextnumber|<macro|<assign|equationnr|<plus|<value|equationnr>|1>><assign|thelabel|<apply|theequation>>(<apply|theequation>)>>

  <assign|leqnumber|<macro|<nextnumber><htab|5mm>>>

  <assign|reqnumber|<macro|<htab|5mm><nextnumber>>>

  <assign|eqnumber|<value|reqnumber>>

  \;

  <assign|equation-lab|<macro|body|lab|<with|mode|math|formula
  style|true|interline space|0.45fn|<surround|<format|no page break
  before><vspace*|0.5fn><format|no first indentation><assign|thelabel|<arg|lab>><htab|5mm>|<htab|5mm>(<with|mode|text|<arg|lab>>)<vspace|0.5fn><format|no
  indentation after>|<arg|body>>>>>

  <assign|equation|<macro|body|<with|mode|math|formula style|true|interline
  space|0.45fn|<surround|<format|no page break
  before><vspace*|0.5fn><format|no first indentation><assign|equationnr|<plus|<value|equationnr>|1>><assign|thelabel|<apply|theequation>><htab|5mm>|<htab|5mm>(<apply|theequation>)<vspace|0.5fn><format|no
  indentation after>|<arg|body>>>>>

  <assign|equation*|<macro|body|<with|mode|math|formula style|true|interline
  space|0.45fn|<surround|<format|no page break
  before><vspace*|0.5fn><format|no first indentation><htab|0fn>|<htab|0fn><vspace|0.5fn><format|no
  indentation after>|<arg|body>>>>>

  \;

  <assign|eqnarray*|<macro|body|<with|paragraph mode|center|mode|math|formula
  style|true|interline space|0.45fn|<surround|<format|no page break
  before><vspace*|0.5fn>|<vspace|0.5fn><format|no indentation
  after>|<tformat|<twith|table hyphen|y>|<twith|table
  width|1par>|<twith|table min cols|3>|<twith|table max
  cols|3>|<cwith|1|-1|1|1|cell lsep|0spc>|<cwith|1|-1|-1|-1|cell
  rsep|0spc>|<cwith|1|-1|1|-1|cell bsep|0sep>|<cwith|1|-1|1|-1|cell
  tsep|0sep>|<cwith|1|-1|1|1|cell hpart|1>|<cwith|1|-1|-1|-1|cell
  hpart|1>|<cwith|1|-1|1|1|cell halign|r>|<cwith|1|-1|1|1|cell
  hyphen|b>|<cwith|1|-1|2|-2|cell halign|c>|<cwith|1|-1|-1|-1|cell
  halign|l>|<cwith|1|-1|-1|-1|cell hyphen|t>|<arg|body>>>>>>

  <assign|eqnarray|<value|eqnarray*>>

  <assign|leqnarray*|<macro|body|<with|mode|math|formula style|true|interline
  space|0.45fn|<surround|<format|no page break
  before><vspace*|0.5fn>|<vspace|0.5fn><format|no indentation
  after>|<tformat|<twith|table hyphen|y>|<twith|table
  width|1par>|<twith|table min cols|3>|<twith|table max
  cols|3>|<cwith|1|-1|1|1|cell lsep|0spc>|<cwith|1|-1|-1|-1|cell
  rsep|0spc>|<cwith|1|-1|1|-1|cell bsep|0sep>|<cwith|1|-1|1|-1|cell
  tsep|0sep>|<cwith|1|-1|-1|-1|cell hpart|1>|<cwith|1|-1|1|1|cell
  halign|r>|<cwith|1|-1|2|-2|cell halign|c>|<cwith|1|-1|-1|-1|cell
  halign|l>|<cwith|1|-1|1|1|cell width|1.5fn>|<cwith|1|-1|2|2|cell
  width|1fn>|<cwith|1|-1|-1|-1|cell hyphen|t>|<arg|body>>>>>>

  <assign|leqnarray|<value|leqnarray*>>

  <assign|align*|<macro|body|<with|paragraph mode|center|mode|math|formula
  style|true|<surround|<format|no page break
  before><vspace*|0.5fn>|<vspace|0.5fn><format|no indentation
  after>|<tformat|<twith|table hyphen|y>|<twith|table
  width|1par>|<twith|table min cols|2>|<twith|table max
  cols|2>|<cwith|1|-1|1|1|cell lsep|0spc>|<cwith|1|-1|1|1|cell
  rsep|0.5spc>|<cwith|1|-1|-1|-1|cell rsep|0spc>|<cwith|1|-1|-1|-1|cell
  lsep|0spc>|<cwith|1|-1|1|-1|cell bsep|0sep>|<cwith|1|-1|1|-1|cell
  tsep|0sep>|<cwith|1|-1|1|1|cell hpart|1>|<cwith|1|-1|-1|-1|cell
  hpart|1>|<cwith|1|-1|1|1|cell halign|r>|<cwith|1|-1|1|1|cell
  hyphen|b>|<cwith|1|-1|-1|-1|cell halign|l>|<cwith|1|-1|-1|-1|cell
  hyphen|t>|<arg|body>>>>>>

  <assign|align|<value|align*>>

  <assign|gather*|<macro|body|<with|paragraph mode|center|mode|math|formula
  style|true|<surround|<format|no page break
  before><vspace*|0.5fn>|<vspace|0.5fn><format|no indentation
  after>|<tformat|<twith|table hyphen|y>|<twith|table
  width|1par>|<twith|table min cols|1>|<twith|table max
  cols|1>|<cwith|1|-1|1|-1|cell lsep|0spc>|<cwith|1|-1|1|-1|cell
  rsep|0spc>|<cwith|1|-1|1|-1|cell bsep|0spc>|<cwith|1|-1|1|-1|cell
  tsep|1spc>|<cwith|1|-1|1|-1|cell hpart|1>|<cwith|1|-1|1|-1|cell
  hyphen|b>|<cwith|1|-1|1|-1|cell halign|c>|<arg|body>>>>>>

  <assign|gather|<value|gather*>>

  <assign|eqsplit*|<macro|body|<with|paragraph mode|center|mode|math|formula
  style|true|<surround|<format|no page break
  before><vspace*|0.5fn>|<vspace|0.5fn><format|no indentation
  after>|<tformat|<twith|table hyphen|y>|<twith|table
  width|1par>|<twith|table min cols|1>|<twith|table max
  cols|1>|<cwith|1|-1|1|-1|cell lsep|0spc>|<cwith|1|-1|1|-1|cell
  rsep|0spc>|<cwith|1|-1|1|-1|cell bsep|0spc>|<cwith|1|-1|1|-1|cell
  tsep|1spc>|<cwith|1|-1|1|-1|cell hpart|1>|<cwith|1|-1|1|-1|cell
  hyphen|b>|<cwith|-1|-1|1|1|cell halign|r>|<cwith|2|-2|1|1|cell
  halign|c>|<cwith|1|1|1|1|cell halign|l>|<arg|body>>>>>>

  <assign|eqsplit|<value|eqsplit*>>

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
