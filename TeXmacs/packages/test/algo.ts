<TeXmacs|1.0.0.4>

<\body>
  <assign|algo-package|1.0>

  \;

  <assign|fun|<macro|x|<with|mode|text|font family|rm|<arg|x>>>>

  <assign|var|<macro|x|<with|mode|text|font shape|italic|<arg|x>>>>

  <assign|type|<macro|x|<with|mode|text|font family|ss|<arg|x>>>>

  <assign|vardecl|<macro|x|y|<var|<arg|x>>:<type|<arg|y>>>>

  <assign|fundecl|<macro|x|y|<fun|<arg|x>>:<type|<arg|y>>>>

  \;

  <assign|synopsis|<\macro|x>
    <with|interparagraph space|0fn|<vspace*|0.5fn><with|font series|bold|font
    size|1.19|Synopsis><vspace|0.5fn><format|no page break after>>

    <with|left margin|<plus|<value|left margin>|1.5fn>|interparagraph
    space|0fn|<arg|x>>
  </macro>>

  <assign|parameters|<\macro|x>
    <with|interparagraph space|0fn|<vspace*|0.5fn><with|font series|bold|font
    size|1.19|Parameters><vspace|0.5fn><format|no page break after>>

    <with|interparagraph space|0fn|<description|<arg|x>>>
  </macro>>

  \;

  <assign|comment|<macro|x|<htab|5mm><with|mode|text|font shape|slanted|font
  size|0.84|<arg|x>>>>

  <assign|longcomment|<macro|body|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<w\
  ith|left margin|<plus|<apply|left margin>|3fn>|right
  margin|<plus|<apply|right margin>|3fn>|mode|text|font shape|slanted|font
  size|0.84|<arg|body>>>>>

  \;

  <assign|keyw|<macro|x|<with|mode|text|font series|bold|<arg|x>>>>

  <assign|newkeyword|<func|kw|<assign|<merge|kw-|<apply|kw>>|<hold|<keyw|<rel\
  ease|<apply|kw>>>>>>>

  <apply|newkeyword|for>

  <apply|newkeyword|to>

  <apply|newkeyword|do>

  <apply|newkeyword|begin>

  <apply|newkeyword|end>

  <apply|newkeyword|while>

  <apply|newkeyword|repeat>

  <apply|newkeyword|until>

  <apply|newkeyword|return>

  <apply|newkeyword|if>

  <apply|newkeyword|then>

  <apply|newkeyword|else>

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
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
