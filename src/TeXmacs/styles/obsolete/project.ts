<TeXmacs|0.3.4.5>

<\body>
  <assign|project-initialize|<macro|body|<surround|<with|font
  series|bold|font size|1.19|Project initialization
  program><vspace|0.5fn><format|new line><format|no page break
  before>|<vspace|1.5fn>|<with|left margin|<plus|<apply|left
  margin>|3fn>|first indentation|0fn|font family|tt|<arg|body>>>>>

  <assign|project-preamble|<macro|body|<surround|<with|font series|bold|font
  size|1.19|Project body><vspace|0.5fn><format|new line><format|no page break
  before>|<vspace|1.5fn>|<with|left margin|<plus|<apply|left
  margin>|3fn>|first indentation|0fn|<arg|body>>>>>

  <assign|project-body|<macro|body|<surround|<with|font series|bold|font
  size|1.19|Project body><vspace|0.5fn><format|new line><format|no page break
  before>|<vspace|1.5fn>|<with|left margin|<plus|<apply|left
  margin>|3fn>|first indentation|0fn|<arg|body>>>>>

  \;

  <assign|include-document|<macro|name|<with|font shape|small-caps|Include
  document ><arg|name>>>

  <assign|include-project|<macro|name|<with|font shape|small-caps|Include
  project ><arg|name>>>

  <assign|globalize-variable|<macro|var|<with|font shape|small-caps|Globalize
  ><arg|var>>>

  <assign|localize-variable|<macro|var|<with|font shape|small-caps|Localize
  ><arg|var>>>

  <assign|assign-variable|<macro|var|with|<with|font shape|small-caps|Assign
  ><arg|var><with|mode|math|<space|0.5spc>\<assign\><space|0.5spc>><arg|with>\
  >>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
