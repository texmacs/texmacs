<TeXmacs|1.0.0.24>

<\body>
  <assign|env-manage-package|1.0>

  <if|<equal|<value|env-manage-dtd>|<uninit>>|<assign|init-document|<merge|<f\
  unc|<apply|init-stdenv>>|<value|init-document>>>>

  <assign|env-manage-dtd|1.0>

  \;

  <assign|list-add|<func|class|env|Name|render|<assign|<merge|list-|<apply|cl\
  ass>>|<tuple|<apply|<merge|list-|<apply|class>>>|<apply|class>|<apply|env>|\
  <apply|Name>|<apply|render>>>>>

  <assign|list-theorem|>

  <assign|list-exercise|>

  <assign|list-figure|>

  <assign|newtheorem|<func|env|Name|<apply|list-add|theorem|<apply|env>|<appl\
  y|Name>|theorem*>>>

  <assign|newremark|<func|env|Name|<apply|list-add|theorem|<apply|env>|<apply\
  |Name>|remark*>>>

  <assign|newexercise|<func|env|Name|<apply|list-add|exercise|<apply|env>|<ap\
  ply|Name>|exercise*>>>

  <assign|newfigure|<func|env|Name|<apply|list-add|figure|<apply|env>|<apply|\
  Name>|figure*>>>

  \;

  <assign|init-stdenv|<func|>>

  <assign|resetstdenv|<func|<assign|footnotenr|0>>>

  <assign|thefootnote|<func|<apply|footnotenr>>>

  <assign|newstdenv-counter|<func|which|<assign|<merge|the|<apply|which>>|<ho\
  ld|<func|<apply|theprefix><apply|<release|<merge|<apply|which>|nr>>>>>><ass\
  ign|resetstdenv|<merge|<value|resetstdenv>|<hold|<func|<assign|<release|<me\
  rge|<apply|which>|nr>>|0>>>>>>>

  <assign|newstdenv|<func|class|env|Name|render|<assign|<apply|env>|<hold|<ma\
  cro|body|<surround|<assign|<release|<merge|<apply|class>|nr>>|<plus|<apply|\
  <release|<merge|<apply|class>|nr>>>|1>><assign|thelabel|<apply|<release|<me\
  rge|the|<apply|class>>>>>||<expand|<release|<apply|render>>|<translate|<rel\
  ease|<apply|Name>>|english|<apply|language>>
  <apply|<release|<merge|the|<apply|class>>>>|<arg|body>>>>>>>>

  <assign|newsmallfigure|<func|env|Name|<assign|<merge|small-|<apply|env>>|<h\
  old|<macro|body|caption|<assign|<release|<merge|<apply|env>|nr>>|<plus|<val\
  ue|<release|<merge|<apply|env>|nr>>>|1>><assign|thelabel|<apply|<release|<m\
  erge|the|<apply|env>>>>><expand|small-figure*|<release|<apply|env>>|<transl\
  ate|<release|<apply|Name>>|english|<apply|language>>
  <apply|<release|<merge|the|<apply|env>>>>|<arg|body>|<arg|caption>>>>>>>

  <assign|newbigfigure|<func|env|Name|<assign|<merge|big-|<apply|env>>|<hold|\
  <macro|body|caption|<surround|<assign|<release|<merge|<apply|env>|nr>>|<plu\
  s|<value|<release|<merge|<apply|env>|nr>>>|1>><assign|thelabel|<apply|<rele\
  ase|<merge|the|<apply|env>>>>>||<expand|big-figure*|<release|<apply|env>>|<\
  translate|<release|<apply|Name>>|english|<apply|language>>
  <apply|<release|<merge|the|<apply|env>>>>|<arg|body>|<arg|caption>>>>>>>>

  <assign|newstdfigure|<func|env|Name|<apply|newsmallfigure|<apply|env>|<appl\
  y|Name>><apply|newbigfigure|<apply|env>|<apply|Name>>>>

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
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
  </collection>
</initial>
