<TeXmacs|1.0.0.17>

<\body>
  <assign|number-us-package|1.0>

  <assign|number-env-dtd|1.0>

  \;

  <assign|newliststdenv|<func|l|<if|<is_tuple|<apply|l>>|<apply|newliststdenv\
  |<look_up|<apply|l>|0>><apply|newstdenv|<look_up|<apply|l>|1>|<look_up|<app\
  ly|l>|2>|<look_up|<apply|l>|3>|<look_up|<apply|l>|4>>>>>

  <assign|newlistfigure|<func|l|<if|<is_tuple|<apply|l>>|<apply|newlistfigure\
  |<look_up|<apply|l>|0>><apply|newstdfigure|<look_up|<apply|l>|2>|<look_up|<\
  apply|l>|3>>>>>

  <assign|newliststdenv-counter|<func|l|<if|<is_tuple|<apply|l>>|<apply|newli\
  ststdenv-counter|<look_up|<apply|l>|0>><apply|newstdenv-counter|<look_up|<a\
  pply|l>|2>>>>>

  <assign|init-stdenv|<func|<apply|newstdenv-counter|equation><apply|newstden\
  v-counter|theorem><apply|newstdenv-counter|exercise><apply|newliststdenv-co\
  unter|<value|list-figure>><apply|newliststdenv|<value|list-theorem>><apply|\
  newliststdenv|<value|list-exercise>><apply|newlistfigure|<value|list-figure\
  >>>>

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
