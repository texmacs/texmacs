<TeXmacs|1.0.0.25>

<\body>
  <assign|number-book-package|1.0>

  <if|<equal|<value|number-section-dtd>|<uninit>>|<assign|init-document|<merg\
  e|<value|init-document>|<func|<apply|resettop>>>>>

  <assign|number-section-dtd|1.0>

  \;

  <assign|resettop|<func|<assign|chapternr|0><assign|appendixnr|0><assign|sec\
  tionnr|0><apply|resetstdenv>>>

  <assign|resetchapter|<func|<assign|sectionnr|0><apply|resetstdenv>>>

  <assign|resetsection|<func|<assign|subsectionnr|0>>>

  <assign|resetsubsection|<func|<assign|subsubsectionnr|0>>>

  <assign|resetsubsubsection|<func|>>

  \;

  <assign|thechapter|<func|<value|chapternr>>>

  <assign|thesection|<func|<apply|thechapter>.<apply|sectionnr>>>

  <assign|thesubsection|<func|<apply|thesection>.<apply|subsectionnr>>>

  <assign|thesubsubsection|<func|<apply|thesubsection>.<apply|subsubsectionnr\
  >>>

  <assign|theprefix|<func|<apply|thechapter>.>>

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
