<TeXmacs|1.0.0.24>

<\body>
  <assign|section-automatic-long-package|1.0>

  <assign|section-automatic-dtd|1.0>

  \;

  <assign|automatic-chapter|<macro|name|<surround|<assign|thechapter|<func|*>\
  ><apply|resetchapter><assign|thelabel|<apply|thechapter>><apply|header-prim\
  ary|<arg|name>|*|<arg|name>>||<expand|chapter*|<arg|name>>>>>

  <assign|bibliography*|<\macro|aux|style|file-name|name|body>
    <expand|automatic-chapter|<translate|<arg|name>|english|<apply|language>>\
    ><apply|toc-main-2|<translate|<arg|name>|english|<apply|language>>>

    <with|interparagraph space|0fn|font size|0.84|<description|<arg|body>>>
  </macro>>

  <assign|table-of-contents*|<\macro|aux|name|body>
    <expand|automatic-chapter|<translate|<arg|name>|english|<apply|language>>\
    >

    <with|first indentation|0fn|interparagraph space|0fn|<arg|body>>
  </macro>>

  <assign|the-index*|<\macro|aux|name|body>
    <expand|automatic-chapter|<translate|<arg|name>|english|<apply|language>>\
    ><apply|toc-main-2|<translate|<arg|name>|english|<apply|language>>>

    <with|first indentation|0fn|interparagraph space|0fn|font size|0.84|nr
    columns|2|<arg|body>>
  </macro>>

  <assign|the-glossary*|<\macro|aux|name|body>
    <expand|automatic-chapter|<translate|<arg|name>|english|<apply|language>>\
    ><apply|toc-main-2|<translate|<arg|name>|english|<apply|language>>>

    <with|first indentation|0cm|interparagraph space|0fn|font
    size|0.84|<arg|body>>
  </macro>>

  \;

  <assign|bibliography|<\macro|aux|style|file-name|body>
    <expand|bibliography*|<arg|aux>|<arg|style>|<arg|file-name>|Bibliography|\
    <arg|body>>
  </macro>>

  <assign|table-of-contents|<\macro|aux|body>
    <expand|table-of-contents*|<arg|aux>|Table of contents|<arg|body>>
  </macro>>

  <assign|the-index|<\macro|aux|body>
    <expand|the-index*|<arg|aux>|Index|<arg|body>>
  </macro>>

  <assign|the-glossary|<\macro|aux|body>
    <expand|the-glossary*|<arg|aux>|Glossary|<arg|body>>
  </macro>>

  <assign|thebibliography|<\macro|dummy|body>
    <expand|automatic-chapter|<translate|Bibliography|english|<apply|language\
    >>><apply|toc-main-2|<translate|Bibliography|english|<apply|language>>>

    <with|interparagraph space|0fn|font size|0.84|<description|<arg|body>>>
  </macro>>

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
