<TeXmacs|1.0.0.24>

<style|header-article>

<\body>
  <assign|header-jsc-package|1.0>

  \;

  <assign|odd-page-text|<func|s|<assign|odd page header|<with|font
  size|0.84|<format|no first indentation><tabular|<tformat|<cwith|1|1|1|1|cel\
  l bborder|1ln>|<twith|table width|1par>|<cwith|1|1|1|1|cell
  halign|r>|<cwith|1|1|1|1|cell lsep|0spc>|<cwith|1|1|1|1|cell
  rsep|0spc>|<table|<row|<cell|<apply|s><space|4spc><quote|<apply|thepage>>>>\
  >>>>>>>

  <assign|even-page-text|<func|s|<assign|even page header|<with|font
  size|0.84|<format|no first indentation><tabular|<tformat|<cwith|1|1|1|1|cel\
  l bborder|1ln>|<twith|table width|1par>|<cwith|1|1|1|1|cell
  lsep|0spc>|<cwith|1|1|1|1|cell rsep|0spc>|<cwith|1|1|1|1|cell
  halign|l>|<table|<row|<cell|<quote|<apply|thepage>><space|4spc><apply|s>>>>\
  >>>>>>

  \;

  <assign|header-title|<func|name|<apply|even-page-text|<apply|name>>>>

  <assign|header-author|<func|name|<apply|odd-page-text|<apply|name>>>>

  <assign|header-primary|<func|name|nr|what|>>

  <assign|header-secondary|<func|name|nr|what|>>

  \;

  <assign|author*|<macro|body|<arg|body>>>

  \;

  <assign|abstract|<\macro|body>
    <\with|left margin|15mm|right margin|15mm|font size|0.84>
      <value|hrule>

      <arg|body>

      <value|hrule>

      \;
    </with>
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
