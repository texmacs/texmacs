<TeXmacs|1.0.1.10>

<style|header-article>

<\body>
  <assign|header-article-package|1.0>

  \;

  <assign|odd-page-text|<func|s|<assign|odd page header|<with|font
  size|0.84|<format|no first indentation><htab|0mm><with|font
  shape|small-caps|<apply|s>><htab|5mm><quote|<apply|thepage>>>>>>

  <assign|even-page-text|<func|s|<assign|even page header|<with|font
  size|0.84|<format|no first indentation><quote|<apply|thepage>><htab|5mm><with|font
  shape|small-caps|<apply|s>><htab|0mm>>>>>

  \;

  <assign|header-title|<func|name|<apply|even-page-text|<apply|name>>>>

  <assign|header-author|<func|name|<apply|odd-page-text|<apply|name>>>>

  <assign|header-primary|<func|name|nr|what|>>

  <assign|header-secondary|<func|name|nr|what|>>

  \;

  <assign|title*|<macro|body|<with|math font series|bold|font
  series|bold|font shape|small-caps|font size|1.19|<arg|body>>>>

  <assign|author*|<macro|body|<with|font shape|small-caps|<arg|body>>>>

  \;

  <assign|abstract|<macro|body|<surround|<vspace*|2fn>|<vspace|1fn>|<with|left
  margin|15mm|right margin|15mm|font size|0.84|<surround|<format|no first
  indentation><with|font shape|small-caps|<translate|Abstract|english|<apply|language>>>.
  |<apply|rightflush>|<arg|body>>>>>>

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
