<TeXmacs|1.0.1.10>

<style|header-article>

<\body>
  <assign|header-article-2col-package|1.0>

  \;

  <assign|odd page header|>

  <assign|even page header|>

  <assign|odd page footer|<htab|5mm><quote|<apply|thepage>><htab|5mm>>

  <assign|even page footer|<htab|5mm><quote|<apply|thepage>><htab|5mm>>

  \;

  <assign|header-title|<func|name|>>

  <assign|header-author|<func|name|>>

  <assign|header-primary|<func|name|nr|what|>>

  <assign|header-secondary|<func|name|nr|what|>>

  \;

  <assign|make-title|<macro|body|<surround||<vspace|2fn>|<with|nr
  columns|1|paragraph mode|center|<arg|body>>>>>

  \;

  <assign|abstract|<macro|body|<\surround|<vspace*|1.5fn>|<apply|rightflush><vspace|1.5fn>>
    <surround|<format|no first indentation>|<vspace|1.5fn><format|no
    indentation after><format|no page break after>|<with|font
    series|bold|<translate|Abstract|english|<apply|language>>>>

    <arg|body>
  </surround>>>

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
