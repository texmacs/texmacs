<TeXmacs|1.0.0.24>

<style|article>

<\body>
  <assign|tmarticle-article|1.0>

  \;

  <assign|odd-page-text|<func|s|<assign|odd page header|<with|font
  size|0.84|<format|no first indentation><tabular|<tformat|<cwith|1|-1|1|-1|c\
  ell bborder|1ln>|<twith|table width|1par>|<cwith|1|1|2|2|cell
  halign|r>|<cwith|1|-1|1|-1|cell lsep|0spc>|<cwith|1|-1|1|-1|cell
  rsep|0spc>|<cwith|1|1|1|1|cell halign|l>|<table|<row|<cell|<with|font
  shape|small-caps|<apply|s>>>|<cell|<quote|<apply|thepage>>>>>>>>>>>

  <assign|even-page-text|<func|s|<assign|even page header|<with|font
  size|0.84|<format|no first indentation><tabular|<tformat|<cwith|1|-1|1|-1|c\
  ell bborder|1ln>|<twith|table width|1par>|<cwith|1|1|2|2|cell
  halign|r>|<cwith|1|-1|1|-1|cell lsep|0spc>|<cwith|1|-1|1|-1|cell
  rsep|0spc>|<cwith|1|1|1|1|cell halign|l>|<table|<row|<cell|<quote|<apply|th\
  epage>>>|<cell|<with|font shape|small-caps|<apply|s>>>>>>>>>>>

  \;

  <assign|header-title|<func|name|<apply|even-page-text|<apply|name>>>>

  <assign|header-author|<func|name|<apply|odd-page-text|<apply|name>>>>

  <assign|header-primary|<func|name|nr|what|>>

  <assign|header-secondary|<func|name|nr|what|>>

  \;

  <assign|title*|<macro|body|<with|font series|bold|math font
  series|bold|font shape|small-caps|font size|1.54|<arg|body>>>>

  <assign|author*|<macro|body|<with|font shape|italic|font
  size|1.19|<arg|body>>>>

  <assign|abstract|<\macro|body>
    <\with|left margin|15mm|right margin|15mm|font size|0.84>
      <value|hrule>

      <surround|<format|no first indentation>||<arg|body>>

      <value|hrule>

      \;
    </with>
  </macro>>

  \;

  <assign|chapter*|<macro|name|<format|new line><format|no first
  indentation><vspace*|2fn><with|math font series|bold|font series|bold|font
  shape|small-caps|font size|1.54|<htab|0fn><arg|name><htab|0fn>><vspace|2fn>\
  <format|no page break after><format|no indentation after>>>

  <assign|chapter**|<macro|chapname|name|<expand|chapter*|<with|font
  size|1.83|<arg|chapname>><htab|0fn><vspace|1.5fn><format|new
  line><htab|0fn><arg|name>>>>

  <assign|sectionsep|<func|.<space|2spc>>>

  <assign|section*|<macro|name|<format|no first
  indentation><vspace*|2fn><with|math font series|bold|font series|bold|font
  size|1.30|font shape|small-caps|<htab|0fn><arg|name><htab|0fn>><vspace|1fn>\
  <format|no page break after><format|no indentation after>>>

  <assign|subsection*|<macro|name|<format|no first
  indentation><vspace*|1.5fn><with|math font series|bold|font
  series|bold|font size|1.19|<arg|name>><vspace|0.5fn><format|no page break
  after><format|no indentation after>>>

  <assign|subsubsection*|<macro|name|<format|no first
  indentation><vspace*|1fn><with|math font series|bold|font
  series|bold|<arg|name>><vspace|0.5fn><format|no page break after><format|no
  indentation after>>>

  \;

  <assign|theoremname|<macro|name|<with|font shape|small-caps|<arg|name>>>>

  <assign|toc-main-2|<func|what|<assign|tocnr|<plus|<value|tocnr>|1>><label|<\
  apply|thetoc>><write|toc|<vspace*|1fn><with|font series|bold|math font
  series|bold|font shape|small-caps|<value|what>><quote|<value|toc-dots>><pag\
  eref|<apply|thetoc>><vspace|0.5fn>>>>

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
