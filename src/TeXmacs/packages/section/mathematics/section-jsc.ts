<TeXmacs|1.0.0.5>

<style|section-article>

<\body>
  <assign|section-jsc-package|1.0>

  \;

  Sections, subsections and subsubsections

  <assign|sectionsep|<func|.<space|2spc>>>

  <assign|section*|<macro|name|<format|no first
  indentation><vspace*|1fn><htab|0fn><with|font series|bold|math font
  series|bold|<arg|name>><htab|0fn><vspace|1fn><format|no page break after>>>

  <assign|subsection*|<macro|name|<format|no first
  indentation><vspace*|1fn><htab|0fn><with|font
  shape|small-caps|<arg|name>><htab|0fn><vspace|1fn><format|no page break
  after>>>

  <assign|subsubsection*|<macro|name|<format|no first
  indentation><vspace*|1fn><with|font shape|small-caps|<arg|name>><vspace|1fn\
  ><format|no page break after>>>

  \;

  Paragraphs and subparagraphs

  <assign|paragraph*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|font shape|small-caps|<arg|name>.
  ><format|no page break after>>>

  <assign|subparagraph*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|font shape|small-caps|<arg|name>.
  ><format|no page break after>>>

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
