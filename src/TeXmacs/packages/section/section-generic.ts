<TeXmacs|1.0.0.17>

<style|section-automatic-short>

<\body>
  <assign|section-letter-package|1.0>

  <assign|section-latex-dtd|1.0>

  \;

  Chapters

  <assign|chapter*|<macro|name|<vspace*|2fn><with|paragraph mode|center|math
  font series|bold|font series|bold|font size|1.68|<arg|name>><vspace|1fn><fo\
  rmat|no page break after>>>

  <assign|chapter**|<macro|chapname|name|<expand|chapter*|<arg|chapname>.<spa\
  ce|2spc><arg|name>>>>

  <assign|chapter|<macro|name|<apply|toc-main-2|<arg|name>><expand|chapter*|<\
  arg|name>>>>

  <assign|appendix|<macro|name|<apply|toc-main-2|<translate|Appendix:|english\
  |<value|language>> <arg|name>><expand|chapter*|<translate|Appendix:|english\
  |<value|language>> <arg|name>>>>

  \;

  Sections, subsections and subsubsections

  <assign|section*|<macro|name|<vspace*|1.5fn><with|math font
  series|bold|font series|bold|font size|1.41|<arg|name>><vspace|0.5fn><forma\
  t|no page break after>>>

  <assign|section|<macro|name|<apply|toc-normal-1|<arg|name>><expand|section*\
  |<arg|name>>>>

  <assign|subsection*|<macro|name|<vspace*|1fn><with|math font
  series|bold|font series|bold|font size|1.19|<arg|name>><vspace|0.3333fn><fo\
  rmat|no page break after>>>

  <assign|subsection|<macro|name|<apply|toc-normal-2|<arg|name>><expand|subse\
  ction*|<arg|name>>>>

  <assign|subsubsection*|<macro|name|<vspace*|0.75fn><with|math font
  series|bold|font series|bold|<arg|name>><vspace|0.25fn><format|no page
  break after>>>

  <assign|subsubsection|<macro|name|<apply|toc-normal-3|<arg|name>><expand|su\
  bsubsection*|<arg|name>>>>

  \;

  Paragraphs and subparagraphs

  <assign|paragraph*|<macro|name|<vspace*|0.5fn><with|math font
  series|bold|font series|bold|<arg|name>> >>

  <assign|paragraph|<macro|name|<apply|toc-small-1|<arg|name>><expand|paragra\
  ph*|<arg|name>>>>

  <assign|subparagraph*|<macro|name|<vspace*|0.25fn><with|math font
  series|bold|font series|bold|<arg|name>> >>

  <assign|subparagraph|<macro|name|<apply|toc-small-2|<arg|name>><expand|subp\
  aragraph*|<arg|name>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
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
