<TeXmacs|1.0.0.5>

<style|section-automatic-short>

<\body>
  <assign|section-article-package|1.0>

  <assign|section-latex-dtd|1.0>

  \;

  Chapters

  <assign|chapter*|<macro|name|<vspace*|3fn><with|paragraph mode|center|math
  font series|bold|font series|bold|font size|1.68|<arg|name>><vspace|1fn><fo\
  rmat|no page break after>>>

  <assign|chapter**|<macro|chapname|name|<expand|chapter*|<arg|chapname>.<spa\
  ce|2spc><arg|name>>>>

  <assign|chapter|<macro|name|<apply|toc-main-1|<arg|name>><expand|chapter*|<\
  arg|name>>>>

  \;

  Sections

  <assign|sectionsep|<func|<space|2spc>>>

  <assign|section*|<macro|name|<format|no first
  indentation><vspace*|3fn><with|math font series|bold|font series|bold|font
  size|1.41|<arg|name>><vspace|1fn><format|no page break after><format|no
  indentation after>>>

  <assign|section|<macro|name|<assign|thesection|<func|<apply|sectionnr>>><as\
  sign|sectionnr|<plus|<value|sectionnr>|1>><apply|resetsection><assign|thela\
  bel|<apply|thesection>><apply|header-primary|<arg|name>|<apply|thesection>|\
  <translate|Section|english|<value|language>>><apply|toc-main-2|<apply|thela\
  bel><space|2spc><arg|name>><expand|section*|<apply|thesection><apply|sectio\
  nsep><arg|name>>>>

  <assign|appendix|<macro|name|<assign|thesection|<func|<number|<apply|append\
  ixnr>|Alpha>>><assign|appendixnr|<plus|<value|appendixnr>|1>><apply|resetse\
  ction><assign|thelabel|<apply|thesection>><apply|header-primary|<arg|name>|\
  <apply|thesection>|<translate|Appendix|english|<value|language>>><apply|toc\
  -main-2|<translate|Appendix|english|<value|language>>
  <apply|thelabel>.<space|2spc><arg|name>><expand|section*|<translate|Appendi\
  x|english|<value|language>> <apply|thesection>.<space|2spc><arg|name>>>>

  \;

  Subsections and subsubsections

  <assign|subsection*|<macro|name|<format|no first
  indentation><vspace*|2fn><with|math font series|bold|font series|bold|font
  size|1.19|<arg|name>><vspace|0.5fn><format|no page break after><format|no
  indentation after>>>

  <assign|subsection|<macro|name|<assign|subsectionnr|<plus|<value|subsection\
  nr>|1>><apply|resetsubsection><assign|thelabel|<apply|thesubsection>><apply\
  |header-secondary|<arg|name>|<apply|thesubsection>|<translate|Section|engli\
  sh|<value|language>>><apply|toc-normal-1|<apply|thelabel><space|2spc><arg|n\
  ame>><expand|subsection*|<apply|thesubsection><apply|sectionsep><arg|name>>\
  >>

  <assign|subsubsection*|<macro|name|<format|no first
  indentation><vspace*|1fn><with|math font series|bold|font
  series|bold|<arg|name>><vspace|0.5fn><format|no page break after><format|no
  indentation after>>>

  <assign|subsubsection|<macro|name|<assign|subsubsectionnr|<plus|<value|subs\
  ubsectionnr>|1>><apply|resetsubsubsection><assign|thelabel|<apply|thesubsub\
  section>><apply|toc-normal-2|<apply|thelabel><space|2spc><arg|name>><expand\
  |subsubsection*|<apply|thesubsubsection><apply|sectionsep><arg|name>>>>

  \;

  Paragraphs and subparagraphs

  <assign|paragraph*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|math font series|bold|font
  series|bold|<arg|name>> >>

  <assign|paragraph|<macro|name|<apply|toc-small-1|<arg|name>><expand|paragra\
  ph*|<arg|name>>>>

  <assign|subparagraph*|<macro|name|<format|no first
  indentation><vspace*|0.25fn><with|math font series|bold|font
  series|bold|<arg|name>> >>

  <assign|subparagraph|<macro|name|<apply|toc-small-2|<arg|name>><expand|subp\
  aragraph*|<arg|name>>>>

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
