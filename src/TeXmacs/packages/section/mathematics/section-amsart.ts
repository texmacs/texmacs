<TeXmacs|1.0.0.5>

<style|section-article>

<\body>
  <assign|section-amsart-package|1.0>

  \;

  Sections

  <assign|sectionsep|<func|.<space|2spc>>>

  <assign|section*|<macro|name|<format|no first
  indentation><vspace*|1fn><htab|0fn><with|font
  shape|small-caps|<arg|name>><htab|0fn><vspace|0.5fn><format|no page break
  after><format|no indentation after>>>

  <assign|section|<macro|name|<assign|thesection|<func|<apply|sectionnr>>><as\
  sign|sectionnr|<plus|<value|sectionnr>|1>><apply|resetsection><assign|thela\
  bel|<apply|thesection>><apply|header-primary|<arg|name>|<apply|thesection>|\
  <translate|Section|english|<value|language>>><apply|toc-main-2|<apply|these\
  ction>. <arg|name>><expand|section*|<apply|thesection><apply|sectionsep><ar\
  g|name>>>>

  <assign|appendix|<macro|name|<assign|thesection|<func|<number|<apply|append\
  ixnr>|Alpha>>><assign|appendixnr|<plus|<value|appendixnr>|1>><apply|resetse\
  ction><format|new page><format|new line><format|no first
  indentation><vspace*|3fn><apply|header-primary|<arg|name>|<apply|thesection\
  >|<translate|Appendix|english|<value|language>>><assign|thelabel|<apply|the\
  section>><apply|toc-main-2|<translate|Appendix|english|<value|language>><ap\
  ply|thesection>. <arg|name>><with|math font series|bold|font
  series|bold|font size|1.41|<translate|Appendix|english|<apply|language>><sp\
  ace|2spc><apply|thesection>. <arg|name>><vspace|2fn><format|no page break
  after><format|no indentation after>>>

  \;

  <assign|subsection*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|math font series|bold|font
  series|bold|<arg|name>. ><format|no page break after><format|no indentation
  after>>>

  <assign|subsection|<macro|name|<assign|subsectionnr|<plus|<value|subsection\
  nr>|1>><apply|resetsubsection><assign|thelabel|<apply|thesubsection>><apply\
  |header-secondary|<arg|name>|<apply|thesubsection>|<translate|Section|engli\
  sh|<value|language>>><apply|toc-normal-1|<apply|thesubsection>.
  <arg|name>><apply|thesubsection><apply|sectionsep><expand|subsection*|<arg|\
  name>>>>

  <assign|subsubsection*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|math font series|bold|font
  series|bold|<arg|name>. ><format|no page break after><format|no indentation
  after>>>

  <assign|subsubsection|<macro|name|<assign|subsubsectionnr|<plus|<value|subs\
  ubsectionnr>|1>><apply|resetsubsubsection><assign|thelabel|<apply|thesubsub\
  section>><apply|toc-normal-2|<apply|thesubsubsection>.
  <arg|name>><apply|thesubsubsection><apply|sectionsep><expand|subsubsection*\
  |<arg|name>>>>

  \;

  <assign|paragraph*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|math font series|bold|font
  series|bold|<arg|name>. ><format|no page break after><format|no indentation
  after>>>

  <assign|subparagraph*|<macro|name|<format|no first
  indentation><vspace*|0.5fn><with|math font series|bold|font
  series|bold|<arg|name>. ><format|no page break after><format|no indentation
  after>>>

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
