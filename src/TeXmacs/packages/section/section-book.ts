<TeXmacs|1.0.0.25>

<style|section-automatic-long>

<\body>
  <assign|section-book-package|1.0>

  <assign|section-latex-dtd|1.0>

  \;

  Chapters and appendices

  <assign|chapter*|<macro|name|<format|new page before><format|no first
  indentation><format|new line><format|no first
  indentation><vspace*|5fn><with|math font series|bold|font series|bold|font
  size|2|<arg|name>><vspace|2fn><format|no page break after><format|no
  indentation after>>>

  <assign|chapter**|<macro|chapname|name|<expand|chapter*|<arg|chapname><vspa\
  ce|1fn><format|new line><arg|name>>>>

  <assign|chapter|<macro|name|<surround|<assign|thechapter|<func|<apply|chapt\
  ernr>>><assign|chapternr|<plus|<apply|chapternr>|1>><assign|thelabel|<apply\
  |thechapter>><apply|resetchapter><apply|header-primary|<arg|name>|<apply|th\
  echapter>|<translate|Chapter|english|<apply|language>>><apply|toc-main-2|<a\
  pply|thechapter><space|2spc><arg|name>>||<expand|chapter**|<translate|Chapt\
  er|english|<apply|language>> <apply|thechapter>|<arg|name>>>>>

  <assign|appendix|<macro|name|<surround|<assign|thechapter|<func|<number|<ap\
  ply|appendixnr>|Alpha>>><assign|appendixnr|<plus|<apply|appendixnr>|1>><ass\
  ign|thelabel|<apply|thechapter>><apply|resetchapter><apply|header-primary|<\
  arg|name>|<apply|thechapter>|<translate|Appendix|english|<apply|language>>>\
  <apply|toc-main-2|<translate|Appendix|english|<apply|language>>
  <apply|thechapter>.<space|2spc><arg|name>>||<expand|chapter**|<translate|Ap\
  pendix|english|<apply|language>> <apply|thechapter>|<arg|name>>>>>

  <assign|special-chapter|<macro|name|<surround|<assign|thechapter|<func|*>><\
  apply|resetchapter><assign|thelabel|<apply|thechapter>><apply|toc-main-2|<a\
  rg|name>><apply|header-primary|<arg|name>|*|<arg|name>>||<expand|chapter*|<\
  arg|name>>>>>

  <assign|prologue|<macro|<expand|special-chapter|<translate|Prologue|english\
  |<apply|language>>>>>

  <assign|epilogue|<macro|<expand|special-chapter|<translate|Epilogue|english\
  |<apply|language>>>>>

  \;

  Sections

  <assign|sectionsep|<func|<space|2spc>>>

  <assign|section*|<macro|name|<format|no first
  indentation><vspace*|3fn><with|math font series|bold|font series|bold|font
  size|1.41|<arg|name>><vspace|1fn><format|no page break after><format|no
  indentation after>>>

  <assign|section|<macro|name|<assign|sectionnr|<plus|<apply|sectionnr>|1>><a\
  pply|resetsection><apply|header-secondary|<arg|name>|<apply|thesection>|<tr\
  anslate|Section|english|<value|language>>><assign|thelabel|<apply|thesectio\
  n>><apply|toc-normal-1|<apply|thelabel><space|2spc><arg|name>><expand|secti\
  on*|<apply|thesection><apply|sectionsep><arg|name>>>>

  <assign|subsection*|<macro|name|<format|no first
  indentation><vspace*|2fn><with|math font series|bold|font series|bold|font
  size|1.19|<arg|name>><vspace|0.5fn><format|no page break after><format|no
  indentation after>>>

  <assign|subsection|<macro|name|<assign|subsectionnr|<plus|<apply|subsection\
  nr>|1>><apply|resetsubsection><assign|thelabel|<apply|thesubsection>><apply\
  |toc-normal-2|<apply|thelabel><space|2spc><arg|name>><expand|subsection*|<a\
  pply|thesubsection><apply|sectionsep><arg|name>>>>

  <assign|subsubsection*|<macro|name|<format|no first
  indentation><vspace*|1fn><with|math font series|bold|font
  series|bold|<arg|name>><vspace|0.5fn><format|no page break after><format|no
  indentation after>>>

  <assign|subsubsection|<macro|name|<assign|subsubsectionnr|<plus|<apply|subs\
  ubsectionnr>|1>><apply|resetsubsubsection><assign|thelabel|<apply|thesubsub\
  section>><apply|toc-normal-3|<apply|thelabel><space|2spc><arg|name>><expand\
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
