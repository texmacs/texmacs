<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|section-book|1.0|section-latex|1.0>

    <\src-purpose>
      Sectional markup for books.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|section-automatic-long>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter*|<macro|name|<style-with|src-compact|none|<new-page*><no-indent><new-line><no-indent><vspace*|5fn><with|math-font-series|bold|font-series|bold|font-size|2|<arg|name>><vspace|2fn><no-page-break><no-indent*>>>>

  <assign|chapter**|<macro|chapname|name|<chapter*|<arg|chapname><vspace|1fn><new-line><arg|name>>>>

  <assign|chapter|<macro|name|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<assign|thechapter|<macro|<chapternr>>><assign|chapternr|<plus|<chapternr>|1>><assign|thelabel|<thechapter>><resetchapter><header-primary|<arg|name>|<thechapter>|<localize|Chapter>><toc-main-2|<thechapter><space|2spc><arg|name>>>||<chapter**|<localize|Chapter>
  <thechapter>|<arg|name>>>>>>

  <assign|appendix|<macro|name|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<assign|thechapter|<macro|<number|<appendixnr>|Alpha>>><assign|appendixnr|<plus|<appendixnr>|1>><assign|thelabel|<thechapter>><resetchapter><header-primary|<arg|name>|<thechapter>|<localize|Appendix>><toc-main-2|<localize|Appendix>
  <thechapter>.<space|2spc><arg|name>>>||<chapter**|<localize|Appendix>
  <thechapter>|<arg|name>>>>>>

  <assign|special-chapter|<macro|name|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<assign|thechapter|<macro|*>><resetchapter><assign|thelabel|<thechapter>><toc-main-2|<arg|name>><header-primary|<arg|name>|*|<arg|name>>>||<chapter*|<arg|name>>>>>>

  <assign|prologue|<macro|<special-chapter|<localize|Prologue>>>>

  <assign|epilogue|<macro|<special-chapter|<localize|Epilogue>>>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|sectionsep|<macro|<space|2spc>>>

  <assign|section*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|3fn><with|math-font-series|bold|font-series|bold|font-size|1.41|<arg|name>><vspace|1fn><no-page-break><no-indent*>>>>

  <assign|section|<macro|name|<style-with|src-compact|none|<assign|sectionnr|<plus|<sectionnr>|1>><resetsection><header-secondary|<arg|name>|<thesection>|<localize|Section>><assign|thelabel|<thesection>><toc-normal-1|<thelabel><space|2spc><arg|name>><section*|<thesection><sectionsep><arg|name>>>>>

  <assign|subsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|2fn><with|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsection|<macro|name|<style-with|src-compact|none|<assign|subsectionnr|<plus|<subsectionnr>|1>><resetsubsection><assign|thelabel|<thesubsection>><toc-normal-2|<thelabel><space|2spc><arg|name>><subsection*|<thesubsection><sectionsep><arg|name>>>>>

  <assign|subsubsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsubsection|<macro|name|<style-with|src-compact|none|<assign|subsubsectionnr|<plus|<subsubsectionnr>|1>><resetsubsubsection><assign|thelabel|<thesubsubsection>><toc-normal-3|<thelabel><space|2spc><arg|name>><subsubsection*|<thesubsubsection><sectionsep><arg|name>>>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><style-with|src-compact|all|<with|math-font-series|bold|font-series|bold|<arg|name>>
  >>>>

  <assign|paragraph|<macro|name|<toc-small-1|<arg|name>><paragraph*|<arg|name>>>>

  <assign|subparagraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.25fn><style-with|src-compact|all|<with|math-font-series|bold|font-series|bold|<arg|name>>
  >>>>

  <assign|subparagraph|<macro|name|<toc-small-2|<arg|name>><subparagraph*|<arg|name>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>