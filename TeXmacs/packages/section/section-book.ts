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
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
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

  <assign|chapter|<macro|name|<style-with|src-compact|none|<assign|the-chapter|<macro|<chapter-nr>>><assign|chapter-nr|<plus|<chapter-nr>|1>><assign|the-label|<the-chapter>><resetchapter><header-primary|<arg|name>|<the-chapter>|<localize|Chapter>><toc-main-2|<the-chapter><space|2spc><arg|name>><chapter**|<localize|Chapter>
  <the-chapter>|<arg|name>>>>>

  <assign|appendix|<macro|name|<style-with|src-compact|none|<assign|the-chapter|<macro|<number|<appendix-nr>|Alpha>>><assign|appendix-nr|<plus|<appendix-nr>|1>><assign|the-label|<the-chapter>><resetchapter><header-primary|<arg|name>|<the-chapter>|<localize|Appendix>><toc-main-2|<localize|Appendix>
  <the-chapter>.<space|2spc><arg|name>><chapter**|<localize|Appendix>
  <the-chapter>|<arg|name>>>>>

  <assign|special-chapter|<macro|name|<style-with|src-compact|none|<assign|the-chapter|<macro|*>><resetchapter><assign|the-label|<the-chapter>><toc-main-2|<arg|name>><header-primary|<arg|name>|*|<arg|name>><chapter*|<arg|name>>>>>

  <assign|prologue|<macro|<special-chapter|<localize|Prologue>>>>

  <assign|epilogue|<macro|<special-chapter|<localize|Epilogue>>>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|section-sep|<macro|<space|2spc>>>

  <assign|section*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|3fn><with|math-font-series|bold|font-series|bold|font-size|1.41|<arg|name>><vspace|1fn><no-page-break><no-indent*>>>>

  <assign|section|<macro|name|<style-with|src-compact|none|<assign|section-nr|<plus|<section-nr>|1>><resetsection><header-secondary|<arg|name>|<the-section>|<localize|Section>><assign|the-label|<the-section>><toc-normal-1|<the-label><space|2spc><arg|name>><section*|<the-section><section-sep><arg|name>>>>>

  <assign|subsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|2fn><with|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsection|<macro|name|<style-with|src-compact|none|<assign|subsection-nr|<plus|<subsection-nr>|1>><resetsubsection><assign|the-label|<the-subsection>><toc-normal-2|<the-label><space|2spc><arg|name>><subsection*|<the-subsection><section-sep><arg|name>>>>>

  <assign|subsubsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsubsection|<macro|name|<style-with|src-compact|none|<assign|subsubsection-nr|<plus|<subsubsection-nr>|1>><resetsubsubsection><assign|the-label|<the-subsubsection>><toc-normal-3|<the-label><space|2spc><arg|name>><subsubsection*|<the-subsubsection><section-sep><arg|name>>>>>

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