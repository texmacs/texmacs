<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-acmconf|1.0>

    <\src-purpose>
      Sectional markup for the acmconf style.
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

  <use-package|section-article>

  <assign|section-article-2col-package|1.0>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter*|<macro|name|<style-with|src-compact|none|<vspace*|3fn><with|par-mode|center|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|1fn><no-page-break>>>>

  <assign|section*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1.5fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|1.5fn><no-page-break><no-indent*>>>>

  <assign|section|<macro|name|<style-with|src-compact|none|<assign|thesection|<macro|<sectionnr>>><assign|sectionnr|<plus|<value|sectionnr>|1>><resetsection><assign|thelabel|<thesection>><header-primary|<arg|name>|<thesection>|<localize|Section>><toc-main-2|<thelabel><space|2spc><arg|name>><section*|<thesection><sectionsep><arg|name>>>>>

  <assign|appendix|<macro|name|<style-with|src-compact|none|<assign|thesection|<macro|<number|<appendixnr>|Alpha>>><assign|appendixnr|<plus|<value|appendixnr>|1>><resetsection><assign|thelabel|<thesection>><header-primary|<arg|name>|<thesection>|<localize|Appendix>><toc-main-2|<localize|Appendix>
  <thesection>.<space|2spc><arg|name>><section*|<localize|Appendix>
  <thesection>.<space|2spc><arg|name>>>>>

  <\active*>
    <\src-comment>
      Subsections, subsubsections, paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|subsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1.5fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.75fn><no-page-break><no-indent*>>>>

  <assign|subsubsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|paragraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><style-with|src-compact|all|<with|math-font-series|bold|font-series|bold|<arg|name>>
  >>>>

  <assign|subparagraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.25fn><style-with|src-compact|all|<with|math-font-series|bold|font-series|bold|<arg|name>>
  >>>>

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