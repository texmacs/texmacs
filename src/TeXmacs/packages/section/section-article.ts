<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-article|1.0>

    <\src-purpose>
      Sectional markup for articles.
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

  <use-package|section-base>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<vspace*|3fn><with|par-mode|center|math-font-series|bold|font-series|bold|font-size|1.68|<arg|name>><vspace|1fn><no-page-break>>>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|3fn><with|math-font-series|bold|font-series|bold|font-size|1.41|<arg|name>><vspace|1fn><no-page-break><no-indent*>>>>

  <assign|appendix-title|<macro|title|<style-with|src-compact|none|<section-title|<localize|Appendix><sectional-sep><arg|title>>>>>

  <assign|appendix-numbered-title|<macro|title|<style-with|src-compact|none|<section-title|<localize|Appendix>
  <the-appendix><sectional-sep><arg|title>>>>>

  <\active*>
    <\src-comment>
      Subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|2fn><with|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name><paragraph-sep>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.25fn><with|math-font-series|bold|font-series|bold|<arg|name><subparagraph-sep>>>>>

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