<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-book|1.0>

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

  <use-package|section-base>

  <assign|sectional-short-style|<macro|false>>

  <assign|chapter-clean|<macro|<reset-section><reset-std-env>>>

  <assign|display-std-env|<macro|nr|<chapter-prefix><arg|nr>>>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<new-page*><no-indent><new-line><no-indent><vspace*|5fn><with|math-font-series|bold|font-series|bold|font-size|2|<arg|name>><vspace|2fn><no-page-break><no-indent*>>>>

  <assign|chapter-numbered-title|<macro|title|<style-with|src-compact|none|<chapter-title|<localize|Chapter>
  <the-chapter><vspace|1fn><new-line><arg|title>>>>>

  <assign|appendix-numbered-title|<macro|title|<style-with|src-compact|none|<chapter-title|<localize|Appendix>
  <the-appendix><vspace|1fn><new-line><arg|title>>>>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|3fn><with|math-font-series|bold|font-series|bold|font-size|1.41|<arg|name>><vspace|1fn><no-page-break><no-indent*>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|2fn><with|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><style-with|src-compact|all|<with|math-font-series|bold|font-series|bold|<arg|name>>
  >>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.25fn><style-with|src-compact|all|<with|math-font-series|bold|font-series|bold|<arg|name>>
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