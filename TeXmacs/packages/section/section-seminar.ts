<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|section-seminar|1.0|section-latex|1.0>

    <\src-purpose>
      Sectional markup for the seminar style.
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

  <use-package|section-automatic-short>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter*|<macro|name|<style-with|src-compact|none|<vspace*|2fn><with|par-mode|center|math-font-series|bold|font-series|bold|font-size|2|font-shape|long|<arg|name>><vspace|1fn>>>>

  <assign|chapter**|<macro|chapname|name|<chapter*|<arg|chapname>.<space|2spc><arg|name>>>>

  <assign|chapter|<macro|name|<toc-main-2|<arg|name>><chapter*|<arg|name>>>>

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|section*|<macro|name|<style-with|src-compact|none|<vspace*|1.5fn><with|par-mode|center|math-font-series|bold|font-series|bold|font-size|1.41|color|red|<arg|name>><vspace|0.75fn>>>>

  <assign|section|<macro|name|<toc-normal-1|<arg|name>><section*|<arg|name>>>>

  <assign|subsection*|<macro|name|<style-with|src-compact|none|<vspace*|1fn><with|math-font-series|bold|font-series|bold|font-size|1.19|color|red|<arg|name>><vspace|0.5fn>>>>

  <assign|subsection|<macro|name|<toc-normal-2|<arg|name>><subsection*|<arg|name>>>>

  <assign|subsubsection*|<macro|name|<style-with|src-compact|none|<vspace*|0.5fn><with|math-font-series|bold|font-series|bold|color|red|<arg|name>><vspace|0.25fn>>>>

  <assign|subsubsection|<macro|name|<toc-normal-3|<arg|name>><subsubsection*|<arg|name>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
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