<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-jsc|1.0>

    <\src-purpose>
      Sectional markup for the jsc style.
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

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|sectionsep|<macro|.<space|2spc>>>

  <assign|section*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><htab|0fn><with|font-series|bold|math-font-series|bold|<arg|name>><htab|0fn><vspace|1fn><no-page-break>>>>

  <assign|subsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><htab|0fn><with|font-shape|small-caps|<arg|name>><htab|0fn><vspace|1fn><no-page-break>>>>

  <assign|subsubsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|font-shape|small-caps|<arg|name>><vspace|1fn><no-page-break>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|font-shape|small-caps|<arg|name>.
  ><no-page-break>>>>

  <assign|subparagraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|font-shape|small-caps|<arg|name>.
  ><no-page-break>>>>

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