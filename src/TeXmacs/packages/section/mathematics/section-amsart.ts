<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-amsart|1.0>

    <\src-purpose>
      Sectional markup for the acmconf style.
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

  <use-package|section-article>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><htab|0fn><with|font-shape|small-caps|<arg|name>><htab|0fn><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|var-section-title|<macro|name|<style-with|src-compact|none|<new-page><new-line><no-indent><vspace*|3fn><with|math-font-series|bold|font-series|bold|font-size|1.41|<arg|name>><vspace|2fn><no-page-break><no-indent*>>>>

  <assign|appendix-title|<macro|name|<style-with|src-compact|none|<var-section-title|<localize|Appendix>.
  <arg|name>>>>>

  <assign|appendix-numbered-title|<macro|name|<style-with|src-compact|none|<var-section-title|<localize|Appendix>
  <the-appendix>. <arg|name>>>>>

  <\active*>
    <\src-comment>
      Subections and subsubsections.
    </src-comment>
  </active*>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

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