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
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|section-article>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|sectionsep|<macro|.<space|2spc>>>

  <assign|section*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><htab|0fn><with|font-shape|small-caps|<arg|name>><htab|0fn><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|section|<macro|name|<style-with|src-compact|none|<assign|thesection|<macro|<sectionnr>>><assign|sectionnr|<plus|<value|sectionnr>|1>><resetsection><assign|thelabel|<thesection>><header-primary|<arg|name>|<thesection>|<localize|Section>><toc-main-2|<thesection>.
  <arg|name>><section*|<thesection><sectionsep><arg|name>>>>>

  <assign|appendix|<macro|name|<style-with|src-compact|none|<assign|thesection|<macro|<number|<appendixnr>|Alpha>>><assign|appendixnr|<plus|<value|appendixnr>|1>><resetsection><new-page><new-line><no-indent><vspace*|3fn><header-primary|<arg|name>|<thesection>|<localize|Appendix>><assign|thelabel|<thesection>><toc-main-2|<localize|Appendix><thesection>.
  <arg|name>><with|math-font-series|bold|font-series|bold|font-size|1.41|<localize|Appendix><space|2spc><thesection>.
  <arg|name>><vspace|2fn><no-page-break><no-indent*>>>>

  <\active*>
    <\src-comment>
      Subections and subsubsections.
    </src-comment>
  </active*>

  <assign|subsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

  <assign|subsection|<macro|name|<style-with|src-compact|none|<assign|subsectionnr|<plus|<value|subsectionnr>|1>><resetsubsection><assign|thelabel|<thesubsection>><header-secondary|<arg|name>|<thesubsection>|<localize|Section>><toc-normal-1|<thesubsection>.
  <arg|name>><thesubsection><sectionsep><subsection*|<arg|name>>>>>

  <assign|subsubsection*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

  <assign|subsubsection|<macro|name|<style-with|src-compact|none|<assign|subsubsectionnr|<plus|<value|subsubsectionnr>|1>><resetsubsubsection><assign|thelabel|<thesubsubsection>><toc-normal-2|<thesubsubsection>.
  <arg|name>><thesubsubsection><sectionsep><subsubsection*|<arg|name>>>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
  ><no-page-break><no-indent*>>>>

  <assign|subparagraph*|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name>.
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