<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-generic|1.0>

    <\src-purpose>
      Sectional markup for the generic style.
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
      Don't display any numbers.
    </src-comment>
  </active*>

  <assign|part-display-numbers|<macro|false>>

  <assign|chapter-display-numbers|<macro|false>>

  <assign|section-display-numbers|<macro|false>>

  <assign|subsection-display-numbers|<macro|false>>

  <assign|subsubsection-display-numbers|<macro|false>>

  <assign|paragraph-display-numbers|<macro|false>>

  <assign|subparagraph-display-numbers|<macro|false>>

  <assign|appendix-display-numbers|<macro|false>>

  <\active*>
    <\src-comment>
      Parts.
    </src-comment>
  </active*>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<vspace*|3fn><with|par-mode|center|math-font-series|bold|font-series|bold|font-size|2|<arg|name>><vspace|1fn><no-page-break>>>>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<vspace*|2fn><with|par-mode|center|math-font-series|bold|font-series|bold|font-size|1.68|<arg|name>><vspace|1fn><no-page-break>>>>

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<vspace*|1.5fn><with|math-font-series|bold|font-series|bold|font-size|1.41|<arg|name>><vspace|0.5fn><no-page-break>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<vspace*|1fn><with|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|0.3333fn><no-page-break>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<vspace*|0.75fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.25fn><no-page-break>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<vspace*|0.5fn><with|math-font-series|bold|font-series|bold|<arg|name><paragraph-sep>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<vspace*|0.25fn><with|math-font-series|bold|font-series|bold|<arg|name><subparagraph-sep>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>