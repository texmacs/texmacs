<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <\src-style-file|lycee-examen>
        1.0
      </src-style-file>

      <\src-purpose>
        The lycee-examen style.
      </src-purpose>

      <\src-copyright|2002--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you do not have a copy of the license, then write to
        the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
        Boston, MA 02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|exam|number-long-article|alt-colors>

  <\active*>
    <\src-comment>
      Subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|part-display-numbers|<macro|true>>

  <assign|chapter-display-numbers|<macro|true>>

  <assign|section-display-numbers|<macro|true>>

  <assign|subsection-display-numbers|<macro|true>>

  <assign|subsubsection-display-numbers|<macro|true>>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><very-large|<arg|name>><vspace|0.5fn>>>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><larger|<arg|name>><vspace|0.5fn>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><large|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><sharp-size|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><arg|name><vspace|0.5fn>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name><paragraph-sep>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.25fn><arg|name><subparagraph-sep>>>>>

  <\active*>
    <\src-comment>
      Exercises
    </src-comment>
  </active*>

  <assign|exercise-numbered|<macro|text|num|<arg|num>)>>

  <assign|exercise-unnumbered|<macro|text|>>

  <assign|problem-numbered|<macro|text|num|<arg|num>)>>

  <assign|problem-unnumbered|<macro|text|>>

  <assign|render-exercise|<\macro|which|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-left|3fn>
        <surround|<no-page-break*><with|font-series|bold|<aligned-item|<arg|which>>>|<no-indent*>|<arg|body>>
      </indent-left>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>