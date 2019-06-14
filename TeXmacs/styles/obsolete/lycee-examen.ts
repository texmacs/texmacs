<TeXmacs|1.99.9>

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
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
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

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.25fn><arg|name>>>>>

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
    <\padded>
      <\indent-left|3fn>
        <surround|<no-page-break*><with|font-series|bold|<aligned-item|<arg|which>>>|<no-indent*>|<arg|body>>
      </indent-left>
    </padded>
  </macro>>

  <\active*>
    <\src-comment>
      Other
    </src-comment>
  </active*>

  <assign|strong|<value|uncolored-strong>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>