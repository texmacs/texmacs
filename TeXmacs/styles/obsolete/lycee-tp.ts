<TeXmacs|1.99.9>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <\src-style-file|lycee-tp>
        1.0
      </src-style-file>

      <\src-purpose>
        The lycee-tp style.
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

  <use-package|exam|doc|alt-colors>

  <assign|par-first|0fn>

  <assign|language|french>

  <assign|prog-scripts|maxima>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|title*|<macro|body|<with|math-font-series|bold|font-series|bold|font-size|1.30|<arg|body>>>>

  <assign|titre|<\macro|l|body|r>
    <\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
      <title*|<arg|l><htab|5mm><acronym|TP> :
      <with|font-shape|small-caps|<arg|body>><htab|5mm><arg|r>>

      <hrule>
    </surround>
  </macro>>

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
      Other.
    </src-comment>
  </active*>

  <assign|item-strong|<macro|name|<arg|name>>>

  <assign|hdots|<macro|<datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.1fn>.<space|0.1fn>>>>>|<htab|5mm>>>>

  <assign|fillout|<macro|x|<superpose|<with|color|grey|....................>|<arg|x>>>>

  <assign|strong|<value|uncolored-strong>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>