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
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
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
      Parts and chapters.
    </src-comment>
  </active*>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|3fn><really-huge|<arg|name>><vspace|1.5fn>>>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><huge|<arg|name>><vspace|1fn>>>>>

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><very-large|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><large|<arg|name>><vspace|0.3333fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|0.75fn><arg|name><vspace|0.25fn>>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name><paragraph-sep>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.25fn><arg|name><subparagraph-sep>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>