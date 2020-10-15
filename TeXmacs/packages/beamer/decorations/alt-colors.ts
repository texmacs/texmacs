<TeXmacs|1.99.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|alt-colors|1.0>

    <\src-purpose>
      An example style package for fancy colors on slides.
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Titles in dark cyan
    </src-comment>
  </active*>

  <assign|heading-color|#006060>

  <provide|uncolored-doc-title-name|<value|doc-title-name>>

  <provide|uncolored-part-title|<value|part-title>>

  <provide|uncolored-chapter-title|<value|chapter-title>>

  <provide|uncolored-section-title|<value|section-title>>

  <provide|uncolored-subsection-title|<value|subsection-title>>

  <provide|uncolored-subsubsection-title|<value|subsubsection-title>>

  <provide|uncolored-paragraph-title|<value|paragraph-title>>

  <provide|uncolored-subparagraph-title|<value|subparagraph-title>>

  <assign|with-heading|<macro|x|<with|color|<value|heading-color>|math-color|<value|heading-color>|strong-color|<value|heading-color>|locus-color|<value|heading-color>|visited-color|<value|heading-color>|<arg|x>>>>

  <assign|doc-title-name|<macro|x|<with-heading|<uncolored-doc-title-name|<arg|x>>>>>

  <assign|part-title|<macro|x|<with-heading|<uncolored-part-title|<arg|x>>>>>

  <assign|chapter-title|<macro|x|<with-heading|<uncolored-chapter-title|<arg|x>>>>>

  <assign|section-title|<macro|x|<with-heading|<uncolored-section-title|<arg|x>>>>>

  <assign|subsection-title|<macro|x|<with-heading|<uncolored-subsection-title|<arg|x>>>>>

  <assign|subsubsection-title|<macro|x|<with-heading|<uncolored-subsubsection-title|<arg|x>>>>>

  <assign|paragraph-title|<macro|x|<with-heading|<uncolored-paragraph-title|<arg|x>>>>>

  <assign|subparagraph-title|<macro|x|<with-heading|<uncolored-subparagraph-title|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Mathematics in dark red.
    </src-comment>
  </active*>

  <assign|math-color|dark red>

  <assign|math-colored|<macro|x|<with|color|<value|math-color>|font-family|rm|<arg|x>>>>

  <provide|uncolored-math|<value|math>>

  <provide|uncolored-equation*|<value|equation*>>

  <provide|uncolored-equations-base|<value|equations-base>>

  <assign|math|<macro|x|<math-colored|<uncolored-math|<arg|x>>>>>

  <assign|equation*|<\macro|x>
    <math-colored|<\uncolored-equation*>
      <arg|x>
    </uncolored-equation*>>
  </macro>>

  <assign|equations-base|<\macro|x>
    <math-colored|<\uncolored-equations-base>
      <arg|x>
    </uncolored-equations-base>>
  </macro>>

  <\active*>
    <\src-comment>
      Theorems, lists and strong text in dark blue.
    </src-comment>
  </active*>

  <assign|strong-color|dark blue>

  <provide|uncolored-enunciation-name|<value|enunciation-name>>

  <provide|uncolored-theorem-name|<value|theorem-name>>

  <provide|uncolored-remark-name|<value|remark-name>>

  <provide|uncolored-exercise-name|<value|exercise-name>>

  <provide|uncolored-render-list|<value|render-list>>

  <provide|uncolored-strong|<value|strong>>

  <assign|enunciation-name|<macro|x|<with|color|<value|strong-color>|<uncolored-enunciation-name|<arg|x>>>>>

  <assign|theorem-name|<macro|x|<with|color|<value|strong-color>|<uncolored-theorem-name|<arg|x>>>>>

  <assign|remark-name|<macro|x|<with|color|<value|strong-color>|<uncolored-remark-name|<arg|x>>>>>

  <assign|exercise-name|<macro|x|<with|color|<value|strong-color>|<uncolored-exercise-name|<arg|x>>>>>

  <assign|render-list|<\macro|body>
    <\uncolored-render-list>
      <\with|uncolored-current-item|<or-value|uncolored-current-item|current-item>|current-item|<macro|name|<with|color|<value|strong-color>|<uncolored-current-item|<arg|name>>>>>
        <arg|body>
      </with>
    </uncolored-render-list>
  </macro>>

  <assign|strong|<macro|x|<with|color|<value|strong-color>|<uncolored-strong|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Extra customizations in poster style
    </src-comment>
  </active*>

  <assign|framed-colored|<\macro|body>
    <\with|color|<value|framed-body-color>|math-color|<value|framed-body-math-color>|strong-color|<value|framed-body-strong-color>>
      <arg|body>
    </with>
  </macro>>

  <assign|alternate-colored|<\macro|body>
    <\with|color|<value|alternate-body-color>|math-color|<value|alternate-body-math-color>|strong-color|<value|alternate-body-strong-color>>
      <arg|body>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Blinking
    </src-comment>
  </active*>

  <assign|blink|<macro|x|<anim-repeat|<anim-compose|<anim-constant|<arg|x>|1sec>|<anim-constant||0.5sec>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>