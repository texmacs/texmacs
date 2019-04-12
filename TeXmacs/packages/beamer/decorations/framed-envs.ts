<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|framed-envs|1.0>

    <\src-purpose>
      An example style package for fancy frames around environments.
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
      Framed sections
    </src-comment>
  </active*>

  <provide|unframed-chapter-title|<value|chapter-title>>

  <provide|unframed-section-title|<value|section-title>>

  <provide|unframed-subsection-title|<value|subsection-title>>

  <provide|unframed-subsubsection-title|<value|subsubsection-title>>

  <assign|chapter-title|<macro|title|<unframed-chapter-title|<wide-std-framed-colored|dark
  grey|pastel red|<arg|title>>>>>

  <assign|section-title|<macro|title|<unframed-section-title|<wide-std-framed-colored|dark
  grey|pastel red|<arg|title>>>>>

  <assign|subsection-title|<macro|title|<unframed-subsection-title|<wide-std-framed-colored|dark
  grey|pastel orange|<arg|title>>>>>

  <assign|subsubsection-title|<macro|title|<unframed-subsubsection-title|<wide-std-framed-colored|dark
  grey|pastel orange|<arg|title>>>>>

  <\active*>
    <\src-comment>
      Framed theorems
    </src-comment>
  </active*>

  <provide|unframed-render-enunciation|<value|render-enunciation>>

  <assign|render-enunciation|<\macro|which|body>
    <\padded-normal|1fn|1fn>
      <\wide-std-framed-colored|dark blue|pastel yellow>
        <\unframed-render-enunciation|<arg|which>>
          <arg|body>
        </unframed-render-enunciation>
      </wide-std-framed-colored>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>