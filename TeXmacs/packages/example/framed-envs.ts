<TeXmacs|1.0.7.5>

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

  <assign|unframed-chapter-title|<value|chapter-title>>

  <assign|unframed-section-title|<value|section-title>>

  <assign|unframed-subsection-title|<value|subsection-title>>

  <assign|unframed-subsubsection-title|<value|subsubsection-title>>

  <assign|chapter-title|<macro|x|<unframed-chapter-title|<wide-std-framed-colored|dark
  grey|pastel red|>>>>

  <assign|section-title|<macro|x|<unframed-section-title|<wide-std-framed-colored|dark
  grey|pastel red|<arg|x>>>>>

  <assign|subsection-title|<macro|x|<unframed-subsection-title|<wide-std-framed-colored|dark
  grey|pastel orange|<arg|x>>>>>

  <assign|subsubsection-title|<macro|x|<unframed-subsubsection-title|<wide-std-framed-colored|dark
  grey|pastel orange|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Framed theorems
    </src-comment>
  </active*>

  <assign|unframed-render-enunciation|<value|render-enunciation>>

  <assign|render-enunciation|<\macro|which|body>
    <\padded-normal|1fn|1fn>
      <\wide-std-framed-colored|dark blue|pastel yellow>
        <\unframed-render-remark|<arg|which>>
          <arg|body>
        </unframed-render-remark>
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