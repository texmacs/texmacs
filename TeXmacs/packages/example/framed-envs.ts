<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|framed-envs|1.0>

    <\src-purpose>
      An example style package for fancy frames around environments.
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
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

  <assign|unframed-render-remark|<value|render-remark>>

  <assign|render-remark|<\macro|which|body>
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