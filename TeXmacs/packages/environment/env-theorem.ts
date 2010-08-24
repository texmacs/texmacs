<TeXmacs|1.0.7.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-theorem|1.0>

    <\src-purpose>
      Theorem-like environments.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|enunciation-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|enunciation-sep|<macro|. >>

  <assign|theorem-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|theorem-sep|<macro|<enunciation-sep>>>

  <assign|remark-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|remark-sep|<macro|<enunciation-sep>>>

  <assign|exercise-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|exercise-sep|<macro|<enunciation-sep>>>

  <\active*>
    <\src-comment>
      Rendering of theorem-like environments and exercises.
    </src-comment>
  </active*>

  <assign|render-enunciation|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<arg|which>||<arg|body>>>
  </macro>>

  <assign|render-remark|<\macro|which|body>
    <render-enunciation|<remark-name|<arg|which><remark-sep>>|<arg|body>>
  </macro>>

  <assign|render-theorem|<\macro|which|body>
    <render-enunciation|<theorem-name|<arg|which><theorem-sep>>|<with|font-shape|italic|<arg|body>>>
  </macro>>

  <assign|render-exercise|<\macro|which|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-left|1.5fn>
        <\small>
          <surround|<exercise-name|<arg|which><exercise-sep>>||<arg|body>>
        </small>
      </indent-left>
    </padded-normal>
  </macro>>

  <\active*>
    <\src-comment>
      Standard theorems, remarks and exercises.
    </src-comment>
  </active*>

  <new-theorem|theorem|Theorem>

  <new-theorem|proposition|Proposition>

  <new-theorem|lemma|Lemma>

  <new-theorem|corollary|Corollary>

  <new-theorem|axiom|Axiom>

  <new-theorem|definition|Definition>

  <new-theorem|notation|Notation>

  <new-theorem|conjecture|Conjecture>

  \;

  <new-remark|remark|Remark>

  <new-remark|example|Example>

  <new-remark|note|Note>

  <new-remark|warning|Warning>

  <new-remark|convention|Convention>

  \;

  <new-exercise|exercise|Exercise>

  <new-exercise|problem|Problem>

  <\active*>
    <\src-comment>
      Further markup related to theorems.
    </src-comment>
  </active*>

  <assign|proof-text|<macro|<localize|Proof>>>

  <assign|dueto|<macro|name|<with|font-shape|right|<theorem-name|(<arg|name>)
  >>>>

  <assign|render-proof|<\macro|which|body>
    <\render-remark|<arg|which>>
      <\surround||<htab|0.5fn><active*|<with|mode|math|\<box\>>>>
        <arg|body>
      </surround>
    </render-remark>
  </macro>>

  <assign|proof|<\macro|body>
    <render-proof|<proof-text>|<arg|body>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>