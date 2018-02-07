<TeXmacs|1.99.6>

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

  <\active*>
    <\src-comment>
      Style parameters
    </src-comment>
  </active*>

  <assign|enunciation-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|enunciation-sep|<macro|. >>

  <assign|theorem-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|theorem-sep|<macro|<enunciation-sep>>>

  <assign|remark-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|remark-sep|<macro|<enunciation-sep>>>

  <assign|exercise-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|exercise-sep|<macro|<enunciation-sep>>>

  <assign|exercise-indentation|1tab>

  \;

  <drd-props|enunciation-sep|macro-parameter|string>

  <drd-props|theorem-sep|macro-parameter|string>

  <drd-props|remark-sep|macro-parameter|string>

  <drd-props|exercise-sep|macro-parameter|string>

  <\active*>
    <\src-comment>
      Rendering of theorem-like environments and exercises.
    </src-comment>
  </active*>

  <assign|render-enunciation|<\macro|which|body>
    <\padded*>
      <surround|<arg|which>|<yes-indent*>|<arg|body>>
    </padded*>
  </macro>>

  <assign|render-remark|<\macro|which|body>
    <render-enunciation|<remark-name|<arg|which><remark-sep>>|<arg|body>>
  </macro>>

  <assign|render-theorem|<\macro|which|body>
    <render-enunciation|<theorem-name|<arg|which><theorem-sep>>|<with|font-shape|italic|<arg|body>>>
  </macro>>

  <assign|render-exercise|<\macro|which|body>
    <\padded>
      <\indent-left|<value|exercise-indentation>>
        <\small>
          <surround|<exercise-name|<arg|which><exercise-sep>>|<yes-indent*>|<arg|body>>
        </small>
      </indent-left>
    </padded>
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

  <new-remark|acknowledgments|Acknowledgments>

  \;

  <new-exercise|exercise|Exercise>

  <new-exercise|problem|Problem>

  <new-theorem|question|Question>

  <\active*>
    <\src-comment>
      Further markup related to theorems.
    </src-comment>
  </active*>

  <assign|qed|<active*|<with|mode|math|\<box\>>>>

  <assign|proof-text|<macro|<localize|Proof>>>

  <assign|solution-text|<macro|<localize|Solution>>>

  <assign|answer-text|<macro|<localize|Answer>>>

  <drd-props|qed|macro-parameter|regular>

  <drd-props|proof-qed|macro-parameter|regular>

  \;

  <assign|dueto|<macro|name|<with|font-shape|right|<theorem-name|(<arg|name>)
  >>>>

  <assign|render-proof|<\macro|which|body>
    <\surround||<htab|0.5fn|0><qed>>
      <\render-remark|<arg|which>>
        <arg|body>
      </render-remark>
    </surround>
  </macro>>

  <assign|proof|<\macro|body>
    <render-proof|<proof-text>|<arg|body>>
  </macro>>

  <assign|proof-of|<\macro|what|body>
    <render-proof|<proof-text> <arg|what>|<arg|body>>
  </macro>>

  <assign|proof*|<\macro|body>
    <render-remark|<proof-text>|<arg|body>>
  </macro>>

  <assign|proof-of*|<\macro|what|body>
    <render-remark|<proof-text> <arg|what>|<arg|body>>
  </macro>>

  \;

  <assign|render-solution|<\macro|which|body>
    <\render-exercise|<arg|which>>
      <\with|par-left|0em>
        <arg|body>
      </with>
    </render-exercise>
  </macro>>

  <assign|solution|<\macro|body>
    <render-solution|<solution-text>|<arg|body>>
  </macro>>

  <assign|solution-of|<\macro|what|body>
    <render-solution|<solution-text> <arg|what>|<arg|body>>
  </macro>>

  \;

  <assign|render-answer|<value|render-remark>>

  <assign|answer|<\macro|body>
    <render-answer|<answer-text>|<arg|body>>
  </macro>>

  <assign|answer-to|<\macro|what|body>
    <answer-answer|<answer-text> <arg|what>|<arg|body>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>