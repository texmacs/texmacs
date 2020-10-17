<TeXmacs|1.99.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-enunciation|1.0>

    <\src-purpose>
      Rendering of theorem-like environments.
    </src-purpose>

    <src-copyright|1998--2018|Joris van der Hoeven>

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

  <assign|enunciation-name|<macro|name|<strong|<arg|name>>>>

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
      Further markup related to theorems.
    </src-comment>
  </active*>

  <assign|dueto|<macro|name|<with|font-shape|right|<theorem-name|(<arg|name>)
  >>>>

  <assign|qed|<macro|<math-ignore|<active*|<with|mode|math|\<box\>>>>>>

  <assign|tab-qed|<macro|<htab|0.5fn|0><qed>>>

  <assign|tmhtml-tab-qed|<macro|<space|1em><qed>>>

  \;

  <assign|proof-text|<macro|<localize|Proof>>>

  <assign|solution-text|<macro|<localize|Solution>>>

  <assign|answer-text|<macro|<localize|Answer>>>

  \;

  <assign|render-proof|<\macro|which|body>
    <\surround||<if|<occurs-inside|<quote|<qed>>|body>||<tab-qed>>>
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

  \;

  <assign|render-solution|<\macro|which|body>
    <\render-exercise|<arg|which>>
      <\with|par-left|0em>
        <arg|body>
      </with>
    </render-exercise>
  </macro>>

  <assign|solution-old|<\macro|body>
    <render-solution|<solution-text>|<arg|body>>
  </macro>>

  <assign|solution-of|<\macro|what|body>
    <render-solution|<solution-text> <arg|what>|<arg|body>>
  </macro>>

  \;

  <assign|render-answer|<value|render-remark>>

  <assign|answer-old|<\macro|body>
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