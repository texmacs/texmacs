<TeXmacs|1.0.6.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-theorem|1.0>

    <\src-purpose>
      Theorem-like environments.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|theorem-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|theorem-sep|<macro|. >>

  <assign|exercise-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|exercise-sep|<macro|. >>

  <\active*>
    <\src-comment>
      Rendering of theorem-like environments and exercises.
    </src-comment>
  </active*>

  <assign|render-remark|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<theorem-name|<arg|which><theorem-sep>>||<arg|body>>>
  </macro>>

  <assign|render-theorem|<\macro|which|body>
    <render-remark|<arg|which>|<with|font-shape|italic|<arg|body>>>
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