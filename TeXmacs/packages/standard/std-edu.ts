<TeXmacs|1.99.8>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-edu|1.0>

    <\src-purpose>
      Standard educational macros.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Same rendering for exercise, problem and question
    </src-comment>
  </active*>

  <assign|render-exercise|<\macro|which|body>
    <\padded*>
      <surround|<exercise-name|<arg|which><exercise-sep>>||<arg|body>>
    </padded*>
  </macro>>

  <\active*>
    <\src-comment>
      Folding
    </src-comment>
  </active*>

  <assign|folded|<\macro|x|y>
    <\surround||<right-flush><action|<with|color|#336666|<strong|<math|<op|\<Downarrow\>>>>>|mouse-unfold|<arg|x>>>
      <arg|x>
    </surround>

    <hidden|<arg|y>>
  </macro>>

  <assign|unfolded|<\macro|x|y>
    <surround||<right-flush><action|<with|color|#336666|<strong|<math|<op|\<Uparrow\>>>>>|mouse-fold|<arg|x>>|<arg|x>>

    <arg|y>
  </macro>>

  <\active*>
    <\src-comment>
      Buttons
    </src-comment>
  </active*>

  <assign|box-off|<xmacro|x|<with|locus-color|preserve|<action|\<box\>|mouse-unfold|<arg|x>>>>>

  <assign|box-on|<xmacro|x|<with|locus-color|preserve|<action|\<blacksquare\>|mouse-fold|<arg|x>>>>>

  <assign|cross-off|<xmacro|x|<with|locus-color|preserve|<action|\<box\>|mouse-unfold|<arg|x>>>>>

  <assign|cross-on|<xmacro|x|<with|locus-color|preserve|<action|\<boxtimes\>|mouse-fold|<arg|x>>>>>

  <assign|circle-off|<xmacro|x|<with|locus-color|preserve|<action|\<#25CB\>|mouse-unfold|<arg|x>>>>>

  <assign|circle-on|<xmacro|x|<with|locus-color|preserve|<action|\<#25CF\>|mouse-fold|<arg|x>>>>>

  <drd-props|box-off|arity|0>

  <drd-props|box-on|arity|0>

  <drd-props|cross-off|arity|0>

  <drd-props|cross-on|arity|0>

  <drd-props|circle-off|arity|0>

  <drd-props|circle-on|arity|0>

  \;

  <assign|button-nr|0>

  <assign|inc-button-nr|<macro|<assign|button-nr|<plus|<value|button-nr>|1>>>>

  <assign|numeric-sep|>

  <assign|numeric-off|<xmacro|x|<inc-button-nr><with|locus-color|preserve|<action|<with|ornament-shape|rounded|ornament-border|0ln|<resize|<ornament|<value|button-nr>><shift|<value|numeric-sep>|<minus|<value|ornament-hpadding>>|>|<plus|1r|-1tab>|||>>|mouse-unfold|<arg|x>>>>>

  <assign|numeric-on|<xmacro|x|<inc-button-nr><with|locus-color|preserve|<action|<with|ornament-shape|rounded|ornament-border|1ln|<resize|<ornament|<value|button-nr>><shift|<value|numeric-sep>|<minus|<value|ornament-hpadding>>|>|<plus|1r|-1tab>|||>>|mouse-fold|<arg|x>>>>>

  <assign|alpha-sep|>

  <assign|alpha-off|<xmacro|x|<inc-button-nr><with|locus-color|preserve|<action|<with|ornament-shape|rounded|ornament-border|0ln|<resize|<ornament|<with|font-shape|italic|<number|<value|button-nr>|alpha>>><shift|<value|numeric-sep>|<minus|<value|ornament-hpadding>>|>|<plus|1r|-1tab>|||>>|mouse-unfold|<arg|x>>>>>

  <assign|alpha-on|<xmacro|x|<inc-button-nr><with|locus-color|preserve|<action|<with|ornament-shape|rounded|ornament-border|1ln|<resize|<ornament|<with|font-shape|italic|<number|<value|button-nr>|alpha>>><shift|<value|numeric-sep>|<minus|<value|ornament-hpadding>>|>|<plus|1r|-1tab>|||>>|mouse-fold|<arg|x>>>>>

  <drd-props|numeric-off|arity|0>

  <drd-props|numeric-on|arity|0>

  <drd-props|alpha-off|arity|0>

  <drd-props|alpha-on|arity|0>

  <\active*>
    <\src-comment>
      Multiple choice environments
    </src-comment>
  </active*>

  <assign|mc-item|<macro|but|text|<arg|but> <arg|text>>>

  <assign|mc-wide-item|<macro|but|text|<with|par-left|<plus|<value|par-left>|<value|item-hsep>>|<surround|<with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|but>|<minus|1r|<minus|<item-hsep>|0.5fn>>||<plus|1r|0.5fn>|>||<arg|text>>>>>

  <drd-props|mc-item|arity|2|accessible|all|border|no>

  \;

  <assign|mc-basic-sep|2em>

  <assign|mc-basic-one|<macro|a|<hgroup|<arg|a>><space|<value|mc-basic-sep>>
  >>

  <assign|mc-basic|<xmacro|args|<with|dummy|<value|mc-basic-sep>|button-nr|0|<map-args|mc-basic-one|concat|args>>>>

  <assign|mcs-basic|<xmacro|args|<with|dummy|<value|mc-basic-sep>|button-nr|0|<map-args|mc-basic-one|concat|args>>>>

  <drd-props|mc-basic|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs-basic|arity|<tuple|repeat|1|1>|accessible|all>

  \;

  <assign|mc-std-cols|5>

  <assign|mc-std-one|<macro|a|nr|<resize|<arg|a>|||<over|0.9999par|<value|mc-std-cols>>|><if|<equal|<mod|<plus|<arg|nr>|1>|<value|mc-std-cols>>|0>|<line-break>>>>

  <assign|mc-std|<xmacro|args|<with|dummy|<value|mc-std-cols>|button-nr|0|<map-args|mc-std-one|concat|args>>>>

  <assign|mcs-std|<xmacro|args|<with|dummy|<value|mc-std-cols>|button-nr|0|<map-args|mc-std-one|concat|args>>>>

  <drd-props|mc-std|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs-std|arity|<tuple|repeat|1|1>|accessible|all>

  \;

  <assign|mc-list-one|<macro|a|<with|mc-item|<value|mc-wide-item>|<arg|a>>>>

  <assign|mc-list|<xmacro|args|<surround||<right-flush>|<with|button-nr|0|<map-args|mc-list-one|document|args>>>>>

  <assign|mcs-list|<xmacro|args|<surround||<right-flush>|<with|button-nr|0|<map-args|mc-list-one|document|args>>>>>

  <drd-props|mc-list|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs-list|arity|<tuple|repeat|1|1>|accessible|all>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|src-special|normal>
  </collection>
</initial>