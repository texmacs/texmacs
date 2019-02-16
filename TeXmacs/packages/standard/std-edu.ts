<TeXmacs|1.99.9>

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

  <use-module|(education edu-edit)>

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
      Unnumbered buttons
    </src-comment>
  </active*>

  <assign|render-button|<macro|state|off|on|<with|locus-color|preserve|<action|<if|<arg|state>|<arg|on>|<arg|off>>|mouse-toggle-button|<arg|state>>>>>

  \;

  <assign|button-box-off|\<box\>>

  <assign|button-box-on|\<blacksquare\>>

  <assign|button-box-cross|\<boxtimes\>>

  <assign|button-box|<macro|state|<render-button|<arg|state>|<value|button-box-off>|<value|button-box-on>>>>

  <assign|button-box*|<macro|state|<render-button|<arg|state>|<value|button-box-off>|<value|button-box-cross>>>>

  \;

  <assign|button-circle-off|\<#25CB\>>

  <assign|button-circle-on|\<#25CF\>>

  <assign|button-circle-off*|\<oempty\>>

  <assign|button-circle-cross|\<otimes\>>

  <assign|button-circle|<macro|state|<render-button|<arg|state>|<value|button-circle-off>|<value|button-circle-on>>>>

  <assign|button-circle*|<macro|state|<render-button|<arg|state>|<value|button-circle-off*>|<value|button-circle-cross>>>>

  <\active*>
    <\src-comment>
      Numbered buttons
    </src-comment>
  </active*>

  <assign|button-nr|0>

  <assign|circled-button-padding|0.5spc>

  <assign|render-circled-button|<macro|state|text|sep|<with|ornament-shape|rounded|ornament-color||ornament-border|<if|<arg|state>|1ln|0ln>|ornament-hpadding|<value|circled-button-padding>|ornament-vpadding|<value|circled-button-padding>|locus-color|preserve|<resize|<ornament|<action|<arg|text>|mouse-toggle-button|<arg|state>>><shift|<arg|sep>|<minus|<value|ornament-hpadding>>|>|<plus|1r|-1tab>|||>><assign|button-nr|<plus|<value|button-nr>|1>>>>

  \;

  <assign|button-arabic-sep|>

  <assign|button-alpha-sep|>

  <assign|button-Alpha-sep|>

  <assign|button-roman-sep|>

  <assign|button-Roman-sep|>

  <assign|button-arabic|<macro|state|<render-circled-button|<arg|state>|<number|<plus|<value|button-nr>|1>|arabic>|<value|button-arabic-sep>>>>

  <assign|button-alpha|<macro|state|<render-circled-button|<arg|state>|<with|font-shape|italic|<number|<plus|<value|button-nr>|1>|alpha><value|button-alpha-sep>><resize|<phantom|.>|||0.001em|>|>>>

  <assign|button-Alpha|<macro|state|<render-circled-button|<arg|state>|<with|font-shape|italic|<number|<plus|<value|button-nr>|1>|Alpha><value|button-Alpha-sep>><resize|<phantom|.>|||0.001em|>|>>>

  <assign|button-roman|<macro|state|<render-circled-button|<arg|state>|<number|<plus|<value|button-nr>|1>|roman>|<value|button-roman-sep>>>>

  <assign|button-Roman|<macro|state|<render-circled-button|<arg|state>|<number|<plus|<value|button-nr>|1>|Roman>|<value|button-Roman-sep>>>>

  <\active*>
    <\src-comment>
      Button themes
    </src-comment>
  </active*>

  <assign|button|<value|button-box>>

  <assign|with-button-box|<macro|body|<with|button|<value|button-box>|<arg|body>>>>

  <assign|with-button-box*|<macro|body|<with|button|<value|button-box*>|<arg|body>>>>

  <assign|with-button-circle|<macro|body|<with|button|<value|button-circle>|<arg|body>>>>

  <assign|with-button-circle*|<macro|body|<with|button|<value|button-circle*>|<arg|body>>>>

  <assign|with-button-arabic|<macro|body|<with|button|<value|button-arabic>|<arg|body>>>>

  <assign|with-button-alpha|<macro|body|<with|button|<value|button-alpha>|<arg|body>>>>

  <assign|with-button-Alpha|<macro|body|<with|button|<value|button-Alpha>|<arg|body>>>>

  <assign|with-button-roman|<macro|body|<with|button|<value|button-roman>|<arg|body>>>>

  <assign|with-button-Roman|<macro|body|<with|button|<value|button-Roman>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Multiple choice environments
    </src-comment>
  </active*>

  <assign|mc-field-sep| >

  <assign|mc-field|<macro|state|text|<button|<arg|state>><value|mc-field-sep><arg|text>>>

  <assign|mc-wide-field|<macro|state|text|<with|par-left|<plus|<value|par-left>|<value|item-hsep>>|<surround|<with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<button|<arg|state>>|<minus|1r|<minus|<item-hsep>|0.5fn>>||<plus|1r|0.5fn>|>||<arg|text>>>>>

  <drd-props|mc-field|arity|2|accessible|1|border|no>

  \;

  <assign|mc-monospaced-cols|5>

  <assign|mc-monospaced-one|<macro|a|nr|<resize|<arg|a>|||<over|0.9999par|<value|mc-monospaced-cols>>|><if|<equal|<mod|<plus|<arg|nr>|1>|<value|mc-monospaced-cols>>|0>|<line-break>>>>

  <assign|mc-monospaced|<xmacro|args|<with|dummy|<value|mc-monospaced-cols>|button-nr|0|<map-args|mc-monospaced-one|concat|args>>>>

  <assign|mcs-monospaced|<xmacro|args|<with|dummy|<value|mc-monospaced-cols>|button-nr|0|<map-args|mc-monospaced-one|concat|args>>>>

  <drd-props|mc-monospaced|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs-monospaced|arity|<tuple|repeat|1|1>|accessible|all>

  \;

  <assign|mc-horizontal-sep|2em>

  <assign|mc-horizontal-one|<macro|a|<hgroup|<arg|a>><space|<value|mc-horizontal-sep>>
  >>

  <assign|mc-horizontal|<xmacro|args|<with|dummy|<value|mc-horizontal-sep>|button-nr|0|<map-args|mc-horizontal-one|concat|args>>>>

  <assign|mcs-horizontal|<xmacro|args|<with|dummy|<value|mc-horizontal-sep>|button-nr|0|<map-args|mc-horizontal-one|concat|args>>>>

  <drd-props|mc-horizontal|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs-horizontal|arity|<tuple|repeat|1|1>|accessible|all>

  \;

  <assign|mc-vertical-one|<macro|a|<with|mc-field|<value|mc-wide-field>|<arg|a>>>>

  <assign|mc-vertical|<xmacro|args|<surround||<right-flush>|<with|button-nr|0|<map-args|mc-vertical-one|document|args>>>>>

  <assign|mcs-vertical|<xmacro|args|<surround||<right-flush>|<with|button-nr|0|<map-args|mc-vertical-one|document|args>>>>>

  <drd-props|mc-vertical|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs-vertical|arity|<tuple|repeat|1|1>|accessible|all>

  \;

  <assign|mc|<value|mc-monospaced>>

  <assign|mcs|<value|mcs-monospaced>>

  <drd-props|mc|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|mcs|arity|<tuple|repeat|1|1>|accessible|all>

  <\active*>
    <\src-comment>
      Possibly hidden text in gaps
    </src-comment>
  </active*>

  <assign|gap-font|>

  <assign|gap-font-size|1>

  <assign|render-gap-text|<macro|body|<with|font|<if|<unequal|<value|gap-font>|>|<value|gap-font>|<value|font>>|font-size|<value|gap-font-size>|<arg|body>>>>

  \;

  <assign|hide-reply|<macro|user|answer|<superpose|<phantom|<arg|answer>>|<arg|user>>>>

  <assign|hide-reply|<macro|user|answer|<extend|<arg|user>|||<times|<look-up|<box-info|<arg|answer>|w>|0>|1tmpt>|>>>

  <assign|show-reply|<macro|user|answer|<arg|answer>>>

  <drd-props|hide-reply|arity|2|accessible|0>

  <drd-props|show-reply|arity|2|accessible|1>

  <\active*>
    <\src-comment>
      Text with gaps
    </src-comment>
  </active*>

  <assign|gap-dots-color|>

  <assign|gap-dots-vsep|0.2fn>

  <assign|gap-dots-hsep|0fn>

  <assign|gap-dots-hpadding|0.5spc>

  <assign|gap-dots-min-width|2em>

  <assign|gap-dots-short|<macro|body|<repeat-through|<with|color|<if|<equal|<value|gap-dots-color>|>|<value|color>|<value|gap-dots-color>>|<move|.||<minus|<value|gap-dots-vsep>>><hspace|<value|gap-dots-hsep>>>|<render-gap-text|<arg|body>>>>>

  <assign|gap-dots|<macro|body|<gap-dots-short|<hspace|<value|gap-dots-hpadding>><extend|<arg|body>|||<value|gap-dots-min-width>|><hspace|<value|gap-dots-hpadding>>>>>

  <assign|gap-dots*|<macro|body|<gap-dots-short|<arg|body><htab|5mm>>>>

  <assign|gap-dots-wide|<macro|body|<gap-dots-short|<hspace|<value|gap-dots-hpadding>><arg|body><htab|5mm>>>>

  <assign|gap-dots-long|<\macro|body>
    <\with|par-par-sep|0fn>
      <extern|ext-apply-on-paragraphs|gap-dots*|<quote-arg|body>>
    </with>
  </macro>>

  <drd-props|gap-dots|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-dots-wide|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-dots-long|with-like|yes|arity|1|accessible|all>

  \;

  <assign|gap-underlined-color|#c0f0f0>

  <assign|gap-underlined-vsep|0.2fn>

  <assign|gap-underlined-hpadding|0.5spc>

  <assign|gap-underlined-min-width|2em>

  <assign|gap-underlined-short|<macro|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<if|<equal|<value|gap-underlined-color>|>|<value|color>|<value|gap-underlined-color>>|<repeat*|<with|color|<unquote|<value|color>>|<arg|x>>|<move|<resize|--|<plus|0.6667l|0.3333r>||<plus|0.3333l|0.6667r>|>||<minus|-0.5ex|<value|gap-underlined-vsep>>>>>>|<render-gap-text|<arg|body>>>>>>>

  <assign|gap-underlined|<macro|body|<gap-underlined-short|<hspace|<value|gap-underlined-hpadding>><extend|<arg|body>|||<value|gap-underlined-min-width>|><hspace|<value|gap-underlined-hpadding>>>>>

  <assign|gap-underlined*|<macro|body|<gap-underlined-short|<arg|body><htab|5mm>>>>

  <assign|gap-underlined-wide|<macro|body|<gap-underlined-short|<hspace|<value|gap-underlined-hpadding>><arg|body><htab|5mm>>>>

  <assign|gap-underlined-long|<\macro|body>
    <\with|par-par-sep|0fn>
      <map-args|gap-underlined*|document|body>
    </with>
  </macro>>

  <assign|gap-underlined-long|<\macro|body>
    <\with|par-par-sep|0fn>
      <extern|ext-apply-on-paragraphs|gap-underlined*|<quote-arg|body>>
    </with>
  </macro>>

  <drd-props|gap-underlined|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-underlined-wide|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-underlined-long|with-like|yes|arity|1|accessible|all>

  \;

  <assign|gap-box-color|#f0e8e0>

  <assign|gap-box-shadow-color|#f8f4f0>

  <assign|gap-box-sunny-color|#e0d0c0>

  <assign|gap-box-hpadding|0.5spc>

  <assign|gap-box-vpadding|0.5spc>

  <assign|gap-box-min-width|2em>

  <assign|gap-box|<macro|body|<quasi|<datoms|<macro|x|<with|ornament-color|<value|gap-box-color>|ornament-shadow-color|<value|gap-box-shadow-color>|ornament-sunny-color|<value|gap-box-sunny-color>|ornament-hpadding|<value|gap-box-hpadding>|ornament-vpadding|<value|gap-box-vpadding>|<ornament|<extend|<arg|x>|||<value|gap-box-min-width>|>>>>|<render-gap-text|<arg|body>>>>>>

  <assign|gap-box-wide|<macro|body|<gap-box|<arg|body><htab|5mm>>>>

  <assign|gap-box-long|<\macro|body>
    <\with|par-par-sep|0fn|ornament-color|<value|gap-box-color>|ornament-shadow-color|<value|gap-box-shadow-color>|ornament-sunny-color|<value|gap-box-sunny-color>|ornament-hpadding|<value|gap-box-hpadding>|ornament-vpadding|<value|gap-box-vpadding>>
      <\ornament>
        <\surround||<right-flush>>
          <\render-gap-text>
            <arg|body>
          </render-gap-text>
        </surround>
      </ornament>
    </with>
  </macro>>

  <drd-props|gap-box|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-box-wide|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-box-long|with-like|yes|arity|1|accessible|all>

  \;

  <assign|gap|<value|gap-dots>>

  <assign|gap-wide|<value|gap-dots-wide>>

  <assign|gap-long|<value|gap-dots-long>>

  <drd-props|gap|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-wide|with-like|yes|arity|1|accessible|all>

  <drd-props|gap-long|with-like|yes|arity|1|accessible|all>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|src-special|normal>
  </collection>
</initial>