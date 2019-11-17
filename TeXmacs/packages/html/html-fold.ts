<TeXmacs|1.99.11>

<style|<tuple|source|std|english>>

<\body>
  <active*|<\src-title>
    <src-package|html-fold|1.0>

    <\src-purpose>
      Export foldable content and keep it foldable using Javascript.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  \;

  <provide|html-extra-css|<tuple>>

  <provide|html-extra-javascript-src|<tuple>>

  <assign|html-extra-css|<merge|<value|html-extra-css>|<tuple|http://www.texmacs.org/css/tmfold.css>>>

  <assign|html-extra-javascript-src|<merge|<value|html-extra-javascript-src>|<tuple|http://www.texmacs.org/javascript/texmacs_fold.js>>>

  <\active*>
    <\src-comment>
      Helper macros for buttons.
    </src-comment>
  </active*>

  <assign|tmhtml-unfold-button|<macro|body|x|<html-class|detailed-button|<arg|body>>>>

  <assign|tmhtml-fold-button|<macro|body|x|<html-class|summary-button|<arg|body>>>>

  <assign|tmhtml-unfold-button*|<macro|body|x|<html-class|toggle-button|<arg|body>>>>

  <assign|tmhtml-fold-button*|<macro|body|x|<html-class|toggle-button|<arg|body>>>>

  \;

  <assign|html-protect|<macro|x|<html-class|container|<arg|x>>>>

  <assign|right-aligned|<macro|x|<move|<arg|x>|-1r|>>>

  <assign|tmhtml-right-aligned|<macro|x|<html-style|float: left; text-align:
  right; min-width: 2.5em; margin-left: -3em; margin-right:
  0.5em|<html-protect|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Tags for folding and unfolding.
    </src-comment>
  </active*>

  <assign|base-folded-plain|<\macro|x|y>
    <html-class|always toggle-button|<arg|x>>

    <html-class|detailed|<arg|y>>
  </macro>>

  <assign|tmhtml-folded-plain|<macro|x|y|<html-div-class|switchable|<base-folded-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-plain|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-folded-explain|<macro|x|y|<html-div-class|switchable|<base-folded-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-explain|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-folded-grouped|<macro|x|y|<html-div-class|switchable|<base-folded-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-grouped|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-plain|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-folded-std|<\macro|x|y>
    <\with|par-left|<plus|<value|par-left>|1.5fn>>
      <html-class|always|<surround|<tmhtml-right-aligned|<fold-button|<active*|<with|mode|math|\<bullet\>>>|<arg|x>><unfold-button|<active*|<with|mode|math|<op|\<circ\>>>>|<arg|x>>>||<arg|x>>>

      <html-class|detailed|<arg|y>>
    </with>
  </macro>>

  <assign|tmhtml-folded|<macro|x|y|<html-div-class|switchable|<base-folded-std|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-std|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-folded-std|<macro|x|y|<html-div-class|switchable|<base-folded-std|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-std|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-std|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-folded-env|<\macro|x|y>
    <html-class|always|<surround|<tmhtml-right-aligned|<fold-button|<active*|<with|mode|math|\<bullet\>>>|<arg|x>><unfold-button|<active*|<with|mode|math|<op|\<circ\>>>>|<arg|x>>>||<arg|x>>>

    <html-class|detailed|<arg|y>>
  </macro>>

  <assign|tmhtml-folded-env|<macro|x|y|<html-div-class|switchable|<base-folded-env|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-env|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-env|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-folded-reverse|<\macro|x|y>
    <\with|par-left|<plus|<value|par-left>|1.5fn>>
      <html-class|detailed|<arg|x>>

      <html-class|always|<surround|<tmhtml-right-aligned|<fold-button|<active*|<with|mode|math|\<bullet\>>>|<arg|y>><unfold-button|<active*|<with|mode|math|<op|\<circ\>>>>|<arg|y>>>||<arg|y>>>
    </with>
  </macro>>

  <assign|tmhtml-folded-reverse|<macro|x|y|<html-div-class|switchable|<base-folded-reverse|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-reverse|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-reverse|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-folded-documentation|<\macro|x|y>
    <\html-class|always>
      <strong|<large|<surround|<vspace*|1fn>|<space|1em><fold-button|<with|color|#336666|<math|<op|\<Uparrow\>>>>|<arg|x>><unfold-button|<with|color|#336666|<math|<op|\<Downarrow\>>>>|<arg|x>><vspace|0.25fn>|<arg|x>>>>
    </html-class>

    <html-class|detailed|<arg|y>>
  </macro>>

  <assign|tmhtml-folded-documentation|<macro|x|y|<html-div-class|switchable|<base-folded-documentation|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-unfolded-documentation|<macro|x|y|<html-div-class|switchable
  enabled|<base-folded-documentation|<arg|x>|<arg|y>>>>>

  <\active*>
    <\src-comment>
      Summarized and detailed.
    </src-comment>
  </active*>

  <assign|base-summarized-plain|<\macro|x|y>
    <html-class|summary toggle-button|<arg|x>>

    <html-class|detailed toggle-button|<arg|y>>
  </macro>>

  <assign|tmhtml-summarized-plain|<macro|x|y|<html-div-class|switchable|<base-summarized-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-detailed-plain|<macro|x|y|<html-div-class|switchable
  enabled|<base-summarized-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-summarized-grouped|<macro|x|y|<html-div-class|switchable|<base-summarized-plain|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-detailed-grouped|<macro|x|y|<html-div-class|switchable
  enabled|<base-summarized-plain|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-summarized-std|<\macro|x|y>
    <\with|par-left|<plus|<value|par-left>|1.5fn>>
      <html-class|summary|<surround|<tmhtml-right-aligned|<unfold-button|<active*|<with|mode|math|<op|\<circ\>>>>|<arg|x>>>||<arg|x>>>

      <html-class|detailed|<surround|<tmhtml-right-aligned|<fold-button|<active*|<with|mode|math|\<bullet\>>>|<arg|y>>>||<arg|y>>>
    </with>
  </macro>>

  <assign|tmhtml-summarized-std|<macro|x|y|<html-div-class|switchable|<base-summarized-std|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-detailed-std|<macro|x|y|<html-div-class|switchable
  enabled|<base-summarized-std|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-summarized-env|<\macro|x|y>
    <html-class|summary|<surround|<tmhtml-right-aligned|<unfold-button|<active*|<with|mode|math|<op|\<circ\>>>>|<arg|x>>>||<arg|x>>>

    <html-class|detailed|<surround|<tmhtml-right-aligned|<fold-button|<active*|<with|mode|math|\<bullet\>>>|<arg|y>>>||<arg|y>>>
  </macro>>

  <assign|tmhtml-summarized|<macro|x|y|<html-div-class|switchable|<base-summarized-env|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-detailed|<macro|x|y|<html-div-class|switchable
  enabled|<base-summarized-env|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-summarized-env|<macro|x|y|<html-div-class|switchable|<base-summarized-env|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-detailed-env|<macro|x|y|<html-div-class|switchable
  enabled|<base-summarized-env|<arg|x>|<arg|y>>>>>

  \;

  <assign|base-summarized-documentation|<\macro|x|y>
    <\html-class|summary>
      <strong|<large|<surround|<vspace*|1fn>|<space|1em><unfold-button|<with|color|#336666|<math|<op|\<ldots\>>>>|<arg|x>><vspace|0.25fn>|<arg|x>>>>
    </html-class>

    <html-class|detailed|<surround|<vspace*|1fn>|<space|1em><fold-button|<with|color|#336666|<math|<op|\<Leftarrow\>>>>|<arg|y>><vspace|0.25fn>|<arg|y>>>
  </macro>>

  <assign|base-summarized-documentation|<\macro|x|y>
    <strong|<\large>
      <\html-class|summary>
        <surround|<vspace*|1fn>|<space|1em><unfold-button|<with|color|#336666|<math|<op|\<ldots\>>>>|<arg|x>><vspace|0.25fn>|<arg|x>>
      </html-class>
    </large>>

    <html-class|detailed|<surround|<vspace*|1fn>|<space|1em><fold-button|<with|color|#336666|<math|<op|\<Leftarrow\>>>>|<arg|y>><vspace|0.25fn>|<arg|y>>>
  </macro>>

  <assign|tmhtml-summarized-documentation|<macro|x|y|<html-div-class|switchable|<base-summarized-documentation|<arg|x>|<arg|y>>>>>

  <assign|tmhtml-detailed-documentation|<macro|x|y|<html-div-class|switchable
  enabled|<base-summarized-documentation|<arg|x>|<arg|y>>>>>

  <\active*>
    <\src-comment>
      Summarized and detailed.
    </src-comment>
  </active*>

  <assign|no-html-unfolded-io|<value|unfolded-io>>

  <assign|tmhtml-folded-io|<macro|prompt|in|out|<html-div-class|switchable|<no-html-unfolded-io|<arg|prompt>|<html-class|always|<arg|in>>|<html-class|detailed|<arg|out>>>>>>

  <assign|tmhtml-unfolded-io|<macro|prompt|in|out|<html-div-class|switchable
  enabled|<no-html-unfolded-io|<arg|prompt>|<html-class|always|<arg|in>>|<html-class|detailed|<arg|out>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>