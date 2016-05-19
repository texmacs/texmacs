<TeXmacs|1.99.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-animate|1.0>

    <\src-purpose>
      Macros for animations.
    </src-purpose>

    <src-copyright|2016|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Basic macros.
    </src-comment>
  </active*>

  <assign|anim-nr-frames|25>

  <assign|anim-edit|<macro|body|current|duration|step|now|<arg|current>>>

  <\active*>
    <\src-comment>
      Translations.
    </src-comment>
  </active*>

  <assign|translate-start-x|-1.0>

  <assign|translate-start-y|0.0>

  <assign|translate-end-x|0.0>

  <assign|translate-end-y|0.0>

  <assign|appear-translate|<macro|body|duration|<anim-translate|<arg|body>|<arg|duration>|<tuple|<value|translate-start-x>|<value|translate-start-y>>|>>>

  <assign|disappear-translate|<macro|body|duration|<anim-translate|<arg|body>|<arg|duration>||<tuple|<value|translate-end-x>|<value|translate-end-y>>>>>

  <assign|smooth-translate|<macro|body|duration|<anim-translate|<arg|body>|<arg|duration>|<tuple|<value|translate-start-x>|<value|translate-start-y>>|<tuple|<value|translate-end-x>|<value|translate-end-y>>>>>

  <\active*>
    <\src-comment>
      Progressive appearance/disappearance.
    </src-comment>
  </active*>

  <assign|progressive-start-l|0.5>

  <assign|progressive-start-r|0.5>

  <assign|progressive-start-b|0.5>

  <assign|progressive-start-t|0.5>

  <assign|progressive-end-l|0.0>

  <assign|progressive-end-r|1.0>

  <assign|progressive-end-b|0.0>

  <assign|progressive-end-t|1.0>

  <assign|appear-progressive|<macro|body|duration|<anim-progressive|<arg|body>|<arg|duration>|<tuple|<value|progressive-start-l>|<value|progressive-start-b>|<value|progressive-start-r>|<value|progressive-start-t>>|>>>

  <assign|disappear-progressive|<macro|body|duration|<anim-progressive|<arg|body>|<arg|duration>||<tuple|<value|progressive-end-l>|<value|progressive-end-b>|<value|progressive-end-r>|<value|progressive-end-t>>>>>

  <assign|smooth-progressive|<macro|body|duration|<anim-progressive|<arg|body>|<arg|duration>|<tuple|<value|progressive-start-l>|<value|progressive-start-b>|<value|progressive-start-r>|<value|progressive-start-t>>|<tuple|<value|progressive-end-l>|<value|progressive-end-b>|<value|progressive-end-r>|<value|progressive-end-t>>>>>

  <\active*>
    <\src-comment>
      Fading in/out.
    </src-comment>
  </active*>

  <assign|fade-start|0.0>

  <assign|fade-end|1.0>

  <assign|appear-fade|<macro|body|duration|<anim-static|<morph|<tuple|0|<with|opacity|<value|fade-start>|<arg|body>>>|<tuple|1|<with|opacity|1.0|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>

  <assign|disappear-fade|<macro|body|duration|<anim-static|<morph|<tuple|0|<with|opacity|<value|fade-end>|<arg|body>>>|<tuple|1|<with|opacity|0.0|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>

  <assign|smooth-fade|<macro|body|duration|<anim-static|<morph|<tuple|0|<with|opacity|<value|fade-start>|<arg|body>>>|<tuple|1|<with|opacity|<value|fade-end>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>

  <\active*>
    <\src-comment>
      Zooming in and out.
    </src-comment>
  </active*>

  <assign|zoom-start|0.1>

  <assign|zoom-end|1.0>

  <assign|appear-zoom|<macro|body|duration|<with|orig-magnification|<value|magnification>|<anim-static|<morph|<tuple|0|<with|magnification|<times|<value|zoom-start>|<value|orig-magnification>>|<arg|body>>>|<tuple|1|<with|magnification|<times|1.0|<value|orig-magnification>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>

  <assign|disappear-zoom|<macro|body|duration|<with|orig-magnification|<value|magnification>|<anim-static|<morph|<tuple|0|<with|magnification|<times|<value|zoom-end>|<value|orig-magnification>>|<arg|body>>>|<tuple|1|<with|magnification|<times|0.1|<value|orig-magnification>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>

  <assign|smooth-zoom|<macro|body|duration|<with|orig-magnification|<value|magnification>|<anim-static|<morph|<tuple|0|<with|magnification|<times|<value|zoom-start>|<value|orig-magnification>>|<arg|body>>>|<tuple|1|<with|magnification|<times|<value|zoom-end>|<value|orig-magnification>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|src-special|normal>
  </collection>
</initial>