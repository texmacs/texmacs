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

  <assign|invisible|<macro|body|<with|opacity|0|<arg|body>>>>

  <drd-props|anim-edit|arity|5|hidden|0|accessible|1|duration|2|duration|3|duration|4>

  <\active*>
    <\src-comment>
      Translations.
    </src-comment>
  </active*>

  <assign|translate-start-x|-1.0>

  <assign|translate-start-y|0.0>

  <assign|translate-end-x|1.0>

  <assign|translate-end-y|0.0>

  <assign|translate-in|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-translate|<arg|body>|<arg|duration>|<tuple|<value|translate-start-x>|<value|translate-start-y>>|>>>>

  <assign|translate-out|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-translate|<arg|body>|<arg|duration>||<tuple|<value|translate-end-x>|<value|translate-end-y>>>>>>

  <assign|translate-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-translate|<arg|body>|<arg|duration>|<tuple|<value|translate-start-x>|<value|translate-start-y>>|<tuple|<value|translate-end-x>|<value|translate-end-y>>>>>>

  <\active*>
    <\src-comment>
      Progressive appearance/disappearance.
    </src-comment>
  </active*>

  <assign|progressive-start-l|0>

  <assign|progressive-start-r|0>

  <assign|progressive-start-b|0>

  <assign|progressive-start-t|1>

  <assign|progressive-end-l|1>

  <assign|progressive-end-r|1>

  <assign|progressive-end-b|0>

  <assign|progressive-end-t|1>

  <assign|progressive-in|<macro|body|duration|<anim-progressive|<arg|body>|<arg|duration>|<tuple|<value|progressive-start-l>|<value|progressive-start-b>|<value|progressive-start-r>|<value|progressive-start-t>>|>>>

  <assign|progressive-out|<macro|body|duration|<anim-progressive|<arg|body>|<arg|duration>||<tuple|<value|progressive-end-l>|<value|progressive-end-b>|<value|progressive-end-r>|<value|progressive-end-t>>>>>

  <assign|progressive-smooth|<macro|body|duration|<anim-progressive|<arg|body>|<arg|duration>|<tuple|<value|progressive-start-l>|<value|progressive-start-b>|<value|progressive-start-r>|<value|progressive-start-t>>|<tuple|<value|progressive-end-l>|<value|progressive-end-b>|<value|progressive-end-r>|<value|progressive-end-t>>>>>

  <\active*>
    <\src-comment>
      Fading in/out.
    </src-comment>
  </active*>

  <assign|fade-start|0.0>

  <assign|fade-end|1.0>

  <assign|fade-in|<macro|body|duration|<superpose|<invisible|<arg|body>>|<with|orig-opacity|<value|opacity>|<anim-static|<morph|<tuple|0|<with|opacity|<times|<value|fade-start>|<value|orig-opacity>>|<arg|body>>>|<tuple|1|<with|opacity|<times|1.0|<value|orig-opacity>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>>

  <assign|fade-out|<macro|body|duration|<superpose|<invisible|<arg|body>>|<with|orig-opacity|<value|opacity>|<anim-static|<morph|<tuple|0|<with|opacity|<times|<value|fade-end>|<value|orig-opacity>>|<arg|body>>>|<tuple|1|<with|opacity|<times|0.0|<value|orig-opacity>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>>

  <assign|fade-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<with|orig-opacity|<value|opacity>|<anim-static|<morph|<tuple|0|<with|opacity|<times|<value|fade-start>|<value|orig-opacity>>|<arg|body>>>|<tuple|1|<with|opacity|<times|<value|fade-end>|<value|orig-opacity>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>>

  <\active*>
    <\src-comment>
      Zooming in and out.
    </src-comment>
  </active*>

  <assign|zoom-start|0.1>

  <assign|zoom-end|1.0>

  <assign|zoom-in|<macro|body|duration|<superpose|<invisible|<arg|body>>|<with|orig-magnification|<value|magnification>|<anim-static|<morph|<tuple|0|<with|magnification|<times|<value|zoom-start>|<value|orig-magnification>>|<arg|body>>>|<tuple|1|<with|magnification|<times|1.0|<value|orig-magnification>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>>

  <assign|zoom-out|<macro|body|duration|<superpose|<invisible|<arg|body>>|<with|orig-magnification|<value|magnification>|<anim-static|<morph|<tuple|0|<with|magnification|<times|<value|zoom-end>|<value|orig-magnification>>|<arg|body>>>|<tuple|1|<with|magnification|<times|0.1|<value|orig-magnification>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>>

  <assign|zoom-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<with|orig-magnification|<value|magnification>|<anim-static|<morph|<tuple|0|<with|magnification|<times|<value|zoom-start>|<value|orig-magnification>>|<arg|body>>>|<tuple|1|<with|magnification|<times|<value|zoom-end>|<value|orig-magnification>>|<arg|body>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>>

  <\active*>
    <\src-comment>
      Emphasis animations
    </src-comment>
  </active*>

  <assign|emboss-start-dx|0ln>

  <assign|emboss-start-dy|0ln>

  <assign|emboss-end-dx|-2ln>

  <assign|emboss-end-dy|-2ln>

  <assign|shadowed-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-static|<morph|<tuple|0|<with|shadow-dx|<value|emboss-start-dx>|shadow-dy|<value|emboss-start-dy>|<shadowed-raise|<arg|body>>>>|<tuple|1|<with|shadow-dx|<value|emboss-end-dx>|shadow-dy|<value|emboss-end-dy>|<shadowed-raise|<arg|body>>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>

  <assign|emboss-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-static|<morph|<tuple|0|<with|emboss-dx|<value|emboss-start-dx>|emboss-dy|<value|emboss-start-dy>|<emboss|<arg|body>>>>|<tuple|1|<with|emboss-dx|<value|emboss-end-dx>|emboss-dy|<value|emboss-end-dy>|<emboss|<arg|body>>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>

  <assign|outlined-emboss-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-static|<morph|<tuple|0|<with|emboss-dx|<value|emboss-start-dx>|emboss-dy|<value|emboss-start-dy>|<outlined-emboss|<arg|body>>>>|<tuple|1|<with|emboss-dx|<value|emboss-end-dx>|emboss-dy|<value|emboss-end-dy>|<outlined-emboss|<arg|body>>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>

  <\active*>
    <\src-comment>
      Experimental animations
    </src-comment>
  </active*>

  <assign|degrade-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-static|<morph|<tuple|0|<with|degraded-threshold|0|<degraded|<arg|body>>>>|<tuple|1|<with|degraded-threshold|1|<degraded|<arg|body>>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>

  <assign|gnaw-smooth|<macro|body|duration|<superpose|<invisible|<arg|body>>|<anim-static|<morph|<tuple|0|<with|gnawed-strength|0|<gnawed|<arg|body>>>>|<tuple|1|<with|gnawed-strength|10|<gnawed|<arg|body>>>>>|<arg|duration>|<over|<arg|duration>|<value|anim-nr-frames>>|<arg|duration>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|src-special|normal>
  </collection>
</initial>