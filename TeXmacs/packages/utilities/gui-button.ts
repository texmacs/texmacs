<TeXmacs|2.1.2>

<style|<tuple|source|english>>

<\body>
  <active*|<\src-title>
    <src-package|gui-button|1.0>

    <\src-purpose>
      Stylable buttons
    </src-purpose>

    <src-copyright|2022|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|std-shadow>

  <\active*>
    <\src-comment>
      Rendering mactos
    </src-comment>
  </active*>

  <assign|render-button-normal|<macro|x|<with|ornament-corner|50%|ornament-color|pastel
  grey|ornament-shadow-color|dark grey|ornament-sunny-color|white|<ornament|<arg|x>>>>>

  <assign|render-button-hover|<macro|x|<with|shadow-recolor|#6060c0|<drop-contour|<with|ornament-corner|50%|ornament-color|pastel
  grey|ornament-shadow-color|dark grey|ornament-sunny-color|white|<ornament|<arg|x>>>>>>>

  <assign|render-button-pressed|<macro|x|<with|shadow-recolor|#6060c0|<drop-contour|<with|ornament-corner|50%|ornament-color|pastel
  grey|ornament-sunny-color|dark grey|ornament-shadow-color|white|<ornament|<arg|x>>>>>>>

  <\active*>
    <\src-comment>
      Buttons that can trigger a scheme action
    </src-comment>
  </active*>

  <assign|action-button|<macro|x|cmd|<style-with|src-compact|none|<dynamic-case|click|<with|locus-color|preserve|<action|<render-button-pressed|<arg|x>>|<arg|cmd>|<arg|x>>>|mouse-over|<render-button-hover|<arg|x>>|<render-button-normal|<arg|x>>>>>>

  <assign|xaction-button|<macro|x|cmd|<action|<render-button-pressed|<arg|x>>|<arg|cmd>|<arg|x>>>>

  <drd-props|action-button|arity|2|accessible|0>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>