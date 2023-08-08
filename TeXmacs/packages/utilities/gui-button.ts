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

  <use-module|(utils misc gui-utils)>

  <\active*>
    <\src-comment>
      Rendering macros
    </src-comment>
  </active*>

  <assign|render-button-normal|<macro|x|<with|ornament-corner|30%|ornament-color|pastel
  grey|ornament-shadow-color|dark grey|ornament-sunny-color|white|<ornament|<arg|x>>>>>

  <assign|render-button-hover|<macro|x|<with|shadow-recolor|#6060c0|<drop-contour|<with|ornament-corner|30%|ornament-color|pastel
  grey|ornament-shadow-color|dark grey|ornament-sunny-color|white|<ornament|<arg|x>>>>>>>

  <assign|render-button-pressed|<macro|x|<with|shadow-recolor|#6060c0|<with|ornament-corner|30%|ornament-color|pastel
  grey|ornament-sunny-color|dark grey|ornament-shadow-color|white|<ornament|<arg|x>>>>>>

  <\active*>
    <\src-comment>
      Buttons that can trigger a scheme action
    </src-comment>
  </active*>

  <assign|action-button|<macro|x|cmd|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<render-button-pressed|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<render-button-hover|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<render-button-normal|<arg|x>>|gui-on-select|<arg|cmd>>>>>>

  <drd-props|action-button|arity|2|accessible|0>

  <\active*>
    <\src-comment>
      Rendering macros
    </src-comment>
  </active*>

  <assign|menu-button-normal|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|pastel
  grey|ornament-shadow-color|pastel grey|ornament-sunny-color|pastel
  grey|<ornament|<space|0cm|-0.2em|0.8em><arg|x>>>>>

  <assign|menu-button-hover|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|#d0e0f0|ornament-shadow-color|#d0e0f0|ornament-sunny-color|#d0e0f0|<ornament|<space|0cm|-0.2em|0.8em><arg|x>>>>>

  <assign|menu-button-pressed|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|pastel
  grey|ornament-shadow-color|white|ornament-sunny-color|dark
  grey|<ornament|<space|0cm|-0.2em|0.8em><arg|x>>>>>

  <\active*>
    <\src-comment>
      Menu buttons
    </src-comment>
  </active*>

  <assign|menu-button|<macro|x|cmd|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<menu-button-pressed|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<menu-button-hover|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<menu-button-normal|<arg|x>>|gui-on-select|<arg|cmd>>>>>>

  <drd-props|menu-button|arity|2|accessible|0>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>