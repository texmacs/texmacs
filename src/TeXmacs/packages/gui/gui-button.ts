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
      GUI color scheme
    </src-comment>
  </active*>

  <assign|gui-bg-color|#e0e0e0>

  <assign|gui-sunny-color|white>

  <assign|gui-shadow-color|#707070>

  <assign|gui-blur-color|#6060c0>

  <assign|gui-select-color|#d0e0f0>

  <assign|button-bg-color|white>

  <\active*>
    <\src-comment>
      Buttons that can trigger a scheme action
    </src-comment>
  </active*>

  <assign|action-button-normal|<macro|x|<with|ornament-corner|30%|ornament-color|<value|gui-bg-color>|ornament-shadow-color|<value|gui-shadow-color>|ornament-sunny-color|<value|gui-sunny-color>|<ornament|<arg|x>>>>>

  <assign|action-button-hover|<macro|x|<with|shadow-recolor|<value|gui-blur-color>|<drop-contour|<with|ornament-corner|30%|ornament-color|<value|gui-bg-color>|ornament-shadow-color|<value|gui-shadow-color>|ornament-sunny-color|<value|gui-sunny-color>|<ornament|<arg|x>>>>>>>

  <assign|action-button-pressed|<macro|x|<with|ornament-corner|30%|ornament-color|<value|gui-bg-color>|ornament-sunny-color|<value|gui-shadow-color>|ornament-shadow-color|<value|gui-sunny-color>|<ornament|<arg|x>>>>>

  <assign|action-button|<macro|x|cmd|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<action-button-pressed|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<action-button-hover|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<action-button-normal|<arg|x>>|gui-on-select|<arg|cmd>>>>>>

  <drd-props|action-button|arity|2|accessible|0>

  <\active*>
    <\src-comment>
      Menu buttons
    </src-comment>
  </active*>

  <assign|menu-button-normal|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-bg-color>|ornament-shadow-color|<value|gui-bg-color>|ornament-sunny-color|<value|gui-bg-color>|<ornament|<space|0cm|-0.2em|0.8em><arg|x>>>>>

  <assign|menu-button-hover|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-select-color>|ornament-shadow-color|<value|gui-select-color>|ornament-sunny-color|<value|gui-select-color>|<ornament|<space|0cm|-0.2em|0.8em><arg|x>>>>>

  <assign|menu-button-pressed|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-select-color>|ornament-shadow-color|<value|gui-sunny-color>|ornament-sunny-color|<value|gui-shadow-color>|<ornament|<space|0cm|-0.2em|0.8em><arg|x>>>>>

  <assign|menu-button|<macro|x|cmd|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<menu-button-pressed|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<menu-button-hover|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<menu-button-normal|<arg|x>>|gui-on-select|<arg|cmd>>>>>>

  <drd-props|menu-button|arity|2|accessible|0>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>