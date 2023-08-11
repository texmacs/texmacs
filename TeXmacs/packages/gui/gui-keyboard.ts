<TeXmacs|2.1.2>

<style|<tuple|source|english>>

<\body>
  <active*|<\src-title>
    <src-package|gui-keyboard|1.0>

    <\src-purpose>
      Markup for the creation of custom keyboards
    </src-purpose>

    <src-copyright|2023|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|gui-button>

  <\active*>
    <\src-comment>
      GUI color scheme
    </src-comment>
  </active*>

  <assign|keyboard-color|#404040>

  <assign|key-color|#808080>

  <assign|key-shadow-color|#202020>

  <assign|key-sunny-color|#c0c0c0>

  <assign|key-pressed-color|#606090>

  <assign|key-pressed-shadow-color|#202020>

  <assign|key-pressed-sunny-color|#a0a0a0>

  <assign|key-text-color|white>

  <assign|key-width|3em>

  <assign|key-height|3em>

  <assign|key-padding|0.5em>

  <assign|key-inner-padding|0.125em>

  <\active*>
    <\src-comment>
      Keyboards
    </src-comment>
  </active*>

  <assign|keyboard|<macro|body|<tformat|<cwith|1|-1|1|1|cell-background|<value|keyboard-color>>|<cwith|1|-1|1|1|cell-lsep|<value|key-padding>>|<cwith|1|-1|1|1|cell-rsep|<value|key-padding>>|<cwith|1|1|1|1|cell-tsep|<value|key-padding>>|<cwith|-1|-1|1|1|cell-bsep|<value|key-padding>>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Keys
    </src-comment>
  </active*>

  <assign|with-key|<macro|body|<with|ornament-shape|rounded|ornament-corner|100%|ornament-border|1ln|ornament-color|<value|key-color>|ornament-shadow-color|<value|key-shadow-color>|ornament-sunny-color|<value|key-sunny-color>|color|<value|key-text-color>|ornament-hpadding|<value|key-inner-padding>|ornament-vpadding|<value|key-inner-padding>|<arg|body>>>>

  <assign|render-key-button-inner|<macro|x|<ornament|<tabular*|<tformat|<cwith|1|1|1|1|cell-width|<minus|<value|key-width>|<plus|<value|key-padding>|<value|key-inner-padding>>>>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|<minus|<value|key-height>|<plus|<value|key-padding>|<value|key-inner-padding>>>>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-valign|c>|<table|<row|<cell|<arg|x>>>>>>>>>

  <assign|render-key-button|<macro|x|<tabular*|<tformat|<cwith|1|1|1|1|cell-width|<value|key-width>>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|<value|key-height>>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-valign|c>|<table|<row|<cell|<render-key-button-inner|<arg|x>>>>>>>>>

  <assign|key-button-normal|<macro|x|<with-key|<render-key-button|<arg|x>>>>>

  <assign|key-button-hover|<macro|x|<with-key|<render-key-button|<arg|x>>>>>

  <assign|key-button-pressed|<macro|x|<with-key|<with|ornament-shadow-color|<value|key-pressed-sunny-color>|ornament-sunny-color|<value|key-pressed-shadow-color>|ornament-color|<value|key-pressed-color>|<render-key-button|<arg|x>>>>>>

  <assign|std-key|<macro|x|cmd|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<key-button-pressed|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<key-button-hover|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<key-button-normal|<arg|x>>|gui-on-select|<arg|cmd>>>>>>

  <drd-props|std-key|arity|2|accessible|0>

  <assign|extended-key|<macro|x|cmd|width|<with|key-width|<times|<arg|width>|<value|key-width>>|<std-key|<arg|x>|<arg|cmd>>>>>

  <assign|simple-key|<macro|x|<std-key|<arg|x>|<merge|(emu-key
  "|<arg|x>|")>>>>

  <assign|pressed-key|<macro|x|pressed|<if|<arg|pressed>|<with|save-color|<value|key-pressed-color>|save-shadow-color|<value|key-pressed-shadow-color>|save-sunny-color|<value|key-pressed-sunny-color>|<with|key-pressed-color|<value|key-color>|key-pressed-shadow-color|<value|key-sunny-color>|key-pressed-sunny-color|<value|key-shadow-color>|key-color|<value|save-color>|key-shadow-color|<value|save-sunny-color>|key-sunny-color|<value|save-shadow-color>|<arg|x>>>|<arg|x>>>>

  <assign|modifier-key|<macro|x|cmd|width|pressed|<pressed-key|<extended-key|<arg|x>|<arg|cmd>|<arg|width>>|<arg|pressed>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>