<TeXmacs|1.0.6.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-form|1.0>

    <\src-purpose>
      Input fields, buttons, toggles, radio buttons and notebooks
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Input fields
    </src-comment>
  </active*>

  <assign|short-input|<macro|name|val|<short-bright|<locus|<widget-id|<arg|name>>|<arg|val>>>>>

  <assign|wide-input|<\macro|name|scx|val>
    <\block-bright>
      <canvas|0px|1fnbot|1par|1fntop|<arg|scx>||<locus|<widget-id|<arg|name>>|<group|<arg|val>>>>
    </block-bright>
  </macro>>

  <assign|block-input|<\macro|name|val>
    <\block-highlight|<value|bright-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-med-sep>>
      <\locus|<widget-id|<arg|name>>>
        <arg|val>
      </locus>
    </block-highlight>
  </macro>>

  <assign|canvas-input|<\macro|name|y1|y2|scx|scy|val>
    <\block-bright>
      <\canvas||<arg|y1>|1par|<arg|y2>|<arg|scx>|<arg|scy>>
        <\locus|<widget-id|<arg|name>>>
          <arg|val>
        </locus>
      </canvas>
    </block-bright>
  </macro>>

  <assign|hidden-input|<macro|name|val|body|<surround|<hidden|<locus|<widget-id|<arg|name>>|<arg|val>>>||<with|<merge|widget-value-|<arg|name>>|<arg|val>|<arg|body>>>>>

  <drd-props|short-input|arity|2|enable-writability|all>

  <drd-props|wide-input|arity|3|enable-writability|all>

  <drd-props|block-input|arity|2|enable-writability|all>

  <drd-props|canvas-input|arity|6|enable-writability|all>

  <\active*>
    <\src-comment>
      Buttons
    </src-comment>
  </active*>

  <assign|invisible-button|<macro|on|body|<group|<arg|body>>>>

  <assign|square-button|<macro|on|body|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<short-lower|<arg|body>>|<short-raise|<arg|body>>>>>>

  <assign|circular-button|<macro|on|body|<with|box-color|<value|button-color>|<arg|body><circular-box|<arg|body>>>>>

  <assign|render-button|<macro|on|body|<compound|<merge|<value|button-shape>|-button>|<arg|on>|<arg|body>>>>

  <assign|button|<macro|body|cmd|<render-button|false|<action|<arg|body>|<widget-cmd|<arg|cmd>>|<arg|body>>>>>

  <drd-props|button|arity|2|accessible|none|enable-writability|all>

  <\active*>
    <\src-comment>
      Boxes
    </src-comment>
  </active*>

  <assign|gui-magnify|<macro|factor|body|<with|magnification|<times|<value|magnification>|<arg|factor>>|<arg|body>>>>

  <assign|gui-centered-cell|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-width|1em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|1em>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|circular-box|<macro|body|<style-with|src-compact|none|<superpose|<gui-centered-cell|<gui-magnify|2.5|<style-with|src-compact|none|<move|<with|bg-color|<value|color>|<with|mode|math|color|<value|box-color>|\<bullet\>>>|0.02em|-0.055em>>>>|<gui-centered-cell|<gui-magnify|1.1|<with|mode|math|\<bigcirc\>>>>|<gui-centered-cell|<arg|body>>>>>>

  <assign|square-box|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-width|0.9em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|0.9em>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-background|<value|box-color>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|render-box|<macro|body|<compound|<merge|<value|box-shape>|-box>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Markers
    </src-comment>
  </active*>

  <assign|bullet-marker|<macro|on|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<gui-magnify|1.5|<move|<with|mode|math|\<bullet\>>|0.02em|-0.055em>>|>>>>

  <assign|checked-marker|<macro|on|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<gui-magnify|1.25|<move|<with|mode|math|\<checked\>>|0.2em|0.2em>>|>>>>

  <assign|render-marker|<macro|on|<compound|<merge|<value|marker-shape>|-marker>|<arg|on>>>>

  <assign|render-box-marker|<macro|on|<render-box|<render-marker|<arg|on>>>>>

  <\active*>
    <\src-comment>
      Toggles
    </src-comment>
  </active*>

  <assign|toggle-cmd|<macro|name|val|<style-with|src-compact|none|<widget-cmd|<merge|(widget-set!
  "|<arg|name>|" "|<not|<arg|val>>|")>>>>>

  <assign|toggle-box|<macro|name|val|<style-with|src-compact|none|<action|<render-box-marker|<if|<equal|<arg|name>|>|<arg|val>|<arg|val>>>|<toggle-cmd|<arg|name>|<arg|val>>|<arg|val>><hidden|<locus|<widget-id|<arg|name>>|<arg|val>>>>>>

  <assign|toggle-button|<macro|name|val|text|<style-with|src-compact|none|<action|<render-button|<if|<equal|<arg|name>|>|<arg|val>|<arg|val>>|<arg|text>>|<toggle-cmd|<arg|name>|<arg|val>>|<arg|val>><hidden|<locus|<widget-id|<arg|name>>|<arg|val>>>>>>

  <drd-props|toggle-box|arity|2|enable-writability|all>

  <drd-props|toggle-button|arity|3|accessible|none|enable-writability|all>

  <\active*>
    <\src-comment>
      Radio boxes
    </src-comment>
  </active*>

  <assign|radio-box|<macro|name|val|<style-with|src-compact|none|<action|<render-box-marker|<equal|<arg|val>|<widget-value|<arg|name>>>>|<widget-cmd|<merge|(widget-set!
  "|<arg|name>|" "|<arg|val>|")>>|<arg|val>>>>>

  <assign|radio-button|<macro|name|val|text|<style-with|src-compact|none|<action|<render-button|<equal|<arg|val>|<widget-value|<arg|name>>>|<arg|text>>|<widget-cmd|<merge|(widget-set!
  "|<arg|name>|" "|<arg|val>|")>>|<arg|val>>>>>

  <drd-props|radio-box|arity|2|enable-writability|all>

  <drd-props|radio-button|arity|3|accessible|none|enable-writability|all>

  <\active*>
    <\src-comment>
      Conditionally shown widgets and pagelets in a notebook
    </src-comment>
  </active*>

  <assign|hidden-macro|<macro|body|<hidden|<arg|body>>>>

  <assign|conditional|<macro|cond|body|<compound|<if|<arg|cond>|identity|hidden-macro>|<arg|body>>>>

  <assign|pagelet|<\macro|name|val|body>
    <\conditional|<equal|<arg|val>|<widget-value|<arg|name>>>>
      <arg|body>
    </conditional>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>