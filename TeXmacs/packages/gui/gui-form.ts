<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-form|1.0>

    <\src-purpose>
      Input fields, buttons, toggles, radio buttons and notebooks
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Environment variables for buttons
    </src-comment>
  </active*>

  <assign|button-shape|square>

  <assign|box-shape|circular>

  <assign|box-color|pastel blue>

  <assign|marker-shape|checked>

  <\active*>
    <\src-comment>
      Input fields
    </src-comment>
  </active*>

  <assign|short-input|<macro|name|val|<short-bright|<locus|<widget-id|<arg|name>>|<arg|val>>>>>

  <assign|wide-input|<macro|name|val|<wide-bright|<locus|<widget-id|<arg|name>>|<arg|val>>>>>

  <assign|block-input|<\macro|name|val>
    <\block-bright>
      <\locus|<widget-id|<arg|name>>>
        <arg|val>
      </locus>
    </block-bright>
  </macro>>

  <assign|hidden-input|<macro|name|val|body|<surround|<hidden|<locus|<widget-id|<arg|name>>|<arg|val>>>||<with|<merge|widget-value-|<arg|name>>|<arg|val>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Buttons
    </src-comment>
  </active*>

  <assign|invisible-button|<macro|on|body|<group|<arg|body>>>>

  <assign|square-button|<macro|on|body|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<short-lower|<arg|body>>|<short-raise|<arg|body>>>>>>

  <assign|render-button|<macro|on|body|<compound|<merge|<value|button-shape>|-button>|<arg|on>|<arg|body>>>>

  <assign|button|<macro|body|cmd|<render-button|false|<action|<arg|body>|<widget-cmd|<arg|cmd>>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Boxes
    </src-comment>
  </active*>

  <assign|magnify|<macro|factor|body|<with|magnification|<times|<value|magnification>|<arg|factor>>|<arg|body>>>>

  <assign|gui-centered-cell|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-width|1em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|1em>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|circular-box|<macro|body|<style-with|src-compact|none|<superpose|<gui-centered-cell|<magnify|2.5|<style-with|src-compact|none|<move|<with|bg-color|<value|color>|<with|mode|math|color|<value|box-color>|\<bullet\>>>|0.02em|-0.055em>>>>|<gui-centered-cell|<magnify|1.1|<with|mode|math|\<bigcirc\>>>>|<gui-centered-cell|<arg|body>>>>>>

  <assign|square-box|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-width|0.9em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|0.9em>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-background|<value|box-color>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|render-box|<macro|body|<compound|<merge|<value|box-shape>|-box>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Markers
    </src-comment>
  </active*>

  <assign|bullet-marker|<macro|on|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<magnify|1.5|<move|<with|mode|math|\<bullet\>>|0.02em|-0.055em>>|>>>>

  <assign|checked-marker|<macro|on|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<magnify|1.25|<move|<with|mode|math|\<checked\>>|0.2em|0.2em>>|>>>>

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

  <\active*>
    <\src-comment>
      Radio boxes
    </src-comment>
  </active*>

  <assign|radio-box|<macro|name|val|<style-with|src-compact|none|<action|<render-box-marker|<equal|<arg|val>|<widget-value|<arg|name>>>>|<widget-cmd|<merge|(widget-set!
  "|<arg|name>|" "|<arg|val>|")>>|<arg|val>>>>>

  <assign|radio-button|<macro|name|val|text|<style-with|src-compact|none|<action|<render-button|<equal|<arg|val>|<widget-value|<arg|name>>>|<arg|text>>|<widget-cmd|<merge|(widget-set!
  "|<arg|name>|" "|<arg|val>|")>>|<arg|val>>>>>

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