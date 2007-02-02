<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-utils|1.0>

    <\src-purpose>
      Standard GUI utilities
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

  <assign|bg-color|light grey>

  <assign|page-medium|automatic>

  <assign|scroll-bars|false>

  <assign|font-family|ss>

  <assign|par-sep|0.2em>

  <assign|par-par-sep|0em>

  <assign|par-line-sep|0em>

  <assign|page-screen-left|0.5em>

  <assign|page-screen-right|0.5em>

  <assign|page-screen-top|0.5em>

  <assign|page-screen-bottom|0.5em>

  <\active*>
    <\src-comment>
      Gui environment variables
    </src-comment>
  </active*>

  <assign|gui-small-sep|0.25em>

  <assign|gui-med-sep|0.5em>

  <assign|gui-big-sep|1em>

  <assign|gui-up-color|light grey>

  <assign|gui-down-color|#e0e0e0>

  <assign|gui-input-color|#f0f0f0>

  <assign|gui-sun-color|white>

  <assign|gui-shadow-color|grey>

  <assign|gui-toggle-color|pastel blue>

  <assign|gui-toggle-type|circle>

  <assign|gui-marker-type|checked>

  <\active*>
    <\src-comment>
      Tables
    </src-comment>
  </active*>

  <assign|gui-dense-table|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0px>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|-1|1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|gui-normal-table|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|gui-airy-table|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-big-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-big-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|gui-dense-bar|<macro|body|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-lsep|0px>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|-1|1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|gui-normal-bar|<macro|body|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|-1|1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Framed boxes
    </src-comment>
  </active*>

  <assign|gui-small-framed|<macro|main-col|border1-col|border2-col|pad|body|<gui-dense-table|<tformat|<twith|table-hmode|auto>|<cwith|1|1|1|-1|cell-background|<arg|border1-col>>|<cwith|3|3|1|-1|cell-background|<arg|border2-col>>|<cwith|1|-1|3|3|cell-background|<arg|border2-col>>|<cwith|1|-1|1|1|cell-background|<arg|border1-col>>|<cwith|1|-1|1|1|cell-width|1px>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|1px>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|1|1|1|-1|cell-height|1px>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|3|3|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-vmode|exact>|<cwith|2|2|2|2|cell-lsep|<arg|pad>>|<cwith|2|2|2|2|cell-rsep|<arg|pad>>|<cwith|2|2|2|2|cell-bsep|<arg|pad>>|<cwith|2|2|2|2|cell-tsep|<arg|pad>>|<cwith|2|2|2|2|cell-vcorrect|a>|<cwith|2|2|2|2|cell-hyphen|n>|<cwith|2|2|2|2|cell-hpart|1>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|<arg|body>>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <assign|gui-line-framed|<macro|main-col|border1-col|border2-col|pad|body|<gui-dense-table|<tformat|<cwith|1|1|1|-1|cell-background|<arg|border1-col>>|<cwith|3|3|1|-1|cell-background|<arg|border2-col>>|<cwith|1|-1|1|1|cell-background|<arg|border1-col>>|<cwith|1|-1|1|1|cell-width|1px>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|1|1|-1|cell-height|1px>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|3|3|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-vmode|exact>|<cwith|2|2|2|2|cell-background|<arg|main-col>>|<cwith|2|2|2|2|cell-lsep|<arg|pad>>|<cwith|2|2|2|2|cell-rsep|<arg|pad>>|<cwith|2|2|2|2|cell-bsep|<arg|pad>>|<cwith|2|2|2|2|cell-tsep|<arg|pad>>|<cwith|2|2|2|2|cell-vcorrect|a>|<cwith|2|2|2|2|cell-hyphen|n>|<cwith|2|2|2|2|cell-hpart|1>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|<arg|body>>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <assign|gui-big-framed|<macro|main-col|border1-col|border2-col|pad|body|<gui-dense-table|<tformat|<cwith|1|1|1|-1|cell-background|<arg|border1-col>>|<cwith|3|3|1|-1|cell-background|<arg|border2-col>>|<cwith|1|-1|3|3|cell-background|<arg|border2-col>>|<cwith|1|-1|1|1|cell-background|<arg|border1-col>>|<cwith|1|-1|1|1|cell-width|1px>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|1px>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|1|1|1|-1|cell-height|1px>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|3|3|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-vmode|exact>|<cwith|2|2|2|2|cell-background|<arg|main-col>>|<cwith|2|2|2|2|cell-lsep|<arg|pad>>|<cwith|2|2|2|2|cell-rsep|<arg|pad>>|<cwith|2|2|2|2|cell-bsep|<arg|pad>>|<cwith|2|2|2|2|cell-tsep|<arg|pad>>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<\cell>
    <arg|body>
  </cell>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  \;

  <assign|gui-big-raise|<style-with|src-compact|none|<macro|body|<gui-big-framed|<value|gui-up-color>|<value|gui-sun-color>|<value|gui-shadow-color>|<value|gui-med-sep>|<arg|body>>>>>

  <assign|gui-big-lower|<style-with|src-compact|none|<macro|body|<gui-big-framed|<value|gui-down-color>|<value|gui-shadow-color>|<value|gui-sun-color>|<value|gui-med-sep>|<arg|body>>>>>

  <assign|gui-big-input|<style-with|src-compact|none|<macro|body|<gui-big-framed|<value|gui-input-color>|<value|gui-shadow-color>|<value|gui-sun-color>|<value|gui-med-sep>|<arg|body>>>>>

  <assign|gui-small-raise|<style-with|src-compact|none|<macro|body|<gui-small-framed|<value|gui-up-color>|<value|gui-sun-color>|<value|gui-shadow-color>|<value|gui-small-sep>|<arg|body>>>>>

  <assign|gui-small-lower|<style-with|src-compact|none|<macro|body|<gui-small-framed|<value|gui-down-color>|<value|gui-shadow-color>|<value|gui-sun-color>|<value|gui-small-sep>|<arg|body>>>>>

  <assign|gui-small-input|<style-with|src-compact|none|<macro|body|<gui-small-framed|<value|gui-input-color>|<value|gui-shadow-color>|<value|gui-sun-color>|<value|gui-small-sep>|<arg|body>>>>>

  <assign|gui-line-raise|<style-with|src-compact|none|<macro|body|<gui-line-framed|<value|gui-up-color>|<value|gui-sun-color>|<value|gui-shadow-color>|<value|gui-small-sep>|<arg|body>>>>>

  <assign|gui-line-lower|<style-with|src-compact|none|<macro|body|<gui-line-framed|<value|gui-down-color>|<value|gui-shadow-color>|<value|gui-sun-color>|<value|gui-small-sep>|<arg|body>>>>>

  <assign|gui-line-input|<style-with|src-compact|none|<macro|body|<gui-line-framed|<value|gui-input-color>|<value|gui-shadow-color>|<value|gui-sun-color>|<value|gui-small-sep>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Switches
    </src-comment>
  </active*>

  <assign|gui-centered-bar|<macro|l|c|r|<gui-dense-table|<tformat|<cwith|1|-1|1|-1|cell-hyphen|n>|<cwith|1|1|1|1|cell-hpart|1>|<cwith|1|1|3|3|cell-hpart|1>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|2|2|cell-halign|c>|<cwith|1|1|1|1|cell-halign|l>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|>|<cwith|1|1|2|2|cell-hmode|auto>|<table|<row|<\cell>
    <arg|l>
  </cell>|<cell|<arg|c>>|<\cell>
    <arg|r>
  </cell>>>>>>>

  <assign|gui-centered-switch|<macro|body|<style-with|src-compact|none|<gui-centered-bar|<gui-hrule-sep|0px|1px|0px|5px|<value|gui-down-color>>|<arg|body>|<gui-hrule-sep|0px|0px|1px|5px|<value|gui-down-color>>>>>>

  <\active*>
    <\src-comment>
      Vertical separators
    </src-comment>
  </active*>

  <assign|gui-hrule-sep|<macro|around|left|right|height|col|<gui-dense-table|<tformat|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|1|1|-1|cell-height|<arg|around>>|<cwith|2|2|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-height|<arg|height>>|<cwith|4|4|1|-1|cell-height|1px>|<cwith|5|5|1|-1|cell-height|<arg|around>>|<cwith|2|2|1|-1|cell-background|<value|gui-shadow-color>>|<cwith|4|4|1|-1|cell-background|<value|gui-sun-color>>|<cwith|3|3|1|1|cell-background|<value|gui-shadow-color>>|<cwith|3|3|2|2|cell-background|<arg|col>>|<cwith|3|3|3|3|cell-background|<value|gui-sun-color>>|<cwith|1|-1|1|1|cell-width|<arg|left>>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|<arg|right>>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <assign|gui-vspace-sep|<macro|height|<gui-dense-table|<tformat|<cwith|1|-1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-height|<arg|height>>|<table|<row|<cell|>>>>>>>

  <assign|gui-hrule|<macro|<gui-hrule-sep|<value|gui-med-sep>|0px|0px|0px|<value|gui-down-color>>>>

  <assign|gui-vspace|<macro|<gui-vspace-sep|<value|gui-med-sep>>>>

  <assign|gui-tab|<macro|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|0px>|<cwith|1|1|1|1|cell-rsep|0px>|<cwith|1|1|1|1|cell-bsep|0px>|<cwith|1|1|1|1|cell-tsep|0px>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-vcorrect|n>|<table|<row|<\cell>
    \;
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Toggles
    </src-comment>
  </active*>

  <assign|magnify|<macro|factor|body|<with|magnification|<times|<value|magnification>|<arg|factor>>|<arg|body>>>>

  <assign|gui-centered-cell|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-width|1em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|1em>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|gui-circle-toggle|<macro|body|<style-with|src-compact|none|<superpose|<gui-centered-cell|<magnify|2.5|<style-with|src-compact|none|<move|<with|bg-color|<value|color>|<with|mode|math|color|<value|gui-toggle-color>|\<bullet\>>>|0.02em|-0.055em>>>>|<gui-centered-cell|<magnify|1.1|<with|mode|math|\<bigcirc\>>>>|<gui-centered-cell|<arg|body>>>>>>

  <assign|gui-square-toggle|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-width|0.9em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|0.9em>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-background|<value|gui-toggle-color>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|gui-generic-toggle|<macro|body|<compound|<merge|gui-|<value|gui-toggle-type>|-toggle>|<arg|body>>>>

  <assign|gui-bullet-marker|<macro|on|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<magnify|1.5|<move|<with|mode|math|\<bullet\>>|0.02em|-0.055em>>|>>>>

  <assign|gui-checked-marker|<macro|on|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<magnify|1.25|<move|<with|mode|math|\<checked\>>|0.2em|0.2em>>|>>>>

  <assign|gui-generic-marker|<macro|on|<compound|<merge|gui-|<value|gui-marker-type>|-marker>|<arg|on>>>>

  <assign|gui-toggle|<macro|on|<gui-generic-toggle|<gui-generic-marker|<arg|on>>>>>

  <assign|gui-button-toggle|<macro|on|text|<style-with|src-compact|none|<if|<equal|<arg|on>|true>|<gui-small-lower|<arg|text>>|<gui-small-raise|<arg|text>>>>>>

  <\active*>
    <\src-comment>
      Buttons
    </src-comment>
  </active*>

  <assign|gui-button-bar|<macro|body|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-lsep|0px>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|-1|1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>