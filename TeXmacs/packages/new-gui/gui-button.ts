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
      Customizations
    </src-comment>
  </active*>

  <assign|font|<extern|gui-system-font>>

  <assign|font-base-size|8>

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

  <assign|gui-input-color|white>

  <assign|gui-input-list-color|>

  <assign|gui-input-sunny-color|#b0b0b0>

  <assign|gui-input-shadow-color|#707070>

  <assign|gui-title-bg-color|#c0c0c0>

  <\active*>
    <\src-comment>
      Atomic markup
    </src-comment>
  </active*>

  <assign|icon|<macro|name|<style-with|src-compact|none|<image|<style-with|src-compact|none|<find-file|$TEXMACS_PATH/misc/pixmaps/modern/32x32/table|$TEXMACS_PATH/misc/pixmaps/modern/32x32/settings|$TEXMACS_PATH/misc/pixmaps/modern/24x24/main|$TEXMACS_PATH/misc/pixmaps/modern/20x20/mode|$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|<arg|name>>>||50%||-10%>>>>

  <assign|monochrome|<macro|w|h|col|<resize|<raw-table|<tformat|<twith|table-valign|b>|<cwith|1|-1|1|-1|cell-background|<arg|col>>|<cwith|1|-1|1|-1|cell-width|<arg|w>>|<cwith|1|-1|1|-1|cell-hmode|exact>|<cwith|1|-1|1|-1|cell-height|<arg|h>>|<cwith|1|-1|1|-1|cell-vmode|exact>|<table|<row|<\cell>
    \;
  </cell>>>>>||0px||<arg|h>>>>

  <\active*>
    <\src-comment>
      Buttons that can trigger a scheme action
    </src-comment>
  </active*>

  <assign|gui-contour|<macro|body|<with|shadow-recolor|<value|gui-blur-color>|<drop-contour|<arg|body>>>>>

  <assign|action-button-normal*|<macro|x|<with|ornament-corner|30%|ornament-color|<value|gui-bg-color>|ornament-shadow-color|<value|gui-shadow-color>|ornament-sunny-color|<value|gui-sunny-color>|<ornament|<arg|x>>>>>

  <assign|action-button-hover*|<macro|x|<gui-contour|<with|ornament-corner|30%|ornament-color|<value|gui-bg-color>|ornament-shadow-color|<value|gui-shadow-color>|ornament-sunny-color|<value|gui-sunny-color>|<ornament|<arg|x>>>>>>

  <assign|action-button-pressed*|<macro|x|<with|ornament-corner|30%|ornament-color|<value|gui-bg-color>|ornament-sunny-color|<value|gui-shadow-color>|ornament-shadow-color|<value|gui-sunny-color>|<ornament|<arg|x>>>>>

  \;

  <assign|action-button*|<macro|x|cmd|<mark*|<arg|x>|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<action-button-pressed*|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<action-button-hover*|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<action-button-normal*|<arg|x>>|gui-on-select|<arg|cmd>>>>>>>

  <drd-props|action-button*|arity|2|accessible|0>

  \;

  <assign|action-button-normal|<macro|x|<action-button-normal*|<arg|x><htab|0mm>>>>

  <assign|action-button-hover|<macro|x|<action-button-hover*|<arg|x><htab|0mm>>>>

  <assign|action-button-pressed|<macro|x|<action-button-pressed*|<arg|x><htab|0mm>>>>

  <assign|action-button|<macro|x|cmd|<mark*|<arg|x>|<action-button*|<arg|x><htab|0mm>|<arg|cmd>>>>>

  <\active*>
    <\src-comment>
      Menu buttons
    </src-comment>
  </active*>

  <assign|menu-button-normal*|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-bg-color>|ornament-shadow-color|<value|gui-bg-color>|ornament-sunny-color|<value|gui-bg-color>|<ornament|<space|0cm|-0.3em|0.9em><arg|x>>>>>

  <assign|menu-button-hover*|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-select-color>|ornament-shadow-color|<value|gui-select-color>|ornament-sunny-color|<value|gui-select-color>|<ornament|<space|0cm|-0.3em|0.9em><arg|x>>>>>

  <assign|menu-button-pressed*|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-select-color>|ornament-shadow-color|<value|gui-sunny-color>|ornament-sunny-color|<value|gui-shadow-color>|<ornament|<space|0cm|-0.3em|0.9em><arg|x>>>>>

  \;

  <assign|menu-button*|<macro|x|cmd|<mark*|<arg|x>|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<menu-button-pressed*|<arg|x>>|gui-on-select|<arg|cmd>>|mouse-over|<relay|<menu-button-hover*|<arg|x>>|gui-on-select|<arg|cmd>>|<relay|<menu-button-normal*|<arg|x>>|gui-on-select|<arg|cmd>>>>>>>

  <drd-props|menu-button*|arity|2|accessible|0>

  \;

  <assign|menu-button-normal|<macro|x|<menu-button-normal*|<arg|x><htab|0mm>>>>

  <assign|menu-button-hover|<macro|x|<menu-button-hover*|<arg|x><htab|0mm>>>>

  <assign|menu-button-pressed|<macro|x|<menu-button-pressed*|<arg|x><htab|0mm>>>>

  <assign|menu-button|<macro|x|cmd|<mark*|<arg|x>|<menu-button*|<arg|x><htab|0mm>|<arg|cmd>>>>>

  <\active*>
    <\src-comment>
      Alternate menu buttons
    </src-comment>
  </active*>

  <assign|menu-button-xnormal*|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-shadow-color|<value|gui-shadow-color>|ornament-sunny-color|<value|gui-sunny-color>|<ornament|<space|0cm|-0.3em|0.9em><arg|x>>>>>

  <assign|menu-button-xhover*|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-color|<value|gui-select-color>|ornament-shadow-color|<value|gui-shadow-color>|ornament-sunny-color|<value|gui-sunny-color>|<ornament|<space|0cm|-0.3em|0.9em><arg|x>>>>>

  <assign|menu-button-xpressed*|<macro|x|<with|ornament-shape|classic|ornament-border|1ln|ornament-vpadding|2ln|ornament-shadow-color|<value|gui-sunny-color>|ornament-sunny-color|<value|gui-shadow-color>|<ornament|<space|0cm|-0.3em|0.9em><arg|x>>>>>

  \;

  <assign|with-explicit-buttons|<macro|body|<with|menu-button-normal*|<value|menu-button-xnormal*>|menu-button-hover*|<value|menu-button-xhover*>|<arg|body>>>>

  <assign|with-pressed-buttons|<macro|body|<with|menu-button-normal*|<value|menu-button-xpressed*>|menu-button-hover*|<value|menu-button-pressed*>|menu-button-pressed*|<value|menu-button-xhover*>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Toggles
    </src-comment>
  </active*>

  <assign|toggle-off|<macro|<math|\<box\>>>>

  <assign|toggle-on|<macro|<math|\<blacksquare\>>>>

  <assign|toggle-off-hover|<gui-contour|<toggle-off>>>

  <assign|toggle-on-hover|<gui-contour|<toggle-on>>>

  <assign|toggle-on-button|<macro|on|cmd|<mark*|<arg|x>|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<toggle-off-hover>|gui-on-toggle|<arg|cmd>>|mouse-over|<relay|<toggle-on-hover>|gui-on-toggle|<arg|cmd>>|<relay|<toggle-on>|gui-on-toggle|<arg|cmd>>>>>>>

  <assign|toggle-off-button|<macro|on|cmd|<mark*|<arg|x>|<style-with|src-compact|none|<dynamic-case|click,drag|<relay|<toggle-on-hover>|gui-on-toggle|<arg|cmd>>|mouse-over|<relay|<toggle-off-hover>|gui-on-toggle|<arg|cmd>>|<relay|<toggle-off>|gui-on-toggle|<arg|cmd>>>>>>>

  <assign|toggle-button|<macro|on|cmd|<compound|<if|<arg|on>|<value|toggle-on-button>|<value|toggle-off-button>>|<arg|on>|<arg|cmd>>>>

  <\active*>
    <\src-comment>
      Table markup
    </src-comment>
  </active*>

  <assign|raw-table|<macro|body|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|1|-1|cell-hpart|0>|<cwith|1|-1|1|-1|cell-vpart|0>|<arg|body>>>>

  <assign|raw-table*|<macro|body|<raw-table|<tformat|<cwith|1|-1|1|1|cell-halign|r>|<arg|body>>>>>

  <assign|align-table|<macro|body|<raw-table*|<tformat|<cwith|2|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|1|cell-rsep|1spc>|<arg|body>>>>>

  <assign|glue|<macro|hext|vext|w|h|<space|<arg|w>|0px|<arg|h>><if|<arg|hext>|<htab|0px>>>>

  <assign|hlist|<xmacro|items|<extern|gui-hlist-table|raw-table|<quote-arg|items>>>>

  <assign|vlist|<xmacro|items|<extern|gui-vlist-table|raw-table|<quote-arg|items>>>>

  <assign|tiled|<xmacro|items|<extern|gui-tiled|raw-table|<quote-arg|items>>>>

  <assign|tiled*|<xmacro|items|<extern|gui-tiled|raw-table*|<quote-arg|items>>>>

  <assign|align-tiled|<xmacro|items|<extern|gui-tiled|align-table|<quote-arg|items>>>>

  <drd-props|hlist|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|vlist|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|tiled|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|align-tiled|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|tiled**|arity|<tuple|repeat|1|1>|accessible|all>

  <\active*>
    <\src-comment>
      Choice lists
    </src-comment>
  </active*>

  <assign|choice-list|<xmacro|items|<extern|gui-choice-list|vlist|menu-button|menu-button-pressed|<quote-arg|items>>>>

  <assign|check-list|<xmacro|items|<extern|gui-check-list|vlist|menu-button|menu-button-pressed|<quote-arg|items>>>>

  <drd-props|choice-list|arity|<tuple|repeat|2|1>|accessible|all>

  <drd-props|check-list|arity|<tuple|repeat|2|1>|accessible|all>

  <\active*>
    <\src-comment>
      Input fields
    </src-comment>
  </active*>

  <assign|input-field|<macro|type|cmd|width|x|<with|ornament-corner|30%|ornament-color|<value|gui-input-color>|ornament-shadow-color|<value|gui-input-shadow-color>|ornament-sunny-color|<value|gui-input-sunny-color>|<ornament|<if|<equal|<arg|width>|>|<inflate|<arg|x>>|<clipped|<inflate|<arg|x>>|||<arg|width>|>>>>>>

  <assign|input-popup|<macro|type|cmd|width|x|y|<input-field|<arg|type>|<arg|cmd>|<arg|width>|<focus-balloon|<arg|x>|<arg|y>|left|Bottom>>>>

  <assign|input-list|<macro|type|cmd|width|x|y|<with|gui-input-color|<value|gui-input-list-color>|<input-field|<arg|type>|<arg|cmd>|<arg|width>|<popup-balloon|<arg|x>|<arg|y>|left|Bottom>>>>>

  <drd-props|input-field|arity|4|accessible|3>

  <drd-props|input-popup|arity|5|accessible|3>

  <drd-props|input-list|arity|5|accessible|none>

  <\active*>
    <\src-comment>
      Tabs
    </src-comment>
  </active*>

  <assign|tabs|<\macro|names|bodies>
    <wide-raw-table|<tformat|<cwith|1|1|1|1|cell-hyphen|n>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-bsep|1spc>|<cwith|1|1|1|1|cell-tsep|1spc>|<table|<row|<cell|<arg|names>>>|<row|<\cell>
      <arg|bodies>
    </cell>>>>>
  </macro>>

  <assign|tabs-bar|<value|hlist>>

  <assign|tabs-body|<value|switch>>

  <assign|xtabs-body|<macro|body|<arg|body>>>

  <assign|active-tab|<macro|name|<arg|name><space|1em>>>

  <assign|passive-tab|<macro|name|<action|<greyed|<arg|name><space|1em>>|tab-select|<arg|name>>>>

  <drd-props|tabs|arity|2|accessible|all>

  <drd-props|tabs-bar|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|tabs-body|arity|<tuple|repeat|1|1>|accessible|all>

  <\active*>
    <\src-comment>
      Styling
    </src-comment>
  </active*>

  <assign|text-opaque|<macro|body|<arg|body>>>

  <assign|text-center|<macro|body|<center|<arg|body>>>>

  <assign|text-button|<macro|body|<menu-button-normal*|<arg|body>>>>

  <assign|text-pressed|<macro|body|<menu-button-pressed*|<arg|body>>>>

  \;

  <assign|wide-raw-table|<macro|body|<raw-table|<tformat|<twith|table-width|1par>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-background|<value|gui-bg-color>>|<arg|body>>>>>

  <assign|wide-raw-cell|<style-with|src-compact|all|<macro|body|<wide-raw-table|<tformat|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>>

  <assign|title-style|<macro|body|<with|gui-bg-color|<value|gui-title-bg-color>|<wide-raw-cell|<large|<arg|body>>>>>>

  <assign|section-style|<macro|body|<with|gui-bg-color|<value|gui-title-bg-color>|<wide-raw-cell|<arg|body>>>>>

  <assign|subsection-style|<macro|body|<with|gui-bg-color|<value|gui-title-bg-color>|<wide-raw-cell|<arg|body>>>>>

  <assign|section-tabs-style|<macro|body|<arg|body>>>

  <assign|plain-style|<macro|body|<normal-size|<arg|body>>>>

  <assign|discrete-style|<macro|body|<small|<arg|body>>>>

  <\active*>
    <\src-comment>
      Resizing
    </src-comment>
  </active*>

  <assign|minipar|<macro|body|w|h|<raw-table|<tformat|<cwith|1|1|1|1|cell-width|<arg|w>>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-height|<arg|h>>|<cwith|1|1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-valign|T>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <assign|minipar*|<macro|body|w|h|<clipped|<minipar|<arg|body>|<arg|w>|<arg|h>>||<minus|1t|<arg|h>>|<plus|1l|<arg|w>>|>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>