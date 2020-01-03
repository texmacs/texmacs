<TeXmacs|1.99.5>

<style|<tuple|source|std|english>>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-gui|1.0>

    <\src-purpose>
      Macros for documentation of GUI elements
    </src-purpose>

    <src-copyright|2001--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(doc tmdoc-markup)>

  <\active*>
    <\src-comment>
      Keyboard shortcuts.
    </src-comment>
  </active*>

  <assign|render-key|<macro|key|<active*|<move|<small|<with|font-family|tt|<with|ornament-color|#e0e0e0|ornament-sunny-color|#f0f0f0|ornament-shadow-color|#c0c0c0|ornament-hpadding|2ln|ornament-vpadding|2ln|ornament-border|2ln|<ornament|<compound|inflate|<arg|key>>>>>>||0.075ex>>>>

  <assign|render-keys|<macro|keys|<extern|tmdoc-render-keys|<quote-arg|keys>>>>

  <assign|key|<macro|shortcut|<extern|tmdoc-key|<quote-arg|shortcut>>>>

  <assign|key*|<macro|shortcut|<extern|tmdoc-key*|<quote-arg|shortcut>>>>

  <assign|shortcut|<macro|command|<extern|tmdoc-shortcut|<quote-arg|command>>>>

  <assign|prefix|<macro|keys|<extern|tmdoc-key|<quote-arg|keys>>>>

  <drd-props|key|arity|1|string|0>

  <drd-props|key*|arity|1|string|0>

  <drd-props|shortcut|arity|1|string|0>

  <drd-props|prefix|arity|1|string|0>

  <\active*>
    <\src-comment>
      <TeXmacs> menus.
    </src-comment>
  </active*>

  <assign|menu-item|<macro|body|<with|font-family|ss|<localize|<arg|body>>>>>

  <assign|menu-extra|<macro|body|<active*|<with|mode|math|\<rightarrow\>>><menu-item|<arg|body>>>>

  <assign|render-menu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|0>><map-args|menu-extra|concat|x|1>>>>

  <assign|menu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|0>><map-args|menu-extra|concat|x|1><index-write|<map-args|menu-item|tuple|x>>>>>

  <assign|submenu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|1>><map-args|menu-extra|concat|x|2><index-write|<map-args|menu-item|tuple|x>>>>>

  <assign|subsubmenu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|2>><map-args|menu-extra|concat|x|3><index-write|<map-args|menu-item|tuple|x>>>>>

  <assign|subsubsubmenu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|3>><map-args|menu-extra|concat|x|4><index-write|<map-args|menu-item|tuple|x>>>>>

  <assign|subsubsubsubmenu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|4>><map-args|menu-extra|concat|x|5><index-write|<map-args|menu-item|tuple|x>>>>>

  <\active*>
    <\src-comment>
      Images.
    </src-comment>
  </active*>

  <assign|icon|<macro|name|<image|<find-file|$TEXMACS_PATH/misc/pixmaps/modern/32x32/table|$TEXMACS_PATH/misc/pixmaps/modern/24x24/main|$TEXMACS_PATH/misc/pixmaps/modern/20x20/mode|$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|http://www.gnu.org/software/texmacs-doc/images/pixmaps|<arg|name>>||2ex||-0.333ex>>>

  <assign|screenshot|<macro|name|<image|<find-file|$TEXMACS_DOC_PATH/images/screenshots|$TEXMACS_HOME_PATH/doc/images/screenshots|$TEXMACS_PATH/doc/images/screenshots|http://www.gnu.org/software/texmacs-doc/images/screenshots|<arg|name>>|0.5w|0.5h||>>>

  <\active*>
    <\src-comment>
      <TeXmacs>-specific GUI elements.
    </src-comment>
  </active*>

  <assign|text-cursor|<macro|<resize|<with|color|red|\|>|0.5w||0.5w|>>>

  <assign|math-cursor|<macro|<math-ignore|<resize|<with|color|#c000ff|\|>|0.5w||0.5w|>>>>

  <assign|cursor|<macro|<math-ignore|<if|<equal|<value|mode>|math>|<math-cursor>|<text-cursor>>>>>

  <assign|small-envbox|<macro|body|<with|color|#e8f0f0|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<table|<row|<cell|<with|color|black|<arg|body>>>>>>>>>>

  <assign|small-focus|<macro|body|<with|color|cyan|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<table|<row|<cell|<with|color|black|<arg|body>>>>>>>>>>

  <assign|big-envbox|<macro|body|<with|color|#e8f0f0|<rigid|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|black|<arg|body>>
  </cell>>>>>>>>>

  <assign|big-focus|<macro|body|<with|color|cyan|<rigid|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|black|<arg|body>>
  </cell>>>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|3>
  </collection>
</initial>