<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Conventions for this manual>

  <paragraph*|Menu entries>

  Throughout the <TeXmacs> manual, menu entries will be typeset using a
  <em|sans serif> font, like in <menu|Document>, <menu|File|Load> or
  <menu|Format|Font shape|Italic>.

  <paragraph*|Keyboard modifiers>

  <TeXmacs> makes use of the following keyboard modifiers:

  <\description>
    <item*|<prefix|S->>For shift key combinations.

    <item*|<prefix|C->>For control key combinations.

    <item*|<prefix|A->>For alternate key combinations.

    <item*|<prefix|M->>For meta key combinations.
  </description>

  For instance, <key*|M-S-x> stands for the action which consists of
  simultaneously pressing the three keys <prefix|M->, <prefix|S-> and
  <key|x>.

  The actual keyboard modifier keys depend on your system as indicated in the
  following table

  <\big-table>
    \;

    <descriptive-table|<tformat|<cwith|1|-1|3|3|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-row-span|1>|<cwith|1|-1|1|-1|cell-col-span|1>|<cwith|1|-1|1|-1|cell-bsep|3spc>|<cwith|1|-1|1|-1|cell-tsep|3sep>|<cwith|1|1|1|-1|cell-bsep|1spc>|<cwith|2|-1|1|-1|cell-bsep|2sep>|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-valign|c>|<cwith|1|-1|1|-1|cell-hyphen|n>|<table|<row|<cell|>|<cell|<key*|C->>|<cell|<key*|A->>|<cell|<key*|M->>>|<row|<\cell>
      <name|Windows> or <name|Linux>/<name|Unix>

      \ with <name|Windows> keyboard
    </cell>|<cell|<render-key|Ctrl>>|<cell|left <render-key|Alt>
    (<math|\<dag\>>)>|<cell|<render-key|<math|<shift|<draw-over|<phantom|ihj>|<with|gr-frame|<tuple|scale|0.707111cm|<tuple|0.5gw|0.415159gh>>|gr-mode|<tuple|group-edit|move>|gr-fill-color|black|gr-color|white|gr-snap|<tuple|control
    point|grid point|grid curve point|curve-grid intersection|curve-curve
    intersection|text border point|text border>|magnify|0.707106780759852|<graphics|<with|fill-color|black|<cline|<point|-0.190551|-0.0360167>|<point|0.149110332054505|-0.126306389734092>|<point|0.151690038364863|0.212495039026326>|<point|-0.190550998809366|0.148862283370816>>>|<with|color|white|fill-color|black|<with|color|white|fill-color|black|<line|<point|-0.193989980156105|0.049973520664109>|<point|0.145671135070777|0.0525532105172642>>>>|<with|color|white|fill-color|black|<line|<point|-0.0529667|0.176379>|<point|-0.0521067601534595|-0.072132557216563>>>>>|0cm>||2ln>>><math|><math|>
    (<math|\<dag\>>)>>|<row|<cell|<name|Apple>>|<cell|<render-key|<with|font-family|ss|^<small|<with|font-family|ss|
    Control>>>> (<math|\<dag\>>)>|<cell|<render-key|<with|font-family|ss|\<option
    key\><small|<with|font-family|ss| Option>>>>
    (<math|\<dag\>>)>|<cell|<render-key|<with|font-family|ss|<small|\<place
    of interest sign\> Command>>> (<math|\<dag\>>)>>|<row|<cell|fallback
    combination>|<cell|<key*|escape escape escape>>|<cell|<key*|escape
    escape>>|<cell|<key*|escape>>>>>>
  <|big-table>
    Actual modifier keys on common platforms.

    <tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|10cm>|<cwith|1|1|1|1|cell-hmode|min>|<table|<row|<\cell>
      <math|\<dag\>> Some modifier key combinations are preempted by the
      operating system. The behavior may be different for the right and left
      modifier key.
    </cell>>>>>
  </big-table>

  <paragraph*|Keyboard shortcuts>

  Keyboard shortcuts are obtained by pressing several keys or \Pmodified
  keys\Q in succession. For instance, the shortcut <key|- \<gtr\>>
  corresponds on first pressing the <key|-> key and then the key
  <key|\<gtr\>>. Inside mathematical formulas, this shortcut inserts the
  arrow <math|<op|\<rightarrow\>>>. Similarly, the shortcut <key*|C-x C-f>
  consists of first pressing the keys <prefix|C-> and <key|x> together, and
  next pressing the keys <prefix|C-> and <key|f> again together. In the
  <name|Emacs> \Plook and feel\Q, this shortcut enables you to open a new
  file.

  Some common keyboard prefixes are detailed in the section on <hlink|general
  keyboard rules|../text/keyboard/man-general-rules.en.tm>. In cases when
  <TeXmacs> keyboard shortcuts are superseded by shortcuts from the operating
  system, <hlink|equivalents for the keyboard
  modifiers|../config/man-config-keyboard.en.tm#kbd-escape-table> can be
  obtained using the <key*|escape> key. For instance, <key*|escape> is
  equivalent to <key|escape> and <key*|escape escape> is equivalent to
  <key|escape escape>.

  Notice that the <TeXmacs> menus and keyboard behavior are <em|contextual>,
  <abbr|i.e.> they depend on the current mode (i.e. text mode or \Pmath
  mode\Q), the current language and the position of the cursor inside your
  document. For instance, inside math mode, you have special keyboard
  shortcuts which are handy for typing mathematical formulas, but which are
  useless in text mode.

  <paragraph*|Special keys>

  On some platforms, some special keys such as the Return key are depicted by
  short glyphs. Below follows the table with all such special keys and there
  meaning.

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|3|3|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-row-span|1>|<cwith|1|-1|1|-1|cell-col-span|1>|<cwith|7|11|1|1|cell-lborder|1ln>|<cwith|7|11|1|2|cell-halign|l>|<cwith|7|11|1|2|cell-row-span|1>|<cwith|7|11|1|2|cell-col-span|1>|<table|<row|<cell|Key>|<cell|Meaning>|<cell|Key>|<cell|Meaning>>|<row|<cell|<key*|S->>|<cell|Shift
  modifier>|<cell|<key*|left>>|<cell|Cursor
  left>>|<row|<cell|<key*|capslock>>|<cell|Caps
  lock>|<cell|<key*|right>>|<cell|Cursor right>>|<row|<cell|<key*|C->>|<cell|Control
  modifier>|<cell|<key*|up>>|<cell|Cursor
  up>>|<row|<cell|<key*|A->>|<cell|Alternate
  modifier>|<cell|<key*|down>>|<cell|Cursor
  down>>|<row|<cell|<key*|M->>|<cell|Meta
  modifier>|<cell|<key*|home>>|<cell|Home>>|<row|<cell|<key*|return>>|<cell|Return>|<cell|<key*|end>>|<cell|End>>|<row|<cell|<key*|delete>>|<cell|Forward
  delete>|<cell|<key*|pageup>>|<cell|Page
  up>>|<row|<cell|<key*|backspace>>|<cell|Backspace>|<cell|<key*|pagedown>>|<cell|Page
  down>>|<row|<cell|<key*|escape>>|<cell|Escape>|<cell|<key*|space>>|<cell|Space>>|<row|<cell|<key*|tab>>|<cell|Tab>|<cell|>|<cell|>>>>>>
    Special keys.
  </big-table>

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>