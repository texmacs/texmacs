<TeXmacs|1.99.4>

<style|<tuple|tmdoc|english>>

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
    </cell>|<cell|<math|<block*|<tformat|<cwith|1|1|1|1|cell-tsep|2sep>|<cwith|1|1|1|1|cell-bsep|1pt>|<cwith|1|1|1|1|cell-background|pastel
    grey>|<table|<row|<cell|<small|<with|font-family|ss|Ctrl>>>>>>>>>|<cell|left
    <block*|<tformat|<cwith|1|1|1|1|cell-tsep|2sep>|<cwith|1|1|1|1|cell-bsep|1pt>|<cwith|1|1|1|1|cell-background|pastel
    grey>|<table|<row|<cell|<small|<with|font-family|ss|Alt>>>>>>>
    (<math|\<dag\>>)>|<cell|<math|<block*|<tformat|<cwith|1|1|1|1|cell-valign|B>|<cwith|1|1|1|1|cell-bsep|1sep>|<cwith|1|1|1|1|cell-tsep|1sep>|<cwith|1|1|1|1|cell-background|pastel
    grey>|<table|<row|<cell|<image|<tuple|<#89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7AF40000000473424954080808087C0864880000000970485973000005120000051201887C28AB0000001974455874536F667477617265007777772E696E6B73636170652E6F72679BEE3C1A00000125494441545885EDD7314A04411005D037CBA888890A06060AEEAAAC8A1879082FA118987A172F60642E7804032F60BEA978005104770D6616749C19E9DD6647613E144D43FFAADF55DD1495980D96B08D9D7C1D5B2F891420C116BA25D6C372056F141A68017DA438C235EEF1943B0BB5615A089062B3E2265DACE4E7D6B087F3D01B1491E2CAF7F42D4EEB3454C0E52C0316D16932782B602C20F82FC616D028FE8480B6048D22C145BE16F186D70ADE9DACC31DFEE2FF05EF7507520C2A04D461983B1F04F24AF121BC8DAEE27402DE8F763CCD1B88F27B1A7F84FF5640A22D419B818633106BA0D1C10D1EF01CC88D928114675FF673D8503E94F465335E6398C72E4E6423DA316EF128EB9A13F58268B5948D6D65993BC07A05671453401DAAC4ED7F02B4F05CA282CD20DD0000000049454E44AE426082>|png>|15px|15px||-1px>>>>>>><math|>
    (<math|\<dag\>>)>>|<row|<cell|<name|Apple>>|<cell|<block*|<tformat|<cwith|1|1|1|1|cell-tsep|2sep>|<cwith|1|1|1|1|cell-bsep|1pt>|<cwith|1|1|1|1|cell-background|pastel
    grey>|<table|<row|<cell|<with|font-family|ss|<small|\<place of interest
    sign\> Command>>>>>>>>|<cell|<block*|<tformat|<cwith|1|1|1|1|cell-tsep|2sep>|<cwith|1|1|1|1|cell-bsep|1pt>|<cwith|1|1|1|1|cell-background|pastel
    grey>|<table|<row|<cell|<small|<with|font-family|ss|\<option key\>
    Option>>>>>>> (<math|\<dag\>>)>|<cell|<block*|<tformat|<cwith|1|1|1|1|cell-tsep|2sep>|<cwith|1|1|1|1|cell-bsep|1pt>|<cwith|1|1|1|1|cell-background|pastel
    grey>|<table|<row|<cell|<small|<with|font-family|ss|Ctrl>>>>>>>>>|<row|<cell|fallback
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

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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