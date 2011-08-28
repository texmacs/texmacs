<TeXmacs|1.0.7.10>

<style|tmdoc>

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

  <paragraph*|Keyboard shortcuts>

  More complex keyboard shortcuts are obtain by pressing several keys or
  ``modified keys'' in succession. For instance, the shortcut <key|- \<gtr\>>
  corresponds on first pressing the <key|-> key and then the key
  <key|\<gtr\>>. Inside mathematical formulas, this shortcut inserts the
  arrow <math|<op|\<rightarrow\>>>. Similarly, the shortcut <key*|C-x C-f>
  consists of first pressing the keys <prefix|C-> and <key|x> together, and
  next pressing the keys <prefix|C-> and <key|f> again together. In the
  <name|Emacs> ``look and feel'', this shortcut enables you to open a new
  file.

  Some common keyboard prefixes are detailed in the section on <hlink|general
  keyboard rules|../text/keyboard/man-general-rules.en.tm>. In cases when
  <TeXmacs> keyboard shortcuts are superseded by shortcuts from the operating
  system, we notice that <hlink|equivalents for the keyboard
  modifiers|../config/man-config-keyboard.en.tm#kbd-escape-table> can be
  obtained using the <key*|escape> key. For instance, <key*|escape> is
  equivalent to <key|escape> and <key*|escape escape> is equivalent to
  <key|escape escape>.

  Notice that the <TeXmacs> menus and keyboard behavior are <em|contextual>,
  <abbr|i.e.> they depend on the current mode (i.e. text mode or ``math
  mode''), the current language and the position of the cursor inside your
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
    <associate|language|english>
  </collection>
</initial>