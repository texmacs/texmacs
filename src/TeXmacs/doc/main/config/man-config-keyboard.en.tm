<TeXmacs|1.0.7.17>

<style|tmdoc>

<\body>
  <tmdoc-title|Keyboard configuration>

  The behaviour of keyboard inside <TeXmacs> depends on a few user
  preferences, as specified in the menu <menu|Edit|Preferences>:

  <\itemize>
    <item>The <hlink|<menu|Look and feel>|man-preferences.en.tm#preferences:look-and-feel>
    determines the main rules for keyboard shortcuts and attempts to make the
    behaviour as close as possible to the standards for the selected look and
    feel.

    <item>Some minor customizations are possible via
    <hlink|<menu|Edit|Preferences|Keyboard>|man-preferences.en.tm#preferences:keyboard>.
  </itemize>

  We will now detail specific issues related to the keyboard configuration on
  various systems.

  Please refer to the section on <hlink|general
  conventions|../start/man-conventions.en.tm> for explanations on the way
  keyboard shortcuts are printed in this manual. For more information on
  keyboard shortcuts, we refer to the general section on how the
  <hlink|master the keyboard|../text/keyboard/man-keyboard.en.tm>.

  <paragraph*|Standard conformance>

  <TeXmacs> attempts to be as standard-conformant regarding the various look
  and feels. However, there are a few general situations in which <TeXmacs>
  reserves some keyboard shortcuts for the sake of user-friendliness:

  <\itemize>
    <item>The function keys <key|F5>--<key|F12> are reserved for special
    actions.

    <item>Most standards admit a ``principal modifier key'' for forming
    keyboard shortcuts (<prefix|std><nbsp>for your look and feel) and
    sometimes another modifier key for other shortcuts (<abbr|e.g.> the
    <key|windows> key under <name|Windows> and <prefix|A-> under <name|Mac
    OS>). The remaining free modifier (<prefix|cmd> for your look and feel)
    is reserved for <TeXmacs>.

    <item><TeXmacs> contains many keyboard macros involving one or more
    modifier keys and the special keys <key|left>, <key|right>, <key|up>,
    <key|down>, <key|home>, <key|end>, <key|pageup>, <key|pagedown>,
    <key|backspace>, <key|delete>, <key|space>, <key|tab> and <key|return>.
    The behaviour of shortcuts of this kind is occasionally non standard.
  </itemize>

  <paragraph*|Potential conflicts>

  The <TeXmacs>-specific shortcuts are rarely in conflict with standard
  conventions. <space|0.2spc>Nevertheless, in
  table<nbsp><reference|kbd-conflict-table>, we have displayed some more or
  less standard shortcuts, which might work in other applications, but which
  will usually not work inside <TeXmacs>.

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|24|24|1|4|cell-halign|l>|<cwith|27|27|1|4|cell-halign|l>|<cwith|6|6|1|4|cell-halign|l>|<cwith|8|8|1|4|cell-halign|l>|<cwith|7|7|3|4|cell-halign|l>|<cwith|7|7|3|4|cell-halign|l>|<cwith|8|8|3|4|cell-halign|l>|<cwith|8|8|3|4|cell-halign|l>|<cwith|11|11|1|4|cell-halign|l>|<table|<row|<cell|Look
  and feel>|<cell|Shortcut>|<cell|Alternative>|<cell|Meaning>>|<row|<cell|Emacs>|<cell|<key|F10>>|<cell|>|<cell|Show
  menu bar in window>>|<row|<cell|Emacs>|<cell|<key|M-!>>|<cell|>|<cell|Shell
  command>>|<row|<cell|Emacs>|<cell|<key|M-'>/<key|M-`>/<key|M-^>>|<cell|>|<cell|Needed
  for <TeXmacs> accents>>|<row|<cell|Emacs>|<cell|<key|M-/>/<key|M-\\>/<key|M-:>/<key|M-;>>|<cell|>|<cell|>>|<row|<cell|Emacs>|<cell|<key|M-left>/<key|M-right>>|<cell|<key|C-left>/<key|C-right>>|<cell|Move
  word back/forward>>|<row|<cell|Emacs>|<cell|<key|M-a>/<key|M-e>>|<cell|<key|C-up>/<key|C-down>>|<cell|Move
  paragraph back/forward>>|<row|<cell|Emacs>|<cell|<key|M-b>/<key|M-f>>|<cell|<key|C-left>/<key|C-right>>|<cell|Move
  word back/forward>>|<row|<cell|Emacs>|<cell|<key|M-l>/<key|M-t>>|<cell|>|<cell|Locase/transpose
  words (not impl.)>>|<row|<cell|Windows>|<cell|<key|F5>>|<cell|>|<cell|Refresh/Switch
  to next pane>>|<row|<cell|Windows>|<cell|<key|F6>/<key|C-F6>/<key|C-S-F6>>|<cell|>|<cell|Switch
  to next/previous pane/tab>>|<row|<cell|Windows>|<cell|<key|C-space>>|<cell|>|<cell|Remove
  formatting>>|<row|<cell|Windows>|<cell|<key|C-tab>>|<cell|>|<cell|Switch to
  next child window>>|<row|<cell|Windows>|<cell|<key|C-backspace>/<key|C-delete>>|<cell|>|<cell|Delete
  word>>|<row|<cell|Mac OS>|<cell|<key|C-F5>/<key|C-F6>/<key|C-S-F6>>|<cell|>|<cell|Move
  focus to toolbar/panels>>|<row|<cell|Mac
  OS>|<cell|<key|C-F7>>|<cell|>|<cell|Override keyboard access
  mode>>|<row|<cell|Mac OS>|<cell|<key|F9>/<key|F10>>|<cell|>|<cell|Tile or
  untile windows>>|<row|<cell|Mac OS>|<cell|<key|F11>/<key|F12>>|<cell|>|<cell|Hide
  or show windows/dashboard>>|<row|<cell|Mac
  OS>|<cell|<key|tab>/<key|S-tab>>|<cell|>|<cell|Navigate through
  controls>>|<row|<cell|Mac OS>|<cell|<key|C-tab>,
  <key|C-S-tab>>|<cell|>|<cell|Move focus within control
  groups>>|<row|<cell|Mac OS>|<cell|<key|C-space>/<key|M-C-space>>|<cell|>|<cell|Toggle
  between input sources>>|<row|<cell|Mac OS>|<cell|<key|C-left>/<key|C-right>>|<cell|<key|structured:move
  left>/<key|structured:move right>>|<cell|Move one cell left/right in
  table>>|<row|<cell|Mac OS>|<cell|<key|C-up>/<key|C-down>>|<cell|<key|structured:move
  up>/<key|structured:move down>>|<cell|Move one cell up/down in
  table>>|<row|<cell|Mac OS>|<cell|<key|home>/<key|end>>|<cell|<key|M-up>/<key|M-down>>|<cell|Move
  to start/end of document>>|<row|<cell|Mac OS>|<cell|<key|A-pageup>,
  <key|C-up>, <key|C-pageup>>|<cell|<key|pageup>>|<cell|Page
  up>>|<row|<cell|Mac OS>|<cell|<key|A-pagedown>, <key|C-down>,
  <key|C-pagedown>>|<cell|<key|pagedown>>|<cell|Page down>>|<row|<cell|Mac
  OS>|<cell|<key|C-a>/<key|C-e>>|<cell|<key|A-up>/<key|A-down>>|<cell|Move to
  start/end of block>>>>>>
    <label|kbd-conflict-table>Some shortcuts that might work in other
    applications, but usually not in <TeXmacs>.
  </big-table>

  <paragraph*|System-wide shortcuts which may take precedence>

  In addition to the above standard shortcuts, some system-wide applications
  may define additional global shortcuts, which take precedence over the
  <TeXmacs> shortcuts. For instance, under <name|Mac OS X>, the application
  <name|Spaces> uses the shortcuts <key|C-left>, <key|C-right>, <key|C-up>,
  <key|C-down>, <key|C-1>, <key|C-2>, <key|C-3> and <key|C-4> to switch
  between multiple screens.

  One solution to the above problems is to change the problematic global
  shortcuts in the responsible applications. For instance, <name|Spaces> can
  be configured to use <prefix|M-A-C-> as a prefix instead of <prefix|C->
  (click on the popup menu behind ``To switch between spaces'' and
  simultaneously press <prefix|M->, <prefix|A-> and <prefix|C->). Notice that
  <prefix|fn> is another key which is not used by <TeXmacs>.

  If you cannot or do not want to change the system-wide shortcuts, then you
  may use the <rigid|<key*|escape>-key> in order to produce equivalents for
  the modifier keys <prefix|M->, <prefix|A-> and <prefix|C->. For instance,
  under <name|Mac OS>, <prefix|C-> is equivalent to <key*|escape escape>.
  Hence, the <TeXmacs> shortcut <key|C-right> can also be obtained by typing
  <key*|escape escape right>, which may coexist with the <name|Spaces>
  shortcut <key|C-right>. Table<nbsp><reference|kbd-escape-table> shows the
  modifier key combinations which can be obtained using <key*|escape>.

  <\big-table|<descriptive-table|<tformat|<table|<row|<cell|Shortcut>|<cell|Modifier
  keys>>|<row|<cell|<key*|escape>>|<cell|<key|escape>>>|<row|<cell|<key*|escape
  escape>>|<cell|<key|escape escape>>>|<row|<cell|<key*|escape escape
  escape>>|<cell|<key|escape escape escape>>>|<row|<cell|<key*|S-escape>>|<cell|<key|S-escape>>>|<row|<cell|<key*|S-escape
  S-escape><space|1em>>|<cell|<key|S-escape
  S-escape>>>|<row|<cell|<key*|S-escape S-escape
  S-escape>>|<cell|<key|S-escape S-escape S-escape>>>>>>>
    <label|kbd-escape-table>Keyboard shortcuts for modifier keys or modifier
    key combinations.
  </big-table>

  <paragraph*|User-defined shortcuts>

  If, for some reason, the standard <TeXmacs> shortcuts are not sufficient or
  suitable for you, then you may <hlink|define your own
  shortcuts|../scheme/man-custom-keyboard.en.tm>.

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