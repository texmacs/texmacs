<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Cut and paste>

  You can select text and formulas by maintaining the left mouse button. In
  order to delete the selected region, use <menu|Edit|Cut> or
  <shortcut|(kbd-cut)>. In order to copy the selected region, first click on
  <menu|Edit|Copy> or hit <shortcut|(kbd-copy)>. Next, paste it as many times
  as you want to the location of your cursor, using <menu|Edit|Paste> or
  <shortcut|(kbd-paste)>. Alternatively, you may copy a selected region using
  the middle mouse button.

  It is also possible to change the text properties of a selected region. For
  instance, in order to transform some black text in red, you select it using
  the left mouse button and click on <render-menu|Format|<rigid|<render-menu|Color|<tabular*|<tformat|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|1spc>|<cwith|1|1|1|1|cell-rsep|1spc>|<cwith|1|1|1|1|cell-bsep|1spc>|<cwith|1|1|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-width|1.5em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-background|red>|<table|<row|<cell|>>>>>>>>.
  Similarly, if you select a formula and you click on <menu|Insert|Fraction>,
  then the formula becomes the numerator of the newly created fraction.

  When using the copy and paste mechanism to communicate with other
  applications, text is copied and pasted using the <TeXmacs> data format.
  You may specify other import and export formats using
  <menu|Tools|Miscellaneous|Import selections as> <abbr|resp.>
  <menu|Tools|Miscellaneous|Export selections as>. Alternatively, you may
  directly copy to or paste from an external format using the first group of
  entries in the <menu|Edit|Copy to> and <menu|Edit|Paste from> submenus. For
  instance, a <LaTeX> formula can be pasted inside a <TeXmacs> formula using
  <menu|Edit|Paste from|LaTeX>.

  By default, copying and pasting uses the \Pprimary clipboard\Q. Using the
  remaining entries in the <menu|Edit|Copy to> and <menu|Edit|Paste from>
  menus, you may specify as many other clipboards as you like. This allows
  you to keep multiple selections in memory, ready to be pasted.

  There are two ways to make selections using the keyboard. When using the
  cursor keys <key|left>, <key|right>, <abbr|etc.> while holding down the
  <prefix|S-> button, you may select text while moving around the cursor.
  Alternatively, you may press <shortcut|(kbd-select-enlarge)> once to fix a
  starting position. When moving around using the cursor keys, the text
  between the starting position and the current position keeps being
  selected. The selection gets cleared by pressing
  <shortcut|(selection-cancel)>.

  Notice that the <shortcut|(kbd-select-enlarge)> shortcut also allows you to
  make <em|structured selections>. You may select the current word you are in
  by pressing <shortcut|(kbd-select-enlarge)> twice. Each additional time you
  press <shortcut|(kbd-select-enlarge)> results in the selection of the
  smallest structure that englobes the current selection. Ultimately, when
  the entire document gets selected, pressing <shortcut|(kbd-select-enlarge)>
  once more clears the selection.

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>