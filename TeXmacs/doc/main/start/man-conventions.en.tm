<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conventions for this manual>

  Throughout the <TeXmacs> manual, menu entries will be typeset using a
  <em|sans serif> font, like in <apply|menu|Document>, <apply|menu|File|Load>
  or <apply|menu|Text|Font shape|Italic>. Keyboard input will be typeset in a
  <em|typewriter> font inside boxes, like in <key|C-s>. At the righthand side
  of menu entries, you see keystroke equivalents, when these are available.
  The following abbreviations are used for such keystrokes:

  <\description>
    <expand|item*|<key|S->>For shift key combinations.

    <expand|item*|<key|C->>For control key combinations.

    <expand|item*|<verbatim|><key|A->>For alternate key combinations.

    <expand|item*|<key|M->>For meta key combinations.

    <expand|item*|<key|H->>For hyper key combinations.
  </description>

  For instance, <key|A-C-b> stands for <key|<key-alternate>-<key-control>-b>.
  Spaces inside keyboard shortcuts indicate multiple key-presses. For
  instance, <expand|kbd-table|N b> stands for <key|<key-meta>-t>
  <key|N> <key|b>.

  The <key|<key-alternate>>, <key|<key-meta>> and
  <key|<key-hyper>> keys are not available on all keyboards. On
  recent PC's, the <key|<key-meta>> key is often replaced by the
  <key|<key-windows>> key. In the case when one or several
  modifier keys are missing on your keyboard, you may use
  <key|<key-escape>> instead of <key|M->,
  <key|<key-escape> <key-escape>> instead of <key|A->
  and <key|F5>, <key|<key-escape> <key-escape>
  <key-escape>> or <key|A-C-> instead of <key|H->. For instance,
  <key|<key-escape> w> is equivalent to <key|A-w>. You may also
  <apply|hyper-link|configure the keyboard
  modifiers|../config/man-config-kbd-modkeys.en.tm> in order to take full
  advantage out of the powerful set of keyboard shortcuts which is provided
  by <TeXmacs>.

  Notice that the <TeXmacs> menus and keyboard behavior are <em|contextual>,
  <abbr|i.e.> they depend on the current mode (i.e. text mode or ``math
  mode''), the current language and the position of the cursor inside your
  document. For instance, inside math mode, you have special keyboard
  shortcuts which are handy for typing mathematical formulas, but which are
  useless in text mode.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Document>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Load>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Font shape>|<with|font
      family|<quote|ss>|Italic>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
