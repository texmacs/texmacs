<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|General prefix rules>

  Since there are many keyboard shortcuts, it is important to have some ways
  of classifying them in several categories, in order to make it easier to
  memorize them. As a general rule, keyboard shortcuts which fall in the same
  category are indentified by a common prefix. The main such common prefixes
  are:

  <\description>
    <expand|item*|<key|C-<with|mode|math|x>>>Control key based shortcuts are
    used for frequently used editing commands. They depend very much on the
    ``look and feel'' in <apply|menu|Edit|Preferences>. For instance, if you
    use an <name|Emacs>-compatible look and feel, then the shortcuts of the
    form <key|C-<with|mode|math|x>> correspond to <name|Emacs> commands, like
    <key|C-y> for pasting text.

    <expand|item*|<key|A-<with|mode|math|x>>>The alternate key is used for
    commands which depend on the mode that you are in. For instance,
    <expand|kbd-text|s> produces <strong|strong> text in text mode and a
    square root <with|mode|math|<sqrt|>> in math mode. Notice that
    <key|<key-escape> <key-escape>> is equivalent to
    <key|A->.

    <expand|item*|<key|M-<with|mode|math|x>>>The meta key is used for general
    purpose <apply|TeXmacs> commands, which can be used in all modes. For
    instance, <expand|kbd-gen|!> produces a label. It is also used for
    additional editing commands, like <key|A-w> for copying text if you use
    the <name|Emacs> look and feel. Notice that <key|<key-escape>>
    is equivalent to <key|M->.

    <expand|item*|<key|H-<with|mode|math|x>>>The user keyboard modifier key
    is used for producing special symbols like Greek characters in math mode.
    You may configure your keyboard so as to let caps-lock play the rôle of
    the hyper key. The <key|F5> is equivalent to <key|H->.
  </description>

  We recall that the particular modifier keys which are used in order to
  obtain the <key|M-> and <key|H-> prefixes can be
  <apply|hyper-link|configured|../../config/man-config-kbd-modkeys.en.tm> in
  <apply|menu|Edit|Preferences>.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Preferences>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Preferences>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
