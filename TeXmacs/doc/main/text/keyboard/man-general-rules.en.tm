<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|General prefix rules>

  Since there are many keyboard shortcuts, it is important to have some ways
  of classifying them in several categories, in order to make it easier to
  memorize them. As a general rule, keyboard shortcuts which fall in the same
  category are identified by a common prefix. The active prefixes heavily
  depend on the selected \Plook and feel\Q in <menu|Edit|Preferences>. In the
  current look and feel of your <TeXmacs> system,<space|0.4spc>the main
  common prefixes are as follows:

  <\description>
    <item*|<prefix|std>>Standard shortcuts, which are similar to shortcuts
    used by other applications (for the selected look and feel). For
    instance, <shortcut|(clipboard-paste "primary")> can be used for pasting
    text on your system.

    <item*|<prefix|cmd>><TeXmacs> shortcuts, which often rely on the current
    editing mode. For instance, <key|text s> produces <strong|strong> text in
    text mode and a square root <math|<sqrt|>> in math mode.

    <item*|<prefix|altcmd>>Compound <TeXmacs> shortcuts. Usually, these
    shortcuts first indicate the kind of markup to which the command will
    apply and then specify the specific command. For instance, the
    <key|executable> prefix is used for inserting executable markup, which is
    useful for <hlink|writing style files|../../../devel/style/style.en.tm>.
    One example is the shortcut <key|executable +> for the insertion of an
    addition.

    <item*|<prefix|structured:geometry>>This prefix is used in combination
    with arrow keys and certain other special keys for <hlink|positioning and
    resizing objects|../../editing/man-structured-geometry.en.tm>

    <item*|<prefix|structured:move>>This prefix is used in combination with
    arrow keys and some other special keys for <hlink|structured cursor
    movements|../../editing/man-structured-move.en.tm>.

    <item*|<prefix|special>>This prefix is occasionally used in combination
    with letters and punctuation symbols for creating some additional easy to
    remind shortcuts.

    <item*|<prefix|symbol>>This prefix can be used in combination with normal
    letters for the insertion of special symbols. For instance,
    <key|text:symbol s> yields ÿ and <key|math:symbol a> yields
    <math|<op|\<amalg\>>>. The <prefix|symbol> prefix is also used for the
    insertion of \Pliteral characters\Q. For instance, <key|symbol \\> will
    always produce the \\ character, whereas the <key|\\> key is used for
    entering <hlink|hybrid commands|man-hybrid.en.tm>.
  </description>

  Unfortunately, <prefix|altcmd>-based shortcuts are superseded by system
  shortcuts on several systems. For instance, accented characters and common
  special symbols are entered using this prefix under <name|Mac OS>. In that
  case, you may use the <key*|escape> key as an equivalent for <key|altcmd>.
  For more information, we refer to the section on <hlink|keyboard
  configuration|../../config/man-config-keyboard.en.tm>.

  <tmdoc-copyright|1998--2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>