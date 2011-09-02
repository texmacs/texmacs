<TeXmacs|1.0.7.12>

<style|tmdoc>

<\body>
  <tmdoc-title|Improving the current implementation>

  <with|color|red|[Should be completed]>

  <section|Memory management>

  If I have the courage, I might one day write a garbage collector for
  <TeXmacs>.

  <section|Encodings and fonts>

  <\itemize>
    <item>Systematically use Unicode.

    <item>Some changes have still to be made in the way font encodings are
    handled. This should make it easier to maintain fonts with characters
    from several physical fonts, virtual fonts, special characters, etc.

    <item>Currently, the current logical font is determined from a fixed set
    of environment variables only. The current typesetting font should rather
    be a tree (instead of a string), which is evaluated (so that environment
    variables are replaced) and then passed to
    <verbatim|find_font(display,tree)>. The current font can then be a joined
    font and future fonts might depend on user environment variables (i.e.
    colored fonts, using more than one color).
  </itemize>

  <section|Speed>

  In order to speed up the program, we already made the major change that not
  the whole document is typeset again when making local changes. However,
  several other main optimizations should still be made:

  <\itemize>
    <item>Encode the system environment variables; this will globally
    accelerate the program. Also, intermediate data during the typesetting
    process might be encoded in a more binary way.

    <item>The typesetter should be made lazy in a more fundamental way.
  </itemize>

  <section|Scheme>

  The <name|Guile>/<scheme> interface should become more robust and well
  documented. Several things still need to be done for this:

  <\itemize>
    <item>Implement a system for ``contextual overloading''.

    <item>Better preservation of locality.

    <item>Systematic use of closures throughout the code.

    <item>A clean interface for manipulating <TeXmacs> content (a unified
    interface for both internal <TeXmacs> trees and the associated <scheme>
    representation).

    <item>Documentation.
  </itemize>

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