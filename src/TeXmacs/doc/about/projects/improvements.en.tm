<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Improving the current implementation>

  <with|color|red|[Should be updated]>

  <section|Memory management>

  If I have the courage, I might one day write a garbage collector for
  <apply|TeXmacs>.

  <section|Graphical interface>

  We wish to move towards <with|font shape|small-caps|Guile-Gtk> or another
  portable graphical user interface.

  <section|Encodings and fonts>

  <\itemize>
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
    <item>Improving the speed of loading (and saving) files. This will
    accelerate loading fonts.

    <item>Encode the system environment variables; this will globally
    accelerate the program.

    <item>The way system style environment variables are handled for
    typesetting concats and paragraphs is far from optimal. Some serious
    rethinking should be undertaken here.

    <item>The computation of the current context at a given cursor position
    takes a lot of time, especially the computation of the environment. This
    slows down the cursor movement for complex texts using <TeX> fonts (it
    scrolls much quicklier when using X fonts).
  </itemize>

  <section|Miscellaneous changes>

  The following ad hoc implementations should be changed or made more robust:

  <\itemize>
    <item>Cursor movement along lines of a paragraph (when moving to the
    right at the end of a line, the cursor should jump to the start of the
    next line).

    <item>Widths of fraction bar, square root upper line and negations.

    <item>Boxes should not have origins; its children should have positions
    instead.

    <item>Clear separation between files which depend on the system (i.e.
    <verbatim|fast_alloc.cpp>, <verbatim|file.hpp>, <verbatim|dir.hpp>) in some
    directory and the others.
  </itemize>

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Memory
      management><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      interface><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Encodings and
      fonts><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Speed><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Miscellaneous
      changes><value|toc-dots><pageref|toc-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
