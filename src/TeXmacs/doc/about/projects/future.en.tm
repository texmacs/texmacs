<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Plans for the future>

  <with|color|red|[Should be updated]>

  <section|Typesetting>

  Some major typesetting facilities which have not yet been implemented are
  the following:

  <\itemize>
    <item>Separation of a document in several parts.

    <item>Dynamical objects like in HTML.

    <item>Environments for computer algebra system interfaces.
  </itemize>

  Many minor facilities should also be completed. We list a few of them:

  <\itemize>
    <item>Consider fractions as operators <with|mode|math|\<Rightarrow\>>
    spaces before and after. Similarly for scripts
    <with|mode|math|\<Rightarrow\>> small space before left scripts and after
    right scripts.
  </itemize>

  <section|Extra facilities for editing texts>

  Although cursor movement, selections, etc. have already been implemented,
  some other standard editing facilities have not yet been completed. Let us
  mention a few of these:

  <\itemize>
    <item>Searching texts, formulas, certain environments etc.

    <item>Query replace.

    <item>Mathematical facilities: simplification of a selected region,
    substitutions of formulas in other formulas, etc.

    <item>Version control.

    <item>Data compression and protection.

    <item>Grammar checkers and automatic translation programs. Does someone
    know where to find detailed free dictionaries and stuff like that?

    <item>Incorporation of free speech recognition program.
  </itemize>

  <section|A universal spreadsheet>

  We would like to incorporate a ``universal spreadsheet'' facility into
  <TeXmacs>. The idea is that all dependencies between the cells in the sheet
  are analyzed by <TeXmacs>, but all actual computations are delegated to an
  extern system of your choice, like one of the currently supported computer
  algebra systems. Also, the data of the spreadsheet will not necessarily be
  formatted in a rectangular table; one can also imagine dependencies between
  nodes of a tree, elements of a graph, or anything else.

  <section|Technical pictures>

  I also would like to include a facility for drawing technical pictures. In
  this implementation you should be able to benefit from the fact that you
  can define macros for making geometrical constructions. It would for
  instance be possible to write a style file for drawing electronic circuits
  or chemical components with a nice icon bar for selecting circuits or
  components, just as you select lines and circles in usual pictures.

  <section|Interface with computer algebra systems>

  The following improvements should still be made in order to link
  <apply|TeXmacs> to computer algebra systems:

  <\enumerate>
    <item>Improving the layout of computer algebra sessions.

    <item>Add extra features to increase the interoperability between
    <apply|TeXmacs> and computer algebra systems and to give additional
    control over the layout of big output.

    <item>More semantics for the objects being communicated. This may either
    be high level information (like Openmath or HTML 4.0 mathematical markup)
    or low level information (including information about the representation
    of data), depending on the required speed.

    <item>Further possibilities for evolution concern highlighting, debugging
    facilities and so on.
  </enumerate>

  <section|Interaction with other GNU-like projects>

  It might be nice to increase the interaction between <apply|TeXmacs> and
  other GNU-like projects, such as Gnome or multiplatform GUI's. This might
  facilitate the incorporation of extern data into <apply|TeXmacs> documents
  or increase the number of supported platforms. On the other hand, several
  <apply|TeXmacs> features, such as its font handling, might be interesting
  for other projects too.

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
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Typesetting><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Extra facilities for editing
      texts><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>A universal
      spreadsheet><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Technical
      pictures><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Interface with computer algebra
      systems><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Interaction with other GNU-like
      projects><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
