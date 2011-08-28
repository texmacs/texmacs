<TeXmacs|1.0.7.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Plans for the future>

  <with|color|red|[Should be completed]>

  <section|Typesetting>

  The typesetter should be reorganized and improved as follows:

  <\itemize>
    <item>Decouple the style-sheet language from the actual typesetter.

    <item>Better support for agglomerated documents (from physically
    different documents).

    <item>Incorporation of better linking primitives (XLink, Proclus).

    <item>More types of dynamic objects, like animations, help balloons,
    <abbr|etc.>

    <item>Better line-breaking of large formulas.
  </itemize>

  <section|Extra facilities for editing texts>

  Although cursor movement, selections, etc. have already been implemented,
  some other standard editing facilities have not yet been completed. Let us
  mention a few of these:

  <\itemize>
    <item>Searching/replacing texts, formulas, certain environments etc. and
    regular expressions.

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

  <section|Tools for usage on the web and in networks>

  It would be nice to have a better integration of <TeXmacs> with the web. As
  a first step, we need a clean internal plug-in for <name|Wget> or
  <name|Curl> with support for cookies, security, etc. At a second stage, the
  Html converters should be improved to take advantage of this. After that,
  we may think about things like collaborative authoring via the web,
  integration with preprint servers, <abbr|etc.>

  Another interesting thing would be to incorporate tools for live
  conferencing inside <TeXmacs>. We actually expect this to be quite easy and
  this would open a different road towards collaborative authoring, instant
  messenging, <abbr|etc.>

  <section|Interface with computer algebra systems>

  The following improvements should still be made in order to link <TeXmacs>
  to computer algebra systems:

  <\enumerate>
    <item>Improving the layout of computer algebra sessions.

    <item>Add extra features to increase the interoperability between
    <TeXmacs> and computer algebra systems and to give additional control
    over the layout of big output.

    <item>More semantics for the objects being communicated. This may either
    be high level information (like Openmath or HTML 4.0 mathematical markup)
    or low level information (including information about the representation
    of data), depending on the required speed.

    <item>Further possibilities for evolution concern highlighting, debugging
    facilities and so on.
  </enumerate>

  <section|Interaction with other GNU projects>

  We would appreciate help for writing a converter from <TeXmacs> to the
  <name|Texinfo> format, which is the standard documentation format for the
  GNU project.

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