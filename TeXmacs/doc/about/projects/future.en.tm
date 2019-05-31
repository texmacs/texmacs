<TeXmacs|1.99.9>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Plans for the future>

  There are many things that we would like to integrate in <TeXmacs>. On
  <hlink|<name|Savannah>|http://savannah.gnu.org/projects/texmacs/>, we
  maintain a<nbsp>wish list with <hlink|suggestions|http://www.texmacs.org/tmweb/contact/wishes.en.html>
  by users and other detailed wanted features. Here follows an incomplete
  list of more general directions for future development, as on may 31, 2019.

  <section|Typesetting>

  The typesetter should be reorganized and improved as follows:

  <\itemize>
    <item>Decouple the style-sheet language from the actual typesetter.

    <item>Better support for agglomerated documents (from physically
    different documents).

    <item>More types of dynamic objects, like animations, help balloons,
    <abbr|etc.>

    <item>Better line-breaking of large formulas.
  </itemize>

  <section|Extra facilities for editing texts>

  Although cursor movement, selections, etc. have already been implemented,
  some other standard editing facilities have not yet been completed. Let us
  mention a few of these:

  <\itemize>
    <item>Mathematical facilities: simplification of a selected region,
    substitutions of formulas in other formulas, etc.

    <item>Grammar checkers and automatic translation programs. Does someone
    know where to find detailed free dictionaries and stuff like that?

    <item>Incorporation of a free speech recognition program.
  </itemize>

  <section|Converters to other formats>

  There is a constant need for good converters from and to other data
  formats:

  <\itemize>
    <item>Of course, <TeX>/<LaTeX> cannot really be considered as a format
    (it is really a language with an ill defined grammar). Nevertheless it is
    important to have good heuristic converters in both directions. We still
    would appreciate help in this direction, in particular in order to
    support standard documents styles of various journals.

    <item><TeXmacs> is also compatible with <name|Html>, but some things such
    as forms have not been implemented yet. The importation of tables could
    also be further improved and it would be nice if the importer could
    support cascaded style sheets.

    <item>It would be nice to have converters for the document formats used
    by <name|Open Office>, <name|Word>, and the internal formats used by
    important publishers such as <name|Elsevier>.

    <item>It would be nice to have converters for various markdown formats as
    used by sites such as <name|Wikipedia>. Similarly, it would be nice to
    have an interface for <name|Jupyter> notebooks.

    <item>We would appreciate help for writing a converter from <TeXmacs> to
    the <name|Texinfo> format, which is the standard documentation format for
    the GNU project.
  </itemize>

  <section|Technical pictures>

  We have started to include a facility for drawing technical pictures, but
  the responsable developer (Henri <name|Lesourd>) is currently no longer
  part of the <TeXmacs> team. It would be nice to rewrite parts of the tool
  and extend it further so as to allow for the definition of graphical
  macros. This would allow users to define new geometrical constructions. It
  would for instance be possible to write a style file for drawing electronic
  circuits or chemical components with a nice icon bar for selecting circuits
  or components, just as you select lines and circles in usual pictures.

  <section|Presentation mode>

  We wish to further extend the laptop presentation mode into the following
  directions:

  <\itemize>
    <item>A larger number of themes.

    <item>Implementation of transitions between slides.

    <item>Further improvements for the animation editor.
  </itemize>

  We would also very much appreciate help on artwork and the design of nice
  themes.

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

  <\itemize>
    <item>Improving the layout of computer algebra sessions.

    <item>Add extra features to increase the interoperability between
    <TeXmacs> and computer algebra systems and to give additional control
    over the layout of big output.

    <item>More semantics for the objects being communicated. This may either
    be high level information (like HTML 4.0 mathematical markup or Openmath)
    or low level information (including information about the representation
    of data), depending on the required speed.

    <item>Further possibilities for evolution concern syntax highlighting,
    debugging facilities and so on.
  </itemize>

  <section|<TeXmacs> on tablets and mobile phones>

  After the release of <TeXmacs> 2.1, we wish to upgrade <TeXmacs> to
  <name|Qt>5, which should allow the deployment on new platforms such as
  <name|Android>. This also opens the road for a specific interface for
  tablets and mobile phones.

  <tmdoc-copyright|1998--2019|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>