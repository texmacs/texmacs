<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|General architecture of <TeXmacs>>

  <section|Introduction>

  The <apply|TeXmacs> program has been written in C++. You need
  <verbatim|g++> and the <verbatim|makefile> utility in order to compile
  <apply|TeXmacs>. Currently, the source (in the <verbatim|src> directory) of
  the <apply|TeXmacs> implementation has been divided into the following
  parts:

  <\itemize>
    <item>A set of basic and generic data structures in the <verbatim|Basic>
    directory.

    <item>Standard resources for <apply|TeXmacs>, such as <apply|TeX> fonts,
    languages, encodings and dictionaries, in the <verbatim|Resource>
    directory.

    <item>A documented graphical toolkit in the <verbatim|Window> directory
    (although the documentation is a bit outdated).

    <item>The extension language for <apply|TeXmacs> in the <verbatim|Prg>
    directory.

    <item>The typesetting part of the editor in the directory
    <verbatim|src/Typeset>.

    <item>The editor in the directory <verbatim|src/Edit>.

    <item>The <apply|TeXmacs> server in the directory <verbatim|src/Server>.
  </itemize>

  All parts use the data structures from <verbatim|Basic>. The graphical
  toolkit depends on <verbatim|Resource> for the <apply|TeX> fonts. The
  extension language is independent from <verbatim|Resource> and
  <verbatim|Window>. The typesetting part depends on all other parts except
  from <verbatim|Prg>. The main editor and the <apply|TeXmacs> server use all
  previous parts.

  The <apply|TeXmacs> data are contained in the directory <verbatim|edit>
  which corresponds to the <apply|TeXmacs> distribution without the source
  code. Roughly speaking, we have the following kind of data:

  <\itemize>
    <item>Font data in <verbatim|fonts> (encodings, <verbatim|.pk> files,
    etc.).

    <item>Language data in <verbatim|languages> (hyphenation patterns,
    dictionaries, etc.).

    <item>Document styles in <verbatim|style>.

    <item>Initialization and other <apply|scheme> programs in
    <verbatim|progs>.
  </itemize>

  The directory <verbatim|misc> contains some miscellaneous data like the
  edit icon (<verbatim|misc/pixmaps/traditional/--x17/edit.xpm>).

  <section|Intern representation of texts>

  <apply|TeXmacs> represents all texts by trees (for a fixed text, the
  corresponding tree is called the <em|edit tree>). The nodes of such a tree
  are labeled by standard <em|operators> which are listed in
  <verbatim|Basic/Data/tree.hpp> and <verbatim|Basic/Data/tree.cpp>. The labels
  of the leaves of the tree are strings, which are either invisible (such as
  lengths or macro definitions), or visible (the real text).

  The meaning of the text and the way it is typeset essentially depend on the
  current environment. The environment mainly consists of a relative hash
  table of type <verbatim|rel_hashmap\<less\>string,tree\<gtr\>>, i.e. a
  mapping from the environment variables to their tree values. The current
  language and the current font are examples of system environment variables;
  new variables can be defined by the user.

  <subsection|Text>

  All text strings in <apply|TeXmacs> consist of sequences of either specific
  or universal symbols. A specific symbol is a character, different from
  <verbatim|'\\0'>, <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>. Its
  meaning may depend on the particular font which is being used. A universal
  symbol is a string starting with <verbatim|'\<less\>'>, followed by an
  arbitrary sequence of characters different from <verbatim|'\\0'>,
  <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>, and ending with
  <verbatim|'\<gtr\>'>. The meaning of universal characters does not depend
  on the particular font which is used, but different fonts may render them
  in a different way.

  <subsection|The language>

  The language of the text is capable performing a further semantic analysis
  of a text phrase. At least, it is capable of splitting a phrase up into
  <em|words> (which are smaller phrases) and inform the typesetter about the
  desired spaces between words and hyphenation information. In the future,
  additional semantics may be added into languages. For instance, spell
  checkers might be implemented for natural languages and parsers for
  mathematical formulas or programming languages.

  <section|Typesetting texts>

  Roughly speaking, the typesetter of <apply|TeXmacs> takes a tree on input
  and produces a box, while accessing and modifying the typesetting
  environment. The <verbatim|box> class is multifunctional. Its principal
  method is used for displaying the box on a post-script device (either the
  screen or a printer). But it also contains a lot of typesetting
  information, such as logical and ink bounding boxes, the positions of
  scripts, etc.

  Another functionality of boxes is to convert between physical cursors
  (positions on the screen) and logical cursors (paths in the edit tree).
  Actually, boxes are also organized into a tree, which often simplifies the
  conversion. However, because of macro expansions and line and page
  breaking, the conversion routines may become quite intricate. Notice also
  that, besides a horizontal and vertical position, the physical cursor also
  contains an infinitesimal horizontal position. Roughly speaking, this
  infinitesimal coordinate is used to give certain boxes (such as color
  changes) an extra infinitesimal width.

  <section|Making modifications in texts>

  In <verbatim|Edit/Modify> you find different routines for modifying the
  edit tree. Modifications go in several steps:

  <\enumerate>
    <item>A certain input event triggers an action, such as
    <verbatim|make_fraction>, which intends to modify the edit tree.

    <item>All modifications which <verbatim|make_fraction> or its subroutines
    will make to the edit tree eventually break down to seven elementary
    modification routines, namely <verbatim|assign>, <verbatim|insert>,
    <verbatim|remove>, <verbatim|split>, <verbatim|join>,
    <verbatim|ins_unary> and <verbatim|rem_unary>.

    <item>Before performing the required modification, the elementary
    modification routine first notifies all views of the same text of the
    modification.

    <item>On notification, each view updates several things, such as the
    cursor position. It also notifies the modification to the typesetter of
    the text, since the typesetter maintains a list of already typeset
    paragraphs.

    <item>When all views have been notified of the modification, we really
    perform it.

    <item>Each user action like a keystroke or a mouse click is responsible
    for inserting <em|undo points> between sequences of elementary
    modifications. When undoing a modification, the editor will move to the
    previous undo point.
  </enumerate>

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
    <associate|toc-3|<tuple|2.1|?>>
    <associate|toc-4|<tuple|2.2|?>>
    <associate|toc-5|<tuple|3|?>>
    <associate|toc-6|<tuple|4|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Introduction><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Intern representation of
      texts><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      2.1<space|2spc>Text<value|toc-dots><pageref|toc-3>

      2.2<space|2spc>The language<value|toc-dots><pageref|toc-4>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Typesetting
      texts><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Making modifications in
      texts><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
