<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Conversion from <LaTeX> to <TeXmacs>>

  In order to import a <LaTeX> document into <TeXmacs>, you may use
  <menu|File|Import|Latex>. Don't forget to save the file under a new name
  with the <verbatim|.tm> extension, if you want to edit it.

  As explained in the introduction, the conversion of <LaTeX> documents into
  <TeXmacs> is more problematic than conversions the other way around. As
  long as you restrict yourself to using the most common <LaTeX> commands,
  the conversion process should not give rise to any major difficulties.
  However, as soon as your documents contain \Pweird <TeX> primitives\Q
  (think about <verbatim|\\csname>...), then the converter may get confused.
  We also notice that <TeXmacs> is currently unable to convert <LaTeX> style
  files and no plans exist to enhance the converter in this direction.

  There are two major reasons for <LaTeX> documents to get imported in an
  inappropriate way, and which can easily be corrected by the user. First of
  all, the parser may get confused because of some exotic syntactic
  construct. This typically happens in presence of catcodes or uncommon
  styles of macro definitions. Sometimes, the parser may also be mistaken
  about the current mode, in which case text gets parsed as a mathematical
  formula or <em|vice versa>. In both cases, the imported document usually
  becomes \Pweird\Q at a certain point. In order to solve the problem, we
  suggest you to identify the corresponding point in the <LaTeX> source file
  and to make an appropriate change which avoids the parser of getting
  confused.

  A second common error is that certain <LaTeX> macros are not recognized by
  the converter, in which case they will appear in red. This typically
  happens if you use one of the hundreds additional <LaTeX> packages or if
  you defined some additional macros in another document. In the case when
  the troublesome macro occurs only a few times, then we suggest you to
  manually expand the macro in the <LaTeX> source file before importation.
  Otherwise, you may try to put the definitions of the missing macros in the
  preamble of the <LaTeX> document. Alternatively, you may create a small
  style package with <TeXmacs> counterparts for the macros which were not
  recognized.

  The behaviour of the converter may be customized using several user
  preferences in the <menu|Edit|Preferences|Converters|LaTeX|LaTeX--\<gtr\>TeXmacs>
  menu:

  <\description-long>
    <item*|Import sophisticated objects as pictures>This option allows
    <TeXmacs> to compile the <LaTeX> document in a temporary directory, with
    the package <verbatim|preview> installed, in order to import some macros
    or environments as pictures. The source of each picture is also imported
    in order to be re-exported if needed. Currently, the following macros are
    imported as pictures when this option is set: <verbatim|\\xymatrix>,
    <verbatim|pspicture>, <verbatim|tikzpicture>.

    <item*|Keep track of the <LaTeX> source code>One should be interested in
    this option in order to use <TeXmacs> to make small or isolated
    modifications into a <LaTeX> file (e.g. for a proofreading). This option
    allows <TeXmacs> to import the <LaTeX> document with added markup in
    order to track the original sources of the document paragraphs. These
    tracked sources are, as far as possible, re-used during a <LaTeX>
    re-export.

    <item*|Ensure transparent tracking>This option, subject to the above,
    verify that the added markup does not change the result of the
    conversion. It has been added for testing purpose and may <em|strongly>
    increase the time of the import process (at least it double it).
  </description-long>

  <tmdoc-copyright|1998--2013|Joris van der Hoeven, François Poulain>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>