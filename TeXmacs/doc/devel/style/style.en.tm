<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Writing <TeXmacs> style files>

  One of the fundamental strengths of <TeXmacs> is the possibility to write
  your own style files and packages. The purpose of style files is multiple:

  <\itemize>
    <item>They allow the abstraction of repetitive elements in texts, like
    sections, theorems, enumerations, etc.

    <item>They form a mechanism which allow you to structure your text. For
    instance, you may indicate that a given portion of your text is an
    abbreviation, a quotation or ``important''.

    <item>Standard document styles enable you to write professionally looking
    documents, because the corresponding style files have been written with a
    lot of care by people who know a lot about typography and aesthetics.
  </itemize>

  The user may select a major style from the <menu|Document|Style> menu. The
  major style usually reflects the kind of document you want to produce (like
  a letter, an article or a book) or a particular layout policy (like
  publishing an article in a given journal).

  Style packages, which are selected from the <menu|Document|Style> menu, are
  used for further customization of the major style. For instance, the
  <tmpackage|number-europe> package enables European-style theorem numbering
  and the <tmpackage|maxima> package contains macros for customizing the
  layout of sessions of the <name|Maxima> computer algebra system. Several
  packages may be used together.

  When you want to add your own markup to <TeXmacs> or personalize the
  layout, then you have to choose between writing a principal style file or a
  style package. In most cases, you will probably prefer to write a style
  package, since this will allow you to combine it arbitrary other styles.
  However, in some cases you may prefer to create a new principal style,
  usually by personalizing an existing style. This is usually the case if you
  want to mimic the layout policy of some journal. In this chapter, we will
  both explain how to write your own style packages and how to customize the
  standard styles.

  <\traverse>
    <branch|Writing a simple style package|design/style-example.en.tm>

    <branch|Rendering of style files and packages|presentation/src-present.en.tm>

    <branch|The style-sheet language|design/style-language.en.tm>

    <branch|Customizing the standard <TeXmacs>
    styles|customize/customize.en.tm>

    <branch|Further notes and tips|notes/style-notes.en.tm>
  </traverse>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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