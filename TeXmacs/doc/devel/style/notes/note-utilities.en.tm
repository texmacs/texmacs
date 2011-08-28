<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard utilities>

  In the <hyper-link|package <tmpackage|std-utils>|../../../main/styles/std/std-utils-dtd.en.tm>,
  the user may find several useful additional macros for writing style files.
  It mainly contains macros for

  <\itemize>
    <item>Writing block environments which span over the entire paragraph
    width. Notice that the <tmpackage|title-base> package provides some
    <hyper-link|additional macros|../../../main/styles/section/section-base-helper.en.tm>
    for wide section titles.

    <item>Writing wide block environments which are underlined, overlined or
    in a frame box.

    <item>Recursive indentation.

    <item>Setting page headers and footers.

    <item>Localization of text.
  </itemize>

  It is good practice to use these standard macros whenever possible when
  writing style files. Indeed, the low-level <TeXmacs> internals may be
  subject to minor changes. When building upon standard macros with a clear
  intention, you increase the upward compatibility of your style-sheets.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>