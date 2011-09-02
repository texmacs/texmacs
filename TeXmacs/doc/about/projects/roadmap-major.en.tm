<TeXmacs|1.0.7.12>

<style|tmdoc>

<\body>
  <tmdoc-title|Major objectives for the upcoming stable versions>

  Our main focus for the next stable version 1.1 (to be released during 2012)
  is on the improvement of the user interface and documentation. This
  comprises the following developments:

  <\itemize>
    <item>Completion and stabilization of the <name|Qt> port and the
    resulting ports for <name|Windows> and <name|MacOS>.

    <item>Development of a markup-based graphical user interface for popup
    windows. This new interface might only be ready after the next stable
    version 1.1.

    <item>Development of user-friendly tools for documentation and
    translations, which can be used in a collaborative version over the web.

    <item>Completion of a <TeXmacs> user manual and make it available in book
    form.
  </itemize>

  The next stable version 1.1 should also contain a more robust version of
  the graphical drawing tool, which is developed by Henri Lesourd. Finally,
  we plan to create an association for the proposition of free scientific
  software. One of the objectives of this association is to create a
  simplified system for making donations to <TeXmacs> and sell our software
  and documentation.

  Within a slightly longer time period of about one or two years, we have
  also started to reorganize <TeXmacs> so as to make it a stable development
  platform for developments. The aim is to reach as quickly as possible a
  point where the different parts of <TeXmacs> are well documented and
  modularized, so that they can easily be further developed in parallel by
  different people. Most of these deeper developments will reach their
  maturity only in the after-next stable version<nbsp>1.2, and comprise the
  following items:

  <\itemize>
    <item>Improve the quality of the <TeXmacs> makefiles so as to make them
    completely compatible with <verbatim|automake> and <verbatim|autoconf>.

    <item>Replace the current widget system by a markup-based system, with
    the possibility to use widgets (from Gtk, Qt, Aqua, etc.) from standard
    GUI's instead of the <TeXmacs>-provided style files.

    <item>Separate the style rewriting engine from the typesetter and make
    both completely lazy.

    <item>Increase the robustness and use of DRDs (Data Relation
    Definitions), which contain meta-information about <TeXmacs> or
    user-provided DTDs.

    <item>Migrate as much as possible of the high-level interface from C++ to
    <name|Scheme>.

    <item>Increase the robustness of <TeXmacs> and its <name|Scheme> APIs by
    providing detailed exception semantics and tools for debugging.

    <item>Provide extended documentation for developers and intelligent
    interactive ways to use this documentation.
  </itemize>

  <tmdoc-copyright|2007|Joris van der Hoeven>

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