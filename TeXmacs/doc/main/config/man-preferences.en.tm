<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|User preferences>

  For an optimal typing experience, you may wish to configure <TeXmacs> in a
  way which suits your needs best. This can be done from within the
  <menu|Edit|Preferences> menu. Most importantly, you should choose a ``look
  and feel'' in <menu|Edit|Preferences|Look and feel>. This will enable you
  for instance to let the keyboard shortcuts used by <TeXmacs> be similar to
  what you are used to in other applications.

  The following user preferences are available:

  <\description>
    <item*|<menu|Look and feel>><label|preferences:look-and-feel>This
    preference controls the general ``look and feel'' of <TeXmacs>, and
    mainly affects the behaviour of the keyboard. The <menu|default> look and
    feel depends on your system (<menu|Gnome>, <menu|KDE> or <menu|Emacs>
    under <name|Linux>, <menu|Mac OS> under <name|Mac OS>, and <menu|Windows>
    under <name|Windows>). The <menu|Emacs> look and feel can be used as an
    alternative on all systems; it has been the default for all <TeXmacs>
    versions prior to<nbsp>1.0.7.6.

    More details on the <hlink|keyboard configuration on different
    systems|man-config-keyboard.en.tm> can be found below.

    <item*|<menu|Interactive questions>>This preference specifies how the
    user will be prompted for input when required. Questions may either be
    displayed in separate windows or on the status bar of <TeXmacs>.

    <item*|<menu|Details in menus>>This preference specify the level of
    detail in the menus. The less frequently used features will be left out
    when selecting <menu|Simplified menus>.

    <item*|<menu|View>>The preference corresponds to the same viewing options
    as in the top-level <menu|View><nbsp>menu.

    <item*|<menu|Language>>Your preferred language for the <TeXmacs>
    interface.

    <item*|<menu|Keyboard>><label|preferences:keyboard>In addition to the
    general look and feel, a few additional settings determine the behaviour
    of the keyboard:

    <\itemize>
      <item>The <menu|Cyrillic input method> specifies <hlink|how to type
      text in Cyrillic languages|man-russian.en.tm>.

      <item>Quotes can be automatically closed according to the
      <menu|Automatic quotes> style.

      <item>Brackets can be automatically closed by enabling
      <menu|Automatically close brackets>.
    </itemize>

    <item*|<menu|Printer>>The printer setup can be configured from this
    submenu.

    <item*|<menu|Security>>In theory, <TeXmacs> documents may embed macros or
    hyperlinks which give rise to the execution of arbitrary commands (as
    specified by the author). In practice, this feature may involve a
    security risk,. Therefore, the <menu|Security> preference allows the user
    to specify what should be done with untrusted executable code.

    <item*|<menu|Converters>>The behaviour of converters between <TeXmacs>
    various other data formats may be configured from this menu. For more
    details, we refer to the <hlink|chapter on compatibility with other
    formats|../convert/man-convert.en.tm>.

    <item*|<menu|Scripts>>Specify a default scripting language for all
    external scripts.

    <item*|<menu|Tools>><TeXmacs> features a few additional tools which the
    user may wish to work under certain circumstances:

    <\itemize>
      <item>A debugging tool for <TeXmacs> developers.

      <item>A linking tool for entering typed hyperlinks and complex
      annotations.

      <item>A versioning tool for comparing two versions of a <TeXmacs>
      document.

      <item>A remote connection tool (which currently does not work anymore).
    </itemize>

    <item*|<menu|Autosave>>This preference specifies how often documents will
    be ``autosaved''. Any edits to a file which was not autosaved will be
    lost on undesired termination of <TeXmacs>. This typically occurs after
    an erroneous manipulations by the user, certain bugs in <TeXmacs>, or a
    power problem.

    <item*|<menu|Bibtex command>>The user may specify an alternative to
    <verbatim|bibtex> for the compilation of bibliographies using <BibTeX>.
    Notice that recent versions of <TeXmacs> integrate a<nbsp>native
    alternative tool for the compilation of bibliographies.
  </description>

  <tmdoc-copyright|1998--2010|Joris van der Hoeven>

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