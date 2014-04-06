<TeXmacs|1.99.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Advanced user preferences>

  In addition to the <hlink|preferences dialog|man-preferences.en.tm>, where
  most of the important customization may be made, there are some other user
  preferences available. These are either too ``fine-grained'' to be added to
  the main dialog or simply unsupported and therefore they are only made
  accessible through the <scheme> procedures <scm|set-preference> and
  <scm|get-preference> as explained in <hlink|<scheme> procedures for user
  preferences|../../devel/scheme/utils/utils-preferences.en.tm>. These are
  stored, along the rest, in your <tt|preferences.scm> file.

  <subsection|Syntax highlighting>

  <TeXmacs> will color <scheme>, <c++> and <mathemagix> source files using
  either default colors or the ones you configure with the preferences
  <scm|"syntax:LAN:CLASS"> where LAN is one of <tt|cpp,scheme,magix> (ok?).
  The colors may be either named or hexadecimal as in <tt|#3e45ef>.

  <subsection|User interface>

  <\explain>
    <scm|("gui:line-input:autocommit" <scm-arg|boolean>)><explain-synopsis|preference>
  <|explain>
    Whether to automatically accept the contents of text widgets in the focus
    toolbar upon exit. Setting this to false means one must press the
    <key|Enter> key to validate the input or it will be lost when the widget
    looses focus.
  </explain>

  <\explain>
    <scm|("gui:mini-fontsize" <scm-arg|integer>)><explain-synopsis|preference>
  <|explain>
    Specifies the font size in points of so-called
    <with|font-shape|italic|mini> elements in the interface. These include
    the footer and the focus toolbar. Please note that abusing this might
    cause some elements in the interface to be misplaced. Reasonable values
    are in the range 10-15.
  </explain>

  <subsection|Auto-update system>

  Please refer to <hlink|Notification and download of
  updates|../../devel/scheme/api/automatic-updates.en.tm>.

  <tmdoc-copyright|2013|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>