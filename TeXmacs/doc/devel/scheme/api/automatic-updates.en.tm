<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Notification and download of updates>

  As of <name|svn> revision 7196, <TeXmacs> supports automatic notification
  of available downloads from a repository and their installation using the
  <name|Sparkle> framework for <name|MacOS> and <name|WinSparkle> under
  <name|Windows>.

  In order to guarantee the origin of releases, these must be signed with a
  <name|DSA> key, whose public part will be bundled with the application. On
  the server side a so-called <with|font-shape|italic|appcast> must be
  updated for each release. It is an <name|xml> file containing information
  about available downloads, their contents and their digital signatures,
  following the specification for <name|Sparkle>/<name|WinSparkle>. For the
  moment we refer to <name|Sparkle>'s documentation for more details.

  In principle it should be easy for anyone to release their custom versions
  of <TeXmacs> and let their users autoupdate them with a simple change in
  the config files. For this they only need provide the public key and the
  <name|url> of the appcast.

  <subsection|Operating system specifics>

  Under <name|MacOS> the process of creation of the appcast is partially
  automated through the <name|make> build rule <verbatim|MACOS_RELEASE>.
  Calling <verbatim|make MACOS_RELEASE> will compile and bundle <TeXmacs>,
  then zip and finally digitally sign the resulting
  <verbatim|TeXmacs-*.app.zip> with the script
  <verbatim|admin/misc/sign_update>. In order for this to work, one has to
  set the environment variable <verbatim|TEXMACS_PRIVATE_DSA> to point to the
  location of the private <name|DSA> key used to sign releases. At the end of
  the build process a chunk of <name|XML> is printed that can be pasted in
  the <verbatim|appcast.xml> file.

  Under <name|Windows> digital signatures are not yet supported by
  <name|WinSparkle> and as such will be ignored (Aug. 2013).

  There is no support for automatic notification of releases under
  <name|Linux> yet. Automatic download and installation is unlikely to happen
  due to the way packaging systems work for most distributions.

  <subsection|Client side interface>

  <\explain>
    <scm|(check-updates-background)><explain-synopsis|check for updates in
    the background>
  <|explain>
    Start a background check for updates. A dialog box pops up only if
    there's an update. Configuration variables must be properly set for this
    call to work. In particular, the appcast url must be set via the
    preference <scm|"updater:appcast">.
  </explain>

  <\explain>
    <scm|(check-updates-foreground)><explain-synopsis|check for updates in
    the foreground>
  <|explain>
    Start a check for updates immediately popping up a dialog with the
    progress. This call is non-blocking at least with <name|Sparkle> and
    <name|WinSparkle> since they run in separate threads.
  </explain>

  <\explain>
    <scm|(check-updates-interval <scm-arg|integer>)><explain-synopsis|sets
    the update interval>
  <|explain>
    Sets the interval in hours to wait between automatic checks if these are
    activated via <scm|"updater:automatic-checks">. Note that this
    <with|font-series|bold|does not> alter the value of the preference
    <scm|"updater:interval">, whose use is preferred.
  </explain>

  <\explain>
    <scm|(check-updates-interval <scm-arg|boolean>)><explain-synopsis|sets
    the update interval>
  <|explain>
    Tells <TeXmacs> whether to automatically check for updates. Note that
    this <with|font-series|bold|does not> alter the value of the preference
    <scm|"updater:automatic-checks">, whose use is preferred.
  </explain>

  The following preferences determine the behaviour of the automatic update
  system:

  <\explain>
    <scm|("updater:appcast" <scm-arg|url>)><explain-synopsis|preference>
  <|explain>
    The <name|url> to the appcast which will be used by the startup check. An
    empty or undefined value will deactivate both automatic and manual
    checks.
  </explain>

  <\explain>
    <scm|("updater:automatic-checks" <scm-arg|boolean>)><explain-synopsis|preference>
  <|explain>
    Whether <TeXmacs> should automatically look for updates in the background
    (some time) after startup. Use <scm|"updater:check-interval"> to set the
    number of hours to wait between checks.
  </explain>

  <\explain>
    <scm|("updater:check-interval" <scm-arg|integer>)><explain-synopsis|preference>
  <|explain>
    How often should <TeXmacs> look for updates? The interval is given in
    hours, with a minimum of one. Setting this to zero deactivates automatic
    checks by setting <scm|"updater:automatic-checks"> to false.
  </explain>

  <\explain>
    <scm|("updater:public-dsa-key" <scm-arg|url>)><explain-synopsis|preference>
  <|explain>
    The file with the public <name|DSA> key to use to verify the digital
    signature of releases. This feature is currently (Aug. 2013) only
    supported under <name|MacOS>, but the preference value is ignored:
    <name|Sparkle> will use the value set in the
    <verbatim|SUPublicDSAKeyFile> key in the application bundle's
    <tt|Info.plist> dictionary.
  </explain>

  <tmdoc-copyright|2013|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>