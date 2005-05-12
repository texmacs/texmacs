<TeXmacs|1.0.5>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  Since <TeXmacs>-1.0.5, the <name|Windows>-port of <TeXmacs> has become
  sufficiently stable for the needs of most users. It has the advantage of
  being <hlink|easy to install|#install> and relatively modest in size (less
  than 20Mb). Some minor <hlink|problems|#problems> are still particular to
  the Windows port. We invite you to report any other problems or <hlink|help
  us|#help> to solve them.

  As an alternative, we also provide a <hlink|<name|Cygwin>|cygwin.en.tm>
  port of <TeXmacs>. This port behaves in a very similar way as the
  <name|Unix> version of <TeXmacs>, but it requires the prior installation of
  <name|Cygwin> and a<nbsp><LaTeX> system. It is likely to be a bit slower
  than the native <name|Windows> version, but the number of supported
  plug-ins is still a bit larger.

  <section|Installation of <TeXmacs>><label|install>

  In order to install <TeXmacs> on Windows, you should:

  <\enumerate>
    <item>Download <verbatim|<hlink|<merge|<TeXmacs-version-release|win>|.exe>|ftp://ftp.texmacs.org/pub/TeXmacs/windows/stable/<merge|<TeXmacs-version-release|win>|.exe>>>
    (18Mb). If you don't have ftp access, then you may also
    <hlink|download|http://www.texmacs.org/Download/ftp/windows/stable/<merge|<TeXmacs-version-release|win>|.exe>>
    this file from the web.

    <item>Install <TeXmacs> by executing <verbatim|<merge|<TeXmacs-version-release|win>|.exe>>
    and following the instructions on the screen. You may delete
    <verbatim|<merge|<TeXmacs-version-release|win>|.exe>> as soon as the
    installation is complete.

    <item>You should now be able to launch <TeXmacs> from the <TeXmacs> icon
    on your desktop or from the applications or accessories submenu of your
    main Windows menu on the bottom left of the screen.
  </enumerate>

  Notice that an uninstaller for <TeXmacs> is also provided in the
  applications or accessories submenu of your main Windows menu. When
  upgrading to a new version of <TeXmacs>, it is recommended to uninstall the
  previous version first.

  <section|Optional helper programs>

  Although the <name|Windows> version of <TeXmacs> does not require
  <TeX>/<LaTeX> to be installed on your system, you might still want to
  install such a distribution in order to increase the number of available
  fonts, or in order to compile bibliographies using <name|Bib><TeX>.
  Therefore, if you have a fast internet connection, then you may want to
  download and install <hlink|<name|Mik><TeX>|http://www.miktex.org/>.

  <section|Known problems><label|problems>

  <\itemize>
    <item>The online help system (using <key|F1>) is broken.

    <item>Connections via pipes are still under development. Some interfaces
    work (<name|Pari>, <name|Yacas> and <name|Maxima>), but others don't. Any
    help with getting these other ones working would be appreciated.

    <item>Popup menus sometimes remain visible after clicking on one of the
    items. In that case, clicking anywhere in the <TeXmacs> window should
    make them disappear.

    <item>In order to compile bibliographies, you need <name|Bib><TeX> (which
    is provided as part of <name|Mik><TeX>).
  </itemize>

  <section|Helping us><label|help>

  The first part of the <name|Windows> port has been made by <name|Dan
  Martens> as a part of a project for his studies. The second part has been
  financed by <hlink|<name|Springer Verlag>|http://www.springer.de/> and the
  <hlink|Omega group|http://www.ags.uni-sb.de/~omega/> at Saarbrücken. Dan is
  willing to make further improvements to the Windows port modulo payment. By
  <hyper-link|donating|../contribute/donations.en.tm> to the <TeXmacs>
  project, you may make this possible. For more precise requests and
  information, please <hyper-link|contact us|../contact/contact.en.tm>. If
  you are a developer, then you may also want to dive into the source code
  and improve it.

  <section|The source code for Windows>

  <name|Dan Martens> used <name|Microsoft Visual Studio> in order to compile
  <TeXmacs>. The source code of the <name|Windows> port can be installed by
  downloading <verbatim|<hlink|<merge|<TeXmacs-version-release|win>|-src.exe>|ftp://ftp.texmacs.org/pub/TeXmacs/windows/stable/<merge|<TeXmacs-version-release|win>|-src.exe>>>
  and selecting the corresponding option during the installation procedure.
  By default, you will find the project file in

  <verbatim| \ \ \ C:\\WinTeXmacs\\src\\Winport\\Tmwin>

  After configuring <name|Visual Studio> (Dan...?), you should now be able to
  build <TeXmacs>.

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>