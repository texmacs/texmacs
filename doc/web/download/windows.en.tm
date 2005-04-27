<TeXmacs|1.0.4.3>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  We recently released a <with|mode|math|\<beta\>>-version of the
  <name|Windows>-port of <TeXmacs>. This <name|Windows> version of <TeXmacs>
  is easy to install and should provide you with the basic functionality of
  the program. However, the port is still a bit buggy, so please let us know
  about possible problems.

  For a fully operational <TeXmacs>, you may either install
  <hlink|<name|Cygwin>|cygwin.en.tm> on your <name|Windows> system, or
  migrate to <no-break><name|Unix>. You may also <hlink|help us|#help> with
  improving the <name|Windows> port.

  <section|Installation of <TeXmacs>>

  In order to install <TeXmacs> on Windows, you should:

  <\enumerate>
    <item>Download <verbatim|<hlink|<merge|<TeXmacs-version-release|win>|.exe>|ftp://ftp.texmacs.org/pub/TeXmacs/windows/stable/<merge|<TeXmacs-version-release|win>|.exe>>>
    (15Mb).

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

    <item>Connections via pipes are still under development. Therefore, you
    cannot use <TeXmacs> as an interface to computer algebra systems.

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
  selecting the corresponding option during the installation procedure. By
  default, you will find the project file in

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