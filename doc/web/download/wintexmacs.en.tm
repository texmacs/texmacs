<TeXmacs|1.0.6.4>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing
  Win<TeXmacs>|<tmweb-download-links>>

  Version 1.0.5 has been ported to <name|Windows>. This <name|Win><TeXmacs>
  version has has the advantage of being <hlink|easy to install|#install> and
  relatively modest in size (less than 20Mb). However, by lack of volunteers,
  the <name|Windows> version is no longer supported. In order to run more
  recent versions of <TeXmacs> under <name|Windows>, we either recommend the
  use of the <hlink|<name|Cygwin> version|cygwin.en.tm> or the easy to
  install <name|Cyg<TeXmacs>>, which is internally based on <name|Cygwin>.

  <section|Installation of Win<TeXmacs>><label|install>

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
  <hlink|donating|../contribute/donations.en.tm> to the <TeXmacs> project,
  you may make this possible. For more precise requests and information,
  please <hlink|contact us|../contact/contact.en.tm>. If you are a developer,
  then you may also want to dive into the source code and improve it.

  <section|The source code for Windows>

  In order to compile <TeXmacs> under <name|Windows> using <name|Microsoft
  Visual Studio>, you have to do the following:

  <\enumerate>
    <item>Make sure that the latest <name|Visual Studio> service packs are
    installed.

    <item>Download and install the <hlink|<name|Windows
    SDK>|http://www.microsoft.com/downloads/details.aspx?FamilyId=A55B6B43-E24F-4EA3-A93E-40C0EC4F68E5&displaylang=en>.

    <item>Download and install <verbatim|<hlink|<merge|<TeXmacs-version-release|win>|-src.exe>|ftp://ftp.texmacs.org/pub/TeXmacs/windows/stable/<merge|<TeXmacs-version-release|win>|-src.exe>>>.
    You will have to check ``source code'' during the installion.

    <item>Open the project file <verbatim|C:\\WinTeXmacs\\src\\Winport\\Tmwin\\tmwin.dsw>
    in <name|Visual Studio>.

    <item>Compile.
  </enumerate>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>