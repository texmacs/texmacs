<TeXmacs|1.0.6.4>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  In order to install <TeXmacs> under <name|Windows>, you have three options:

  <\enumerate>
    <item>For simple users, we recommend the installation of
    <name|Cyg<TeXmacs>> as <hlink|explained below|#install>. From an internal
    point of view, this version of <TeXmacs> is based on
    <hlink|<name|Cygwin>|http://www.cygwin.com>, a Linux-like environment for
    Windows. For convenience of the user, we provide a simple stand-alone
    installer which requires no prior installation of <name|Cygwin> itself.

    <item>If you plan to use other programs from the <name|Unix> world
    besides <TeXmacs>, then we recommend the <hlink|installation of
    <name|Cygwin>|cygwin.en.tm> itself. Inside <name|Cygwin>, the <TeXmacs>
    program is available as one of the packages.

    <item>If you have slow internet access, then you may also consider the
    installation of the native <hlink|Win<TeXmacs>|wintexmacs.en.tm> port of
    <TeXmacs>, which is both small and easy to install. Unfortunately, the
    latest available version is 1.0.5, because we no longer have time or
    volunteers to maintain and further develop this native port.
  </enumerate>

  <section|Installation of <TeXmacs>><label|install>

  In order to install the Cyg<TeXmacs> version of <TeXmacs> on Windows, you
  should:

  <\enumerate>
    <item>Download <hlink|CygTeXmacs.exe|ftp://ftp.texmacs.org/pub/TeXmacs/cygwin/CygTeXmacs.exe>
    (59Mb). If you don't have ftp access, then you may also
    <hlink|load|http://www.texmacs.org/Download/ftp/cygwin/CygTeXmacs.exe>
    this file from the web.

    <item>Install <TeXmacs> by executing <verbatim|CygTeXmacs.exe> and
    following the instructions on the screen.

    <item>You should now be able to launch <TeXmacs> from the launcher in the
    directory where you decided to install <TeXmacs>.
  </enumerate>

  <section|Optional helper programs>

  Although the <name|Windows> version of <TeXmacs> does not require
  <TeX>/<LaTeX> to be installed on your system, you might still want to
  install such a distribution in order to increase the number of available
  fonts, or in order to compile bibliographies using <name|Bib><TeX>.
  Therefore, if you have a fast internet connection, then you may want to
  download and install <hlink|<name|Mik><TeX>|http://www.miktex.org/>.

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>