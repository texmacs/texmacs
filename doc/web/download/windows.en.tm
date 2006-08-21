<TeXmacs|1.0.6.6>

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
    <item>Download <hlink|CygTeXmacs.zip|ftp://ftp.texmacs.org/pub/TeXmacs/cygwin/CygTeXmacs.zip>
    (82Mb). If you don't have ftp access, then you may also
    <hlink|load|http://www.texmacs.org/Download/ftp/cygwin/CygTeXmacs.zip>
    this file from the web.

    <item>Unzip the file in the directory <verbatim|C:\\CygTeXmacs> (or any
    other directory without whitespace characters).

    <item>Launch <TeXmacs> by clicking on
    <verbatim|C:\\CygTeXmacs\\starttexmacs.bat>.
  </enumerate>

  <section|Notes>

  <\itemize>
    <item>Although the <name|Windows> version of <TeXmacs> does not require
    <TeX>/<LaTeX> to be installed on your system, you might still want to
    install such a distribution in order to increase the number of available
    fonts, or in order to compile bibliographies using <name|Bib><TeX>.
    Therefore, if you have a fast internet connection, then you may want to
    download and install <hlink|<name|Mik><TeX>|http://www.miktex.org/>.

    <item>If you installed <name|Mik><TeX> in addition to <TeXmacs>, then
    missing fonts may be generated automatically. This results in a
    significant slow-down of <TeXmacs>. Don't panic: the fonts have to be
    generated only once. Subsequent runs of <TeXmacs> will be fast.

    <item><TeXmacs> can be used in combination with the <name|Windows>
    version of <hlink|<name|Maxima>|http://maxima.sourceforge.net/>. At the
    moment, this requires you to install <name|Maxima> at the standard place
    which is proposed during its installation.
  </itemize>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>