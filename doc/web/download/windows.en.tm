<TeXmacs|1.0.3.7>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  We recently released an <with|mode|math|\<alpha\>>-version of the
  <name|Windows>-port of <TeXmacs>. This <name|Windows> version of <TeXmacs>
  is easy to install and should provide you with the basic functionality of
  the program. However, the port is still incomplete and a list with
  <hlink|known problems|#problems> is given below. For a fully operational
  <TeXmacs>, you may either install <hlink|<name|Cygwin>|cygwin.en.tm> on
  your <name|Windows> system, or migrate to <no-break><name|Unix>. You may
  also <hlink|help us|#help> with improving the <name|Windows> port.

  <section|Installation of <TeXmacs>>

  In order to install <TeXmacs> on Windows, you should:

  <\enumerate>
    <item>Download <hlink|wintexmacs.exe|ftp://ftp.texmacs.org/pub/TeXmacs/windows/wintexmacs.exe>
    (18Mb).

    <item>Unpack the <TeXmacs> archive by executing
    <verbatim|wintexmacs.exe>. You have to specify a folder where you wish to
    unpack the <TeXmacs> archive. You may delete this folder as well as
    <verbatim|wintexmacs.exe> when the installation will be complete.

    <item>In the folder where you unpacked <TeXmacs>, launch the installer
    <verbatim|setup.exe> and follow the instructions on the screen.

    <item>You should now be able to launch <TeXmacs> from the applications or
    accessories submenu of your main Windows menu on the bottom left of the
    screen.
  </enumerate>

  <section|Optional helper programs>

  Currently, <TeXmacs> saves its files in <name|PostScript> before printing
  them. If you did not install any software on your system to print or
  previsualize <name|PostScript> files, then you might wish to install
  <hlink|<name|Ghostscript> <abbr|resp.> <name|Ghostview>|http://www.cs.wisc.edu/~ghost/>.

  Although the <name|Windows> version of <TeXmacs> does not require
  <TeX>/<LaTeX> to be installed on your system, you might still want to
  install such a distribution in order to increase the number of available
  fonts, or in order to compile bibliographies using <name|Bib><TeX>.
  Therefore, if you have a fast internet connection, then you may want to
  download and install <hlink|<name|Mik><TeX>|http://www.miktex.org/>.

  <section|Known problems><label|problems>

  <\itemize>
    <item>The <key|AltGr>-key, some dead keys and some other keys do not work
    on my French keyboard.

    <item>There are problems with displaying <name|Postscript> images.

    <item>The online help system (using <key|F1>) is broken.

    <item>Connections via pipes have not been implemented yet. Therefore, you
    cannot use <TeXmacs> as an interface to computer algebra systems.

    <item><TeXmacs> is not yet web-aware.

    <item>Characters are not correctly clipped when being displayed.

    <item>In order to compile bibliographies, you need <name|Bib><TeX>.
  </itemize>

  <section|Helping us><label|help>

  The <name|Windows> port has essentially been made by <name|Dan Martens> as
  a part of a project for his studies. However, he has no time for serious
  maintaining and further development, unless somebody is willing to pay for
  that. By <hyper-link|donating|../contribute/donations.en.tm> a few thousand
  dollars or euros to the <TeXmacs> project, it is very likely that he can
  make the <name|Windows> port as good as the native <name|Linux> version.
  For more precise requests and information, please <hyper-link|contact
  us|../contact/contact.en.tm>. If you are a developer, then you may also
  want to dive into the source code and improve/complete it.

  <section|The source code for Windows>

  <name|Dan Martens> used <name|Microsoft Visual Studio> in order to compile
  <TeXmacs>. The source code of the <name|Windows> port can be retrieved from
  our <hyper-link|CVS|cvs.en.tm> server using

  <verbatim| \ \ \ cvs co windows>

  From the point where you checked out, you will need to do the following:

  <\verbatim>
    \ \ \ \ rm -rf src/TeXmacs/fonts/pk

    \ \ \ \ rm -rf src/TeXmacs/fonts/tfm

    \ \ \ \ cp -R windows/TeXmacs/fonts/pk src/TeXmacs/fonts

    \ \ \ \ cp -R windows/TeXmacs/fonts/tfm src/TeXmacs/fonts

    \ \ \ \ cp -R windows/TeXmacs/fonts/type1 src/TeXmacs/fonts

    \ \ \ \ cp -R windows/src/Winport src/src
  </verbatim>

  You will now find the project file in

  <verbatim| \ \ \ src/Winport/Tmwin>

  After setting the paths (Dan...?) and configuring <name|Visual Studio>
  (Dan...?), you should now be able to build <TeXmacs>.

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
  </collection>
</initial>