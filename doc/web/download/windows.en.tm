<TeXmacs|1.0.3.1>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  We recently released a <with|mode|math|\<beta\>>-version of the
  <name|Windows>-port of <TeXmacs>. This <name|Windows> version of <TeXmacs>
  is easy to install and should provide you with the basic functionality of
  the program. However, the port is still incomplete and a list with
  <hlink|known problems|#problems> is given below. For a fully operational
  <TeXmacs>, you may either install <name|Cygwin> on your <name|Windows>
  system, or migrate to <no-break><name|Unix>. You may also <hlink|help
  us|#help> with improving the <name|Windows> port.

  <section|Installation of <TeXmacs>>

  In order to install <TeXmacs> on windows, download ???, execute it, and
  follow the instructions which are given on your screen. If you install the
  <TeXmacs> icon, then you may launch the program by double clicking on it.

  <section|Optional helper programs>

  Currently, <TeXmacs> saves its files in <name|PostScript> before printing
  them. If you did not install any software on your system to print or
  previsualize <name|PostScript> files, then you might wish to install
  <name|Ghostscript> <abbr|resp.> <name|Ghostview>. This can be done by ???.

  Although the <name|Windows> version of <TeXmacs> does not require
  <TeX>/<LaTeX> to be installed on your system, you might still want to
  install such a distribution in order to increase the number of available
  fonts, or in order to compile bibliographies using <name|Bib><TeX>.
  Therefore, if you have a fast internet connection, then you may want to
  download and install <name|Mik><TeX>. This is done by ???.

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

  After setting the paths ... and configuring <name|Visual Studio> ..., you
  should now be able to build <TeXmacs>.

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-5|<tuple|5|?>>
    <associate|help|<tuple|4|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|problems|<tuple|3|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Install
      <with|font-shape|<quote|small-caps>|Cygwin> with
      <with|font-family|<quote|tt>|setup.exe>><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Download
      the T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>
      distribution><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Compile
      T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Finishing
      touches><value|toc-dots><pageref|toc-4><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>