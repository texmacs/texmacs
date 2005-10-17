<TeXmacs|1.0.5.10>

<style|tmweb>

<\body>
  <tmweb-current|Download|Fonts><tmweb-title|Extra fonts for GNU
  <TeXmacs>|<tmweb-download-links>>

  GNU <TeXmacs> has been designed in order to cooperate with the Te<TeX>
  distribution of <TeX> on <name|Unix> systems and in particular under
  GNU/<name|Linux>. The Te<TeX> distribution comes with several Type<nbsp>1
  fonts as well as the <name|Metafont> system to generate additional bitmap
  fonts.

  Unfortunately, the Type 1 fonts shipped with Te<TeX> are not suitable for
  most European languages. The default version of <TeXmacs> therefore comes
  with a few additional fonts and falls back on <name|Metafont> for missing
  fonts. Nevertheless, more additional fonts can be downloaded both as an
  <hlink|RPM package|#fonts-rpm> or as a <hlink|tarball|#fonts-tarball>.
  Moreover, in case that you did not install Te<TeX> (or another suitable
  <TeX> distribution), you may install an even more <hlink|complete set of
  fonts|#fonts-complete>.

  <paragraph|RPM with extra fonts><label|fonts-rpm>

  If you installed <TeXmacs> using the RPM, then you may download an
  <hlink|RPM (5.0 Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-extra-fonts-1.0-1.noarch.rpm>
  with extra fonts and install it using

  <\code>
    \ \ \ \ rpm -i TeXmacs-extra-fonts-1.0-1.noarch.rpm
  </code>

  <paragraph|Tarball with extra fonts><label|fonts-tarball>

  If you installed <TeXmacs> from the static binaries, then you may download
  a <hlink|tarball (5.8 Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-extra-fonts-1.0-noarch.tar.gz>
  with extra fonts. After downloading, <verbatim|cd> into the directory
  <verbatim|~/.TeXmacs> and unpack the fonts using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-extra-fonts-1.0-noarch.tar.gz \| tar xvf -
  </code>

  The same procedure applies when you installed <TeXmacs> from the source
  code. In that case, and before compiling the source code, you have to
  unpack the tarball in the <verbatim|TeXmacs> subdirectory of the directory
  with the source code. Again, this directory should have a subdirectory
  <verbatim|fonts>.

  <paragraph|Tarball for systems without <TeX>/<LaTeX>><label|fonts-complete>

  Whenever you don't have a suitable <TeX>/<LaTeX> distribution like Te<TeX>
  installed on your system, then you may download a <hlink|tarball (10.0
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-windows-fonts-1.0-noarch.tar.gz>
  with a reasonably complete set of Type 1 fonts for basic use of <TeXmacs>
  (in fact, these are precisely the fonts which are provided in the native
  <name|Windows> version of <TeXmacs>). After downloading, <verbatim|cd> into
  the directory <verbatim|~/.TeXmacs> and unpack the fonts using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-windows-fonts-1.0-noarch.tar.gz \| tar xvf -
  </code>

  <\remark>
    If you do not have ftp access, then you might wish to
    <hlink|download|http://www.texmacs.org/Download/ftp> from the web.
  </remark>

  <\remark>
    More exotic fonts may be added by the user in the directory of
    <verbatim|~/.TeXmacs/fonts>). Just add additional <verbatim|tfm> files to
    the <verbatim|tfm> subdirectory, <verbatim|pfb> files to the
    <verbatim|pfb> subdirectory and so on. Notice that <TeXmacs> only
    recognizes a subset of the fonts which come with traditional <TeX>
    distributions. To find out whether a particular font is supported, check
    the files in the directory <verbatim|$TEXMACS_PATH/progs/fonts>.
  </remark>

  <tmdoc-copyright|1999--2005|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>