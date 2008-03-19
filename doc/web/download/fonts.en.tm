<TeXmacs|1.0.6>

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

  Also, <TeXmacs> 1.0.6.1 and later comes with support for Chinese, Japanese
  and Korean to <TeXmacs>. This requires the installation of additional
  <hlink|CJK fonts|#fonts-cjk>.

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
  <verbatim|~/.TeXmacs> (which you have to create if it does not already
  exist) and unpack the fonts using

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
  the directory <verbatim|~/.TeXmacs> (which you have to create if it does
  not already exist) and unpack the fonts using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-windows-fonts-1.0-noarch.tar.gz \| tar xvf -
  </code>

  <paragraph|Fonts for Chinese, Japanese and Korean><label|fonts-cjk>

  From version 1.0.6.1 on, <TeXmacs> comes with support for Chinese, Japanese
  and Korean. However, the standard distributions do not include the
  necessary fonts, which can be downloaded separately:

  <\itemize>
    <item>Tarball with <hlink|Chinese fonts (6.8
    Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-chinese-fonts.tar.gz>.

    <item>Tarball with <hlink|Japanese fonts (9.1
    Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-japanese-fonts.tar.gz>.

    <item>Tarball with <hlink|Korean fonts (10.5
    Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-korean-fonts.tar.gz>.
  </itemize>

  After downloading, <verbatim|cd> into the directory <verbatim|~/.TeXmacs>
  (which you have to create if it does not already exist) and unpack the
  fonts using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-<em|language>-fonts.tar.gz \| tar xvf -
  </code>

  <\remark>
    If you do not have ftp access, then you might wish to
    <hlink|download|http://www.texmacs.org/Download/ftp/fonts> from the web.
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

  <\remark>
    Sometimes, it may happen that manually installed fonts are not
    immediately recognized. In that case, you may try to launch <TeXmacs>
    once using the command

    <\code>
      \ \ \ \ texmacs --delete-font-cache &
    </code>

    This should not be necessary for the installation of the officially
    supported extra fonts at their standard locations.
  </remark>

  <tmdoc-copyright|1999--2005|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>