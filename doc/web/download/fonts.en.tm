<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Download|Fonts><tmweb-title|Using <TeX> fonts with GNU
  <TeXmacs>|<apply|tmweb-download-links>>

  GNU <TeXmacs> has been designed in order to cooperate with the Te<TeX>
  distribution of <TeX>. This distribution (usually) comes with the programs
  <with|font family|tt|MakeTeXTFM> and <with|font family|tt|MakeTeXPK> to
  generate new fonts automatically using MetaFont. Furthermore, it (usually)
  provides the utility <with|font family|tt|kpsepath>, for finding the
  different directories where <TeX> fonts have been installed.

  When starting <TeXmacs> for the first time, the program examines the way
  your <TeX> system has been installed. The result of this examination is
  written the file <with|font family|tt|$TEXMACS_HOME_PATH/system/TEX_PATHS>.
  By default, <with|font family|tt|$TEXMACS_HOME_PATH> contains <with|font
  family|tt|~/.TeXmacs>. Whenever you change your <TeX> system, or if you add
  new fonts, it may be a good idea to remove this file in order to recreate
  it. In the directory <with|font family|tt|$TEXMACS_HOME_PATH/fonts/error>,
  you also find a trace of all fonts which <TeXmacs> failed to create. When
  adding new fonts, you should remove all files in this directory.

  If the user did not install the Te<TeX> distribution or no <TeX>
  distribution at all, we provide by default a limited set of compiled fonts
  in <with|font family|tt|$TEXMACS_PATH/data/tfm> and <with|font
  family|tt|$TEXMACS_PATH/data/pk>. The user may also specify his own paths
  <with|font family|tt|TEX_TFM_PATH> and <with|font family|tt|TEX_PK_PATH>
  for .tfm and .pk files respectively. <TeXmacs> uses the following algorithm
  to find fonts:

  <\itemize>
    <item>Look whether the font is in one of directories specified by the
    user paths <with|font family|tt|TEX_TFM_PATH> or <with|font
    family|tt|TEX_PK_PATH>.

    <item>Look whether the font is installed by default in <with|font
    family|tt|$TEXMACS_PATH/data/tfm> or <with|font
    family|tt|$TEXMACS_PATH/data/pk>.

    <item>Look whether the font was already generated in the Te<TeX>
    distribution (using <with|font family|tt|kpsepath>).

    <item>Automatically generate the font using <with|font
    family|tt|MakeTeXTFM> and/or <with|font family|tt|MakeTeXPK>.
  </itemize>

  We also precompiled a larger set of fonts at 600 dpi resolution, which can
  be added to the set of default fonts from <TeXmacs>. In order to do so,
  first download <hlink|Extra default fonts (2.9
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/targz/TeXmacs-600dpi-fonts.tar.gz>.
  Next unpack them in the installation directory using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-600dpi-fonts.tar.gz \| tar xvf -
  </code>

  If you installed <TeXmacs> using an rpm package, then you should rather
  download the <hlink|rpm font package (2.9
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/TeXmacs-fonts-1.0-1.noarch.rpm>
  and install it using

  <\code>
    \ \ \ \ rpm -i TeXmacs-fonts-1.0-1.noarch.rpm
  </code>

  If you do not have ftp access, then you might wish to
  <hlink|download|http://www.texmacs.org/Download/ftp> from the web.

  <apply|tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>