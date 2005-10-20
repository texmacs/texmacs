<TeXmacs|1.0.5.11>

<style|tmweb>

<\body>
  <tmweb-current|Download|Sources><tmweb-title|Suggestions for packaging
  <TeXmacs>|<tmweb-download-links>>

  In the case that you wish to package <TeXmacs> for some <name|Linux>,
  <name|Unix> or <name|Knoppix> distribution, it may be useful to be aware of
  a few points. Depending on the type of distribution and its physical
  support, this will allow you to optimize as a function of size, speed, or
  required dependencies.

  <paragraph|General points>

  The development releases of <TeXmacs> carry four numbers, like
  <verbatim|1.0.4.6> or <verbatim|1.0.5.7>. The stable releases either two or
  three, like <verbatim|1.0>, <verbatim|1.0.4> or <verbatim|1.1>. Stable
  releases are rather frequent (twice or thrice a year), so we recommend to
  use them for all major distributions.

  Please send us an email if you maintain a <TeXmacs> package for some
  distribution, so that we can maintain a list with distributions which
  support <TeXmacs>.

  <paragraph|<TeXmacs> dependencies>

  Theoretically speaking, <TeXmacs> only depends on <name|X> and <name|Guile>
  in order to be built on <name|Linux>. In<nbsp>addition, you either need
  <name|FreeType 2> or a <TeX>/<LaTeX> distribution such as Te<TeX> for the
  fonts (see also below).

  However, several more specific features of <TeXmacs> depend on external
  programs. In particular, spell checking makes use of <name|Ispell> or
  <name|Aspell>, the rendering of images depends on <name|Ghostview> and/or
  <name|Imlib2>, the <name|Iconv> library is needed for the <name|Html>
  converters, and we make use of several command-line utilities and
  <name|ImageMagick> for conversions between different image formats. The
  <TeXmacs> package should at least recommend the installation of these other
  programs.

  It should be noticed that <name|Imlib<nbsp>2> contains a major bug, which
  makes it impossible to use it in a<nbsp>static binary for <TeXmacs>. For
  this reason, we have disabled the use of this library by default.
  Nevertheless, in order to accelerate the rendering of non-<name|Postscript>
  images, we recommend the use of this library when using shared linking. In
  that case, you should configure using the <verbatim|--with-imlib2=yes>
  option. In the future, we plan to find a more reliable substitute for
  <name|Imlib 2>.

  We also notice that external libraries like <name|Freetype 2> and
  <name|Imlib<nbsp>2> can be linked against <TeXmacs> either at compilation
  or at run time. In the second case, the library will only be used when it
  is present on the system, thereby removing one hard dependency. In order to
  force linking at compilation time, configure using
  <verbatim|--with-imlib2=linked> and similarly for other libraries.

  <paragraph|Font issues>

  In order to keep the official <TeXmacs> distributions reasonably small,
  they are shipped with only a minimal set of <name|Type 1> fonts. For
  distributions on a <name|CD> or <name|DVD>, we recommend to include a more
  complete set of <hlink|extra Type 1 fonts|fonts.en.tm#fonts-tarball>.
  Typically, the extra fonts should be merged with the set of minimal fonts
  and distributed in a separate package. The main <TeXmacs> package should
  then depend on the font package.

  Notice that <TeXmacs> is no longer dependent on a <TeX>/<LaTeX>
  distribution such as Te<TeX> for the fonts. In the case when you want to
  remove the dependency on Te<TeX>, then you need to provide an even more
  <hlink|complete set of extra fonts|fonts.en.tm#fonts-complete>.

  On the other limit, if Te<TeX> is installed, then no extra <name|Type 1>
  fonts are really essential for <TeXmacs> to work, since <name|Metafont>
  will be able to generate them automatically. However, this process is quite
  long and tends to frighten new users. In the case of <name|Knoppix>
  distributions, the situation is even worse, because all fonts created by
  <name|Metafont> are lost whenever you turn of the computer.

  <paragraph|Improving the boot speed>

  On <name|Knoppix> systems, it may be interested to shortcut several things
  which are done when you run <TeXmacs> for the first time, by making use of
  the <TeXmacs> cache. In order to do so, install a brandnew version of
  <TeXmacs> and remove your <verbatim|~/.TeXmacs> directory. Start <TeXmacs>
  once and ask the program to build the complete user manual using
  <menu|Help|Full manuals|User manual>. When <TeXmacs> will be done,
  carefully copy the files

  <\code>
    \ \ \ \ ~/.TeXmacs/system/settings.scm

    \ \ \ \ ~/.TeXmacs/system/setup.scm

    \ \ \ \ ~/.TeXmacs/system/cache/*
  </code>

  to some location in the <TeXmacs> distributiuon. You may now modify the
  <TeXmacs> script so as to copy these files back whenever the file
  <verbatim|~/.TeXmacs/system/settings.scm> does not exist (before booting
  <TeXmacs> in the usual way). This should reduce the boot time to a few
  seconds.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>