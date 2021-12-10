<TeXmacs|2.1.1>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Sources><tmweb-title|Compiling <TeXmacs> from the
  source code|<tmweb-download-links>>

  On this page, we explain how to compile <TeXmacs> from the source code on
  <name|Unix>-like systems, including <name|MacOS> and <name|Windows> with
  <name|Mingw> or <name|Cygwin>. In the case of <name|Windows>, you may first
  want to install the appropriate build environment and follow our
  <hlink|instructions for compilation under <name|Windows>|winqt.en.tm>.

  <section|Verify the <TeXmacs> dependencies>

  Before you install <TeXmacs> on your system, you have to make sure that you
  have the other programs on which <TeXmacs> depends, namely:

  <\description>
    <item*|<hlink|<name|Qt>|http://qt.nokia.com/products/>>A cross-platform
    library for the development of user interfaces.

    <item*|<hlink|<name|Guile>|http://www.gnu.org/s/guile/>>The GNU
    implementation of <name|Scheme>, which is used as an extension language.
    Please note that as of Oct. 2013, <name|Guile> 2.x is
    <with|font-series|bold|not supported>; you will need <name|Guile> 1.8.

    <item*|<hlink|<name|Freetype2>|http://www.freetype.org/freetype2/index.html>>A
    library for font rendering.
  </description>

  Some more software that you might wish to install for more functionality
  and better performance are <hlink|<name|Ghostscript>|http://pages.cs.wisc.edu/~ghost/>,
  <hlink|<name|Aspell>|http://aspell.net/>,
  <hlink|<name|Libiconv>|http://www.gnu.org/s/libiconv/index.html>,
  <hlink|<name|Netpbm>|http://netpbm.sourceforge.net/>,
  <hlink|<name|ImageMagick>|www.imagemagick.org/> and
  <hlink|<name|Sparkle>|http://sparkle.andymatuschak.org/> or
  <hlink|<name|WinSparkle>|http://winsparkle.org/>.

  <section|Download and unpack the source code>

  <paragraph|Download the latest version>

  Download the <hlink|latest version|<merge|https://www.texmacs.org/Download/ftp/tmftp/source/|<merge|<TeXmacs-version-release|devel>|-src.tar.gz>>>
  of the source code, \ <verbatim|cd> into the directory where you wish to
  compile <TeXmacs> and type

  <\shell-code>
    tar -zxvf <merge|<TeXmacs-version-release|devel>|-src.tar.gz>
  </shell-code>

  All files will be unpacked into the directory
  <with|font-family|tt|<merge|<TeXmacs-version-release|devel>|-src>>, which
  is the 'installation directory'.

  <paragraph|Download the current development version>

  Since <TeXmacs>-1.0.7.1, the development of <TeXmacs> is done using the
  <hlink|Subversion|http://subversion.tigris.org/> (<name|SVN>) concurrent
  versioning system. In order to download the current <name|SVN> version of
  <TeXmacs> in read-only mode, you should type the following in a console,
  while at a directory of your choice where the sources will be saved

  <\shell-code>
    svn co svn://svn.savannah.gnu.org/texmacs/trunk/src
  </shell-code>

  Notice that you may also <hlink|browse|http://svn.savannah.gnu.org/viewvc/trunk/?root=texmacs>
  the current <TeXmacs> sources on the web. The <TeXmacs> website and several
  other things are also maintained through <name|SVN>; just replace
  <verbatim|src> by <verbatim|web> or <verbatim|misc> in the above checkout
  command.

  In order to commit changes to the <name|SVN> repository, you first need a
  user account on <hlink|<name|Savannah>|http://savannah.gnu.org> and send a
  request to join the <TeXmacs> developers team. You may then checkout and
  commit changes as described on <hlink|the subversion page for
  <TeXmacs>|https://savannah.gnu.org/svn/?group=texmacs>. You may also use
  <name|Savannah> in order to submit <hlink|patches|../contact/patches.en.tm>
  to <TeXmacs>. A <hlink|Git|http://git-scm.com/> mirror of the main
  Subversion repository is maintained at <hlink|Github|https://github.com/texmacs/texmacs>.

  <section|Compile, install and run>

  <TeXmacs> supports the standard <name|GNU> compilation and installation
  procedure. Assuming that you logged yourself in as root (needed only for
  the install command), <verbatim|cd> into the installation directory and
  type

  <\shell-code>
    ./configure

    make

    make install
  </shell-code>

  <\warning*>
    If you have both version 4 and version 5 of the <name|Qt> libraries
    installed in your system, compilation will fail unless you select the
    former. This can be done by preceding the above commands with
    <shell|export QT_SELECT=4> or <shell|setenv QT_SELECT 4> depending on
    your shell (you may need to <shell|make clean> before).
  </warning*>

  The first command examines your particular system configuration. The second
  command launches the compilation. The last command installs <TeXmacs> in
  <with|font-family|tt|/usr/local>. Under <name|MacOS> the recommended way is
  to run <shell|make MACOS_BUNDLE> instead of <shell|make> and <shell|make
  install>. This will create a <tt|<merge|<TeXmacs-version-release|devel>|.app>>
  application bundle in <tt|../distr> which you can move and open as any
  other application. For other systems, if everything works fine with the
  commands above you should be able to run <TeXmacs> by typing

  <\shell-code>
    texmacs &
  </shell-code>

  If this does not work you should make sure that
  <with|font-family|tt|/usr/local/bin> is in your <with|font-family|tt|PATH>.
  Depending on your shell, you can ensure this by typing

  <\shell-code>
    export PATH=/usr/local/bin:$PATH
  </shell-code>

  or

  <\shell-code>
    setenv PATH /usr/local/bin:$PATH
  </shell-code>

  <section|Configuration and build options>

  If you cannot log yourself in as root, or if you want to install <TeXmacs>
  elsewhere than in <with|font-family|tt|/usr/local>, then you should use

  <\shell-code>
    ./configure --prefix=<with|color|brown|[target directory]>
  </shell-code>

  instead of <with|font-family|tt|./configure>. In this case, <TeXmacs> will
  be installed in <with|font-family|tt|<with|color|brown|[target directory]>>
  and you will have to set your <with|font-family|tt|PATH> accordingly, as to
  contain <with|font-family|tt|<with|color|brown|[target directory]>/bin>.

  The configuration method

  <\shell-code>
    ./configure --disable-qt
  </shell-code>

  allows you to build the historical X11 version of <TeXmacs> instead of the
  <name|Qt>-based version. For more configuration options, type
  <shell|./configure --help>. For systems where both <name|Guile> 1.8 and 2.0
  are installed you may use the following command for configuration:

  <\shell-code>
    ./configure \\

    GUILE_CFLAGS="`pkg-config --static --cflags guile-1.8`" \\

    GUILE_LDFLAGS="`pkg-config --static --libs guile-1.8`" \\

    GUILE_DATA_PATH="`pkg-config --variable=datadir guile-1.8`" \\

    GUILE_VERSION="`pkg-config --modversion guile-1.8`"
  </shell-code>

  For more build methods, please look at the <name|Makefile>.

  <section|Happy <TeXmacs>-ing!>

  If you like the program, then please consider
  <hlink|donating|../contribute/donations.en.tm> money or services to us. Of
  course, you may also <hlink|contribute|../contribute/contribute.en.tm>
  yourself. In case of problems, please <hlink|subscribe|../home/ml.en.tm> to
  the <verbatim|texmacs-dev> or <verbatim|texmacs-users> mailing lists and
  ask your questions there. You may also directly
  <hlink|contact|../contact/contact.en.tm> us, but you might need to be more
  patient.

  <tmdoc-copyright|1999\U2019|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>