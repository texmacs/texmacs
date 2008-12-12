<TeXmacs|1.0.7>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Compiling Qt-<TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  <section|Downloading dependencies><label|install>

  Create a directory where to download external software, such as
  <verbatim|C:\\download>, and download

  <\quote-env>
    <\verbatim>
      <hlink|ftp://ftp.trolltech.com/qt/source/qt-win-opensource-4.4.3-mingw.exe|ftp://ftp.trolltech.com/qt/source/qt-win-opensource-4.4.3-mingw.exe>

      <hlink|http://downloads.sourceforge.net/mingw/MSYS-1.0.10.exe|http://downloads.sourceforge.net/mingw/MSYS-1.0.10.exe>

      <hlink|http://downloads.sourceforge.net/mingw/msysDTK-1.0.1.exe|http://downloads.sourceforge.net/mingw/msysDTK-1.0.1.exe>

      <hlink|http://ftp.gnu.org/gnu/libtool/libtool-2.2.6a.tar.gz|http://ftp.gnu.org/gnu/libtool/libtool-2.2.6a.tar.gz>

      <hlink|http://ftp.sunet.se/pub/gnu/gmp/gmp-4.2.4.tar.bz2|http://ftp.sunet.se/pub/gnu/gmp/gmp-4.2.4.tar.bz2>

      <hlink|http://download.savannah.gnu.org/releases/freetype/freetype-2.3.7.tar.gz|http://download.savannah.gnu.org/releases/freetype/freetype-2.3.7.tar.gz>

      <hlink|http://ftp.gnu.org/pub/gnu/guile/guile-1.8.4.tar.gz|http://ftp.gnu.org/pub/gnu/guile/guile-1.8.4.tar.gz>

      <hlink|ftp://ftp.texmacs.org/pub/TeXmacs/misc/guile.patch|ftp://ftp.texmacs.org/pub/TeXmacs/misc/guile.patch>

      <hlink|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-windows-fonts-1.0-noarch.tar.gz|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-windows-fonts-1.0-noarch.tar.gz>
    </verbatim>
  </quote-env>

  <section|Install <name|Qt> and <name|MinGW>>

  Install <name|Qt 4.4.3> (by executing <verbatim|qt-win-opensource-4.4.3-mingw.exe>)
  at the standard location <verbatim|C:\\Qt\\4.4.3>. During the installation
  process, you will be prompted whether you want to install <name|MinGW>.
  Answer yes, and install <name|MinGW> at the standard location
  <verbatim|c:\\MinGW>.

  <section|Install <name|Msys>>

  Install <name|Msys 1.0.10> (by executing <verbatim|MSYS-1.0.10.exe>) at the
  standard location <verbatim|C:\\msys\\1.0>. Also install the additional
  developer toolkit (by executing <verbatim|msysDTK-1.0.10.exe>). During the
  installation process, you will be prompted for the location
  <verbatim|c:\\MinGW> of <name|MinGW>.

  <section|Install simple dependencies>

  <\itemize>
    <item>Launch an <name|Msys> shell for <name|MinGW>.

    <item>Create the directories <verbatim|/usr/local> and
    <verbatim|/usr/local/src> for installing and building the dependencies.

    <item>Add <verbatim|/usr/local/bin> to your <verbatim|PATH>.

    <item>Decompress and untar the sources for
    <verbatim|libtool-2.2.6a.tar.gz>, <verbatim|gmp-4.2.4.tar.gz> and
    <verbatim|freetype-2.3.7.tar.gz>.

    <item>Perform standard configure, make, make install procedure (with
    <verbatim|--prefix=/usr/local>).
  </itemize>

  <section|Install <name|Guile>>

  <\itemize>
    <item>Decompress and untar the sources for <verbatim|guile-1.8.4.tar.gz>.

    <item>Apply the the patch <verbatim|guile.patch>.

    <item>Perform standard configure, make, make install, using the following
    configure command:

    <\shell-fragment>
      ./configure \\

      \ \ --prefix=/usr/local \\

      \ \ CPPFLAGS="-L/usr/local/include" \\

      \ \ LDFLAGS="-L/usr/local/lib"
    </shell-fragment>
  </itemize>

  <section|Prepare <TeXmacs>>

  <\itemize>
    <item>Download the latest <name|Svn> sources of <TeXmacs>, say at
    <verbatim|~/texmacs/src>.

    <item>Copy <verbatim|TeXmacs-windows-fonts-1.0-noarch.tar.gz> to
    <verbatim|~/texmacs/src/TeXmacs> and decompress the fonts there.

    <item>Uncomment the line

    <\scm-fragment>
      (debug-set! stack 1000000)
    </scm-fragment>

    in <verbatim|~/texmacs/src/TeXmacs/progs/kernel/boot/compat.scm>.
  </itemize>

  <section|Compile <TeXmacs>>

  <\itemize>
    <item>Set the paths

    <\shell-fragment>
      export TEXMACS_PATH=$HOME/texmacs/src/TeXmacs/

      export GUILE_LOAD_PATH=/usr/local/share/guile/1.8/

      export TEXMACS_PWD=$HOME

      export PATH=$TEXMACS_PATH/bin:$PATH
    </shell-fragment>

    <item>Perform standard configure, make, make install, using the following
    configure command:

    <\shell-fragment>
      ./configure \\

      \ \ --enable-qt \\

      \ \ --prefix=/usr/local/ \\

      \ \ CPPFLAGS="-I/usr/local/include" \\

      \ \ LDFLAGS="-L/usr/local/lib" \\

      \ \ GUILE_LOAD_PATH="/usr/local/share/guile/1.8/"
    </shell-fragment>

    <item>Run

    <\shell-fragment>
      texmacs
    </shell-fragment>
  </itemize>

  <tmdoc-copyright|2008|Massimiliano Gubinelli|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>