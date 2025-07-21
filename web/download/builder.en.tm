<TeXmacs|2.1.4>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Sources><tmweb-title|Standard builders for
  <TeXmacs> developers|<tmweb-download-links>>

  <TeXmacs> developers may use the following download and build process for
  <TeXmacs> and its dependencies in order to be fully compatible with the
  main <TeXmacs> developers.

  <section|Under MacOS>

  Make sure that <hlink|homebrew|https://brew.sh/> is installed on your
  system and install the following packages:

  <\shell-code>
    brew install pkgconfig subversion texinfo cmake
  </shell-code>

  Now download the builder support from the official <TeXmacs> website and go
  the corresponding directory:

  <\shell-code>
    svn co svn://svn.savannah.gnu.org/texmacs/trunk/misc/builder

    cd builder
  </shell-code>

  You may now build all dependencies of <TeXmacs> and then the latest SVN
  version of <TeXmacs> itself:

  <\shell-code>
    ./scripts/build --os=macos-qt6
  </shell-code>

  <section|Under Linux (Ubuntu 24)>

  Make sure that the following packages are installed:

  <\shell-code>
    apt install subversion
  </shell-code>

  Now download the builder support from the official <TeXmacs> website and go
  the corresponding directory:

  <\shell-code>
    svn co svn://svn.savannah.gnu.org/texmacs/trunk/misc/builder

    cd builder
  </shell-code>

  You may now build all dependencies of <TeXmacs> and then the latest SVN
  version of <TeXmacs> itself:

  <\shell-code>
    ./scripts/build --os=appimage
  </shell-code>

  <section|Windows>

  Make sure that <hlink|msys2|https://www.msys2.org> together with the
  <verbatim|pacman> tool is installed, launch <verbatim|mingw64> from the
  start menu and do the following

  <\shell-code>
    pacman -Syu
  </shell-code>

  Then restart the terminal and do

  <\shell-code>
    pacman -S subversion
  </shell-code>

  Now download the builder support from the official <TeXmacs> website and go
  the corresponding directory:

  <\shell-code>
    svn co svn://svn.savannah.gnu.org/texmacs/trunk/misc/builder

    cd builder
  </shell-code>

  You may now build all dependencies of <TeXmacs> and then the latest SVN
  version of <TeXmacs> itself:

  <\shell-code>
    ./scripts/build
  </shell-code>

  <tmdoc-copyright|2025|Joris van der Hoeven|Liza Belos>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>