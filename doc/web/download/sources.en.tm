<TeXmacs|1.0.6.12>

<style|tmweb>

<\body>
  <tmweb-current|Download|Sources><tmweb-title|Compile <TeXmacs> from the
  source code|<tmweb-download-links>>

  <section|Verify the <TeXmacs> dependencies>

  Before you install <TeXmacs> on your system, you have to make sure that the
  other programs on which <TeXmacs> depends, namely <TeX> and
  <name|Guile>/<name|Scheme> (from the <name|Gnome> project) have been
  installed correctly. You can do this by checking whether the
  <with|font-family|tt|latex> and <with|font-family|tt|guile-config> binaries
  exists in your path

  <\code>
    \ \ \ \ which latex

    \ \ \ \ which guile-config
  </code>

  If one of these commands yields an error message, then <hlink|click
  here|requirements.en.tm>. On some systems, you also need to make sure that
  you installed the development packages (<abbr|i.e.> X11-dev or X11-devel)
  for <name|Xlib>.

  <section|Download and unpack the source code>

  Download the <hlink|latest version (3.8
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|tgz>|-src.tar.gz>>>
  of the source code, or the <hlink|latest stable version (3.8
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|stgz>|-src.tar.gz>>>,
  <verbatim|cd> into the directory where you wish to compile <TeXmacs> and
  type

  <\code>
    \ \ \ \ tar -zxvf <merge|<TeXmacs-version-release|tgz>|-src.tar.gz>
  </code>

  All files will be unpacked into the directory
  <with|font-family|tt|<merge|<TeXmacs-version-release|tgz>|-src>>, which is
  the 'installation directory'. If you do not have ftp access, then you might
  wish to <hlink|download|http://www.texmacs.org/Download/ftp/targz> from the
  web.

  <section|Compile, install and run>

  <TeXmacs> supports the standard GNU compilation and installation procedure.
  Assuming that you logged yourself in as root, cd into the installation
  directory and type

  <\code>
    \ \ \ \ ./configure

    \ \ \ \ make

    \ \ \ \ make install
  </code>

  The first command examines your particular system configuration. The second
  command launches the compilation. The last command installs <TeXmacs> in
  <with|font-family|tt|/usr/local>.

  If everything works fine, you should be able to run <TeXmacs> by

  <\code>
    \ \ \ \ texmacs &
  </code>

  If this does not work, you should make sure that
  <with|font-family|tt|/usr/local/bin> is in your <with|font-family|tt|PATH>.
  Depending on your shell, you can ensure this by typing

  <\code>
    \ \ \ \ export PATH=/usr/local/bin:$PATH \ \ or

    \ \ \ \ setenv PATH /usr/local/bin:$PATH
  </code>

  If you are interested by packaging <TeXmacs> for some <name|Linux> or
  <name|Unix> distribution, then please take a look at our
  <hlink|suggestions|packaging.en.tm>.

  <section|Extra fonts?>

  You may optionally download a <hlink|tarball (5.8
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-extra-fonts-1.0-noarch.tar.gz>
  with extra Type<nbsp>1 fonts. These fonts should be unpacked in the
  directory <verbatim|~/.TeXmacs> (which you should create if it does not
  already exist) using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-extra-fonts-1.0-noarch.tar.gz \| tar xvf -
  </code>

  Although <TeXmacs> is able to automatically generate bitmap analogues for
  the extra fonts, this process may take some time at a first run. Type 1
  fonts are also rendered better by certain <name|Pdf> viewers and they are
  often preferred by publishers.

  Chinese, Japanese and Korean users also need to separately download extra
  <hlink|CJK fonts|fonts.en.tm#fonts-cjk>, whenever these languages do not
  correctly show up in the language menus.

  <section|Be cool...>

  Please <hlink|let us know|../contact/contact.en.tm> if you like or dislike
  the program. It is very useful for us to have a rough idea about the number
  of permanent users and for what applications you are interested in
  <TeXmacs>. Furthermore, we are glad to receive your suggestions and
  problems, no matter how silly they may seem to you. You may also
  <hlink|subscribe|../home/ml.en.tm> to the <verbatim|texmacs-users> or
  <verbatim|texmacs-info> mailing lists. If you really like the program, then
  please consider <hlink|donating|../contribute/donations.en.tm> money or
  services to us. Of course, you may also
  <hlink|contribute|../contribute/contribute.en.tm> yourself.

  <appendix|Configuration options>

  If you cannot log yourself in as root, or if you want to install <TeXmacs>
  elsewhere than in <with|font-family|tt|/usr/local>, then you should use

  <\code>
    \ \ \ \ ./configure --prefix=[target directory]
  </code>

  instead of <with|font-family|tt|./configure>. In this case, <TeXmacs> will
  be installed in <with|font-family|tt|[target directory]> and you will have
  to set your <with|font-family|tt|PATH> accordingly, as to contain
  <with|font-family|tt|[target directory]/bin>. Other options supported by
  <with|font-family|tt|configure> are

  <\itemize>
    <item><with|font-family|tt|--bindir=[binary directory]>: sets the
    directory where the <TeXmacs> binaries should be installed
    (<with|font-family|tt|[target directory]/bin>, by default).

    <item><with|font-family|tt|--datadir=[data directory]>: sets the
    directory where other <TeXmacs> data should be installed.
    (<with|font-family|tt|[target directory]/share>, by default).
  </itemize>

  <appendix|Static compilation>

  By default, we build <TeXmacs> using dynamically linked libraries. If you
  rather want to build the static version, use

  <\code>
    \ \ \ \ make STATIC_TEXMACS
  </code>

  <appendix|Possible problems>

  <\itemize>
    <item><with|font-shape|italic|Everything compiles fine, but you
    frequently get a segmentation fault when running the editor.>

    <with|font-series|bold|Solution:> This problem may have several causes:

    <\itemize>
      <item>You don't use a standard version of the GNU C++ compiler. We use
      gcc-2.95.3 on a GNU/<name|Linux> system on a PC. More recent versions
      (3.3.*, 3.4.*, 4.*, etc.) of <verbatim|g++> should work as well. Some
      versions don't support optimization, in which case you should configure
      using the <verbatim|--disable-optimize> option.

      <item>You are compiling <TeXmacs> on a not yet supported system. Please
      take a look at <with|font-family|tt|configure.in> in order to see
      whether your system is supported. If not, try to add a new entry for
      your system.
    </itemize>

    <item><with|font-shape|italic|You use <name|Cygwin> and you do not manage
    to get <name|Guile> working.>

    <with|font-series|bold|Solution:> There may be a problem with the file
    name <with|font-family|tt|ice-9/and-let*.scm> (stars are not allowed in
    file names), which should be renamed as
    <with|font-family|tt|ice-9/and-let-star.scm>. You should make the
    corresponding modifications in <with|font-family|tt|ice-9/Makefile.in>.
    Since you might be unable to extract <with|font-family|tt|ice-9/and-let*.scm>
    from the archive, you can find a copy of it (for guile-1.4)
    <hlink|here|http://www.texmacs.org/Data/and-let-star.scm>.

    <item><with|font-shape|italic|You have problems with compiling or linking
    with X Window.>

    <with|font-series|bold|Solution:> modify some of the compilation options
    in configure.in.

    <item>If you encounter other problems, or if our fixes don't work, then
    please <hlink|contact us|../contact/contact.en.tm>.
  </itemize>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>