<TeXmacs|1.0.3.11>

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

  If one of these commands yields an error message, then <hyper-link|click
  here|requirements.en.tm>.

  <section|Download and unpack the source code>

  Download the <hlink|latest version (2.5
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|tgz>|-src.tar.gz>>>
  of the source code, or the <hlink|latest stable version (2.4
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|stgz>|-src.tar.gz>>>,
  cd into the directory where you wish to compile <TeXmacs> and type

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

  <section|Be cool...>

  Please <hyper-link|let us know|../contact/contact.en.tm> if you like or
  dislike the program. It is very useful for us to have a rough idea about
  the number of permanent users and for what applications you are interested
  in <TeXmacs>. Furthermore, we are glad to receive your suggestions and
  problems, no matter how silly they may seem to you. You may also
  <hyper-link|suscribe|../home/ml.en.tm> to the <verbatim|texmacs-users> or
  <verbatim|texmacs-info> mailing lists. If you really like the program, then
  please consider <hyper-link|donating|../contribute/donations.en.tm> money
  or services to us. Of course, you may also
  <hyper-link|contribute|../contribute/contribute.en.tm> yourself.

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
      gcc-2.95.3 on a GNU/<name|Linux> system on a PC; the versions 2.95.2
      and 1.1.* should also work, but the optimizer may be bugged in other
      versions.

      <item>You are compiling <TeXmacs> on a not yet supported system. Please
      take a look at <with|font-family|tt|configure.in> in order to see
      whether your system is supported. If not, please check that the macros
      <with|font-family|tt|WORD_LENGTH>, <with|font-family|tt|WORD_LENGTH_INC>
      and <with|font-family|tt|WORD_MASK> are OK for your system.
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

    <item><with|font-shape|italic|<TeXmacs> runs fine, but certain fonts are
    not displayed correctly.>

    <with|font-series|bold|Solution:> This problem may have several causes:

    <\itemize>
      <item>You use a recent version (\<gtr\>= 7.3) of <name|RedHat Linux> or
      one of its clones. On such systems, the installation of Te<TeX> is
      bugged and you need to change some permissions in order to allow font
      generation by Metafont. As root, type

      <\code>
        chmod -R a+rw /var/lib/texmf/*
      </code>

      <item>You do not use a standard <TeX>/<LaTeX> distribution, such as
      Te<TeX> (standard distributions come with scripts for generating fonts,
      such as <with|font-family|tt|mktextfm> or
      <with|font-family|tt|MakeTeXTFM>). You may either install a more
      standard <TeX>/<LaTeX> distribution, or download some
      <hyper-link|pregenerated fonts|fonts.en.tm>.
    </itemize>

    <item>If you encounter other problems, or if our fixes don't work, then
    please <hyper-link|contact us|../contact/contact.en.tm>.
  </itemize>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>