<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Download|Sources><tmweb-title|Compile <TeXmacs> from
  the source code|<apply|tmweb-download-links>>

  <section|Verify the <TeXmacs> dependencies>

  Before you install <TeXmacs> on your system, you have to make sure that the
  other programs on which <TeXmacs> depends, namely <TeX> and
  <name|Guile>/<name|Scheme> (from the <name|Gnome> project) have been
  installed correctly. You can do this by checking whether the <with|font
  family|tt|latex> and <with|font family|tt|guile-config> binaries exists in
  your path

  <\code>
    \ \ \ \ which latex

    \ \ \ \ which guile-config
  </code>

  If one of these commands yields an error message, then
  <apply|hyper-link|click here|requirements.en.tm>.

  <section|Download and unpack the source code>

  Download the <hlink|latest version (2.5
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<extern|texmacs-version-release|tgz>|-src.tar.gz>>>
  of the source code, or the <hlink|latest stable version (2.4
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<extern|texmacs-version-release|stgz>|-src.tar.gz>>>,
  cd into the directory where you wish to compile <TeXmacs> and type

  <\code>
    \ \ \ \ tar -zxvf <merge|<extern|texmacs-version-release|tgz>|-src.tar.gz>
  </code>

  All files will be unpacked into the directory <with|font
  family|tt|<merge|<extern|texmacs-version-release|tgz>|-src>>, which is the
  'installation directory'. If you do not have ftp access, then you might
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
  <with|font family|tt|/usr/local>.

  If everything works fine, you should be able to run <TeXmacs> by

  <\code>
    \ \ \ \ texmacs &
  </code>

  If this does not work, you should make sure that <with|font
  family|tt|/usr/local/bin> is in your <with|font family|tt|PATH>. Depending
  on your shell, you can ensure this by typing

  <\code>
    \ \ \ \ export PATH=/usr/local/bin:$PATH \ \ or

    \ \ \ \ setenv PATH /usr/local/bin:$PATH
  </code>

  <section|Be cool...>

  Please <apply|hyper-link|let us know|../contact/contact.en.tm> if you like
  or dislike the program. It is very useful for us to have a rough idea about
  the number of permanent users and for what applications you are interested
  in <TeXmacs>. Furthermore, we are glad to receive your suggestions and
  problems, no matter how silly they may seem to you. You may also
  <apply|hyper-link|suscribe|../home/ml.en.tm> to the
  <verbatim|texmacs-users> or <verbatim|texmacs-info> mailing lists. If you
  really like the program, then please consider
  <apply|hyper-link|donating|../contribute/donations.en.tm> money or services
  to us. Of course, you may also <apply|hyper-link|contribute|../contribute/contribute.en.tm>
  yourself.

  <appendix|Configuration options>

  If you cannot log yourself in as root, or if you want to install <TeXmacs>
  elsewhere than in <with|font family|tt|/usr/local>, then you should use

  <\code>
    \ \ \ \ ./configure --prefix=[target directory]
  </code>

  instead of <with|font family|tt|./configure>. In this case, <TeXmacs> will
  be installed in <with|font family|tt|[target directory]> and you will have
  to set your <with|font family|tt|PATH> accordingly, as to contain
  <with|font family|tt|[target directory]/bin>. Other options supported by
  <with|font family|tt|configure> are

  <\itemize>
    <item><with|font family|tt|--bindir=[binary directory]>: sets the
    directory where the <TeXmacs> binaries should be installed (<with|font
    family|tt|[target directory]/bin>, by default).

    <item><with|font family|tt|--datadir=[data directory]>: sets the
    directory where other <TeXmacs> data should be installed. (<with|font
    family|tt|[target directory]/share>, by default).
  </itemize>

  <appendix|Static compilation>

  By default, we build <TeXmacs> using dynamically linked libraries. If you
  rather want to build the static version, use

  <\code>
    \ \ \ \ make STATIC_TEXMACS
  </code>

  <appendix|Possible problems>

  <\itemize>
    <item><with|font shape|italic|Everything compiles fine, but you
    frequently get a segmentation fault when running the editor.>

    <with|font series|bold|Solution:> This problem may have several causes:

    <\itemize>
      <item>You don't use a standard version of the GNU C++ compiler. We use
      gcc-2.95.3 on a GNU/<name|Linux> system on a PC; the versions 2.95.2
      and 1.1.* should also work, but the optimizer may be bugged in other
      versions.

      <item>You are compiling <TeXmacs> on a not yet supported system. Please
      take a look at <with|font family|tt|configure.in> in order to see
      whether your system is supported. If not, please check that the macros
      <with|font family|tt|WORD_LENGTH>, <with|font
      family|tt|WORD_LENGTH_INC> and <with|font family|tt|WORD_MASK> are OK
      for your system.
    </itemize>

    <item><with|font shape|italic|You use <name|Cygwin> and you do not manage
    to get <name|Guile> working.>

    <with|font series|bold|Solution:> There may be a problem with the file
    name <with|font family|tt|ice-9/and-let*.scm> (stars are not allowed in
    file names), which should be renamed as <with|font
    family|tt|ice-9/and-let-star.scm>. You should make the corresponding
    modifications in <with|font family|tt|ice-9/Makefile.in>. Since you might
    be unable to extract <with|font family|tt|ice-9/and-let*.scm> from the
    archive, you can find a copy of it (for guile-1.4)
    <hlink|here|http://www.texmacs.org/Data/and-let-star.scm>.

    <item><with|font shape|italic|You have problems with compiling or linking
    with X Window.>

    <with|font series|bold|Solution:> modify some of the compilation options
    in configure.in.

    <item><with|font shape|italic|<TeXmacs> runs fine, but certain fonts are
    not displayed correctly.>

    <with|font series|bold|Solution:> This problem may have several causes:

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
      such as <with|font family|tt|mktextfm> or <with|font
      family|tt|MakeTeXTFM>). You may either install a more standard
      <TeX>/<LaTeX> distribution, or download some
      <apply|hyper-link|pregenerated fonts|fonts.en.tm>.
    </itemize>

    <item>If you encounter other problems, or if our fixes don't work, then
    please <apply|hyper-link|contact us|../contact/contact.en.tm>.
  </itemize>

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

<\references>
  <\collection>
    <associate|toc-5|<tuple|A|?>>
    <associate|toc-6|<tuple|B|?>>
    <associate|toc-7|<tuple|C|?>>
    <associate|toc-8|<tuple|C.1|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Verify the
      T<rsub|<space|-0.4spc><move|<resize|<with|index
      level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>
      dependencies><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Download and unpack the source
      code><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Compile, install and
      run><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Be
      cool...><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|Appendix A.<space|2spc>Configuration
      options><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|Appendix B.<space|2spc>Static
      compilation><value|toc-dots><pageref|toc-6><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|Appendix C.<space|2spc>Possible
      problems><value|toc-dots><pageref|toc-7><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>