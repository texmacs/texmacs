<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Download|Binaries><tmweb-title|Installing <TeXmacs> on
  Linux and Unix systems|<apply|tmweb-download-links>>

  <section|Download the package>

  Download the <hlink|most recent (3.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<extern|texmacs-version-release|tgz>|-static-gnu-linux.tar.gz>>>
  or <hlink|last stable (3.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<extern|texmacs-version-release|stgz>|-static-gnu-linux.tar.gz>>>
  static binary distribution of GNU <TeXmacs> for PC's under
  GNU/<name|Linux>. If you have another operating system, then you should
  <hlink|download|ftp://ftp.texmacs.org/pub/TeXmacs/targz/> the most recent
  version of <TeXmacs> for your system. If you do not have ftp access, then
  you might wish to <hlink|download|http://www.texmacs.org/Download/ftp/targz>
  from the web.

  <section|Unpack the package>

  cd into the directory where you wish to install <TeXmacs> and type

  <\code>
    \ \ \ \ gunzip -c TeXmacs-[version]-[your system].tar.gz \| tar xvf -
  </code>

  All files will be unpacked into the directory <with|font
  family|tt|TeXmacs-[version]>. Let <with|font family|tt|[installation
  directory]> be the full path of this directory. You may determine it by
  typing:

  <\code>
    \ \ \ \ cd TeXmacs-[version]

    \ \ \ \ pwd
  </code>

  <section|Set the environment variables>

  Depending on your shell, either type

  <\code>
    \ \ \ \ export TEXMACS_PATH=[installation directory]

    \ \ \ \ export PATH=$TEXMACS_PATH/bin:$PATH
  </code>

  or

  <\code>
    \ \ \ \ setenv TEXMACS_PATH [installation directory]

    \ \ \ \ setenv PATH $TEXMACS_PATH/bin:$PATH
  </code>

  where <with|font family|tt|[installation directory]> is as in step 2. We
  recommend to put these lines in your personal startup script, such as
  <with|font family|tt|.bash_profile>.

  <section|Extra fonts?>

  Before you run <TeXmacs>, you may optionally <hlink|download (2.9
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/targz/TeXmacs-600dpi-fonts.tar.gz>
  pregenerated <TeX> fonts and unpack them in the installation directory
  using

  <\code>
    \ \ \ \ gunzip -c TeXmacs-600dpi-fonts.tar.gz \| tar xvf -
  </code>

  Although these fonts may in principle be generated automatically by
  <TeXmacs>, this process may slow down the editor when you use it for the
  first time.

  <section|Run !>

  <\code>
    \ \ \ \ texmacs &
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

  <appendix|Possible problems>

  <\itemize>
    <item><with|font shape|italic|<TeXmacs> runs fine, but certain fonts are
    not displayed correctly.>

    <with|font series|bold|Solution:> This problem may have several causes:

    <\itemize>
      <item>You use a recent version (\<gtr\>= 7.3) of <name|RedHat Linux> or
      one of its clones. On such systems, the installation of Te<TeX> is
      bugged and you need to change some permissions in order to allow font
      generation by Metafont. As root, type

      <\code>
        \ \ \ \ chmod -R a+rw /var/lib/texmf/*
      </code>

      <item>You do not use a standard <TeX>/<LaTeX> distribution, such as
      Te<TeX> (standard distributions come with scripts for generating fonts,
      such as <with|font family|tt|mktextfm> or <with|font
      family|tt|MakeTeXTFM>). You may either install a more standard
      <TeX>/La<TeX> distribution, or download some pregenerated fonts as
      indicated in step 4.
    </itemize>
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
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|A|?>>
    <associate|toc-8|<tuple|A.1|?>>
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
      series|<quote|bold>|1<space|2spc>Download the
      package><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Unpack the
      package><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Set the environment
      variables><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Extra
      fonts?><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Run
      !><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Be
      cool...><value|toc-dots><pageref|toc-6><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|Appendix A.<space|2spc>Possible
      problems><value|toc-dots><pageref|toc-7><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>