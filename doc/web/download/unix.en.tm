<TeXmacs|1.0.5.1>

<style|tmweb>

<\body>
  <tmweb-current|Download|Binaries><tmweb-title|Installing <TeXmacs> on Linux
  and Unix systems|<tmweb-download-links>>

  <section|Download the package>

  Download the <hlink|most recent (4.1 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|tgz>|-static-gnu-linux.tar.gz>>>
  or <hlink|last stable (4.1 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|stgz>|-static-gnu-linux.tar.gz>>>
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

  All files will be unpacked into the directory
  <with|font-family|tt|TeXmacs-[version]>. Let
  <with|font-family|tt|[installation directory]> be the full path of this
  directory. You may determine it by typing:

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

  where <with|font-family|tt|[installation directory]> is as in step 2. We
  recommend to put these lines in your personal startup script, such as
  <with|font-family|tt|.bash_profile>.

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

  Please <hyper-link|let us know|../contact/contact.en.tm> if you like or
  dislike the program. It is very useful for us to have a rough idea about
  the number of permanent users and for what applications you are interested
  in <TeXmacs>. Furthermore, we are glad to receive your suggestions and
  problems, no matter how silly they may seem to you. You may also
  <hyper-link|subscribe|../home/ml.en.tm> to the <verbatim|texmacs-users> or
  <verbatim|texmacs-info> mailing lists. If you really like the program, then
  please consider <hyper-link|donating|../contribute/donations.en.tm> money
  or services to us. Of course, you may also
  <hyper-link|contribute|../contribute/contribute.en.tm> yourself.

  <appendix|Possible problems>

  <\itemize>
    <item><with|font-shape|italic|<TeXmacs> runs fine, but certain fonts are
    not displayed correctly.>

    <with|font-series|bold|Solution:> This problem may have several causes:

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
      such as <with|font-family|tt|mktextfm> or
      <with|font-family|tt|MakeTeXTFM>). You may either install a more
      standard <TeX>/La<TeX> distribution, or download some pregenerated
      fonts as indicated in step 4.
    </itemize>
  </itemize>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>