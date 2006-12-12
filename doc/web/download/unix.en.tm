<TeXmacs|1.0.6.1>

<style|tmweb>

<\body>
  <tmweb-current|Download|Binaries><tmweb-title|Installing <TeXmacs> on Linux
  and Unix systems|<tmweb-download-links>>

  <section|Download the package>

  Download the <hlink|most recent (5.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|tgz>|-static-gnu-linux.tar.gz>>>
  or <hlink|last stable (5.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<TeXmacs-version-release|stgz>|-static-gnu-linux.tar.gz>>>
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

  <section|Run !>

  <\code>
    \ \ \ \ texmacs &
  </code>

  <section|Extra fonts?>

  Optionally, you may download a <hlink|tarball (5.8
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

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>