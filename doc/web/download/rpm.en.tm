<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Download|RPM><tmweb-title|Installing <TeXmacs> as an
  RPM package|<apply|tmweb-download-links>>

  <section|The idea behind rpm packages>

  If you use <name|RedHat Linux> or another linux distribution (like
  <name|Mandrake>) which supports the <with|font family|tt|rpm> command to
  install software, it is very easy to install GNU <TeXmacs>. You can check
  whether <with|font family|tt|rpm> exists on your system using

  <\code>
    \ \ \ \ which rpm
  </code>

  If the program is not installed, or if you do not have root privilege, then
  you should ask your system administrator to install GNU <TeXmacs>, or use
  the <apply|hyper-link|classical installation method|unix.en.tm>.

  <section|Download, install and run>

  After <hlink|downloading (3.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<extern|texmacs-version-release|rpm>|.i386.rpm>>>
  the GNU <TeXmacs> distribution for PC's under GNU/<name|Linux>, you may
  install the software by typing (as root)

  <\code>
    \ \ \ \ rpm -i <merge|<extern|texmacs-version-release|rpm>|.i386.rpm>
  </code>

  The program can now be launched using

  <\code>
    \ \ \ \ texmacs &
  </code>

  In a similar way, you may <hlink|download (2.2
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<extern|texmacs-version-release|rpm>|.src.rpm>>>
  the source code as an rpm package. You might also prefer to download the
  <hlink|last stable version (3.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<extern|texmacs-version-release|srpm>|.i386.rpm>>>
  of <TeXmacs>, or the corresponding <hlink|source code (2.2
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<extern|texmacs-version-release|srpm>|.src.rpm>>>.
  We also provide <hlink|rpm's for other operating
  systems|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/>. If you do not have ftp
  access, then you might wish to <hlink|download|http://www.texmacs.org/Download/ftp/rpm>
  from the web.

  <section|Extra fonts?>

  Before you run <TeXmacs>, you may optionally <hlink|download (2.9
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/TeXmacs-fonts-1.0-1.noarch.rpm>
  pregenerated <TeX> fonts and install them using

  <\code>
    \ \ \ \ rpm -i TeXmacs-fonts-1.0-1.noarch.rpm
  </code>

  Although these fonts may in principle be generated automatically by
  <TeXmacs>, this process may slow down the editor when you use it for the
  first time.

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
      generation by <name|Metafont>. As root, type

      <\code>
        \ \ \ \ chmod -R a+rw /var/lib/texmf/*
      </code>

      <item>You do not use a standard <TeX>/<LaTeX> distribution, such as
      Te<TeX> (standard distributions come with scripts for generating fonts,
      such as <with|font family|tt|mktextfm> or <with|font
      family|tt|MakeTeXTFM>). You may either install a more standard
      <TeX>/<LaTeX> distribution, or download some pregenerated fonts as
      indicated in step 3.
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
    <associate|toc-5|<tuple|A|?>>
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
      series|<quote|bold>|1<space|2spc>The idea behind rpm
      packages><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Download, install and
      run><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Extra
      fonts?><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Be
      cool...><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|Appendix A.<space|2spc>Possible
      problems><value|toc-dots><pageref|toc-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>