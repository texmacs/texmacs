<TeXmacs|1.0.6.1>

<style|tmweb>

<\body>
  <tmweb-current|Download|RPM><tmweb-title|Installing <TeXmacs> as an RPM
  package|<tmweb-download-links>>

  <section|The idea behind rpm packages>

  If you use <name|RedHat Linux> or another linux distribution (like
  <name|Mandrake>) which supports the <with|font-family|tt|rpm> command to
  install software, it is very easy to install GNU <TeXmacs>. You can check
  whether <with|font-family|tt|rpm> exists on your system using

  <\code>
    \ \ \ \ which rpm
  </code>

  If the program is not installed, or if you do not have root privilege, then
  you should ask your system administrator to install GNU <TeXmacs>, or use
  the <hyper-link|classical installation method|unix.en.tm>.

  <section|Download, install and run>

  After <hlink|downloading (5.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<TeXmacs-version-release|rpm>|.i386.rpm>>>
  the GNU <TeXmacs> distribution for PC's under GNU/<name|Linux>, you may
  install the software by typing (as root)

  <\code>
    \ \ \ \ rpm -i <merge|<TeXmacs-version-release|rpm>|.i386.rpm>
  </code>

  The program can now be launched using

  <\code>
    \ \ \ \ texmacs &
  </code>

  In a similar way, you may <hlink|download (3.8
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<TeXmacs-version-release|rpm>|.src.rpm>>>
  the source code as an rpm package. You might also prefer to download the
  <hlink|last stable version (5.6 Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<merge|<TeXmacs-version-release|srpm>|.i386.rpm>>>
  of <TeXmacs>, or the corresponding <hlink|source code (3.8
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/|<TeXmacs-version-release|srpm>|.src.rpm>>.
  We also provide <hlink|rpm's for other operating
  systems|ftp://ftp.texmacs.org/pub/TeXmacs/rpm/>. If you do not have ftp
  access, then you might wish to <hlink|download|http://www.texmacs.org/Download/ftp/rpm>
  from the web.

  <section|Extra fonts?>

  Optionally, you may download an <hlink|RPM (5.0
  Mb)|ftp://ftp.texmacs.org/pub/TeXmacs/fonts/TeXmacs-extra-fonts-1.0-1.noarch.rpm>
  with extra Type 1 fonts and install it using

  <\code>
    \ \ \ \ rpm -i TeXmacs-extra-fonts-1.0-1.noarch.rpm
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