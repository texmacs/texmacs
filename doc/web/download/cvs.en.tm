<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Download|CVS><tmweb-title|The CVS archive for
  <TeXmacs>|<apply|tmweb-download-links>>

  <TeXmacs> is currently being developed using
  <hlink|CVS|http://www.cvshome.org/> (Concurrent Version Control). This
  allows you to

  <\itemize>
    <item><hlink|Browse|http://www.texmacs.org/cgi-bin/cvsweb.cgi/> the
    <TeXmacs> sources on the web and follow the recent changes.

    <item><hlink|Download|#checkout> the most recent, but unofficial
    development version of <TeXmacs>.

    <item>Easily submit <apply|hyper-link|patches|../contact/patches.en.tm>
    to <TeXmacs>.

    <item>Directly <hlink|make changes|#commit> in certain parts of
    <TeXmacs>.
  </itemize>

  <vspace*|1fn><paragraph|Downloading the CVS version of
  <TeXmacs>><label|checkout>

  In order to download the current CVS version of <TeXmacs>, you should set
  your <verbatim|CVSROOT> environment variable to

  <verbatim| \ \ \ :pserver:the_username@cvs.texmacs.org:/texmacs>

  For read-only access, you should use <verbatim|anonymous> for
  <verbatim|the_username>. Now log in using

  <verbatim| \ \ \ cvs login>

  and checkout the source code using

  <verbatim| \ \ \ cvs co src>

  You may also checkout the external plug-ins using

  <verbatim| \ \ \ cvs co plugins>

  <vspace*|1fn><paragraph|Committing changes by CVS><label|commit>

  In order to commit changes by CVS, you need an account on
  <verbatim|cvs.texmacs.org>. In order to obtain such an account, please
  <apply|hyper-link|contact us|../contact/contact.en.tm> and tell us what
  kind of changes you plan to make. In fact, <TeXmacs> is currently under
  heavy reorganization, so we prefer contributions in the form of
  <apply|hyper-link|patches|../contact/patches.en.tm>. We do easily give
  write-access to the <TeXmacs> plug-ins though.

  For users with a CVS account, we recommend to use crypted access using
  <hlink|<name|ccvssh>|http://ccvssh.sourceforge.net/>. In that case, you
  should use set

  <\verbatim>
    \ \ \ \ CVSROOT=:ext:the_username@cvs.texmacs.org:/texmacs

    \ \ \ \ CVS_RSH=ccvssh
  </verbatim>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|checkout|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|commit|<tuple|<uninit>|?>>
  </collection>
</references>