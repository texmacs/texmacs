<TeXmacs|1.0.4.2>

<style|tmweb>

<\body>
  <tmweb-current|Download|CVS><tmweb-title|The CVS archive for
  <TeXmacs>|<tmweb-download-links>>

  <TeXmacs> is currently being developed using
  <hlink|CVS|http://www.cvshome.org/> (Concurrent Version Control). This
  allows you to

  <\itemize>
    <item><hlink|Browse|http://www.texmacs.org/cgi-bin/cvsweb.cgi/> the
    <TeXmacs> sources on the web and follow the recent changes.

    <item><hlink|Download|#checkout> the most recent, but unofficial
    development version of <TeXmacs>.

    <item>Easily submit <hyper-link|patches|../contact/patches.en.tm> to
    <TeXmacs>.

    <item>Directly <hlink|make changes|#commit> in certain parts of
    <TeXmacs>.
  </itemize>

  <vspace*|1fn><paragraph|Downloading the CVS version of
  <TeXmacs>><label|checkout>

  In order to download the current CVS version of <TeXmacs>, you should set

  <\verbatim>
    \ \ \ \ export CVSROOT=:pserver:the_username@cvs.texmacs.org:/texmacs

    \ \ \ \ export CVS_RSH=ssh
  </verbatim>

  For read-only access, you should use <verbatim|anonymous> for
  <verbatim|the_username>. Now log in using

  <verbatim| \ \ \ cvs login>

  and checkout the source code using

  <verbatim| \ \ \ cvs co src>

  You may also checkout the documentation and the external plug-ins using

  <\verbatim>
    \ \ \ \ cvs co doc

    \ \ \ \ cvs co plugins
  </verbatim>

  <vspace*|1fn><paragraph|Committing changes by CVS><label|commit>

  In order to commit changes by CVS, you first need an account on
  <verbatim|cvs.texmacs.org>. This is done as follows:

  <\enumerate>
    <item>Choose a <verbatim|<em|username>> and a <verbatim|<em|password>>.

    <item>Encrypt your password by typing the following line in a shell:

    <verbatim| \ \ \ perl -e 'print crypt ("<em|password>","SAlt"), "\\n"'>

    <item>If <verbatim|<em|key>> is the encrypted password returned at step
    2, then <hyper-link|send us an email|../contact/contact.en.tm> with the
    line

    <verbatim| \ \ \ <em|username>:<em|key>>
  </enumerate>

  Alternatively, you may create the line <verbatim|<em|username>:<em|key>>
  using the web-interface at

  <verbatim| \ \ \ <hlink|http://www.cri74.org/services/crypt.php3|http://www.cri74.org/services/crypt.php3>>

  Please also tell us what kind of changes you plan to make, so that we can
  give you write-permissions to the appropriate parts of the CVS-tree. Notice
  that for contributions to the <TeXmacs> source code we usually prefer
  <hyper-link|patches|../contact/patches.en.tm>. On the other hand, we
  recommend the use of CVS for the contribution of documentation and
  plug-ins.

  For users with a CVS account, we highly prefer you to use crypted access
  using <hlink|<name|ccvssh>|http://ccvssh.sourceforge.net/>. Besides
  installing <name|ccvssh>, if necessary, this requires you to set

  <\verbatim>
    \ \ \ \ export CVSROOT=:ext:the_username@cvs.texmacs.org:/texmacs

    \ \ \ \ export CVS_RSH=ccvssh
  </verbatim>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>