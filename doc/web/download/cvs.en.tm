<TeXmacs|1.0.2.11>

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
  your <verbatim|CVSROOT> environment variable to

  <verbatim| \ \ \ :pserver:the_username@cvs.texmacs.org:/texmacs>

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
  contributions in the form of <hyper-link|patches|../contact/patches.en.tm>.
  On the other hand, we recommend the use of CVS for the contribution of
  documentation and plug-ins.

  For users with a CVS account, it is highly recommended to use crypted
  access using <hlink|<name|ccvssh>|http://ccvssh.sourceforge.net/>. After
  having installed <name|ccvssh>, this requires you to set

  <\verbatim>
    \ \ \ \ CVSROOT=:ext:the_username@cvs.texmacs.org:/texmacs

    \ \ \ \ CVS_RSH=ccvssh
  </verbatim>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
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

<\auxiliary>
  <\collection>
    <\associate|toc>
      <with|par-left|<quote|6fn>|font-size|<quote|0.84>|Downloading the CVS
      version of T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>><value|toc-dots><pageref|toc-1>>

      <with|par-left|<quote|6fn>|font-size|<quote|0.84>|Committing changes by
      CVS<value|toc-dots><pageref|toc-2>>
    </associate>
  </collection>
</auxiliary>