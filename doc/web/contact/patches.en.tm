<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Contact|Patches><tmweb-title|Submitting patches for
  <TeXmacs>|<apply|tmweb-contact-links>>

  You may contribute patches for <TeXmacs> using the <hlink|patch
  manager|http://savannah.gnu.org/patch/?group=texmacs> on
  <hlink|<name|Savannah>|http://savannah.gnu.org/projects/texmacs/>. In order
  to <hlink|submit a patch|http://savannah.gnu.org/patch/?func=addpatch&group=texmacs>,
  we recommend you to use the <apply|hyper-link|CVS
  version|../download/cvs.en.tm> of <TeXmacs>. In that case, after checking
  out the source code of a particular version of <TeXmacs> using

  <verbatim| \ \ \ cvs co -r TeXmacs-<em|version> src>

  and making your changes, you may use the command

  <verbatim| \ \ \ cvs diff -r TeXmacs-<em|version> src \<gtr\> diffs>

  in order to obtain a patch ready to be submitted in the file
  <verbatim|diffs>. Here <verbatim|<em|version>> stands for the usual version
  number, with ``<verbatim|.>'' replaced by ``<verbatim|_>'' (example:
  <verbatim|TeXmacs-1_0_2_7>). When <hlink|submitting your
  patch|http://savannah.gnu.org/patch/?func=addpatch&group=texmacs>, don't
  forget to carefully select a category and to fill out a summary.

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
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|2|?>>
    <associate|toc-4|<tuple|3|?>>
  </collection>
</references>