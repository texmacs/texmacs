<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard titles>

  The <tmdtd|header-title> <abbr|d.t.d.> provides tags for title information.
  The following high-level tags can only be used when encapsulated inside a
  <markup|make-title> tag:

  <\explain|<explain-macro|title|title>>
    Specify the <src-arg|title> of the document.
  </explain>

  <\explain|<explain-macro|author|author>>
    Specify one or several authors for the document.
  </explain>

  <\explain|<explain-macro|author|address>>
    Specify the address of the author.
  </explain>

  <\explain|<explain-macro|author-block|address>>
    Specify an address of one of the authors (in case of multiple addresses).
  </explain>

  <\explain|<explain-macro|title-email|email>>
    Specify the email address of the author.
  </explain>

  <\explain|<explain-macro|title-date|email>>
    Specify the creation date of the article (which is often today,
    <abbr|i.e.> <inactive*|<date>>).
  </explain>

  The <markup|title> and <markup|author> use the <markup|header-title> and
  <markup|header-author> tags for specifying the running title and header.
  You may override these by reusing <markup|header-title> <abbr|resp.>
  <markup|header-author>.

  The <tmdtd|header-title> <abbr|d.t.d.> also defines the <markup|abstract>
  tag for abstracts of documents. Inside abstracts, you may use
  <markup|keywords> in order to specify keywords for your paper and
  <markup|AMS-class> for entering the <abbr|A.M.S.> subject classification.

  The title tags rely on the following low-level tags for their physical
  layout:

  <\explain|<explain-macro|title*|title>>
    This macro is used for rendering the <src-arg|title> of the document.
  </explain>

  <\explain|<explain-macro|author*|author>>
    This macro is used for rendering the <src-arg|author>(s) of the document.
  </explain>

  <\explain|<explain-macro|address*|address>>
    This macro is used for rendering the <src-arg|address> of an author.
  </explain>

  <\explain|<explain-macro|title-email*|email>>
    This macro is used for rendering the <src-arg|email> address of an
    author.
  </explain>

  <\explain|<explain-macro|title-date*|email>>
    This macro is used for rendering the creation <src-arg|date> of the
    document.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>