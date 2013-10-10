<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard titles>

  The <tmdtd|header-title> <abbr|d.t.d.> provides tags for title information.
  The following high-level tags can only be used when encapsulated inside a
  <markup|make-title> tag:

  <\explain|<markup|title>>
    Specify a title for the document.
  </explain>

  <\explain|<markup|author>>
    Specify one or several authors for the document.
  </explain>

  <\explain|<markup|address>>
    Specify the address of the author.
  </explain>

  <\explain|<markup|address-block>>
    Specify an address of an author (in case of multiple addresses).
  </explain>

  <\explain|<markup|title-email>>
    Specify the email address of the author.
  </explain>

  <\explain|<markup|title-date>>
    Specify the creation date of the article.
  </explain>

  The <markup|title> and <markup|author> use the <markup|header-title> and
  <markup|header-author> tags for specifying the running title and header.
  You may override these by reusing <markup|header-title> <abbr|resp.>
  <markup|header-author>. The above tags also depend on the following
  low-level tags for their physical layout:

  <\explain|<markup|title*>>
    Macro with one argument which specifies the physical layout of titles.
  </explain>

  <\explain|<markup|author*>>
    Macro with one argument which specifies the physical layout of authors.
  </explain>

  <\explain|<markup|address*>>
    Macro with one argument which specifies the physical layout of addresses.
  </explain>

  <\explain|<markup|title-email*>>
    Macro with one argument which specifies the physical layout of email
    addresses.
  </explain>

  <\explain|<markup|title-date*>>
    Macro with one argument which specifies the physical layout of creation
    dates.
  </explain>

  The <tmdtd|header-title> <abbr|d.t.d.> also defines the <markup|abstract>
  tag for abstracts of documents.

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
    <associate|language|english>
  </collection>
</initial>