<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Multiple extractions>

  <TeXmacs> allows users to create multiple bibliographies, tables of
  contents, indexes, <abbr|etc.> inside the same document. Let us explain how
  to do this for bibliographies; the procedure is similar for other types of
  automatically generated content.

  First of all, every separate bibliography is identified by a ``name''. The
  default name of the bibliography is <verbatim|bib>. Using
  <menu|Insert|Link|Alternate|Bibliography>, it is possible to specify a
  different bibliography (than the default one) for a certain region of text.

  For instance, to specify that a given citation should appear in a second
  bibliography with name <verbatim|bib2>, you should proceed as follows:

  <\itemize>
    <item>Click on <menu|Insert|Link|Alternate|Bibliography> and enter
    <verbatim|bib2> on the prompt. This will insert an empty
    <markup|with-bib> tag into your document, with the cursor inside.

    <item>Inside this <markup|with-bib> tag, enter your citation, using
    <menu|Insert|Link|Citation>.
  </itemize>

  If needed, the <markup|with-bib> tag can be made to span over a large
  portion of text. All citations inside this span will be be put into the
  bibliography with name <verbatim|bib2>.

  The bibliography <verbatim|bib2> itself should be created in a similar way:
  first click on <menu|Insert|Link|Alternate|Bibliography> and enter
  <verbatim|bib2> on the prompt. Next insert the bibliography as usual,
  <em|via> <menu|Insert|Automatic|Bibliography>. Now do
  <menu|Document|Update|All> as many times as need in order to generate the
  bibliography and get all links right.

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>