<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard <TeXmacs> styles>

  The main <TeXmacs> styles are:

  <\description>
    <item*|<tmstyle|generic>>This is the default style when you open a new
    document. The purpose of this style is to produce quick, informal
    documents. For this reason, section numbering is disabled and the layout
    of paragraphs is very simple: instead of indenting the first lines of
    paragraphs, they are rather separated by white-space.

    <item*|<tmstyle|article>>This style may be used for writing short
    scientific articles, which are subdivided into sections. The numbering of
    environments like theorems, remarks, <abbr|etc.> is relative to the
    entire document. If you use the <tmpackage|number-long-article> package,
    then the numbers are prefixed by the section number.

    <item*|<tmstyle|book>>This is the basic style for writing books. Books
    are assumed to be subdivided into chapters and numbers of environments
    are prefixed by the chapter number. In general, it is also comfortable to
    store each chapter in a separate file, so that they can be edited more
    efficiently. This issue is explained in more detail in the section about
    <hyper-link|books and multifile documents|../links/man-multifile.en.tm>.

    <item*|<tmstyle|seminar>>Documents based on this style are typically
    printed on slides for presentations using an overhead projector. You may
    also want to use it when making presentation directly from your laptop,
    after selecting <menu|View|Presentation mode>. Notice however, that
    slides correspond to real pages, whereas you rather should use
    ``switches'' in presentation mode.

    <item*|<tmstyle|source>>This is the privileged style for editing style
    files and packages. It enables ``source mode'', so that documents are
    rendered in a way which makes the structure fully apparent. For more
    details, we refer to the section on the <hyper-link|rendering of style
    files|../../devel/style/presentation/src-present.en.tm>.
  </description>

  The <tmstyle|article> style admits several variants, so as to make the
  layout correspond to the policy of specific journals. Currently, we have
  implemented the <TeXmacs> analogue of the <LaTeX> style <tmstyle|amsart>,
  as well as the styles <tmstyle|acmconf> and <tmstyle|jsc>. Similarly, we
  are developing styles <tmstyle|tmarticle> and <tmstyle|tmbook> which
  provide an alternative layout for articles and books.

  In addition to variants of the <tmstyle|article> and <tmstyle|book> styles,
  <TeXmacs> provides also a few other styles, which are based on the main
  styles, but which provide some additional markup.

  <\description>
    <item*|<tmstyle|letter>>This style is based on the informal
    <tmstyle|generic> style, but it provides additional markup for writing
    letters. The additional macro are mainly used for headers and endings of
    letters.

    <item*|<tmstyle|exam>>This style, which is again based on
    <tmstyle|generic>, provides some additional markup for headers of exams.
    It also customizes the rendering of exercises.

    <item*|<tmstyle|tmdoc>>This style is used for writing the <TeXmacs>
    documentation. It contains several tags for special types of content and
    extensions for linking, indexing, document traversal, <abbr|etc.>. Some
    aspects of this style are still under heavy development.
  </description>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>