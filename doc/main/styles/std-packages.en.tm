<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard <TeXmacs> packages>

  First of all, <TeXmacs> provides several packages for customizing the
  behaviour of the standard styles:

  <\description>
    <item*|<tmpackage|number-long-article>>This package induces all numbers
    of environments (theorems, remarks, equations, figures, <abbr|etc.>) to
    be prefixed by the current section number. It is usually used in
    combination with the <tmstyle|article> style (for long articles) and the
    <tmstyle|book> style (for books with long chapters).

    <item*|<tmpackage|number-europe>>By default, <TeXmacs> uses ``American
    style numbering''. This means that the same counter is used for numbering
    similar environments like theorem and proposition. In other words, a
    remark following ``Theorem 3'' will be numbered ``Remark 4''. If you want
    each environment to have its individual counter, then you should enable
    ``European style numbering'', by selecting the <tmpackage|number-europe>
    package.

    <item*|<tmpackage|number-us>>This package may be used in order to switch
    back to American style numbering in the case when a third parties style
    file enforces European style numbering.

    <item*|<tmpackage|structured-list>>This is an experimental package. By
    default, items in unnumbered lists or enumerations take no arguments and
    items in descriptions one argument. When using the
    <tmpackage|structured-list> package, they take an optional additional
    argument with the body of the item.

    <item*|<tmpackage|structured-section>>This is an experimental package. By
    default, sectional tags only take a title argument. When using the
    <tmpackage|structured-section> package, they take an optional additional
    argument with the body of the section. Moreover, the environment
    <markup|rsection> for recursive sections is provided.

    <item*|<tmpackage|varsession>>This package may be used in order to obtain
    an alternative rendering of interactive sessions. The rendering is
    designed to be nice for interactive use, although less adequate for
    printing.
  </description>

  In addition to these packages, and the many packages for internal use,
  <TeXmacs> also provides a few personal example style packages
  <tmpackage|allouche>, <tmpackage|bpr> and <tmpackage|vdh> and several style
  packages for use in combination with external plug-ins (<tmpackage|axiom>,
  <tmpackage|giac>, <tmpackage|macaulay2>, <abbr|etc.>).

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