<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Customization of the environments for floating objects>

  The following macros can be used for customizing the rendering of
  figure-like environments:

  <\explain|<explain-macro|render-small-figure|aux|name|body|caption>>
    This macro is used for rendering small figure-like environments. The
    first argument <src-arg|aux> specifies an auxiliary channel (like
    ``figure'' or ``table'') which is used for inserting the caption inside
    the list of figures. The second argument <src-arg|name> specifies the
    name of the figure (like ``Figure 2.3'' or ``Table <no-break>5''). The
    last arguments <src-arg|body> and <src-arg|caption> correspond to the
    figure itself and a caption.
  </explain>

  <\explain|<explain-macro|render-big-figure|aux|name|body|caption>>
    Similar to <markup|render-small-figure>, but for displaying a big
    figure-like environments.
  </explain>

  The following tags can be used for customizing the appearance the text
  around figures, tables and footnotes:

  <\explain|<explain-macro|figure-name|name>>
    This macro controls the appearance of the text ``Figure''. By default, we
    use bold face.
  </explain>

  <\explain|<explain-macro|figure-sep>>
    This macro produces the separator between the figure and its number and
    the caption. By default, it produces a period followed by a space.
  </explain>

  <\explain|<explain-macro|footnote-sep>>
    This macro produces the separator between the number of the footnote and
    the text. By default, it produces a period followed by a space.
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
    <associate|language|english>
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