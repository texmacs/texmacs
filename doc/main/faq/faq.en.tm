<TeXmacs|1.0.2.3>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Frequently asked questions>

  <\question>
    Can I use <TeXmacs> under Windows or MacOS?
  </question>

  <\answer>
    Not yet, but we plan to port <TeXmacs> to these platforms. As a general
    rule, <TeXmacs> should be relatively easy to port to Unix based systems
    with X Window. It is therefore likely that a version for the Unix-based
    MacOS-X would not be so hard to achieve. We are also waiting for feedback
    from users about the compilation of <TeXmacs> under <name|Cygwin> on
    Windows. Nevertheless, work is in progress to rewrite the graphical user
    interface of <TeXmacs>, so that it becomes more portable. Your
    <hlink|help|http://www.texmacs.org/Web/Contribute.html> might actually be
    very useful here.
  </answer>

  <\question>
    How can I see the <LaTeX> or <TeX> code corresponding to what I see on
    the screen?
  </question>

  <\answer>
    This question is due to a fundamental misunderstanding about <TeXmacs>.
    Indeed, <TeXmacs> is not <em|based> on <TeX>/<LaTeX>, although it does
    support (not yet perfect) <em|conversion> to and from <LaTeX>.
    Furthermore, in theory at least, there is actually no need anymore to
    look at something like the <TeX> source, since <TeXmacs> is guaranteed to
    be fully WYSIWYG. Conversion to <LaTeX> may only be useful, when
    transmitting an accepted paper to the publisher of a journal.
  </answer>

  <\question>
    I just installed <TeXmacs>, but when I start typing or when I take a look
    at the help, <TeXmacs> hangs for a minute. In the terminal where I
    launched the program, I also see that my disk is being filled with files.
  </question>

  <\answer>
    This behaviour is normal. <TeXmacs> calls <name|Metafont> in order to
    generate fonts which are not yet present. The first time you launch
    <TeXmacs>, many fonts may therefore have to be generated. In order to
    avoid this, you may download some <hlink|pregenerated
    fonts|http://www.texmacs.org/Download/Fonts.html>.
  </answer>

  <\question>
    <em|Why don't you use a common graphical user interface like GTK for your
    scrollbars, menus, and so on ?>
  </question>

  <\answer>
    When I started to develop <TeXmacs> about four years ago, the common
    graphical user interfaces were not as good as nowadays. Moreover, I
    wanted the GUI to support some special features, like <TeX> fonts in the
    menus. Nevertheless, now that graphical user interfaces did become much
    better, I plan to switch to guile-gtk as soon as possible. Using
    Guile-gtk in combination with <TeXmacs> has three main advantages:

    <\enumerate>
      <item>One has full access to the GTK widget set, which includes menus,
      scrollable windows, file choosers, iconbars, etc.

      <item>Guile-gtk provides you with a very flexible and customizable way
      to use these widgets.

      <item>The incorporation of Guile-gtk in <TeXmacs> should be natural,
      since <TeXmacs> already supports the <name|Guile>/<name|Scheme>
      extension language.
    </enumerate>
  </answer>

  <\question>
    A publisher sent me a giant <LaTeX> preamble I'm supposed to put in in
    order to prepare a book for them. What is the best way of putting it in
    and figuring if it will work?
  </question>

  <\answer>
    I recommand to convert the preamble to <TeXmacs> and to put the result in
    a <TeXmacs> style file. However, the result will probably be
    disappointing, because conversion between <TeX>/<LaTeX> and <TeXmacs> is
    not yet perfect and style files are particularly problematic. What you
    can also do is write a <TeXmacs> style file by your own which supports
    the major extra constructs you want to use from the editors style file.
    When you convert your book to <LaTeX>, you next use the editors style.
    Some layout will probably need to be redone at that stage, but this
    should actually be the work of the editor... Please look in the <TeXmacs>
    help for more information about convertions between <TeXmacs> and
    <LaTeX>.
  </answer>

  <\question>
    <em|To which formats can I convert <TeXmacs> documents?>
  </question>

  <\answer>
    <TeXmacs> uses its own free, structured data format and Wysiwyg
    postscript output is provided. It is possible to save <TeXmacs> documents
    as scheme expressions without loss of information. Converters from and to
    <LaTeX> are provided, which will be improved between versions 1.0 and
    1.1. Converters from and to <name|Html>/<name|Mathml>/<name|Xml> will
    also appear between versions 1.0 and 1.1 (a rudimentary converter from
    <name|Html> already exists). <name|Pdf> output can be achieved by
    converting <name|Postscript> documents. We would very much appreciate
    your <hlink|help|http://www.texmacs.org/Web/Contribute.html> for writing
    and improving converters for <TeXmacs> documents.
  </answer>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
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