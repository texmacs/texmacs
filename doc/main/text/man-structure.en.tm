<TeXmacs|1.0.2.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Typing structured text>

  Usually, long documents have a structure: they are organized in chapters,
  sections and subsections, they contain different types of text, such as
  regular text, citations, footnotes, theorems, etc. After selecting a
  <def-index|document style> in <menu|Document|Style>, <TeXmacs> takes care
  of specific layout issues, such as numbering of sections, pages, theorems,
  typesetting citations and footnotes in a nice way and so on.

  Currently, four standard document styles have been implemented: letter,
  article, book and seminar. The seminar style is used for making
  transparencies. As soon as you have selected such a style, you can organize
  your text into sections (see <menu|Text|Section>) and use specific
  <def-index|environments>. Examples of environments are theorem,
  proposition, remark and so on (see <menu|Text|Environment>). Other examples
  are lists of items (see <menu|Text|Itemize>) or numbered lists (see
  <menu|Text|Enumerate>).

  When you get more acquainted with <TeXmacs>, it is possible to add your own
  new environments in your own style file. Assume for instance that you often
  make citations and that you want those to appear in italic, with left and
  right margins of 1cm. Instead of manually changing the text and paragraph
  properties each time you make a citation, it is better to create a citation
  environment. Not only it will be faster to create a new citation when doing
  so, but it is also possible to systematically change the layout of your
  citations throughout the document just by changing the definition of the
  citation environment. The latter situation occurs for instance if you
  discover <with|font-shape|italic|a posteriori> that you prefer the
  citations to appear in a smaller font.

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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|document style>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Style>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Text>|<with|font-family|<quote|ss>|Section>>|<pageref|idx-3>>

      <tuple|<tuple|environments>|<pageref|idx-4>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Text>|<with|font-family|<quote|ss>|Environment>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Text>|<with|font-family|<quote|ss>|Itemize>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Text>|<with|font-family|<quote|ss>|Enumerate>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>