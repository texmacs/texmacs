<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Writing a simple text>

  Once you have given your document a name, you may start to type your text.
  Later on, we will explain how to type special characters, which are
  necessary for writing texts in foreign languages. The following is a sample
  text, which you may try to type as an exercise:

  <expand|big-figure|<apply|screenshot|simple-1.en.png>|Typing a simple text
  using TeXmacs.>

  When you are done with typing, we recommend you to first save your
  document, using the <with|font series|bold|save buffer> item in the
  <apply|icon|tm_save.png> icon menu, or by pressing the <key|F3> key. A
  message on the footer should confirm the success of this operation:

  <expand|big-figure|<apply|screenshot|simple-2.en.png>|We just save the
  simple text for security.>

  For your safety, TeXmacs actually <with|font shape|italic|autosaves> your
  document every two minutes. If you forgot to save your document before
  closing TeXmacs, or if your computer is unpowered for some reason, then you
  will be prompted whether you want to retrieve the last unsaved changes in
  your document as soon as you try to load it. Again, a message on the footer
  confirms autosaving:

  <expand|big-figure|<apply|screenshot|simple-3.en.png>|TeXmacs automatically
  <with|font shape|italic|autosaves> your document every two minutes.>

  When you are done with typing, you usually want to print your document out.
  This can be done by selecting the <apply|menu|Print all> item in the
  <apply|icon|tm_print.png> icon menu, or by pressing the <key|F4> key.
  Before printing, it might be that you want to setup your printer using
  <apply|menu|File|Page setup>. You may specify a printing command (like
  <with|font family|tt|lpr>), the paper type of your printer (like <with|font
  family|tt|a4> in Europe or <with|font family|tt|letter> in the
  <abbr|U.S.A.>) and the printer's precision in dots per inch (<with|font
  family|tt|600> is default).

  In order to retrieve your text after closing TeXmacs, you first have to
  push the <apply|menu|Load buffer> item in the <apply|icon|tm_load.png> icon
  menu or hit the <key|F2> key. Next, you may select your file using the file
  browser. In our example, the file <with|font family|tt|test.tm> indeed
  appears in the file browser and we may retrieve it by rapidly clicking
  twice on it.

  <expand|big-figure|<apply|screenshot|load.en.png>|Retrieving the simple
  text from the disk.>

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
    <associate|shrinking factor|4>
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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>
