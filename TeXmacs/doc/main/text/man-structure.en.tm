<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Typing structured text>

  Usually, long documents have a structure: they are organized in chapters,
  sections and subsections, they contain different types of text, such as
  regular text, citations, footnotes, theorems, etc. After selecting a
  <def-index|document style> in <menu|Document|Style>, <TeXmacs> takes care
  of specific layout issues, such as numbering of sections, pages, theorems,
  typesetting citations and footnotes in a nice way and so on.

  Currently, several standard document styles have been implemented:
  <tmstyle|generic>, <tmstyle|article>, <tmstyle|book>, <tmstyle|letter>,
  <tmstyle|exam>, <tmstyle|beamer>, <tmstyle|seminar>, <tmstyle|source>. For
  instance, the article style can be used for writing articles. Besides,
  there are styles for common journals and special purposes, such as the
  <TeXmacs> documentation.

  As soon as you have selected a document style, you can organize your text
  into sections (see <menu|Insert|Section>) and use specific
  <def-index|environments> (also called <em|tags>). Examples of environments
  are theorem, proposition, remark and so on (see <menu|Insert|Enunciation>).
  Other examples are lists of items (see <menu|Insert|Itemize>) or numbered
  lists (see <menu|Insert|Enumerate>). Further examples of frequently used
  tags are <markup|strong> (for writing \Pimportant\Q text), <markup|name>
  (for writing names of persons), etc.

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

  There are a few general editing principles which make it easy to manipulate
  structured documents using <TeXmacs>. One major concept is the <em|current
  focus>, which is best illustrated on an example. Assume that we are in the
  process of entering a classical theorem:

  <\quote-env>
    The following theorem is due to <name|Euler>:

    <\big-envbox>
      <\theorem>
        <small-focus|<math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<math-cursor>>>.
      </theorem>
    </big-envbox>
  </quote-env>

  At the position of the cursor, the grey and cyan boxes indicate the active
  tags: in this case, the cursor is both inside a theorem and a formula. The
  innermost active tag (the formula <math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1>
  in our example) is surrounded by a cyan box and called the <em|current
  focus>.

  The contents of the <menu|Focus> menu and <em|focus toolbar> (the lowest
  toolbar) are highly context dependent and determined as a function of the
  current focus. In our example, the focus toolbar contains a<nbsp>popup menu
  button <menu|Formula>; when selecting <menu|Equation> in this menu, the
  text will change into

  <\quote-env>
    The following theorem is due to <name|Euler>:

    <\big-envbox>
      <\theorem>
        \;

        <\big-focus>
          <\equation*>
            \<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<math-cursor>.
          </equation*>
        </big-focus>
      </theorem>
    </big-envbox>
  </quote-env>

  Similarly, the arrow buttons on the left hand side of the focus toolbar
  allow you to jump to similar tags. In this case, they will allow you to
  quickly traverse all formulas and equations in your document. For more
  information on \P<hlink|structured editing
  operations|../editing/man-structured-editing.en.tm>\Q we refer to the
  chapter on <hlink|editing tools|../editing/man-editing-tools.en.tm>.

  A second important concept is the <em|current editing mode>. Currently,
  there are five major modes: text mode, mathematics mode, program mode,
  graphics mode and source mode. In principle, the current mode can be
  determined from the current focus, but the mode is likely to change less
  often than the focus. The <em|mode dependent toolbar> above the focus
  toolbar contains several buttons which are useful in the current mode. The
  contents of the <menu|Insert> and <menu|Format> menus are also mode
  dependent.

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>