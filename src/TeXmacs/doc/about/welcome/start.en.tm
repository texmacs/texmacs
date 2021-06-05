<TeXmacs|1.99.21>

<style|<tuple|tmdoc|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Getting started with <TeXmacs>>

  This short guide is designed to help you master some of the fundamental
  notions of <TeXmacs>.

  <section|Document structure>

  Please move the cursor inside this <strong|piece of bold text>. You will
  observe a cyan box that appears around it. On the status bar, at the bottom
  right of your window, you may also notice the word<nbsp>\Pstrong\Q. These
  two things reveal part of the <em|structure> of your document. In this
  case, the bold text was produced using the <em|tag> (also called <em|markup
  element> or <em|environment>) <markup|strong> and the box indicates the
  scope of this<nbsp>tag.

  <TeXmacs> comes with hundreds of tags for different purposes. The
  <markup|strong> tag is used to highlight important pieces of text. Most
  tags refer to <em|intent> (like highlighting important text) rather than
  <em|layout> (like printing the characters in boldface). <TeXmacs> makes use
  of customizable style files in order to translate intent into layout. This
  allows you to easily switch to another presentation (<abbr|e.g.> italic
  instead of bold text) if you change your mind.

  Tags can be nested. Move the cursor just before the number 2 (for the
  square) in the formula below and observe that four boxes are displayed :
  three light grey boxes (one for the entire equation, one for the expression
  inside the parentheses, and one for the fraction), plus a cyan box for the
  superscript. The cyan box is always used for the <em|innermost> tag with
  respect to cursor position, and it is called the <em|focus>.

  <\equation>
    z=a*<around*|(|b+<frac|c<rsup|2>|d>|)>.
  </equation>

  <section|Navigation>

  Let us examine more closely the <em|status bar>, located at the very bottom
  of this window, below the editing region. When moving the cursor around in
  this document, you will notice that the right hand side of the status bar
  shows the character just before the cursor. Now move the cursor back in the
  above equation, right after the number 2 and inside the superscript (check
  that the cyan box is displayed around the number 2). As you see, the status
  bar lists all englobing environments in order, from the outermost to the
  innermost one. Experiment by moving the cursor around in the formula while
  keeping an eye on the status bar. You will soon get comfortable using the
  feedback from the colored boxes and from the status bar while navigating in
  documents.

  There is an invisible character in the formula after the <math|a> and
  before the parenthesis in the equation: can you position the cursor and
  read off what it is from the status bar? You are right, it is an invisible
  multiplication. In mathematical formulas, you should refrain yourself from
  using a space if what you really mean is a multiplication. Remember that
  <TeXmacs> expects you to write down what you mean, not how it should be
  displayed. Multiplications should be invisible in the printed version of
  your document, but you might find it convenient to show them when editing
  your document. This can be done using the menu <menu|Document|Informative
  flags|Detailed>.

  Again position your cursor inside the above equation, and try moving around
  using the arrow keys <key*|left> <key*|right> <key*|up> <key*|down>. For
  instance, can you move the cursor from before to <math|d> to after the
  <math|2> and then after the invisible multiplication? Quickly, the cursor
  movements will probably feel very natural. Remember that the boxes (and in
  particular the cyan one) indicate where the cursor is located in the
  document structure.

  As an exercise, here is a word that uses a different layout for each one of
  its characters:

  <\quotation>
    <strong|c><verbatim|r><name|a><move|z||-.2em><with|color|red|y>
  </quotation>

  Experiment adding characters to this word: observe how the exact cursor
  position determines in which environment a new character gets inserted. Can
  you insert a second strong letter \<#2018\>c\<#2019\> and a<nbsp>second
  lowered letter \<#2018\>z\<#2019\> with only two mouse clicks and two
  keystrokes?

  <section|Toolbars>

  The graphical user interface of <TeXmacs> is highly context sensitive. In
  particular, the menus and the toolbars depend on the cursor position.
  Roughly speaking, the three toolbars serve the following purposes:

  <paragraph|Main toolbar>The first toolbar contains context-independent
  icons for common operations on files (<icon|tm_new_x2.png>, <text-dots>,
  <icon|tm_cancel_x2.png>), common editing operations (<icon|tm_cut_x2.png>,
  <text-dots>, <icon|tm_redo_x2.png>) and
  browsing<nbsp>(<icon|tm_back_x2.png>, <text-dots>,
  <icon|tm_forward_x2.png>).

  <paragraph|Mode-dependent toolbar>The icons on the second toolbar only
  depend on the main editing mode (text, mathematics, graphics, etc.). Inside
  ordinary text, the toolbar is subdivided into the following four groups:

  <\itemize>
    <item>the icons <icon|tm_section_x2.png>, <text-dots>,
    <icon|tm_index_x2.png> are used for the insertion of \Plarge
    environments\Q that structure your document, like sections, theorems,
    item lists, etc.

    <item>the icons <icon|tm_emphasize_x2.png>, <text-dots>,
    <icon|tm_color_x2.png> are for common textual markup (emphasis, verbatim,
    color).

    <item>the icons <icon|tm_traverse_x2.png>, <text-dots>,
    <icon|tm_tmdoc_annotate_x2.png> are specific to the <tmstyle|tmdoc> style
    used by this particular document (they provide acccess to some macros
    that are useful when writing <TeXmacs> documentation, and you can ignore
    them for the moment).

    <item>the icons <icon|tm_math_x2.png>, <text-dots>,
    <icon|tm_shell_x2.png> are for the insertion of special types of markup,
    such as equations, tables, images, hyperlinks, animations, or interactive
    sessions.
  </itemize>

  <paragraph|Focus toolbar>The icons on the third and smallest toolbar are
  extremely context sensitive. It offers functionalities that are directly
  connected to the current focus (i.e.<nbsp>the current innermost
  environment, which is indicated by the cyan box).

  Observe how the focus bar changes when the cursor is moved; try clicking on
  the <icon|tm_numbered_x2.png> icon when the focus is on the above bullet
  list (but not immediately after a bullet) and observe what happens. The
  same mechanism allows displayed formulas to be numbered: go ahead and try
  it with the ones in this document!

  <section|Mathematical formulas>

  It is now time to type your first formula, say

  <\equation*>
    <big|int><rsub|a><rsup|b>f<rprime|'><around*|(|x|)>*\<mathd\>x=f<around*|(|b|)>-f<around*|(|a|)>.
  </equation*>

  Here is how we did it (go ahead and try whether you can repreduce the
  example):

  <\itemize>
    <item>Click on the <icon|tm_math_x2.png> icon in the mode-dependent icon
    toolbar to insert a <em|displayed formula> (<abbr|i.e.> a<nbsp>large
    centered formula).

    <item>The integral sign can be found under the <icon|tm_bigop_x2.png>
    icon, available when the cursor is inside a<nbsp>formula. When hovering
    the mouse over the button with <math|<op|<big|int>>>, a help balloon
    allows you to discover the corresponding keyboard
    shortcut<nbsp><shortcut|(math-big-operator "int")>. If you know <LaTeX>,
    then you may also use the <LaTeX> command <key|\\ i n t> followed by
    <key|return>.

    <item>The subscripts and superscripts can be obtained using the
    <icon|tm_subsup_x2.png> icon in the toolbar, or using the keys <key|^>
    and <key|_>. Make sure you exit the subscript environment before creating
    a superscript, otherwise you will be creating a superscript inside the
    subscript: remember that the cyan box is here to help you figure out the
    exact cursor position.

    <item>The prime can be inserted directly from the corresponding key
    <key|'>.

    <item>Note that typing an opening parenthesis <key|(> automatically
    creates the matching parenthesis as well. To exit the parentheses, use
    the arrow <key|right> or <key|)>.

    <item>Before the subexpression <math|\<mathd\>x>, an invisible
    multiplication is required: simply use the <key|*> key. The status bar
    will confirm that the multiplication sign has been entered correctly.

    <item>The upright \<#2018\><math|\<mathd\>>\<#2019\> from differential
    calculus is obtained <em|via> the shortcut <key|d tab tab>. Here
    <key|tab> stands for the tab-key and you should press <key|d> without
    using the shift key.
  </itemize>

  <section|Mathematical symbols>

  <TeXmacs> uses two simple but powerful mechanisms for entering mathematical
  symbols:

  <\itemize>
    <item>Basic symbols can be <em|juxtaposed> to obtain more complex ones.
    For instance, typing <key|- \<gtr\>> (two successive keystrokes here)
    yields <math|\<rightarrow\>>. Can you guess how to insert a<nbsp>symbol
    <math|\<pm\>> before the<nbsp><math|b> in the formula below?

    <\equation*>
      a=b
    </equation*>

    Such sequences of keystrokes are designed to be intuitive and easy to
    remember. Guess how to change the formula above to
    <math|a\<leqslant\>\<pm\>b>, <math|a\<rightarrow\>\<pm\>b> or even
    <math|a\<rightrightarrows\>\<pm\>b>?

    <item><em|Variants> of a symbol can be obtained using the tab-key
    <key|tab>, as demonstrated by the differential
    \<#2018\><math|\<mathd\>>\<#2019\> earlier. When several variants are
    available, pressing <key|tab> repeatedly cycles through them. In the
    formula above, type <key|\<less\>> \ between <math|a> and <math|b>, and
    immediately after this press <key|tab> repeatedly (if you are not sure
    what a symbol represents, keep an eye on the status bar). This mechanism
    can be used in particular to obtain Greek letters: try replacing <math|g>
    and <math|b> by <math|\<gamma\>> and <math|\<beta\>>, respectively, in
    the expression of the Lorentz factor from special relativity,

    <\equation*>
      g=<frac|1|<sqrt|1-b<rsup|2>>>.
    </equation*>
  </itemize>

  The two mechanisms can be combined: can you guess how the mathematical
  symbol <math|\<Updownarrow\>> can be obtained? Hint: it is a variant of
  <math|\<Leftrightarrow\>>.

  Don't forget entering <em|math mode> before trying to insert mathematical
  symbols or markup. New formulas can be inserted using <icon|tm_math_x2.png>
  icon on the mode-dependent icon toolbar or using the keyboard shortcuts
  <key|$> (inline formula) or <shortcut|(make-equation*)> (displayed
  equation).

  <tmdoc-copyright|2021|Basile Audoly|Joris van der Hoeven>

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