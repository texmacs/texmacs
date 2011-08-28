<TeXmacs|1.0.4.7>

<style|tmdoc>

<\body>
  <doc-data|<doc-title|<TeXmacs>--maxima interface>>

  In this tutorial, we shall discuss how to use the free Computer Algebra
  System (CAS) maxima from <TeXmacs>. It should be valid for <TeXmacs>-1.0.5
  and maxima-5.9.2.

  Let's start <TeXmacs>. We shall use the package <tmpackage|varsession>:

  <postscript|c0.png|*5/8|*5/8||||>

  This is not strictly necessary, but interactive sessions look nicer with
  it. We shall also use <menu|Automatically close brackets>:

  <postscript|c1.png|*5/8|*5/8||||>

  Now we shall start one of the versions of maxima installed:

  <postscript|c2.png|*5/8|*5/8||||>

  Also we shall tick <menu|Mathematical input>:

  <postscript|c3.png|*5/8|*5/8||||>

  If you click the icon looking like ``?'' on the toolbar, <TeXmacs> will
  show the maxima manual:

  <postscript|c4.png|*5/8|*5/8||||>

  It is convenient to clone the window:

  <postscript|c5.png|*5/8|*5/8||||>

  Then, in one of the windows you return to the buffer which contains the
  maxima session (in this case, this buffer is called <menu|no name>):

  <postscript|c6.png|*5/8|*5/8||||>

  In the other window, you can read the manual. Blue texts are hyperlink;
  clicking them displays the corresponding sections of the manual.

  Everything is ready for doing calculations. Usual methematical notations
  can be used for input expressions to a large extent. Don't forget to use
  <key|*> for multiplication; it is not visible, just produces a little
  space. Powers are produced using the toolbar or <key|^>; fractions -- by
  the toolbar or <shortcut|(make-fraction)>; square roots -- by the toolbar or <shortcut|(make-sqrt)>;
  large (automatically resizable) brackets -- by <shortcut|(make-bracket-open "(" ")" #t)> (note that with
  our settings this produces both the opening bracket and the closing one,
  and the cursor is left between them). You can use greek letters. For
  example, <with|mode|math|\<alpha\>> can be produced via the toolbar, by
  <key|H-a> (see help for the meaning of the modifier hyper), or by <key|a
  tab>. <with|mode|math|\<Gamma\>> means the
  <with|mode|math|\<Gamma\>>-function; <with|mode|math|\<pi\>> means ... what
  would you think? ... <with|mode|math|\<pi\>>; <with|mode|math|\<gamma\>>
  means the Euler constant; <with|mode|math|\<zeta\>> means the Riemann
  <with|mode|math|\<zeta\>>-function.

  <postscript|c7.png|*5/8|*5/8||||>

  The integral sign is produced via the toolbar or by <key|S-F5 I>. If it has
  limits, it means a definite integral, otherwise indefinite. Then you write
  your integrand, then a space, the differential sign (produced by <key|d tab
  tab>), a space, and your integration variable. The form of the result often
  depends on parameters; in such cases, maxima asks questions. If you don't
  want such interactive queries, you can provide the relevant information
  beforehand.

  <postscript|c8.png|*5/8|*5/8||||>

  You can select some (part of) an output field with the mouse:

  <postscript|c9.png|*5/8|*5/8||||>

  Then click on an input line, and click with the middle mouse button. The
  selected expression is inserted. You can edit it and execute.

  <postscript|c10.png|*5/8|*5/8||||>

  It is also possible to go to an earlier input line, edit it and press
  <key|enter>. The old output below this line will be replaced by the new
  one. This is very useful during the first stage of your work, when you
  investigate various possible approaches. Using this method too often leads
  to a spaghetti-like set of input and output lines, which is difficult to
  understand; it is even difficult to reproduce your calculation later.

  Here are some definite integrals. The base of natural logarithms
  <with|mode|math|\<mathe\>> is produced via the toolbar or by <key|e tab
  tab>, and the infinity symbol -- via the toolbar or by <key|@ @>.

  <postscript|c11.png|*5/8|*5/8||||>

  Sums are similar to integrals. In addition to sums with a lower limit and
  an upper one, maxima understands sums with a lower limit like
  <with|mode|math|n\<in\>[a,b,c]>, where the symbol <with|mode|math|\<in\>>
  is produced by <key|\\ i n enter>. Products are also similar. Binomial
  coefficients are produced by <key|\\ b i n o m enter>. They look like
  matrices, but differ from them! Therefore, don't try to create them via the
  toolbar menu for matrices and other kinds of tables.

  <postscript|c12.png|*5/8|*5/8||||>

  Note a subtle point: the integral sign (or the sum or product sign) is
  considered as a kind of an ``opening bracket'', and there is the
  corresponding ``closing bracket'', which is invisible, but it shows where
  your integral ends. You can see it in the <menu|Document|View|Edit source
  tree> mode as <explain-macro|big\|.>. With out setting of
  <menu|Edit|Preferences|Keyboard|Automatically close brackets>, it is
  automatically produced after the integral sign when you input it; if you
  don't use this mode, you can produce it by <key|\\ b i g tab . enter>.

  <postscript|c13.png|*5/8|*5/8||||>

  Maxima understands matrices and determinants. They are produced via the
  toolbar. New columns and rows are inserted by <shortcut|(structured-insert-right)>, <shortcut|(structured-insert-left)>,
  <shortcut|(structured-insert-down)>, <shortcut|(structured-insert-up)>. The imaginary unit <with|mode|math|\<mathi\>> is
  produced via the toolbar or by <key|i tab tab>.

  <postscript|c14.png|*5/8|*5/8||||>

  <postscript|c15.png|*5/8|*5/8||||>

  You can inserted some text explaining an input line. To this end, placing
  the cursor on this input line, select <menu|Insert text field> from the
  toolbar menu:

  <postscript|c16.png|*5/8|*5/8||||>

  Naturally, this explanation text can be as long as you need, and it can
  contain mathematical formulae.

  <postscript|c17.png|*5/8|*5/8||||>

  And here is a determinant.

  <postscript|c18.png|*5/8|*5/8||||>

  <postscript|c19.png|*5/8|*5/8||||>

  Maxima can solve equations and systems of equations. It returns the list of
  solutions.

  <postscript|c20.png|*5/8|*5/8||||>

  Here we define and plot the function <with|mode|math|f(x,y)=<frac|sin(r)|r>>
  where <with|mode|math|r=<sqrt|x<rsup|2>+y<rsup|2>>>.

  <postscript|c21.png|*5/8|*5/8||||>

  The plot appears in a separate gnuplot window:

  <postscript|c22.png|*5/8|*5/8||||>

  Here we define a recursive function and trace its execution.

  <postscript|c23.png|*5/8|*5/8||||>

  You can create a ``sub-session'' within the maxima session. With the cursor
  on an input line, select <menu|Fold input field> from the toolbar menu:

  <postscript|c24.png|*5/8|*5/8||||>

  A sub-session appears; you can edit its title:

  <postscript|c25.png|*5/8|*5/8||||>

  You can work within this sub-session; it has its own current input line.

  <postscript|c26.png|*5/8|*5/8||||>

  If you dowble-click on the left bar of the sub-session, it gets folded:

  <postscript|c27.png|*5/8|*5/8||||>

  Now only its title is visible. This is convenient for long sub-calculations
  of your project: you can fold them all, and see only the skeleton of your
  calculation. Sub-sessions can be nested. You can unfold them by
  double-clicking once more.

  Finally, we quit maxima:

  <postscript|c28.png|*5/8|*5/8||||>

  If you give a presentation, it is convenient to select <menu|Remove all
  output fields> from the toolbar menu:\ 

  <postscript|c29.png|*5/8|*5/8||||>

  All output lines disappear.

  <postscript|c30.png|*5/8|*5/8||||>

  It suffices to press <key|enter> on an input line, and maxima will be
  re-started. The results will be produced in the presence of your audience,
  just by pressing <key|enter> many times.

  <postscript|c31.png|*5/8|*5/8||||>

  Of course, you can save your work as a .tm file. Next time you start
  <TeXmacs> to edit this file, just press <key|enter> at the first input line
  to re-start maxima.
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>