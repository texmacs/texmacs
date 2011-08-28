<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Mathematical typesetting>

  <section|Introduction>

  In this chapter we describe the algorithms used by <apply|TeXmacs> in order
  to typeset mathematical formulas. This is a difficult subject, because
  esthetics and effectiveness do not always go hand in hand. Until now,
  <apply|TeX> is widely accepted for having achieved an optimal compromise in
  this respect. Nevertheless, we thought that several improvements could
  still be made, which have now been implemented in <apply|TeXmacs>. We will
  shortly describe the motivations behind them.

  In order to obtain esthetic formulas, what criteria should we use? It is
  often stressed that good typesetting allows the reader to concentrate on
  what he reads, without being distracted by ugly typesetting details. Such
  distracting details arise when distinct, though similar parts of text are
  typesetted in a non uniform way:

  <\description>
    <expand|item*|Different base lines.>The eye expects text of a similar
    nature to be typesetted with respect to a same base line. For instance,
    in <with|mode|math|x+y+z>, the bottoms of the <with|mode|math|x> and
    <with|mode|math|z> should be at the same height as the bottom of the
    <with|mode|math|u>-part in the <with|mode|math|y>. This should again be
    the case in <with|mode|math|2<rsup|x>+2<rsup|y>+2<rsup|z>>.

    <expand|item*|Unequal spacing.>Different components of text with
    approximately the same function should be separated by equal amounts of
    space. For instance, in <with|mode|math|a<rsup|2>+f<rsup|2>>, the
    typesetter should notice the hangover of the <with|mode|math|f>. This
    should again be the case in <with|mode|math|e<rsup|a>+e<rsup|f>+e<rsup|x>\
    >. Similarly, the distance between the baselines of the
    <with|mode|math|a> and the <with|mode|math|i> in
    <with|mode|math|a<rsub|i>> should not be disproportially large with
    respect to the height of an <with|mode|math|x>.
  </description>

  Additional difficulties may arise when considering automatically generated
  formulas, in which case line breaking has to be dealt with in a
  satisfactory way.

  Unfortunately, the different esthetic criteria may enter into conflict with
  each other. For instance, consider the formula
  <with|mode|math|x<rsub|p>+x<rsub|p><rsup|2>>. On the one hand, the
  baselines of the scripts should be the same, but the other hand, the first
  subscript should not be ``disproportionally low'' with respect to the
  <with|mode|math|x>. Unfortunately, this dilemma can not been solved in a
  completely satisfactory way without the help of a human for the simple
  reason that the computer has no way to know whether the
  <with|mode|math|x<rsub|p>> and <with|mode|math|x<rsub|p><rsup|i>> are
  ``related''. Indeed, if the <with|mode|math|x<rsub|p>> and
  <with|mode|math|x<rsub|p><rsup|i>> are close (like in
  <with|mode|math|x<rsub|p>+x<rsub|p><rsup|i>>), then it is natural to opt
  for a common base line. However, if they are further away from each other
  (like in <with|mode|math|x<rsub|p>+<big|sum><rsub|i=0><rsup|\<infty\>>c<rsu\
  b|i>x<rsub|p><rsup|i>>), then we might want to opt for different base lines
  and locally optimize the rendering of the first <with|mode|math|x<rsub|p>>.

  Consequently, <apply|TeXmacs> should offer a reasonable compromise for the
  most frequent cases, while offering methods for the user to make finer
  adjustments in the remaining ones. Currently, we just provided the
  <apply|menu|Format|Transform|Move> and <apply|menu|Format|Transform|Resize>
  constructs to move and resize boxes in order to perform such adjustments.
  For instance, if the brackets around the two sums

  <\expand|equation*>
    \<phi\><left|(><big|sum><rsub|i>a<rsub|i>x<rsup|i><right|)>=\<psi\><left|\
    (><big|sum><rsub|j>b<rsub|j>y<rsup|j><right|)>
  </expand>

  have different sizes, then one may resize the bottom of the subscript
  <with|mode|math|j> of the second sum to <verbatim|0fn>. Alternatively, one
  may resize the bottoms of both the <with|mode|math|i> and
  <with|mode|math|j> subscripts to (say) <verbatim|-0.3fn>.

  Notice that one should adjust by preference in a structural and not visual
  way. For instance, one should prefer <verbatim|-0.3fn> to <verbatim|-2mm>
  in the above example, because the second option disallows you to switch to
  another font size for your document. Similarly, you should try not change
  the semantics of the formula. For instance, in the above example, you might
  have added a ``dummy subscript'' to the <with|mode|math|i> subscript of the
  sum. However, this would alter the meaning of the formula (whence make it
  non suitable as input to a computer algebra system) In the future, we plan
  to provide additional constructs in order to facilitate structural
  adjusting. For instance, in the case of a formula like

  <\expand|equation*>
    1+x<rsub|1>+x<rsub|1><rsup|2>+\<cdots\>+x<rsub|2>+x<rsub|1>x<rsub|2>+x<rs\
    ub|1><rsup|2>x<rsub|2>+\<cdots\>x<rsub|2><rsup|2>+x<rsub|1>x<rsub|2><rsup\
    |2>+x<rsub|1><rsup|2>x<rsub|2><rsup|2>+\<cdots\>,
  </expand>

  one might think of a construct to enclose the entire formula into an area,
  where all scripts are forced to be double (using dummy superscripts
  whereever necessary).

  <section|The font parameters>

  Several font parameters are crucial for the correct positioning of the
  different components. The following are often needed:

  <\description>
    <expand|item*|<verbatim|quad>.>The main font reference space
    <verbatim|1fn>, which can be taken as the distance between successive
    lines of text.

    <expand|item*|<verbatim|y1> and <verbatim|y2>.>The bottom and top level
    for the font (we have <verbatim|y2-y1=quad>).

    <expand|item*|<verbatim|sep>.>The reference minimal space between
    distinct components, like the minimal distance between a subscript and a
    superscript. In fact, <verbatim|sep=quad/10>.

    <expand|item*|<verbatim|wline>.>The width of several types of lines, like
    the fraction and square root bars, wide accents, etc.

    <expand|item*|<verbatim|yfrac>.>The height of the fraction bar, which is
    needed for the positioning of fractions and big delimiters. Usually,
    <verbatim|yfrac> is almost equal to <verbatim|yx/2> below.
  </description>

  The following parameters are mainly needed in order to deal with scripts:

  <\description>
    <expand|item*|<verbatim|yx>.>The height of the <with|mode|math|x>
    character, which is needed for the positioning of scripts. All the
    remaining parameters are actually computed as a function of
    <verbatim|yx>.

    <expand|item*|<verbatim|ysub lo base>.>Logical base line for subscripts.

    <expand|item*|<verbatim|ysub hi lim>.>Subscripts may never physically
    exceed this top height.

    <expand|item*|<verbatim|ysup lo base>.>Logical base line for
    superscripts.

    <expand|item*|<verbatim|ysup lo lim>.>Superscripts may never physically
    exceed this bottom height.

    <expand|item*|<verbatim|ysup hi lim>.>Suggestion for a physical top line
    for superscripts.

    <expand|item*|<verbatim|yshift>.>Possible shift of the base lines when we
    are inside fractions or scripts.
  </description>

  The individual strings in a font also have several important positioning
  properties. First of all, they always admit left and right slopes.
  Furthermore, they admit left and right italic corrections, which are needed
  for the positioning of scripts or when passing from text in upright to text
  in italics (or vice versa).

  <section|Some major mathematical constructs>

  <subsection|Fractions>

  The following heuristics are used:

  <\itemize>
    <item>The horizontal middles of the numerator and the denominator are
    taken to be the same.

    <item>The vertical spaces between the numerator resp. denominator and the
    fraction bar is at least <verbatim|sep>.

    <item>The depth (resp. height) of the numerator (resp. denominator) is
    descended (resp. increased) to <verbatim|y1> (resp. <verbatim|y2>) if
    necessary. This forces the base lines of not too large numerators resp.
    denominators to be the same in presence of multiple fractions.

    <item>The fraction bar has a overhang of <verbatim|sep/2> to both sides
    and the logical limits of the fraction are another <verbatim|sep/2>
    further. The logical left limit is zero.
  </itemize>

  The italic corrections are not taken into account during the positioning
  algorithms, because this may create the impression that the numerator and
  denominator are not correctly centered with respect to each other.
  Nevertheless, the italic corrections are taken into account in order to
  compute the logical bounding box of the fraction (whose has italic slopes
  vanish at both sides).

  <subsection|Roots>

  The following heuristics are used:

  <\itemize>
    <item>The vertical space between the main argument and the upper bar is
    at least <verbatim|sep>.

    <item>The root itself is typesetted like a large delimiter. The
    positioning of a potential script works only is very dependent on the
    usage of <apply|TeX> fonts.

    <item>The upper bar has a overhang of <verbatim|sep/2> at the right and
    the logical right limit of the root is situated another <verbatim|sep/2>
    further to the right.
  </itemize>

  We take the logical right border plus the italic correction of the main
  argument in order to determine the right hand limit of the upper bar. The
  left italic correction is not needed.

  <subsection|Negations>

  The following heuristics are used:

  <\itemize>
    <item>The negation bar passes through the logical center of the argument.

    <item>The italic corrections of the argument are only taken into account
    during the computation of the logical limits of the negation box (which
    has zero left and right slopes).
  </itemize>

  <subsection|Wide boxes>

  The following heuristics are used:

  <\itemize>
    <item>We use <apply|TeX> fonts for small accents and an <with|font
    shape|italic|ad hoc> algorithm for the wider ones.

    <item>The distance between the main argument and the accent is at least
    <verbatim|sep> (or a distance which depends on the <apply|TeX> font for
    small accents).

    <item>The accent is positioned horizintally according to the right slope
    of the main argument.

    <item>The slopes for the accented box are inherited from those of the
    main argument and the italic corrections are adjusted accordingly.

    <item>All script height parameters of the accented box are inherited from
    the main argument. The only exception is <verbatim|ysup_hi_lim>, which
    may be increased by the height of the accent, or determined in the
    generic way, whichever leads to the least value. It is indeed better to
    keep superscripts positioned reasonably low, whenever possible.
  </itemize>

  <section|Subscripts and superscripts>

  The positioning of subscripts and superscripts is a complicated affair, due
  to the conflict between locally and globally optimal esthetics mentioned
  above. The base line for a subscript is determined as follows:

  <\enumerate>
    <item>Always pretend that the subscript has height at least
    <verbatim|y2-yshift> in the script font (actually we should use the
    height of an <with|mode|math|M> instead).

    <item>Try to position the script at the base line given by the main
    argument.

    <item>If the top limit (given by the main argument) is physically
    exceeded by the subscript, then the base line is moved further down
    accordingly.
  </enumerate>

  The base line for a superscript is determined as follows:

  <\enumerate>
    <item>Try to physically position the superscript beneath the suggested
    top line. Usually, this will place the superscript to far down.

    <item>Move the superscript up to the logical base line if necessary. This
    will usually occur: most of the time, the logical base line is the just
    the height of an <with|mode|math|x>-script below the suggested top line.

    <item>If the superscript physically descends below the physical under
    limit given by the main box, then we move the superscript further
    upwards.
  </enumerate>

  If both a subscript and a superscript were present, then we still have to
  adjust the base lines: if the top of the subscript and the bottom of the
  superscript are not physically separated by <verbatim|sep>, then we both
  move the subscript and the superscript by the same amount away from each
  other. Because of step 1 in the positioning of the subscript, the base
  lines of double scripts will usually be the same in formulas with several
  of them.

  The right slope and italic correction of a script box may be non trivial.
  In order to compute them, we first determine the script (or main argument),
  whose right limit (taking into account its italic correction) is furthest
  to the right (this may be the main box, in the case of a big integral with
  a tiny subscript). Then the right slope of the main box is inherited by the
  right slope of this script (or main argument). As to the italic correction,
  it is precisely the difference between the right offset of the script plus
  its italic correction minus the logical right coordinate of the entire box.
  The italic correction should be at least zero though. The left slope and
  italic correction are computed in a similar way.

  <section|Big delimiters>

  The automatic positioning and computation of sizes of big delimiters is
  again complicated because of potential conflicts between locally and
  globally optimal esthetics.

  First of all, <apply|TeX> fonts come only with a discrete set of possible
  sizes for large delimiters. This is an advantage from the point of view
  that it favorites delimiters around slightly different expressions to have
  the same baselines. However, it has the disadvantage that delimiters are
  easily made ``one size to large''. For this reason, we actually diminish
  the height and the depth of the delimited expression by the small amount
  <verbatim|sep>, before computing the sizes of the delimiters.

  Secondly, it is best when the vertical middles of big delimiters occur at
  the height of fraction bars. However, in a formula like

  <\expand|equation*>
    f<left|(><frac|1|1+<frac|1|1+<frac|1|1+<frac|1|x>>>><right|)>,
  </expand>

  it may be worth it to descend the delimiters a bit. On the other hand,
  slight vertical shifts in the middles of the delimiters potentially have a
  bad effect on base lines, like in

  <\expand|equation*>
    f<left|(><big|sum><rsub|i=1><rsup|b>X<rsub|i><right|)>+g<left|(><big|sum>\
    <rsub|j=1><rsup|a>Y<rsub|j><right|)>.
  </expand>

  In <apply|TeXmacs>, we use the following compromise: we start with the
  middle of the delimited expression as a first approximation to the middle
  of the delimiters. The real middle is obtained by shifting this middle
  towards the height of fraction bars by an amount which cannot exceed
  <verbatim|sep>.

  From a horizontal point of view, we finally have to notice that we adapted
  the metrics of the big delimiters in a way that potential scripts are
  positioned in a better way. For instance, according to the <apply|TeX>
  <verbatim|tfm> file, in a formula like

  <\expand|equation*>
    <left|(>A+<left|(><big|sum><rsub|i=1><rsup|10>B<rsub|i><right|)><rsup|2><\
    right|)>,
  </expand>

  the square rather seems to be a left superscript of the second closing
  bracket than a right superscript of the first one. This is particularly
  annoying in the case of automatically generated formulas, where this
  situation occurs quite often.

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
    <associate|toc-10|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
    <associate|toc-5|<tuple|<uninit>|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
    <associate|toc-9|<tuple|3.|?>>
  </collection>
</references>
