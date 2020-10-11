<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Environments>

  In a similar way as content-based tags, environments are used to mark
  portions of text with a special meaning. However, while
  <hlink|content-based tags|man-content-tags.en.tm> usually enclose small
  portions of text, environments often enclose portions that are several
  paragraphs long. Frequently used environments in mathematics are
  <markup|theorem> and <markup|proof>, like in the example below:

  <\theorem>
    There exist no positive integers <math|a>, <math|b>, <math|c> and
    <math|n> with <math|n\<geqslant\>3>, such that
    <math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    I do not have room here to write the proof down.
  </proof>

  You may enter environments using <menu|Insert|Environment>. Other
  environments with a similar rendering as theorems are <markup|proposition>,
  <markup|lemma>, <markup|corollary>, <markup|axiom>, <markup|definition>.
  You may use the <markup|dueto> macro (entered using <key|\\ d u e t o
  return>) in order to specify the person(s) to which the theorem is due,
  like in

  <\theorem>
    <dueto|Pythagoras>Under nice circumstances, we have
    <math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  Other frequently used environments with a similar rendering as theorems,
  but which do not emphasize the enclosed text, are <markup|remark>,
  <markup|note>, <markup|example>, <markup|warning>, <markup|exercise> and
  <markup|problem>. The remaining environments <markup|verbatim>,
  <markup|code>, <markup|quote>, <markup|quotation> and <markup|verse> can be
  used in order to enter multiparagraph text or code, quotations or poetry.

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>