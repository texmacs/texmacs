<TeXmacs|1.0.7.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Improved spacing inside formulas (1.0.7.10)>

  In the new version, the spacing around mathematical operators has been made
  dependent on the semantic context. For instance, when used as an infix
  operator in a subtraction <math|x-y>, there are small spaces around the
  minus sign <math|->; this is no longer the case in <math|-x>, where we
  use the minus as a<nbsp>prefix. Similarly, the spacing inside lists of operators
  <math|+,-,\<times\>> is now correct. However, the modification may alter
  the spacing inside some formulas in existing documents. For critical
  documents, you may thus want to review the line breaking.

  Some of the keyboard shortcuts inside formulas have also been modified. For
  instance, <math|\<wedge\>> and <math|\<vee\>> are now obtained by typing
  <key|&> <abbr|resp.> <key|%>. The shortcuts for <math|\<in\>>,
  <math|\<prec\>> and <math|\|> have also been changed. For more information,
  please refer to the documentation on <hlink|editing mathematical
  formulas|$TEXMACS_DOC_PATH/main/math/man-math.en.tm>. At this place, you
  will also find more information about the newly added semantic editing
  features.

  <tmdoc-copyright|2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>