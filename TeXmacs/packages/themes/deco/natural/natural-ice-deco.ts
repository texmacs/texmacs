<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|natural-ice-deco|1.0|natural-ice-deco|1.0>

    <\src-purpose>
      Pine ornament for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-combo>

  <copy-theme|natural-ice|light-ornament>

  <assign|ice|<macro|x|<with-natural-ice|<ornament|<arg|x>>>>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|natural-ice-ornament-color|<pattern|ice-medium-blue.png|*3/5|*3/5|pastel
  blue>>

  <assign|natural-ice-ornament-extra-color|<pattern|ice-dark-blue.png|*3/5|*3/5|dark
  blue>>

  <assign|natural-ice-ornament-title-color|white>

  <assign|natural-ice-ornament-sunny-color|#f4f4ff>

  <assign|natural-ice-ornament-shadow-color|#d8d8ff>

  <\active*>
    <\src-comment>
      Text colors
    </src-comment>
  </active*>

  <assign|natural-ice-strong-color|#602060>

  <assign|natural-ice-math-color|#503050>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>