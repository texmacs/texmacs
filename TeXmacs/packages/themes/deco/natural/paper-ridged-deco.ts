<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-ridged-deco|1.0|paper-ridged-deco|1.0>

    <\src-purpose>
      Ridged paper ornament for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-theme>

  <copy-theme|paper-ridged|light-ornament>

  <assign|ridged-paper|<macro|x|<with-paper-ridged|<ornament|<arg|x>>>>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|paper-ridged-ornament-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcdc>>

  <assign|paper-ridged-ornament-extra-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|paper-ridged-ornament-title-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <assign|paper-ridged-ornament-sunny-color|#f0e0e0>

  <assign|paper-ridged-ornament-shadow-color|#d0a0a0>

  <\active*>
    <\src-comment>
      Text colors
    </src-comment>
  </active*>

  <assign|paper-ridged-strong-color|#504000>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>