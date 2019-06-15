<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|metal-brushed-deco|1.0|metal-brushed-deco|1.0>

    <\src-purpose>
      Metal ornament for presentations and posters.
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

  <copy-theme|metal-brushed|light-ornament>

  <assign|metal|<macro|x|<with-metal-brushed|<ornament|<arg|x>>>>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|metal-brushed-ornament-color|<pattern|metal-brushed-medium.png|*3/5|*3/5|#c0c0d0>>

  <assign|metal-brushed-ornament-extra-color|<pattern|metal-brushed-dark.png|*3/5|*3/5|#8080a0>>

  <assign|metal-brushed-ornament-title-color|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>

  <assign|metal-brushed-ornament-sunny-color|#e0e0e8>

  <assign|metal-brushed-ornament-shadow-color|#9090ac>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>