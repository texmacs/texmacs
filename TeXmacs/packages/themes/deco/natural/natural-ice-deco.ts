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

  <use-package|light-combo|dark-combo>

  <copy-theme|natural-ice|light-deco>

  <new-deco|natural-ice>

  <\active*>
    <\src-comment>
      Light ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-light-scene|light-scene>

  <assign|natural-ice-light-scene-bg-color|<pattern|ice-medium-blue.png|*3/5|*3/5|pastel
  blue>>

  <assign|natural-ice-light-scene-color|black>

  <assign|natural-ice-light-scene-math-color|#503050>

  <assign|natural-ice-light-scene-strong-color|#602060>

  <\active*>
    <\src-comment>
      Dark ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-dark-scene|dark-scene>

  <assign|natural-ice-dark-scene-bg-color|<pattern|ice-dark-blue.png|*3/5|*3/5|dark
  blue>>

  <assign|natural-ice-dark-scene-color|white>

  <assign|natural-ice-dark-scene-math-color|white>

  <assign|natural-ice-dark-scene-strong-color|white>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|natural-ice-ornament-render-title|<value|with-natural-ice-dark-scene>>

  <assign|natural-ice-ornament-render-body|<value|with-natural-ice-light-scene>>

  <assign|natural-ice-ornament-sunny-color|#f4f4ff>

  <assign|natural-ice-ornament-shadow-color|#d8d8ff>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>