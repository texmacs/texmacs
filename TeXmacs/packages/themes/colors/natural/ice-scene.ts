<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|ice-scene|1.0|ice-scene|1.0>

    <\src-purpose>
      Coloring schemes on icy backgrounds
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-scene|dark-scene>

  <\active*>
    <\src-comment>
      Bright (white) ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-bright-scene|light-scene>

  <assign|natural-ice-bright-scene-math-color|#503050>

  <assign|natural-ice-bright-scene-strong-color|#602060>

  <\active*>
    <\src-comment>
      Light ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-light-scene|natural-ice-bright-scene>

  <assign|natural-ice-light-scene-bg-color|<pattern|ice-medium-blue.png|*3/5|*3/5|pastel
  blue>>

  <\active*>
    <\src-comment>
      Lighter ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-lighter-scene|natural-ice-bright-scene>

  <assign|natural-ice-lighter-scene-bg-color|<pattern|ice-light.png|*3/5|*3/5|pastel
  blue>>

  <\active*>
    <\src-comment>
      Dark ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-dark-scene|dark-scene>

  <assign|natural-ice-dark-scene-bg-color|<pattern|ice-dark-blue.png|*3/5|*3/5|dark
  blue>>

  <assign-uniform|natural-ice-dark-scene|white>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>