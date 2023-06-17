<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|steelblue-scene|1.0|steelblue-scene|1.0>

    <\src-purpose>
      Coloring schemes on steelblue backgrounds
    </src-purpose>

    <src-copyright|2013--2023|Joris van der Hoeven>

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

  <copy-theme|steelblue-bright-scene|light-scene>

  <assign|steelblue-bright-scene-math-color|#483030>

  <assign|steelblue-bright-scene-strong-color|#402020>

  <\active*>
    <\src-comment>
      Light ice
    </src-comment>
  </active*>

  <copy-theme|steelblue-light-scene|steelblue-bright-scene>

  <assign|steelblue-light-scene-bg-color|#e0e0e8>

  <\active*>
    <\src-comment>
      Lighter ice
    </src-comment>
  </active*>

  <copy-theme|steelblue-lighter-scene|steelblue-bright-scene>

  <assign|steelblue-lighter-scene-bg-color|#e0e0e8>

  <\active*>
    <\src-comment>
      Dark ice
    </src-comment>
  </active*>

  <copy-theme|steelblue-dark-scene|dark-scene>

  <assign|steelblue-dark-scene-bg-color|#8080a0>

  <assign-uniform|steelblue-dark-scene|white>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>