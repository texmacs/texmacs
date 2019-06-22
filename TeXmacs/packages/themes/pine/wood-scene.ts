<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|wood-scene|1.0|wood-scene|1.0>

    <\src-purpose>
      Coloring schemes on wooden backgrounds
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
      Bright pine wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine-bright-scene|light-scene>

  <assign|wood-pine-bright-scene-bg-color|<pattern|pine-bright.png|*3/5|*3/5|#f8ecd4>>

  <assign|wood-pine-bright-scene-monochrome-bg-color|#f8ecd4>

  <assign|wood-pine-bright-scene-math-color|#500000>

  <assign|wood-pine-bright-scene-strong-color|#0c3000>

  <assign|xwood-pine-bright-scene-bg-color|<pattern|tmfs://artwork/textures/wood/tileable-wood-pattern.jpg|4in|100@>>

  <\active*>
    <\src-comment>
      Light pine wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine-light-scene|wood-pine-bright-scene>

  <assign|wood-pine-light-scene-bg-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|wood-pine-light-scene-monochrome-bg-color|#e0b050>

  <assign|xwood-pine-light-scene-bg-color|<pattern|tmfs://artwork/textures/wood/purty-wood-pattern.jpg|4in|100@>>

  <\active*>
    <\src-comment>
      Dark "pine" wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine-dark-scene|dark-scene>

  <assign|wood-pine-dark-scene-bg-color|<pattern|wood-dark.png|*3/5|*3/5|#40200c>>

  <assign-uniform|wood-pine-dark-scene|#f8f8f4>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>