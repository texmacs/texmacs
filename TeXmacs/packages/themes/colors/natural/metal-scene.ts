<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|metal-scene|1.0|metal-scene|1.0>

    <\src-purpose>
      Coloring schemes on metallic backgrounds
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
      Bright metal
    </src-comment>
  </active*>

  <copy-theme|metal-brushed-bright-scene|light-scene>

  <assign|metal-brushed-bright-scene-bg-color|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>

  <assign|metal-brushed-bright-scene-monochrome-bg-color|#f0f0f0>

  <assign|metal-brushed-bright-scene-strong-color|#202048>

  <assign|metal-brushed-bright-scene-math-color|#602020>

  <\active*>
    <\src-comment>
      Light metal
    </src-comment>
  </active*>

  <copy-theme|metal-brushed-light-scene|metal-brushed-bright-scene>

  <assign|metal-brushed-light-scene-bg-color|<pattern|metal-brushed-medium.png|*3/5|*3/5|#c0c0d0>>

  <assign|metal-brushed-light-scene-monochrome-bg-color|#c0c0d0>

  <\active*>
    <\src-comment>
      Dark metal
    </src-comment>
  </active*>

  <copy-theme|metal-brushed-dark-scene|dark-scene>

  <assign|metal-brushed-dark-scene-bg-color|<pattern|metal-brushed-dark.png|*3/5|*3/5|#8080a0>>

  <assign-uniform|metal-brushed-dark-scene|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>