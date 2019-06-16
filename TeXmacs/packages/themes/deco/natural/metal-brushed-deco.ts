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

  <use-package|light-combo|dark-combo>

  <copy-theme|metal-brushed|light-deco>

  <new-deco|metal-brushed>

  <\active*>
    <\src-comment>
      Light metal
    </src-comment>
  </active*>

  <copy-theme|metal-brushed-light-scene|light-scene>

  <assign|metal-brushed-light-scene-bg-color|<pattern|metal-brushed-medium.png|*3/5|*3/5|#c0c0d0>>

  <\active*>
    <\src-comment>
      Dark metal
    </src-comment>
  </active*>

  <copy-theme|metal-brushed-dark-scene|dark-scene>

  <assign|metal-brushed-dark-scene-bg-color|<pattern|metal-brushed-dark.png|*3/5|*3/5|#8080a0>>

  <assign|metal-brushed-dark-scene-color|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>

  <assign|metal-brushed-dark-scene-math-color|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>

  <assign|metal-brushed-dark-scene-strong-color|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|metal-brushed-ornament-render-title|<value|with-metal-brushed-dark-scene>>

  <assign|metal-brushed-ornament-render-body|<value|with-metal-brushed-light-scene>>

  <assign|metal-brushed-ornament-sunny-color|#e0e0e8>

  <assign|metal-brushed-ornament-shadow-color|#9090ac>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>