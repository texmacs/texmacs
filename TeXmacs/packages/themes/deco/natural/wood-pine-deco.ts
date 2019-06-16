<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|wood-pine-deco|1.0|wood-pine-deco|1.0>

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

  <copy-theme|wood-pine|light-deco>

  <new-deco|wood-pine>

  <\active*>
    <\src-comment>
      Light pine wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine-light-scene|light-scene>

  <assign|wood-pine-light-scene-bg-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|wood-pine-light-scene-color|black>

  <assign|wood-pine-light-scene-math-color|#500000>

  <assign|wood-pine-light-scene-strong-color|#0c3000>

  <\active*>
    <\src-comment>
      Dark wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine-dark-scene|dark-scene>

  <assign|wood-pine-dark-scene-bg-color|<pattern|wood-dark.png|*3/5|*3/5|#40200c>>

  <assign|wood-pine-dark-scene-color|#f8f8f4>

  <assign|wood-pine-dark-scene-math-color|#f8f8f4>

  <assign|wood-pine-dark-scene-strong-color|#f8f8f4>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|wood-pine-ornament-render-title|<value|with-wood-pine-dark-scene>>

  <assign|wood-pine-ornament-render-body|<value|with-wood-pine-light-scene>>

  <assign|wood-pine-ornament-sunny-color|#ffe8c0>

  <assign|wood-pine-ornament-shadow-color|brown>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>