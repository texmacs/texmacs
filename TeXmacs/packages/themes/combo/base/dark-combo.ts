<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-combo|1.0|dark-combo|1.0>

    <\src-purpose>
      Dark theme.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|base-combo>

  <\active*>
    <\src-comment>
      Major colors
    </src-comment>
  </active*>

  <copy-theme|dark-scene|colors>

  <assign|dark-scene-bg-color|black>

  <assign|dark-scene-monochrome-bg-color|black>

  <assign|dark-scene-color|white>

  <assign|dark-scene-math-color|white>

  <assign|dark-scene-strong-color|white>

  <\active*>
    <\src-comment>
      Ornament colors
    </src-comment>
  </active*>

  <copy-theme|dark-deco|deco>

  <assign|dark-deco-ornament-body-color|white>

  <assign|dark-deco-ornament-body-math-color|white>

  <assign|dark-deco-ornament-body-strong-color|white>

  <assign|dark-deco-ornament-title-color|white>

  <assign|dark-deco-ornament-title-math-color|white>

  <assign|dark-deco-ornament-title-strong-color|white>

  <\active*>
    <\src-comment>
      Combined theme
    </src-comment>
  </active*>

  <copy-theme|dark|base>

  <select-theme|dark|dark-scene>

  <select-theme|dark|dark-deco>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>