<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|vador-scene|1.0|vador-scene|1.0>

    <\src-purpose>
      Coloring schemes for dark vador theme
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
      Dark vador background
    </src-comment>
  </active*>

  <copy-theme|vador-background|dark-scene>

  <assign|vador-background-color|#e0e0e0>

  <assign|vador-background-math-color|pastel red>

  <assign|vador-background-strong-color|#fff0c0>

  <\active*>
    <\src-comment>
      Dark vador title
    </src-comment>
  </active*>

  <copy-theme|vador-title|dark-scene>

  <assign|vador-title-bg-color|#603030>

  <assign|vador-title-color|#fff0c0>

  <assign|vador-title-math-color|pastel red>

  <assign|vador-title-strong-color|#fff0c0>

  <\active*>
    <\src-comment>
      Headings for text on a bright background
    </src-comment>
  </active*>

  <copy-theme|vador-heading|dark-scene>

  <assign|vador-heading-bg-color|dark red>

  <assign-uniform|vador-heading|#e0e0e0>

  <\active*>
    <\src-comment>
      Text on a bright background
    </src-comment>
  </active*>

  <copy-theme|vador-bright|light-scene>

  <assign|vador-bright-bg-color|#e0e0e0>

  <assign|vador-bright-color|#200000>

  <assign|vador-bright-math-color|dark red>

  <assign|vador-bright-strong-color|dark orange>

  <\active*>
    <\src-comment>
      Alternative headings for text on a bright background
    </src-comment>
  </active*>

  <copy-theme|vador-alt-heading|dark-scene>

  <assign|vador-alt-heading-bg-color|#602000>

  <assign-uniform|vador-alt-heading|#e0d0c0>

  <\active*>
    <\src-comment>
      Alternative text on a bright background
    </src-comment>
  </active*>

  <copy-theme|vador-alt-bright|light-scene>

  <assign|vador-alt-bright-bg-color|#e0e0c0>

  <assign|vador-alt-bright-color|#400000>

  <assign|vador-alt-bright-math-color|dark red>

  <assign|vador-alt-bright-strong-color|dark orange>

  <\active*>
    <\src-comment>
      Input fields for sessions
    </src-comment>
  </active*>

  <copy-theme|vador-input|vador-background>

  <assign|vador-input-bg-color|#303030>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>