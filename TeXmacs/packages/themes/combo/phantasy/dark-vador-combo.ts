<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-vador-combo|1.0|dark-vador-combo|1.0>

    <\src-purpose>
      Dark vador theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-combo|vador-deco>

  <copy-theme|dark-vador|dark>

  <select-theme|dark-vador|plain-vador>

  <\active*>
    <\src-comment>
      Main colors
    </src-comment>
  </active*>

  <assign|dark-vador-math-color|pastel red>

  <assign|dark-vador-strong-color|#fff0c0>

  <\active*>
    <\src-comment>
      Title bars
    </src-comment>
  </active*>

  <assign|dark-vador-title-bar-color|<macro|#603030>>

  <assign|dark-vador-title-color|<macro|#fff0c0>>

  <\active*>
    <\src-comment>
      Standard ornaments
    </src-comment>
  </active*>

  <assign|dark-vador-ornament-shape|angular>

  <assign|dark-vador-ornament-extra-color|#603030>

  <assign|dark-vador-ornament-title-color|white>

  <assign|dark-vador-ornament-color|black>

  <assign|dark-vador-ornament-sunny-color|#804040>

  <assign|dark-vador-ornament-shadow-color|#402020>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <assign|dark-vador-input-color|#303030>

  <assign|dark-vador-fold-bar-color|#603030>

  <assign|dark-vador-fold-title-color|#603030>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|dark-vador-title-block|<value|title-vador-block>>

  <assign|dark-vador-framed-block|<value|framed-vador-block>>

  <assign|dark-vador-framed-block*|<value|framed-vador-titled-block>>

  <assign|dark-vador-alternate-block|<value|alternate-vador-block>>

  <assign|dark-vador-alternate-block*|<value|alternate-vador-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>