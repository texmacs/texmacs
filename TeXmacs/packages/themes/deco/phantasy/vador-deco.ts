<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|vador-deco|1.0|vador-deco|1.0>

    <\src-purpose>
      Decorations for dark vador theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|dark-deco|vador-scene>

  <\active*>
    <\src-comment>
      Background dark vador
    </src-comment>
  </active*>

  <copy-theme|plain-vador|dark-deco>

  <new-deco|plain-vador>

  <assign|plain-vador-ornament-render-title|<value|with-vador-background>>

  <assign|plain-vador-ornament-render-body|<value|with-vador-background>>

  <\active*>
    <\src-comment>
      Title dark vador
    </src-comment>
  </active*>

  <copy-theme|title-vador|dark-deco>

  <new-deco|title-vador>

  <assign|title-vador-ornament-shape|angular>

  <assign|title-vador-ornament-border|3ln>

  <assign|title-vador-ornament-title-style|top center>

  <assign|title-vador-ornament-render-title|<value|with-vador-title>>

  <assign|title-vador-ornament-render-body|<value|with-vador-title>>

  <assign|title-vador-ornament-sunny-color|#301818>

  <assign|title-vador-ornament-shadow-color|#201010>

  <\active*>
    <\src-comment>
      Framed dark vador
    </src-comment>
  </active*>

  <copy-theme|framed-vador|light-deco>

  <new-deco|framed-vador>

  <assign|framed-vador-ornament-shape|angular>

  <assign|framed-vador-ornament-title-style|top center>

  <assign|framed-vador-ornament-render-title|<value|with-vador-heading>>

  <assign|framed-vador-ornament-render-body|<value|with-vador-bright>>

  <assign|framed-vador-ornament-sunny-color|#804040>

  <assign|framed-vador-ornament-shadow-color|#402020>

  <\active*>
    <\src-comment>
      Alternate dark vador
    </src-comment>
  </active*>

  <copy-theme|alternate-vador|light-deco>

  <new-deco|alternate-vador>

  <assign|alternate-vador-ornament-shape|angular>

  <assign|alternate-vador-ornament-title-style|top left>

  <assign|alternate-vador-ornament-render-title|<value|with-vador-alt-heading>>

  <assign|alternate-vador-ornament-render-body|<value|with-vador-alt-bright>>

  <assign|alternate-vador-ornament-sunny-color|#606030>

  <assign|alternate-vador-ornament-shadow-color|#303018>

  <\active*>
    <\src-comment>
      Input fields
    </src-comment>
  </active*>

  <copy-theme|input-vador|dark-deco>

  <new-deco|input-vador>

  <assign|input-vador-ornament-render-title|<value|with-vador-input>>

  <assign|input-vador-ornament-render-body|<value|with-vador-input>>

  <assign|input-vador-ornament-sunny-color|#301818>

  <assign|input-vador-ornament-shadow-color|#201010>

  <\active*>
    <\src-comment>
      Title bar and fold bar for folding/unfolding
    </src-comment>
  </active*>

  <copy-theme|fold-vador|title-vador>

  <new-deco|fold-vador>

  <assign|fold-vador-ornament-shape|classic>

  <assign|fold-vador-ornament-border|1ln>

  <assign|fold-vador-ornament-title-style|top left>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>