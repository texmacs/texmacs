<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-vador-deco|1.0|dark-vador-deco|1.0>

    <\src-purpose>
      Dark vador decorations for presentations and posters.
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

  <\active*>
    <\src-comment>
      Dark vador background
    </src-comment>
  </active*>

  <copy-theme|vador-background|dark-scene>

  <assign|vador-background-bg-color|none>

  <assign|vador-background-color|#e0c0c0>

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

  <assign|vador-heading-color|#e0e0e0>

  <assign|vador-heading-math-color|#e0e0e0>

  <assign|vador-heading-strong-color|#e0e0e0>

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

  <assign|vador-alt-heading-color|#e0d0c0>

  <assign|vador-alt-heading-math-color|#e0d0c0>

  <assign|vador-alt-heading-strong-color|#e0d0c0>

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
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>