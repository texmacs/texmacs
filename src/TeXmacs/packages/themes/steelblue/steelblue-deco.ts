<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|steelblue-deco|1.0|steelblue-deco|1.0>

    <\src-purpose>
      Steelblue decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2023|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|dark-deco|steelblue-scene>

  <\active*>
    <\src-comment>
      Steel blue
    </src-comment>
  </active*>

  <copy-theme|steelblue|light-deco>

  <new-deco|steelblue>

  <assign|steelblue-ornament-render-title|<value|with-steelblue-dark-scene>>

  <assign|steelblue-ornament-render-body|<value|with-steelblue-light-scene>>

  <assign|steelblue-ornament-sunny-color|#e0e0ec>

  <assign|steelblue-ornament-shadow-color|#a0a0c4>

  <\active*>
    <\src-comment>
      Light steel blue
    </src-comment>
  </active*>

  <copy-theme|steelblue-light|steelblue>

  <new-deco|steelblue-light>

  <assign|steelblue-light-ornament-render-body|<value|with-steelblue-lighter-scene>>

  <assign|steelblue-light-ornament-shadow-color|#f0f0f6>

  <\active*>
    <\src-comment>
      Dark steel blue
    </src-comment>
  </active*>

  <copy-theme|steelblue-dark|dark-deco>

  <new-deco|steelblue-dark>

  <assign|steelblue-dark-ornament-render-title|<value|with-steelblue-dark-scene>>

  <assign|steelblue-dark-ornament-render-body|<value|with-steelblue-dark-scene>>

  <assign|steelblue-dark-ornament-sunny-color|#a0a0c4>

  <assign|steelblue-dark-ornament-shadow-color|#404070>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>