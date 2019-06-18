<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|wood-deco|1.0|wood-deco|1.0>

    <\src-purpose>
      Wooden decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|dark-deco|wood-scene>

  <\active*>
    <\src-comment>
      Pine wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine|light-deco>

  <new-deco|wood-pine>

  <assign|wood-pine-ornament-render-title|<value|with-wood-pine-dark-scene>>

  <assign|wood-pine-ornament-render-body|<value|with-wood-pine-light-scene>>

  <assign|wood-pine-ornament-sunny-color|#ffe8c0>

  <assign|wood-pine-ornament-shadow-color|brown>

  <\active*>
    <\src-comment>
      Dark "pine" wood
    </src-comment>
  </active*>

  <copy-theme|wood-pine-dark|dark-deco>

  <new-deco|wood-pine-dark>

  <assign|wood-pine-dark-ornament-render-title|<value|with-wood-pine-dark-scene>>

  <assign|wood-pine-dark-ornament-render-body|<value|with-wood-pine-dark-scene>>

  <assign|wood-pine-dark-ornament-sunny-color|brown>

  <assign|wood-pine-dark-ornament-shadow-color|dark brown>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>