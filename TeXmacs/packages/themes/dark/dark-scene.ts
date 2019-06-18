<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-scene|1.0|dark-scene|1.0>

    <\src-purpose>
      Common base for coloring schemes.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|base-colors>

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

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>