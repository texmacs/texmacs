<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|transparent-scene|1.0|transparent-scene|1.0>

    <\src-purpose>
      Transparent coloring schemes.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

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
      Light transparent
    </src-comment>
  </active*>

  <copy-theme|transparent-light-scene|light-scene>

  <assign|transparent-light-scene-bg-color|none>

  <assign|transparent-light-scene-monochrome-bg-color|none>

  <\active*>
    <\src-comment>
      Dark transparent
    </src-comment>
  </active*>

  <copy-theme|transparent-dark-scene|dark-scene>

  <assign|transparent-dark-scene-bg-color|none>

  <assign|transparent-dark-scene-monochrome-bg-color|none>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>