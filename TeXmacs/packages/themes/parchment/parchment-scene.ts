<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|parchment-scene|1.0|parchment-scene|1.0>

    <\src-purpose>
      Coloring schemes on parchment backgrounds
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
      Parchment
    </src-comment>
  </active*>

  <copy-theme|parchment-scene|light-scene>

  <assign|parchment-scene-bg-color|<pattern|tmfs://artwork/plain/landscape/paper/paper-antique-latte.jpg|100%|100%>>

  <\active*>
    <\src-comment>
      Transparent parchment
    </src-comment>
  </active*>

  <copy-theme|transparent-parchment-scene|parchment-scene>

  <assign|transparent-parchment-scene-bg-color|none>

  <\active*>
    <\src-comment>
      Transparent parchment title
    </src-comment>
  </active*>

  <copy-theme|transparent-parchment-title-scene|transparent-parchment-scene>

  <assign-uniform|transparent-parchment-title-scene|dark brown>

  <\active*>
    <\src-comment>
      Translucent dark parchment
    </src-comment>
  </active*>

  <copy-theme|dark-parchment-scene|dark-scene>

  <assign|dark-parchment-scene-bg-color|#0024>

  <assign|dark-parchment-scene-color|#fffc>

  <\active*>
    <\src-comment>
      Translucent extra dark parchment
    </src-comment>
  </active*>

  <copy-theme|xdark-parchment-scene|dark-scene>

  <assign|xdark-parchment-scene-bg-color|#00080060>

  <assign|xdark-parchment-scene-color|#fffc>

  <\active*>
    <\src-comment>
      Translucent light parchment
    </src-comment>
  </active*>

  <copy-theme|light-parchment-scene|parchment-scene>

  <assign|light-parchment-scene-bg-color|#fff2>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>