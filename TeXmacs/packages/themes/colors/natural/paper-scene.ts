<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-scene|1.0|paper-scene|1.0>

    <\src-purpose>
      Coloring schemes on paper backgrounds
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
      Light paper
    </src-comment>
  </active*>

  <copy-theme|paper-manila-light-scene|light-scene>

  <assign|paper-manila-light-scene-bg-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#d0d0c0>>

  <assign|paper-manila-light-scene-strong-color|#504000>

  <copy-theme|paper-ridged-light-scene|paper-manila-light-scene>

  <assign|paper-ridged-light-scene-bg-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcdc>>

  <copy-theme|paper-rough-light-scene|paper-manila-light-scene>

  <assign|paper-rough-light-scene-bg-color|<pattern|paper-rough-medium.png|*3/5|*3/5|#dcdcdc>>

  <\active*>
    <\src-comment>
      Dark paper through wood
    </src-comment>
  </active*>

  <copy-theme|paper-manila-dark-scene|dark-scene>

  <assign|paper-manila-dark-scene-bg-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|paper-manila-dark-scene-color|<pattern|paper-manila-bright.png|*3/5|*3/5|#f4f4f0>>

  <assign|paper-manila-dark-scene-math-color|<pattern|paper-manila-bright.png|*3/5|*3/5|#f4f4f0>>

  <assign|paper-manila-dark-scene-strong-color|<pattern|paper-manila-bright.png|*3/5|*3/5|#f4f4f0>>

  <copy-theme|paper-ridged-dark-scene|paper-manila-dark-scene>

  <assign|paper-ridged-dark-scene-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <assign|paper-ridged-dark-scene-math-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <assign|paper-ridged-dark-scene-strong-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <copy-theme|paper-rough-dark-scene|paper-manila-dark-scene>

  <assign|paper-rough-dark-scene-color|<pattern|paper-rough-bright.png|*3/5|*3/5|#f7f7f7>>

  <assign|paper-rough-dark-scene-math-color|<pattern|paper-rough-bright.png|*3/5|*3/5|#f7f7f7>>

  <assign|paper-rough-dark-scene-strong-color|<pattern|paper-rough-bright.png|*3/5|*3/5|#f7f7f7>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>