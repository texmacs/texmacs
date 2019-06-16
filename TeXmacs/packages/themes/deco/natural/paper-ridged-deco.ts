<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-ridged-deco|1.0|paper-ridged-deco|1.0>

    <\src-purpose>
      Ridged paper ornament for presentations and posters.
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

  <copy-theme|paper-ridged|light-deco>

  <new-deco|paper-ridged>

  <\active*>
    <\src-comment>
      Light ridged paper
    </src-comment>
  </active*>

  <copy-theme|paper-ridged-light-scene|light-scene>

  <assign|paper-ridged-light-scene-bg-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcdc>>

  <assign|paper-ridged-light-scene-color|black>

  <assign|paper-ridged-light-scene-math-color|dark red>

  <assign|paper-ridged-light-scene-strong-color|#504000>

  <\active*>
    <\src-comment>
      Dark ridged paper through wood
    </src-comment>
  </active*>

  <copy-theme|paper-ridged-dark-scene|dark-scene>

  <assign|paper-ridged-dark-scene-bg-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|paper-ridged-dark-scene-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <assign|paper-ridged-dark-scene-math-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <assign|paper-ridged-dark-scene-strong-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#c0a08c>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|paper-ridged-ornament-render-title|<value|with-paper-ridged-dark-scene>>

  <assign|paper-ridged-ornament-render-body|<value|with-paper-ridged-light-scene>>

  <assign|paper-ridged-ornament-sunny-color|#f0e0e0>

  <assign|paper-ridged-ornament-shadow-color|#d0a0a0>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>