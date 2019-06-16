<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-manila-deco|1.0|paper-manila-deco|1.0>

    <\src-purpose>
      Manilla paper ornamenrt for presentations and posters.
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

  <copy-theme|paper-manila|light-deco>

  <new-deco|paper-manila>

  <\active*>
    <\src-comment>
      Light manila paper
    </src-comment>
  </active*>

  <copy-theme|paper-manila-light-scene|light-scene>

  <assign|paper-manila-light-scene-bg-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#d0d0c0>>

  <assign|paper-manila-light-scene-color|black>

  <assign|paper-manila-light-scene-math-color|dark red>

  <assign|paper-manila-light-scene-strong-color|#504000>

  <\active*>
    <\src-comment>
      Dark manila paper through wood
    </src-comment>
  </active*>

  <copy-theme|paper-manila-dark-scene|dark-scene>

  <assign|paper-manila-dark-scene-bg-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|paper-manila-dark-scene-color|<pattern|paper-manila-bright.png|*3/5|*3/5|#f4f4f0>>

  <assign|paper-manila-dark-scene-math-color|<pattern|paper-manila-bright.png|*3/5|*3/5|#f4f4f0>>

  <assign|paper-manila-dark-scene-strong-color|<pattern|paper-manila-bright.png|*3/5|*3/5|#f4f4f0>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|paper-manila-ornament-render-title|<value|with-paper-manila-dark-scene>>

  <assign|paper-manila-ornament-render-body|<value|with-paper-manila-light-scene>>

  <assign|paper-manila-ornament-sunny-color|#e8e8e0>

  <assign|paper-manila-ornament-shadow-color|#acac90>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>