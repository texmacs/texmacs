<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-rough-deco|1.0|paper-rough-deco|1.0>

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

  <copy-theme|paper-rough|light-deco>

  <new-deco|paper-rough>

  <\active*>
    <\src-comment>
      Light ridged paper
    </src-comment>
  </active*>

  <copy-theme|paper-rough-light-scene|light-scene>

  <assign|paper-rough-light-scene-bg-color|<pattern|paper-rough-medium.png|*3/5|*3/5|#dcdcdc>>

  <assign|paper-rough-light-scene-color|black>

  <assign|paper-rough-light-scene-math-color|dark red>

  <assign|paper-rough-light-scene-strong-color|#504000>

  <\active*>
    <\src-comment>
      Dark rough paper through wood
    </src-comment>
  </active*>

  <copy-theme|paper-rough-dark-scene|dark-scene>

  <assign|paper-rough-dark-scene-bg-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|paper-rough-dark-scene-color|<pattern|paper-rough-bright.png|*3/5|*3/5|#f7f7f7>>

  <assign|paper-rough-dark-scene-math-color|<pattern|paper-rough-bright.png|*3/5|*3/5|#f7f7f7>>

  <assign|paper-rough-dark-scene-strong-color|<pattern|paper-rough-bright.png|*3/5|*3/5|#f7f7f7>>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|paper-rough-ornament-render-title|<value|with-paper-rough-dark-scene>>

  <assign|paper-rough-ornament-render-body|<value|with-paper-rough-light-scene>>

  <assign|paper-rough-ornament-sunny-color|#e0e0e0>

  <assign|paper-rough-ornament-shadow-color|#a0a0a0>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>