<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|blackboard-scene|1.0|blackboard-scene|1.0>

    <\src-purpose>
      Coloring schemes on blackboard backgrounds
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-scene>

  <\active*>
    <\src-comment>
      Blackboard
    </src-comment>
  </active*>

  <copy-theme|blackboard-scene|dark-scene>

  <assign|blackboard-scene-bg-color|<pattern|tmfs://artwork/plain/landscape/blackboard/blackboard-pxhere.jpg|100%|100%>>

  <assign|blackboard-scene-math-color|#ffffd0>

  <assign|blackboard-scene-strong-color|#ffc080>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>