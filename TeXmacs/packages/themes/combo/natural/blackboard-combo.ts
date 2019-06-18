<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|blackboard-combo|1.0|blackboard-combo|1.0>

    <\src-purpose>
      Granite theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-combo|blackboard-deco>

  <copy-theme|blackboard|dark>

  <select-theme|blackboard|transparent-blackboard-deco>

  <select-theme|blackboard|blackboard-scene>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <select-title-deco*|blackboard|transparent-blackboard-deco>

  <assign|blackboard-title-border|0.5ln>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <select-input-deco|blackboard|transparent-blackboard-deco>

  <select-fold-title-deco|blackboard|transparent-blackboard-deco>

  <select-fold-bar-deco|blackboard|transparent-blackboard-deco>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|blackboard-title-block|<value|transparent-blackboard-deco-block>>

  <assign|blackboard-framed-block|<value|transparent-blackboard-deco-block>>

  <assign|blackboard-framed-block*|<value|transparent-blackboard-deco-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>