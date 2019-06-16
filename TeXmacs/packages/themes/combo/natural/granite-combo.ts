<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|granite-combo|1.0|granite-combo|1.0>

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

  <use-package|dark-combo|granite-deco>

  <copy-theme|granite|dark>

  <select-theme|granite|stone-granite>

  <select-theme|granite|stone-granite-xdark-scene>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|granite-title-bar-color|<macro|<pattern|granite-medium.png|*3/5|*3/5|#202020>>>

  <assign|granite-title-color|<macro|white>>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <assign|granite-input-color|<pattern|granite-medium.png|*3/5|*3/5|#202020>>

  <assign|granite-fold-bar-color|<pattern|granite-medium.png|*3/5|*3/5|#202020>>

  <assign|granite-fold-title-color|<pattern|granite-medium.png|*3/5|*3/5|#202020>>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|granite-title-block|<value|stone-granite-block>>

  <assign|granite-framed-block|<value|stone-granite-block>>

  <assign|granite-framed-block*|<value|stone-granite-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>