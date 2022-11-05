<TeXmacs|2.1.2>

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

  <copy-theme|dark-scene|all-colors>

  <assign|dark-scene-bg-color|black>

  <assign|dark-scene-monochrome-bg-color|black>

  <assign|dark-scene-color|white>

  <assign|dark-scene-math-color|white>

  <assign|dark-scene-strong-color|white>

  <assign|dark-scene-heading-color|#ffc080>

  <assign|dark-scene-locus-color|#d0d0ff>

  <assign|dark-scene-visited-color|#f0c0e8>

  <\active*>
    <\src-comment>
      Gui colors
    </src-comment>
  </active*>

  <assign|dark-scene-cursor-color|#f44>

  <assign|dark-scene-math-cursor-color|#c4f>

  <assign|dark-scene-focus-color|#066>

  <assign|dark-scene-context-color|#4556>

  <assign|dark-scene-selection-color|#f44>

  <assign|dark-scene-table-selection-color|#c4f>

  <assign|dark-scene-match-color|#f8e080>

  <assign|dark-scene-clickable-color|#106030>

  <assign|dark-scene-correct-color|#408040>

  <assign|dark-scene-incorrect-color|#804040>

  <\active*>
    <\src-comment>
      Colors for syntax highlighting
    </src-comment>
  </active*>

  <assign|dark-scene-keyword-color|#d070f0>

  <assign|dark-scene-constant-color|#88bce0>

  <assign|dark-scene-number-color|#88bce0>

  <assign|dark-scene-string-color|#d8a080>

  <assign|dark-scene-comment-color|#d06030>

  <assign|dark-scene-preprocessor-color|#c040c0>

  <assign|dark-scene-modifier-color|#d070f0>

  <assign|dark-scene-declaration-color|#60b0ff>

  <assign|dark-scene-macro-color|#80e0c8>

  <assign|dark-scene-function-color|#d8d8d8>

  <assign|dark-scene-type-color|#98c080>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>