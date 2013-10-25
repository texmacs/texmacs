<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|macro-editor|1.0>

    <\src-purpose>
      An internal style package for editing macros.
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(source macro-edit)>

  <\active*>
    <\src-comment>
      Special tags for macro editing.
    </src-comment>
  </active*>

  <assign|edit-tag|<\macro|name|body>
    <\with|par-first|0fn|par-par-sep|0.5em>
      <src-var|<arg|name>> <math|\<assign\>>

      <arg|body>
    </with>
  </macro>>

  <assign|edit-macro|<xmacro|args|<extern|ext-edit-macro|<quote-arg|args>>>>

  <drd-props|edit-tag|arity|2|accessible|all|border|no>

  <drd-props|edit-macro|arity|<tuple|repeat*|1|2>|accessible|all|border|no>

  <\active*>
    <\src-comment>
      Further tags for page numbering editing.
    </src-comment>
  </active*>

  <assign|unchanged|<macro|<with|color|dark
  grey|font-shape|italic|unchanged>>>

  <assign|page-number|<macro|<with|color|dark
  grey|font-shape|italic|page-number>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>