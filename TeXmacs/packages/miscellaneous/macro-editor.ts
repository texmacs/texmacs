<TeXmacs|1.99.15>

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

  <assign|edit-tag|<\macro|tag-name|tag-body>
    <\with|par-first|0fn|par-par-sep|0.5em>
      <src-var|<arg|tag-name>> <math|\<assign\>>

      <arg|tag-body>
    </with>
  </macro>>

  <assign|edit-macro|<xmacro|macro-args|<extern|ext-edit-macro|<quote-arg|macro-args>>>>

  <assign|edit-math|<macro|body|<math|<arg|body>>>>

  <drd-props|edit-tag|arity|2|accessible|all|border|no>

  <drd-props|edit-macro|arity|<tuple|repeat*|1|2>|accessible|all|border|no>

  <drd-props|edit-math|arity|1|accessible|all|border|no>

  <\active*>
    <\src-comment>
      Special tags for page numbering editing.
    </src-comment>
  </active*>

  <assign|unchanged|<macro|<with|color|dark
  grey|font-shape|italic|unchanged>>>

  <assign|page-number|<macro|<with|color|dark
  grey|font-shape|italic|page-number>>>

  <assign|page-the-page|<macro|<with|color|dark
  grey|font-shape|italic|page-number>>>

  <\active*>
    <\src-comment>
      Special tags for search and replace patterns.
    </src-comment>
  </active*>

  <assign|wildcard|<macro|var|<with|color|dark magenta|<colored-frame|pastel
  magenta|<with|font-shape|italic|<arg|var>>>>>>

  <assign|select-region|<macro|body|<colored-frame|pastel
  yellow|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>