<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|macro-editor|1.0>

      <\src-purpose>
        An internal style for editing macros.
      </src-purpose>

      <\src-copyright|2013>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|generic>

  <use-module|(source macro-edit)>

  <\active*>
    <\src-comment>
      Special tags for macro editing.
    </src-comment>
  </active*>

  <assign|edit-macro|<xmacro|args|<extern|ext-edit-macro|<quote-arg|args>>>>

  <assign|edit-inactive|<macro|x|<quasi|<inactive*|<quote-arg|x>>>>>

  <drd-props|edit-macro|arity|<tuple|repeat*|1|2>|accessible|all|border|no>

  <drd-props|edit-inactive|arity|1|accessible|all|border|no>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>