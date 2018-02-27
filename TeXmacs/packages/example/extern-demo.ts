<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|extern-demo|1.0>

    <\src-purpose>
      An example package that demonstrates the use of extern Scheme macros.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(utils misc extern-demo)>

  \;

  <assign|hello|<macro|body|<extern|ext-hello|<quote-arg|body>>>>

  <drd-props|hello|arity|1|accessible|all>

  \;

  <assign|highlight|<macro|body|sub|<extern|ext-highlight|<quote-arg|body>|<quote-arg|sub>>>>

  <drd-props|highlight|arity|2|accessible|0>

  \;

  <assign|circulant|<xmacro|l|<extern|ext-circulant|<quote-arg|l>>>>

  <drd-props|circulant|arity|<tuple|repeat|1|1>|accessible|all>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>