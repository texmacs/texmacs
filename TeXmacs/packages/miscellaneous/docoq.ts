<TeXmacs|1.99.1>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|DoCoq|1.0>

    <\src-purpose>
      This experimental package contains macros for rendering imported CoqML
      stuff.
    </src-purpose>

    <src-copyright|2013|François Poulain, Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Vernacular commands.
    </src-comment>
  </active*>

  <assign|coq-gallina|<macro|begin|end|body|<arg|body>>>

  <assign|coq-theorem|<macro|type|name|begin|end|body|<\surround|<with|font-series|bold|<with|color|red|<arg|type>>>:
  <with|color|blue|<arg|name>> |<with|font-series|bold|.>>
    <arg|body>
  </surround>>>

  <assign|coq-definition|<macro|type|name|begin|end|body|<\surround|<with|font-series|bold|color|red|<arg|type>:>
  <with|color|blue|<arg|name>> |<with|font-series|bold|.>>
    <arg|body>
  </surround>>>

  <assign|coq-check|<macro|begin|end|body|<\surround|<with|font-series|bold|<with|color|dark
  magenta|Check>:> |<with|font-series|bold|.>>
    <arg|body>
  </surround>>>

  <assign|coq-notation|<macro|name|body|begin|end|<\surround|<with|font-series|bold|<with|color|dark
  magenta|Notation> <arg|name>:> |<with|font-series|bold|.>>
    <arg|body>
  </surround>>>

  <assign|coq-require|<macro|begin|end|body|<\surround|<with|font-series|bold|<with|color|dark
  magenta|Require>:> |<with|font-series|bold|.>>
    <arg|body>
  </surround>>>

  <\active*>
    <\src-comment>
      Terms language.
    </src-comment>
  </active*>

  <assign|coq-operator|<macro|name|begin|end|<arg|name>>>

  <assign|coq-constant|<macro|body|begin|end|<arg|body>>>

  <assign|coq-render-values|<macro|x|<arg|x>, >>

  <assign|coq-render-type|<macro|x| : <arg|x>>>

  <drd-props|coq-untyped|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|coq-typed|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|coq-apply|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|coq-intersperse-recurse-args|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|coq-rec-notation-sep|<macro|; >>

  <assign|coq-intersperse-recurse-args|<xmacro|x|<concat-tuple|<quote-arg|x>|<coq-rec-notation-sep>>>>

  <assign|coq-untyped|<\xmacro|args>
    <concat-tuple|<quote-arg|args>|, >
  </xmacro>>

  <assign|coq-typed|<\xmacro|args>
    <with|arity|<get-arity|<unquote|<quote-arg|args>>>|<if|<lesseq|<value|arity>|2>|<arg|args|0><coq-render-type|<arg|args|1>>|<map-args|coq-render-values|concat|args|0|<minus|<value|arity>|2>><arg|args|<minus|<value|arity>|2>><coq-render-type|<arg|args|<minus|<value|arity>|1>>>>>
  </xmacro>>

  <assign|coq-apply|<xmacro|args|<with|begin|<arg|args|0>|end|<arg|args|1>|operator|<arg|args|2>|arity|<get-arity|<unquote|<quote-arg|args>>>|<if|<equal|<get-label|<value|operator>>|macro>|<map-args|identity|operator|args|3>|<if|<provides|<value|operator>>|<with|op|<value|<eval|<quote-arg|args|2>>>|<map-args|identity|op|args|3>>|<with|color|dark
  cyan|<value|operator> ><around*|(|<map-args|coq-render-values|concat|args|3|<minus|<value|arity>|1>><arg|args|<minus|<value|arity>|1>>|)>>>>>>

  <\active*>
    <\src-comment>
      Misc.
    </src-comment>
  </active*>

  <assign|coq-reference|<macro|body|<arg|body>>>

  <assign|coq-token|<macro|begin|end|body|<arg|body>>>

  <assign|coq-ltac|<macro|body|begin|end|<with|font-family|ss|<arg|body>>>>

  <\active*>
    <\src-comment>
      Hard coded operators
    </src-comment>
  </active*>

  <assign|forall|<macro|1|2|\<forall\><arg|1>, <arg|2>>>

  <assign|:|<macro|1|2|<arg|1><coq-render-type|<arg|2>>>>
</body>

<initial|<\collection>
</collection>>