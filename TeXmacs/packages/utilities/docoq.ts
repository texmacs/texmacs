<TeXmacs|1.0.7.20>

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

  <\active*>
    <\src-comment>
      Tactic language.
    </src-comment>
  </active*>

  <assign|coq-operator|<macro|name|begin|end|<arg|name>>>

  <assign|coq-constant|<macro|body|begin|end|<arg|body>>>

  <assign|coq-render-values|<macro|x|<arg|x>, >>

  <assign|coq-render-type|<macro|x| : <arg|x>>>

  <assign|coq-typed|<\xmacro|args>
    <with|arity|<get-arity|<unquote|<quote-arg|args>>>|<if|<lesseq|<value|arity>|2>|<arg|args|0><coq-render-type|<arg|args|1>>|<map-args|coq-render-values|concat|args|0|<minus|<value|arity>|3>><arg|args|<minus|<value|arity>|2>><coq-render-type|<arg|args|<minus|<value|arity>|1>>>>>
  </xmacro>>

  <assign|coq-apply|<xmacro|args|<with|begin|<arg|args|0>|end|<arg|args|1>|operator|<arg|args|2>|arity|<get-arity|<unquote|<quote-arg|args>>>|<with|color|dark
  cyan|<value|operator> ><around*|(|<map-args|coq-render-values|concat|args|3|<minus|<value|arity>|1>><arg|args|<minus|<value|arity>|1>>|)>>>>

  <\active*>
    <\src-comment>
      Misc.
    </src-comment>
  </active*>

  <assign|coq-token|<macro|begin|end|body|<arg|body>>>
</body>

<initial|<\collection>
</collection>>