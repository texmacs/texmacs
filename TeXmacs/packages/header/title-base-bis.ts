<TeXmacs|1.0.7.17>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|title-base|1.0|header-title|1.0>

    <\src-purpose>
      Common macros for title information
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|title-render-bis>

  <\active*>
    <\src-comment>
      DRD properties of tags with title and author data.

      FIXME: running title and author should be made ``hidden''.
    </src-comment>
  </active*>

  <drd-props|doc-data|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|doc-title|border|no>

  <drd-props|doc-subtitle|border|no>

  <drd-props|doc-author|arity|<tuple|repeat|1|1>|border|no|accessible|all>

  <drd-props|author-data|arity|<tuple|repeat|1|1>|border|no|accessible|all>

  <drd-props|doc-date|border|no>

  <drd-props|doc-note|arity|1|border|no|accessible|all>

  <drd-props|doc-inactive|arity|1|border|no|accessible|all>

  <drd-props|doc-running-title|arity|1|accessible|all>

  <drd-props|doc-running-author|arity|1|accessible|all>

  <drd-props|doc-keywords|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|doc-msc|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|author-name|border|no>

  <drd-props|author-affiliation|border|no>

  <drd-props|author-email|border|no>

  <drd-props|author-homepage|border|no>

  <drd-props|author-misc|arity|1|border|no|accessible|all>

  <drd-props|abstract|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Document titles.
    </src-comment>
  </active*>

  <assign|doc-note|<macro|body|>><active*|<src-short-comment|Added as a
  temporary fix for problem with doc-note>>

  <assign|doc-data|<xmacro|args|<extern|doc-data|<quote-arg|args>>>>

  <\active*>
    <\src-comment>
      Documents authors.
    </src-comment>
  </active*>

  <assign|author-data|<xmacro|args|<extern|author-data|<quote-arg|args>>>>

  <assign|authors-data|<macro|data|<extern|authors-data|<quote-arg|data>>>>

  <assign|author-name|<macro|author|<author-render-name|<style-with|src-compact|none|<author-by|<arg|author>><style-with|src-compact|none|<doc-footnote-ref|<quasi|<doc-author-data-note|<unquote*|<quote-value|the-author-data>>>>>>>>>>

  <\active*>
    <\src-comment>
      Abstracts.
    </src-comment>
  </active*>

  <assign|doc-data-abstract|<xmacro|args|<extern|doc-data-abstract|<quote-arg|args>>>>

  <assign|render-abstract*|<\macro|body|note>
    <\quasi>
      <\render-abstract>
        <surround||<vspace|0.5fn>|<unquote|<quote-arg|body>>>

        <\with|par-par-sep|0.25fn>
          <unquote*|<arg|note>>
        </with>
      </render-abstract>
    </quasi>
  </macro>>

  <assign|abstract|<\macro|body>
    <style-with|src-compact|none|<with|abstract-note|<look-up|<quasi|<doc-data-abstract|<unquote*|<quote-value|the-doc-data>>>>|0>|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<equal|<get-arity|<quote-value|abstract-note>>|0>|render-abstract|render-abstract*>>|<arg|body>|<quote-value|abstract-note>>>>>
  </macro>>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|zoom-factor|1.35021>
  </collection>
</initial>