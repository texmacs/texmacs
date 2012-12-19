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

  <use-package|title-render>

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

  <assign|doc-data-main|<\xmacro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|doc-title>>

      <unquote*|<select|<quote-arg|data>|doc-subtitle>>

      <unquote*|<select|<quote-arg|data>|doc-author|author-data>>

      <unquote*|<select|<quote-arg|data>|doc-date>>

      <unquote*|<select|<quote-arg|data>|doc-inactive>>
    </quasi>
  </xmacro>>

  <assign|doc-data-main*|<\xmacro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|doc-title>>

      <unquote*|<select|<quote-arg|data>|doc-subtitle>>

      <doc-authors-data|<unquote*|<select|<quote-arg|data>|doc-author|author-data>>>

      <unquote*|<select|<quote-arg|data>|doc-date>>

      <unquote*|<select|<quote-arg|data>|doc-inactive>>
    </quasi>
  </xmacro>>

  <assign|doc-note|<macro|body|>><active*|<src-short-comment|Added as a
  temporary fix for problem with doc-note>>

  <assign|doc-data-hidden|<xmacro|data|<quasi|<style-with|src-compact|none|<style-with|src-compact|none|<doc-note|<unquote*|<select|<quote-arg|data>|doc-note>>>><doc-data-bis|<unquote*|<quote-arg|data>>><doc-authors-data-bis|<unquote*|<select|<quote-arg|data>|doc-author|author-data>>><style-with|src-compact|none|<doc-running-title|<unquote*|<select|<quote-arg|data>|doc-title|0>>>><style-with|src-compact|none|<doc-running-title|<unquote*|<select|<quote-arg|data>|doc-running-title|0>>>><doc-running-author|<style-with|src-compact|none|<author-from-authors|<unquote*|<select|<quote-arg|data>|doc-author|author-data|author-name|0>>>>><style-with|src-compact|none|<doc-running-author|<unquote*|<select|<quote-arg|data>|doc-running-author|0>>>>>>>>

  <assign|doc-data-abstract|<\xmacro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|doc-keywords>>

      <unquote*|<select|<quote-arg|data>|doc-msc>>
    </quasi>
  </xmacro>>

  <assign|doc-data-note|<xmacro|data|<\quasi>
    <unquote*|<select|<quote-arg|data>|doc-note|document|<pat-any>>>
  </quasi>>>

  <assign|doc-data|<\xmacro|data>
    <style-with|src-compact|none|<\surround|<assign|the-doc-data|<quote-arg|data>>|<with|doc-note-nr|0|<quasi|<doc-data-hidden|<unquote*|<quote-arg|data>>>>>>
      <\doc-make-title>
        <with|doc-note-nr|0|<\quasi>
          <style-with|src-compact|none|<compound|<unquote|<style-with|src-compact|none|<if|<lesseq|<length|<select|<quote-arg|data>|doc-author|author-data>>|1>|<value|doc-data-main>|<value|doc-data-main*>>>>|<unquote*|<quote-arg|data>>>>
        </quasi>>
      </doc-make-title>
    </surround>>
  </xmacro>>

  <assign|doc-data-bis|<xmacro|body|<quasi|<style-with|src-compact|none|<doc-title-note|<unquote|<quasi|<doc-data-note|<unquote*|<quote-arg|body>>>>>>>>>>

  <\active*>
    <\src-comment>
      Documents with one author.
    </src-comment>
  </active*>

  <assign|doc-author-main|<\macro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|author-name>>

      <unquote*|<select|<quote-arg|data>|author-affiliation>>

      <unquote*|<select|<quote-arg|data>|author-email>>

      <unquote*|<select|<quote-arg|data>|author-homepage>>
    </quasi>
  </macro>>

  <assign|doc-author-data-note|<xmacro|data|<\quasi>
    <unquote*|<select|<quote-arg|data>|author-misc|document|<pat-any>>>
  </quasi>>>

  <assign|author-data|<\xmacro|data>
    <\quasi>
      <\with|the-author-data|<quote-arg|data>>
        <\render-doc-author>
          <doc-author-block|<doc-author-main|<unquote|<quote-arg|data>>>>
        </render-doc-author>
      </with>
    </quasi>
  </xmacro>>

  <assign|doc-author-data-bis|<macro|body|<quasi|<style-with|src-compact|none|<doc-author-note|<unquote|<quasi|<doc-author-data-note|<unquote*|<quote-arg|body>>>>>>>>>>

  <\active*>
    <\src-comment>
      Abstracts.
    </src-comment>
  </active*>

  <assign|abstract|<\macro|body>
    <style-with|src-compact|none|<with|abstract-note|<look-up|<quasi|<doc-data-abstract|<unquote*|<quote-value|the-doc-data>>>>|0>|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<equal|<get-arity|<quote-value|abstract-note>>|0>|render-abstract|render-abstract*>>|<arg|body>|<quote-value|abstract-note>>>>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|zoom-factor|1.35021>
  </collection>
</initial>