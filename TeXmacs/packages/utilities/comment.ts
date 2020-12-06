<TeXmacs|1.99.16>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|comment|1.0>

    <\src-purpose>
      Various types of comments by various authors on a text
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|std-shadow>

  <use-module|(various comment-menu)>

  <\active*>
    <\src-comment>
      Rendering macros
    </src-comment>
  </active*>

  <assign|comment-color|<macro|type|by|<extern|ext-comment-color|<arg|type>|<arg|by>>>>

  <assign|abbreviate-name|<macro|by|<extern|ext-abbreviate-name|<arg|by>>>>

  <assign|render-inline-comment|<macro|type|by|body|<surround|<with|color|<comment-color|<arg|type>|<arg|by>>|[<condensed|<name|<abbreviate-name|<arg|by>>>>:
  >|<with|color|<comment-color|<arg|by>>|]>|<arg|body>>>>

  <assign|render-block-comment|<macro|type|by|body|<surround|<with|color|<comment-color|<arg|type>|<arg|by>>|<condensed|<name|<abbreviate-name|<arg|by>>>>\<rangle\>
  >|<right-flush>|<arg|body>>>>

  <assign|render-box-comment|<\macro|type|by|body>
    <with|shadow-elevation|0.75|<\half-bend-in-shadow>
      <\wide-tabular>
        <tformat|<cwith|1|1|1|1|cell-background|<comment-color|<arg|type>|<arg|by>>>|<cwith|1|-1|1|1|cell-lsep|1spc>|<cwith|1|-1|1|1|cell-rsep|1spc>|<cwith|1|-1|1|1|cell-tsep|0.5spc>|<cwith|1|-1|1|1|cell-bsep|0.5spc>|<cwith|2|-1|1|1|cell-tsep|2spc>|<cwith|2|-1|1|1|cell-bsep|1spc>|<table|<row|<\cell>
          <samp|<with|color|white|locus-color|preserve|<copy|<arg|by>>>>
        </cell>>|<row|<\cell>
          <arg|body>
        </cell>>>>
      </wide-tabular>
    </half-bend-in-shadow>>
  </macro>>

  <\active*>
    <\src-comment>
      Various kinds of comments
    </src-comment>
  </active*>

  <assign|inline-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<with|locus-color|<value|old-color>|<surround|<extern|mirror-initialize|<quote-arg|body>>||<expand-as|<arg|body>|<render-inline-comment|<arg|type>|<arg|by>|<arg|body>>>>>>>>>>

  <assign|block-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<with|locus-color|<value|old-color>|<surround|<extern|mirror-initialize|<quote-arg|body>>||<expand-as|<arg|body>|<render-block-comment|<arg|type>|<arg|by>|<arg|body>>>>>>>>>>

  <assign|unfolded-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<expand-as|<arg|body>|<compound|<if|<equal|<get-label|<quote-arg|body>>|document>|block-comment|inline-comment>|<arg|unique-id>|<arg|mirror-id>|<arg|type>|<arg|by>|<arg|time>|<arg|src>|<arg|body>>>>>

  <assign|nested-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<expand-as|<arg|body>|<compound|<if|<equal|<get-label|<quote-arg|body>>|document>|block-comment|inline-comment>|<arg|unique-id>|<arg|mirror-id>|<arg|type>|<arg|by>|<arg|time>|<arg|src>|<arg|body>>>>>

  <assign|folded-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<expand-as|<arg|body>|<extern|mirror-initialize|<quote-arg|body>><flag|<abbreviate-name|<arg|by>>|<comment-color|<arg|type>|<arg|by>>><hidden|<arg|body>>>>>>>>

  <assign|mirror-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<with|locus-color|<value|old-color>|<surround|<extern|mirror-initialize|<quote-arg|body>>||<render-box-comment|<arg|type>|<arg|by>|<arg|body>>>>>>>>>

  <assign|carbon-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<with|locus-color|<value|old-color>|<surround|<extern|mirror-initialize|<quote-arg|body>>||<arg|body>>>>>>>>

  <assign|preview-comment|<\macro|unique-id|mirror-id|type|by|time|src|body>
    <\with|preview-bg-color|<blend|#fffd|<comment-color|<arg|type>|<arg|by>>>>
      <\preview-balloon>
        <render-block-comment|<arg|type>|<arg|by>|<arg|body>>
      </preview-balloon>
    </with>
  </macro>>

  <drd-props|unfolded-comment|arity|7|accessible|6>

  <drd-props|nested-comment|arity|7|accessible|6>

  <drd-props|mirror-comment|arity|7|accessible|6|border|no>

  <drd-props|carbon-comment|arity|7|accessible|6|border|no>

  <\active*>
    <\src-comment>
      Completely invisible comments (except when they contain visible nested
      comments)
    </src-comment>
  </active*>

  <assign|hidden-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<hidden|<arg|body>>>>

  <assign|hidden-unfolded-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<compound|<if|<extern|ext-contains-shown-comments?|<quote-arg|body>>|<if|<equal|<get-label|<quote-arg|body>>|document>|block-comment|inline-comment>|hidden-comment>|<arg|unique-id>|<arg|mirror-id>|<arg|type>|<arg|by>|<arg|time>|<arg|src>|<arg|body>>>>

  <assign|hidden-folded-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<compound|<if|<extern|ext-contains-shown-comments?|<quote-arg|body>>|folded-comment|hidden-comment>|<arg|unique-id>|<arg|mirror-id>|<arg|type>|<arg|by>|<arg|time>|<arg|src>|<arg|body>>>>

  <assign|hidden-nested-comment|<macro|unique-id|mirror-id|type|by|time|src|body|<compound|<if|<extern|ext-contains-shown-comments?|<quote-arg|body>>|nested-comment|hidden-comment>|<arg|unique-id>|<arg|mirror-id>|<arg|type>|<arg|by>|<arg|time>|<arg|src>|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>