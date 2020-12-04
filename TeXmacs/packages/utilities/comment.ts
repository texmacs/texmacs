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
      Helper macros
    </src-comment>
  </active*>

  <assign|comment-color|<macro|by|dark magenta>>

  <assign|abbreviate-name|<macro|by|<extern|ext-abbreviate-name|<arg|by>>>>

  <\active*>
    <\src-comment>
      Rendering macros
    </src-comment>
  </active*>

  <assign|render-box-comment|<\macro|bar-color|title|body>
    <with|shadow-elevation|0.75|<\half-bend-in-shadow>
      <\wide-tabular>
        <tformat|<cwith|1|1|1|1|cell-background|<arg|bar-color>>|<cwith|1|-1|1|1|cell-lsep|1spc>|<cwith|1|-1|1|1|cell-rsep|1spc>|<cwith|1|-1|1|1|cell-tsep|0.5spc>|<cwith|1|-1|1|1|cell-bsep|0.5spc>|<cwith|2|-1|1|1|cell-tsep|2spc>|<cwith|2|-1|1|1|cell-bsep|1spc>|<table|<row|<\cell>
          <samp|<with|color|white|locus-color|preserve|<arg|title>>>
        </cell>>|<row|<\cell>
          <arg|body>
        </cell>>>>
      </wide-tabular>
    </half-bend-in-shadow>>
  </macro>>

  <\active*>
    <\src-comment>
      Various rendering styles for comments
    </src-comment>
  </active*>

  <assign|show-comment|<macro|unique-id|mirror-id|type|by|time|body|<with|old-color|<value|color>|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<with|locus-color|<value|old-color>|color|<comment-color|<arg|by>>|<surround|<extern|mirror-initialize|<quote-arg|body>>[<condensed|<name|<abbreviate-name|<arg|by>>>>:
  |<if|<equal|<get-label|<quote-arg|body>>|document>|<right-flush>>]|<with|color|<value|old-color>|<arg|body>>>>>>>>>

  <assign|hide-comment|<macro|unique-id|mirror-id|type|by|time|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<style-with|src-compact|none|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<expand-as|<arg|body>|<extern|mirror-initialize|<quote-arg|body>><flag|<abbreviate-name|<arg|by>>|<comment-color|<arg|by>>><hidden|<arg|body>>>>>>>>

  <assign|preview-comment|<\macro|unique-id|mirror-id|type|by|time|body>
    <\preview-balloon>
      <surround|<with|color|<comment-color|<arg|by>>|<condensed|<name|<abbreviate-name|<arg|by>>>>\<rangle\>
      >||<arg|body>>
    </preview-balloon>
  </macro>>

  <assign|mirror-comment|<macro|unique-id|mirror-id|type|by|time|body|<with|old-locus-color|<value|locus-color>|locus-color|preserve|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<\surround|<hidden|<extern|mirror-initialize|<quote-arg|body>>>|>
    <\with|locus-color|<value|old-locus-color>>
      <\render-box-comment|<comment-color|<arg|by>>|<copy|<arg|by>>>
        <arg|body>
      </render-box-comment>
    </with>
  </surround>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>