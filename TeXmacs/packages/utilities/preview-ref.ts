<TeXmacs|1.99.14>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|preview-ref|1.0>

    <\src-purpose>
      Show preview of link destination for references and citations
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(link ref-edit)>

  <assign|reference|<macro|Id|<locus|<id|<hard-id|<arg|Id>>>|<link|hyperlink|<id|<hard-id|<arg|Id>>>|<url|<merge|#|<arg|Id>>>>|<link|mouse-over|<id|<hard-id|<arg|Id>>>|<script|preview-reference|<quote-arg|Id>|<arg|Id>>>|<get-binding|<arg|Id>>>>>

  <assign|pageref|<macro|Id|<locus|<id|<hard-id|<arg|Id>>>|<link|hyperlink|<id|<hard-id|<arg|Id>>>|<url|<merge|#|<arg|Id>>>>|<link|mouse-over|<id|<hard-id|<arg|Id>>>|<script|preview-reference|<quote-arg|Id>|<arg|Id>>>|<get-binding|<arg|Id>|1>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>