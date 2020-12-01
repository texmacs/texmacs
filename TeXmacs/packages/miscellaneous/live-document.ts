<TeXmacs|1.99.16>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|live-document|1.0>

    <\src-purpose>
      Live documents
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(utils relate live-menu)>

  <use-module|(client client-live)>

  <\active*>
    <\src-comment>
      Mirrors
    </src-comment>
  </active*>

  <assign|live-io*|<macro|view-id|live-id|body|<with|old-color|<value|locus-color>|locus-color|preserve|<locus|<id|<arg|live-id>>|<observer|<arg|view-id>|live-notify>|<surround|<hidden|<extern|live-initialize|<quote-arg|body>>>|<if|<equal|<get-label|<quote-arg|body>>|document>|<right-flush>>|<with|locus-color|<value|old-color>|<arg|body>>>>>>>

  <drd-props|live-io*|arity|3|accessible|2|border|no>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>