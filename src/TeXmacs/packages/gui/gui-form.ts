<TeXmacs|1.0.6.7>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-utils|1.0>

    <\src-purpose>
      Standard GUI utilities
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|gui-utils>

  <assign|form|<macro|id|body|<with|current-form|<arg|id>|<wide-normal|<arg|body>>>>>

  <assign|form-set|<macro|key|val|<set-binding|<merge|<value|current-form>|-|<arg|key>>|<arg|val>>>>

  <assign|form-get|<macro|key|<get-binding|<merge|<value|current-form>|-|<arg|key>>>>>

  <assign|input-switch-toggle|<macro|key|body|<tabular|<tformat|<twith|table-rborder|1ln>|<twith|table-tborder|1ln>|<twith|table-bborder|<if|<equal|<value|input-switch-value>|<arg|key>>|0ln|1ln>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|input-switch|<macro|key|val|switches|body|<with|input-switch-value|<arg|val>|<style-with|src-compact|none|<no-indent><tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<cwith|1|-1|1|-1|cell-bsep|0ln>|<cwith|1|-1|1|-1|cell-tsep|0ln>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-background|#e0e0e0>|<table|<row|<cell|<subtable|<tformat|<twith|table-lsep|0cm>|<twith|table-rsep|0cm>|<twith|table-bsep|0cm>|<twith|table-tsep|0cm>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-background|light
  grey>|<cwith|1|-1|1|-1|cell-lsep|0cm>|<cwith|1|-1|1|-1|cell-rsep|0cm>|<cwith|1|-1|1|-1|cell-bsep|0cm>|<cwith|1|-1|1|-1|cell-tsep|0cm>|<cwith|1|1|2|2|cell-bborder|1ln>|<table|<row|<cell|<arg|switches>>|<cell|>>>>>>>|<row|<\cell>
    <style-with|src-compact|none|<no-indent><tabular|<tformat|<twith|table-lborder|1ln>|<twith|table-rborder|1ln>|<twith|table-bborder|1ln>|<cwith|1|1|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-tsep|1spc>|<table|<row|<\cell>
      <arg|body>
    </cell>>>>>>
  </cell>>>>>>>>>

  <assign|input-text|<macro|key|width|body|<surround||<form-set|<arg|key>|<arg|body>>|<block|<tformat|<cwith|1|1|1|1|cell-background|white>|<cwith|1|1|1|1|cell-width|<arg|width>>|<cwith|1|1|1|1|cell-hmode|exact>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|action-button|<macro|cmd|body|<style-with|src-compact|none|<action|<gui-small-raise|<arg|body>>|<arg|cmd>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>