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

  <assign|bg-color|light grey>

  <assign|page-medium|automatic>

  <assign|font-family|ss>

  <assign|par-sep|0.2em>

  <assign|par-par-sep|0em>

  <assign|par-line-sep|0em>

  <\active*>
    <\src-comment>
      Gui environment variables
    </src-comment>
  </active*>

  <assign|gui-small-sep|3px>

  <assign|gui-med-sep|6px>

  <assign|gui-big-sep|12px>

  <\active*>
    <\src-comment>
      Tables
    </src-comment>
  </active*>

  <assign|gui-dense-table|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0px>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|-1|1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|gui-normal-table|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|gui-airy-table|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-big-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-big-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Framed boxes
    </src-comment>
  </active*>

  <assign|gui-small-framed|<macro|main-col|border1-col|border2-col|pad|body|<gui-dense-table|<tformat|<twith|table-hmode|auto>|<cwith|1|1|1|-1|cell-background|<arg|border1-col>>|<cwith|3|3|1|-1|cell-background|<arg|border2-col>>|<cwith|1|-1|3|3|cell-background|<arg|border2-col>>|<cwith|1|-1|1|1|cell-background|<arg|border1-col>>|<cwith|1|-1|1|1|cell-width|1px>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|1px>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|1|1|1|-1|cell-height|1px>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|3|3|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-vmode|exact>|<cwith|2|2|2|2|cell-lsep|<arg|pad>>|<cwith|2|2|2|2|cell-rsep|<arg|pad>>|<cwith|2|2|2|2|cell-bsep|<arg|pad>>|<cwith|2|2|2|2|cell-tsep|<arg|pad>>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|<arg|body>>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <assign|gui-line-framed|<macro|main-col|border1-col|border2-col|pad|body|<gui-dense-table|<tformat|<cwith|1|1|1|-1|cell-background|<arg|border1-col>>|<cwith|3|3|1|-1|cell-background|<arg|border2-col>>|<cwith|1|-1|1|1|cell-background|<arg|border1-col>>|<cwith|1|-1|1|1|cell-width|1px>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|1|1|-1|cell-height|1px>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|3|3|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-vmode|exact>|<cwith|2|2|2|2|cell-background|<arg|main-col>>|<cwith|2|2|2|2|cell-lsep|<arg|pad>>|<cwith|2|2|2|2|cell-rsep|<arg|pad>>|<cwith|2|2|2|2|cell-bsep|<arg|pad>>|<cwith|2|2|2|2|cell-tsep|<arg|pad>>|<cwith|2|2|2|2|cell-vcorrect|a>|<cwith|2|2|2|2|cell-hyphen|n>|<cwith|2|2|2|2|cell-hpart|1>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|<arg|body>>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <assign|gui-big-framed|<macro|main-col|border1-col|border2-col|pad|body|<gui-dense-table|<tformat|<cwith|1|1|1|-1|cell-background|<arg|border1-col>>|<cwith|3|3|1|-1|cell-background|<arg|border2-col>>|<cwith|1|-1|3|3|cell-background|<arg|border2-col>>|<cwith|1|-1|1|1|cell-background|<arg|border1-col>>|<cwith|1|-1|1|1|cell-width|1px>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|1px>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|1|1|1|-1|cell-height|1px>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|3|3|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-vmode|exact>|<cwith|2|2|2|2|cell-background|<arg|main-col>>|<cwith|2|2|2|2|cell-lsep|<arg|pad>>|<cwith|2|2|2|2|cell-rsep|<arg|pad>>|<cwith|2|2|2|2|cell-bsep|<arg|pad>>|<cwith|2|2|2|2|cell-tsep|<arg|pad>>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<\cell>
    <arg|body>
  </cell>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <\active*>
    <\src-comment>
      Framed boxes
    </src-comment>
  </active*>

  <assign|gui-big-lower|<macro|body|<gui-big-framed|#e0e0e0|grey|#f0f0f0|<value|gui-med-sep>|<arg|body>>>>

  <assign|gui-small-raise|<macro|body|<gui-small-framed|light
  grey|#f0f0f0|grey|<value|gui-small-sep>|<arg|body>>>>

  <assign|gui-line-input|<macro|body|<gui-line-framed|#f0f0f0|light
  grey|white|<value|gui-small-sep>|<arg|body>>>>

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