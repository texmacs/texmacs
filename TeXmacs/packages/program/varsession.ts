<TeXmacs|1.0.6.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|var-session|1.0|session|1.0>

    <\src-purpose>
      European-style numbering.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|session>

  <\active*>
    <\src-comment>
      Fields for input, output, text and errors.
    </src-comment>
  </active*>

  <assign|generic-input|<macro|prompt|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-lsep|0.5fn>|<cwith|1|1|2|2|cell-rsep|0.5fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|2|2|cell-lsep|0fn>|<cwith|1|1|2|2|cell-tsep|0.25fn>|<cwith|1|1|2|2|cell-bsep|0.25fn>|<table|<row|<cell|<id-function|<arg|prompt>>>|<\cell>
    <with|math-display|true|<arg|body>>
  </cell>>>>>>>

  <assign|generic-output*|<macro|body|<with|par-left|<plus|<value|par-left>|0.5fn>|par-right|<plus|<value|par-right>|0.5fn>|par-mode|left|math-display|true|<arg|body>>>>

  <assign|generic-output|<macro|body|<surround|<vspace*|0.75fn>|<vspace|0.75fn>|<generic-output*|<arg|body>>>>>

  <assign|generic-textput|<macro|body|<with|par-left|<plus|<value|par-left>|0.5fn>|par-right|<plus|<value|par-right>|0.5fn>|<arg|body>>>>

  <assign|generic-errput|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|pastel
  red>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Folding
    </src-comment>
  </active*>

  <assign|folded|<macro|x|y|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-background|pastel
  orange>|<cwith|1|1|1|1|cell-background|pastel
  brown>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-bsep|0.25fn>|<cwith|1|1|2|2|cell-tsep|0.25fn>|<cwith|1|1|2|2|cell-lsep|0.5fn>|<cwith|1|1|2|2|cell-rsep|0.5fn>|<table|<row|<cell|<action||(mouse-unfold)|<arg|x>>>|<\cell>
    <arg|x>
  </cell>>>>>>>>

  <assign|unfolded|<macro|x|y|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|-1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|-1|1|1|cell-lborder|0.5ln>|<cwith|1|-1|1|1|cell-rborder|0.5ln>|<cwith|1|-1|1|1|cell-bborder|0.5ln>|<cwith|1|-1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0ln>|<cwith|2|2|1|1|cell-tborder|0ln>|<cwith|2|2|2|2|cell-lsep|0fn>|<cwith|2|2|2|2|cell-rsep|0fn>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-bsep|0.25fn>|<cwith|1|1|2|2|cell-tsep|0.25fn>|<cwith|2|2|2|2|cell-tsep|0.5fn>|<cwith|1|-1|1|1|cell-background|pastel
  brown>|<cwith|1|1|2|2|cell-background|pastel
  orange>|<cwith|2|2|2|2|cell-bsep|0fn>|<cwith|1|1|2|2|cell-lsep|0.5fn>|<cwith|1|1|2|2|cell-rsep|0.5fn>|<table|<row|<cell|<action||(mouse-fold)|<arg|x>|<arg|y>>>|<\cell>
    <arg|x>
  </cell>>|<row|<cell|<action||(mouse-fold)|<arg|x>|<arg|y>>>|<\cell>
    <arg|y>
  </cell>>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>