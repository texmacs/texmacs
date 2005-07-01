<TeXmacs|1.0.5.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-fold|1.0>

    <\src-purpose>
      Macros for folding and switches.
    </src-purpose>

    <src-copyright|1998--2005|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Rendering folding tags.
    </src-comment>
  </active*>

  <assign|render-fold-std|<\macro|button|body>
    <with|par-left|<plus|<value|par-left>|1.5fn>|<style-with|src-compact|none|<\surround|<with|par-first|-1.5fn|<yes-indent>><arg|button>|<hflush>>
      <arg|body>
    </surround>>>
  </macro>>

  <assign|fold-padded-normal|<macro|a|b|body|<\surround|<vspace*|<arg|a>>|<htab|0fn|first><vspace|<arg|b>>>
    <\with|padded-normal|<value|old-padded-normal>>
      <arg|body>
    </with>
  </surround>>>

  <assign|render-fold-env|<macro|button|body|<style-with|src-compact|none|<\surround|<with|par-first|-1.5fn|<yes-indent><arg|button>>|<right-flush>>
    <\with|old-padded-normal|<value|padded-normal>|padded-normal|<value|fold-padded-normal>>
      <arg|body>
    </with>
  </surround>>>>

  <assign|render-fold-bracket|<\macro|button|body>
    <with|old-color|<value|color>|color|blue|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-valign|c>|<cwith|1|1|2|2|cell-rsep|0fn>|<cwith|1|1|2|2|cell-vcorrect|n>|<cwith|1|1|2|2|cell-bsep|0.2fn>|<cwith|1|1|2|2|cell-tsep|0.2fn>|<cwith|1|-1|1|-1|cell-hmode|min>|<table|<row|<cell|<subtable|<tformat|<twith|table-valign|T>|<cwith|3|3|1|-1|cell-vpart|1>|<cwith|2|2|2|2|cell-background|pastel
    blue>|<cwith|2|2|1|1|cell-background|pastel
    blue>|<cwith|3|3|1|1|cell-background|pastel
    blue>|<cwith|4|4|1|1|cell-background|pastel
    blue>|<cwith|4|4|2|2|cell-background|pastel
    blue>|<cwith|2|4|1|3|cell-width|5ln>|<cwith|2|4|2|3|cell-width|10ln>|<cwith|2|4|1|3|cell-halign|c>|<cwith|1|-1|1|1|cell-lborder|0.5ln>|<cwith|2|2|1|2|cell-tborder|0.5ln>|<cwith|2|2|2|2|cell-rborder|0.5ln>|<cwith|4|4|1|2|cell-bborder|0.5ln>|<cwith|3|3|1|1|cell-rborder|0.5ln>|<cwith|4|4|2|2|cell-rborder|0.5ln>|<cwith|4|4|2|2|cell-tborder|0.5ln>|<cwith|2|2|2|2|cell-bborder|0.5ln>|<cwith|5|5|1|1|cell-lborder|0ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|1|1|-1|cell-height|3ln>|<cwith|5|5|1|-1|cell-height|3ln>|<cwith|2|2|1|-1|cell-height|3ln>|<cwith|4|4|1|-1|cell-height|3ln>|<cwith|1|-1|1|1|cell-width|5ln>|<cwith|1|-1|2|2|cell-width|7ln>|<cwith|1|-1|3|3|cell-width|7ln>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<arg|button>>|<cell|<arg|button>>|<cell|<arg|button>>>|<row|<cell|<arg|button>>|<cell|<arg|button>>|<cell|<arg|button>>>|<row|<cell|<arg|button>>|<cell|<arg|button>>|<cell|<arg|button>>>|<row|<cell|>|<cell|>|<cell|>>>>>>|<\cell>
      <\with|color|<value|old-color>>
        <arg|body>
      </with>
    </cell>>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Tags for folding and toggles.
    </src-comment>
  </active*>

  <assign|fold-plain|<\macro|x|y>
    <\surround||<right-flush>>
      <arg|x>

      <hidden|<arg|y>>
    </surround>
  </macro>>

  <assign|unfold-plain|<\macro|x|y>
    <\surround||<right-flush>>
      <arg|x>

      <arg|y>
    </surround>
  </macro>>

  <assign|fold-std|<\macro|x|y>
    <\render-fold-std|<action|<resize|<active*|<with|mode|math|<op|\<circ\>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>

      <hidden|<arg|y>>
    </render-fold-std>
  </macro>>

  <assign|unfold-std|<\macro|x|y>
    <\render-fold-std|<action|<resize|<active*|<with|mode|math|\<bullet\>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|x>

      <arg|y>
    </render-fold-std>
  </macro>>

  <assign|fold-env|<\macro|x|y>
    <\render-fold-env|<action|<resize|<specific|screen|<active*|<with|mode|math|<op|\<circ\>>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>

      <hidden|<arg|y>>
    </render-fold-env>
  </macro>>

  <assign|unfold-env|<\macro|x|y>
    <\render-fold-env|<action|<resize|<specific|screen|<active*|<with|mode|math|\<bullet\>>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|x>

      <arg|y>
    </render-fold-env>
  </macro>>

  <assign|fold-bracket|<\macro|x|y>
    <\render-fold-bracket|<action| |(mouse-unfold)|<arg|x>>>
      <arg|x>

      <hidden|<arg|y>>
    </render-fold-bracket>
  </macro>>

  <assign|unfold-bracket|<\macro|x|y>
    <\render-fold-bracket|<action| |(mouse-fold)|<arg|x>>>
      <arg|x>

      <arg|y>
    </render-fold-bracket>
  </macro>>

  \;

  <assign|fold|<value|fold-std>>

  <assign|unfold|<value|unfold-std>>

  \;

  <assign|condensed|<\macro|x|y>
    <\render-fold-env|<action|<resize|<specific|screen|<active*|<with|mode|math|<op|\<circ\>>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>
    </render-fold-env>
  </macro>>

  <assign|detailed|<\macro|x|y>
    <\render-fold-env|<action|<resize|<specific|screen|<active*|<with|mode|math|\<bullet\>>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|y>
    </render-fold-env>
  </macro>>

  <\active*>
    <\src-comment>
      Tags for switches.
    </src-comment>
  </active*>

  <assign|switch|<\xmacro|switch-args>
    <surround||<right-flush>|<\quasi>
      <unquote*|<quote-arg|switch-args>>
    </quasi>>
  </xmacro>>

  <drd-props|switch|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|unroll|<value|switch>>

  <drd-props|unroll|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|expanded|<value|switch>>

  <drd-props|expanded|arity|<tuple|repeat|1|1>|accessible|all>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>