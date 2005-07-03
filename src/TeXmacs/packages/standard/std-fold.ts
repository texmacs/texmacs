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

  <assign|render-folded-std|<\macro|button|body>
    <with|par-left|<plus|<value|par-left>|1.5fn>|<style-with|src-compact|none|<\surround|<with|par-first|-1.5fn|<yes-indent>><arg|button>|<hflush>>
      <arg|body>
    </surround>>>
  </macro>>

  <assign|folded-padded-normal|<macro|a|b|body|<\surround|<vspace*|<arg|a>>|<htab|0fn|first><vspace|<arg|b>>>
    <\with|padded-normal|<value|old-padded-normal>>
      <arg|body>
    </with>
  </surround>>>

  <assign|render-folded-env|<macro|button|body|<style-with|src-compact|none|<\surround|<with|par-first|-1.5fn|<yes-indent><arg|button>>|<right-flush>>
    <\with|old-padded-normal|<value|padded-normal>|padded-normal|<value|fold-padded-normal>>
      <arg|body>
    </with>
  </surround>>>>

  <assign|render-folded-grouped|<\macro|button|body>
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
      Tags for folding and unfolding.
    </src-comment>
  </active*>

  <assign|folded-plain|<\macro|x|y>
    <\surround||<right-flush>>
      <arg|x>

      <hidden|<arg|y>>
    </surround>
  </macro>>

  <assign|unfolded-plain|<\macro|x|y>
    <\surround||<right-flush>>
      <arg|x>

      <arg|y>
    </surround>
  </macro>>

  <assign|folded-std|<\macro|x|y>
    <\render-folded-std|<action|<resize|<active*|<with|mode|math|<op|\<circ\>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>

      <hidden|<arg|y>>
    </render-folded-std>
  </macro>>

  <assign|unfolded-std|<\macro|x|y>
    <\render-folded-std|<action|<resize|<active*|<with|mode|math|\<bullet\>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|x>

      <arg|y>
    </render-folded-std>
  </macro>>

  <assign|folded-env|<\macro|x|y>
    <\render-folded-env|<action|<resize|<specific|screen|<active*|<with|mode|math|<op|\<circ\>>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>

      <hidden|<arg|y>>
    </render-folded-env>
  </macro>>

  <assign|unfolded-env|<\macro|x|y>
    <\render-folded-env|<action|<resize|<specific|screen|<active*|<with|mode|math|\<bullet\>>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|x>

      <arg|y>
    </render-folded-env>
  </macro>>

  <assign|folded-grouped|<\macro|x|y>
    <\render-folded-grouped|<action| |(mouse-unfold)|<arg|x>>>
      <arg|x>

      <hidden|<arg|y>>
    </render-folded-grouped>
  </macro>>

  <assign|unfolded-grouped|<\macro|x|y>
    <\render-folded-grouped|<action| |(mouse-fold)|<arg|x>>>
      <arg|x>

      <arg|y>
    </render-folded-grouped>
  </macro>>

  <assign|folded|<value|folded-std>>

  <assign|unfolded|<value|unfolded-std>>

  <\active*>
    <\src-comment>
      Tags for toggling between summarized and detailed text.
    </src-comment>
  </active*>

  <assign|summarized-plain|<\macro|x|y>
    <\surround||<right-flush>>
      <arg|x>
    </surround>
  </macro>>

  <assign|detailed-plain|<\macro|x|y>
    <\surround||<right-flush>>
      <arg|y>
    </surround>
  </macro>>

  <assign|summarized-std|<\macro|x|y>
    <\render-folded-std|<action|<resize|<specific|screen|<active*|<with|mode|math|<op|\<circ\>>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>
    </render-folded-std>
  </macro>>

  <assign|detailed-std|<\macro|x|y>
    <\render-folded-std|<action|<resize|<specific|screen|<active*|<with|mode|math|\<bullet\>>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|y>
    </render-folded-std>
  </macro>>

  <assign|summarized-env|<\macro|x|y>
    <\render-folded-env|<action|<resize|<specific|screen|<active*|<with|mode|math|<op|\<circ\>>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>
      <arg|x>
    </render-folded-env>
  </macro>>

  <assign|detailed-env|<\macro|x|y>
    <\render-folded-env|<action|<resize|<specific|screen|<active*|<with|mode|math|\<bullet\>>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>
      <arg|y>
    </render-folded-env>
  </macro>>

  <assign|summarized-grouped|<\macro|x|y>
    <\render-folded-grouped|<action| |(mouse-unfold)|<arg|x>>>
      <arg|x>
    </render-folded-grouped>
  </macro>>

  <assign|detailed-grouped|<\macro|x|y>
    <\render-folded-grouped|<action| |(mouse-fold)|<arg|x>>>
      <arg|y>
    </render-folded-grouped>
  </macro>>

  <assign|summarized|<value|summarized-env>>

  <assign|detailed|<value|detailed-env>>

  <\active*>
    <\src-comment>
      Tags for switches.
    </src-comment>
  </active*>

  <assign|document-block|<\xmacro|switch-args>
    <surround||<right-flush>|<\quasi>
      <unquote*|<quote-arg|switch-args>>
    </quasi>>
  </xmacro>>

  <assign|slide|<\macro|body>
    <\surround||<right-flush><new-page>>
      <arg|body>
    </surround>
  </macro>>

  <assign|slides-block|<\xmacro|switch-args>
    <surround||<right-flush>|<\quasi>
      <unquote*|<map|slide|<quote-arg|switch-args>>>
    </quasi>>
  </xmacro>>

  \;

  <assign|switch|<value|document-block>>

  <drd-props|switch|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|screens|<value|document-block>>

  <drd-props|screens|arity|<tuple|repeat|1|1>|accessible|all|border|no>

  <assign|unroll|<value|document-block>>

  <drd-props|unroll|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|expanded|<value|document-block>>

  <drd-props|expanded|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|slides|<value|slides-block>>

  <drd-props|slides|arity|<tuple|repeat|1|1>|accessible|all>

  \;

  <assign|traversed|<value|identity>>

  <assign|fold-back|<value|identity>>

  <assign|keep-unfolded|<value|identity>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>