<TeXmacs|1.0.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-pattern|1.0>

    <\src-purpose>
      Macros for rendering patterns and pattern-based transformations
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

  <assign|pat-or*|<macro|x|<active*|<with|color|dark
  magenta|mode|math|\<vee\>>><arg|x>>>

  <assign|pat-or|<xmacro|x|<active*|<with|color|dark
  magenta|(>><arg|x|0><map-args|pat-or*|concat|x|1><active*|<with|color|dark
  magenta|)>>>>

  <assign|pat-and*|<macro|x|<active*|<with|color|dark
  magenta|mode|math|\<wedge\>>><arg|x>>>

  <assign|pat-and|<xmacro|x|<active*|<with|color|dark
  magenta|(>><arg|x|0><map-args|pat-and*|concat|x|1><active*|<with|color|dark
  magenta|)>>>>

  <assign|pat-and-not|<macro|x|y|<active*|<with|color|dark
  magenta|(>><arg|x><active*|<with|color|dark
  magenta|mode|math|\\>><arg|y><active*|<with|color|dark magenta|)>>>>

  <assign|pat-group*|<macro|x|<active*|<with|color|dark
  magenta|mode|math|\|>><arg|x>>>

  <assign|pat-group|<xmacro|x|<active*|<with|color|dark
  magenta|mode|math|(>><arg|x|0><map-args|pat-group*|concat|x|1><active*|<with|color|dark
  magenta|mode|math|)>>>>

  <assign|pat-compound*|<macro|x|<active*|<with|color|dark
  magenta|mode|math|\|>><arg|x>>>

  <assign|pat-compound|<xmacro|x|<active*|<with|color|dark
  magenta|<with|mode|math|\<langle\>><with|font-family|ss|<inactive*|<arg|x|0>>>>><map-args|pat-compound*|concat|x|1><active*|<with|color|dark
  magenta|mode|math|\<rangle\>>>>>

  <assign|pat-repeat|<macro|x|<arg|x><active*|<with|color|dark
  magenta|mode|math|<rsup|\<ast\>>>>>>

  <assign|pat-any|<macro|<active*|<with|color|magenta|mode|math|<group|\<ast\>>>>>>

  <assign|pat-several-any|<macro|nr|<pat-any><active*|<with|color|dark
  magenta|mode|math|<rsup|<inactive*|<arg|nr>>>>>>>

  <assign|pat-repeat-any|<macro|<pat-repeat|<pat-any>>>>

  <assign|pat-quote|<macro|x|<with|color|magenta|font-shape|italic|<arg|x>>>>

  <assign|pat-range|<macro|start|end|<active*|<with|color|dark
  magenta|(>><arg|start><active*|<with|color|dark
  magenta|-->><arg|end><active*|<with|color|dark magenta|)>>>>

  <assign|pat-match|<macro|x|<arg|x><active*|<with|color|dark
  magenta|mode|math|?>>>>

  <assign|pat-replace|<macro|x|<arg|x><active*|<with|color|dark
  magenta|mode|math|!>>>>

  \;

  <assign|select|<xmacro|x|<extern|ext-select|<arg|x|0>|<quote-arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>