<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|algo|1.0>

    <\src-purpose>
      Markup for typesetting algorithms (unfinished).
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

  <assign|fun|<macro|x|<with|mode|text|font-family|rm|<arg|x>>>>

  <assign|var|<macro|x|<with|mode|text|font-shape|italic|<arg|x>>>>

  <assign|type|<macro|x|<with|mode|text|font-family|ss|<arg|x>>>>

  <assign|vardecl|<macro|x|y|<var|<arg|x>>:<type|<arg|y>>>>

  <assign|fundecl|<macro|x|y|<fun|<arg|x>>:<type|<arg|y>>>>

  \;

  <assign|synopsis|<\macro|x>
    <with|par-par-sep|0fn|<vspace*|0.5fn><with|font-series|bold|font-size|1.19|Synopsis><vspace|0.5fn><no-page-break>>

    <with|par-left|<plus|<value|par-left>|1.5fn>|par-par-sep|0fn|<arg|x>>
  </macro>>

  <assign|parameters|<\macro|x>
    <with|par-par-sep|0fn|<vspace*|0.5fn><with|font-series|bold|font-size|1.19|Parameters><vspace|0.5fn><no-page-break>>

    <with|par-par-sep|0fn|<description|<arg|x>>>
  </macro>>

  \;

  <assign|comment|<macro|x|<htab|5mm><with|mode|text|font-shape|slanted|font-size|0.84|<arg|x>>>>

  <assign|long-comment|<macro|body|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|mode|text|font-shape|slanted|font-size|0.84|<arg|body>>>>>

  \;

  <assign|keyw|<macro|x|<with|mode|text|font-series|bold|<arg|x>>>>

  <assign|new-keyword|<macro|kw|<assign|<merge|kw-|<arg|kw>>|<quasiquote|<keyw|<unquote|<arg|kw>>>>>>>

  <new-keyword|for>

  <new-keyword|to>

  <new-keyword|do>

  <new-keyword|begin>

  <new-keyword|end>

  <new-keyword|while>

  <new-keyword|repeat>

  <new-keyword|until>

  <new-keyword|return>

  <new-keyword|if>

  <new-keyword|then>

  <new-keyword|else>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>