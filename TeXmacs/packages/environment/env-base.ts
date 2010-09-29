<TeXmacs|1.0.7.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-base|1.0>

    <\src-purpose>
      Managing groups of environments.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Groups and counters for all standard environments.
    </src-comment>
  </active*>

  <new-counter-group|std-env>

  <new-counter-group|theorem-env>

  <add-to-counter-group|theorem-env|std-env>

  <group-common-counter|theorem-env>

  <new-counter-group|exercise-env>

  <add-to-counter-group|exercise-env|std-env>

  <group-individual-counters|exercise-env>

  <new-counter-group|figure-env>

  <add-to-counter-group|figure-env|std-env>

  <group-individual-counters|figure-env>

  <add-to-counter-group|equation|std-env>

  <add-to-counter-group|footnote|std-env>

  <assign|resetstdenv|<macro|<reset-std-env>>>

  <\active*>
    <\src-comment>
      Defining new block environments with one parameter.
    </src-comment>
  </active*>

  <assign|new-env|<macro|env|name|grp|render|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|<unquote|<arg|grp>>><assign|<unquote|<merge|<arg|env>|-text>>|<macro|<localize|<unquote|<arg|name>>>>><assign|<unquote|<merge|<arg|env>|-numbered>>|<macro|text|num|<arg|text>
  <with|font-shape|right|<arg|num>>>><assign|<unquote|<merge|<arg|env>|-unnumbered>>|<macro|text|<arg|text>>><assign|<unquote|<arg|env>>|<\macro|body>
    <surround|<compound|<unquote|<merge|next-|<arg|env>>>>||<style-with|src-compact|none|<compound|<unquote|<arg|render>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-numbered>>|<compound|<unquote|<merge|<arg|env>|-text>>>|<compound|<unquote|<merge|the-|<arg|env>>>>>>|<arg|body>>>>
  </macro>><assign|<unquote|<merge|<arg|env>|*>>|<\macro|body>
    <style-with|src-compact|none|<compound|<unquote|<arg|render>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-unnumbered>>|<compound|<unquote|<merge|<arg|env>|-text>>>>>|<arg|body>>>
  </macro>>>>>>

  <assign|new-theorem|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem>>>

  <assign|new-remark|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-remark>>>

  <assign|new-exercise|<macro|env|name|<new-env|<arg|env>|<arg|name>|exercise-env|render-exercise>>>

  <\active*>
    <\src-comment>
      Defining new figure-like environments.
    </src-comment>
  </active*>

  <assign|new-figure|<macro|env|name|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|figure-env><assign|<unquote|<merge|<arg|env>|-text>>|<macro|<localize|<unquote|<arg|name>>>>><assign|<unquote|<merge|small-|<arg|env>>>|<macro|body|caption|<style-with|src-compact|none|<compound|<unquote|<merge|next-|<arg|env>>>><style-with|src-compact|none|<render-small-figure|<unquote|<arg|env>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-text>>>
  <compound|<unquote|<merge|the-|<arg|env>>>>>|<arg|body>|<arg|caption>>>>>><assign|<unquote|<merge|small-|<arg|env>|*>>|<macro|body|caption|<style-with|src-compact|none|<render-small-figure|<unquote|<arg|env>>|<compound|<unquote|<merge|<arg|env>|-text>>>|<arg|body>|<arg|caption>>>>><assign|<unquote|<merge|big-|<arg|env>>>|<\macro|body|caption>
    <surround|<compound|<unquote|<merge|next-|<arg|env>>>>||<style-with|src-compact|none|<render-big-figure|<unquote|<arg|env>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-text>>>
    <compound|<unquote|<merge|the-|<arg|env>>>>>|<arg|body>|<arg|caption>>>>
  </macro>><assign|<unquote|<merge|big-|<arg|env>|*>>|<\macro|body|caption>
    <style-with|src-compact|none|<render-big-figure|<unquote|<arg|env>>|<compound|<unquote|<merge|<arg|env>|-text>>>|<arg|body>|<arg|caption>>>
  </macro>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>