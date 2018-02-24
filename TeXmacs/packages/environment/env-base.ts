<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-base|1.0>

    <\src-purpose>
      Managing groups of environments.
    </src-purpose>

    <src-copyright|1998--2011|Joris van der Hoeven>

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

  <add-to-counter-group|algorithm-env|std-env>

  <group-individual-counters|algorithm-env>

  <new-counter-group|figure-env>

  <add-to-counter-group|figure-env|std-env>

  <group-individual-counters|figure-env>

  <add-to-counter-group|equation|std-env>

  <add-to-counter-group|footnote|std-env>

  <assign|resetstdenv|<macro|<reset-std-env>>>

  <\active*>
    <\src-comment>
      Defining new enunciations.
    </src-comment>
  </active*>

  <assign|env-number|<macro|num|<with|font-shape|right|<arg|num>>>>

  <assign|new-env|<macro|env|name|grp|render|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|<unquote|<arg|grp>>><assign|<unquote|<merge|<arg|env>|-text>>|<macro|<localize|<unquote|<arg|name>>>>><assign|<unquote|<merge|<arg|env>|-numbered>>|<macro|text|num|<arg|text>
  <env-number|<arg|num>>>><assign|<unquote|<merge|<arg|env>|-unnumbered>>|<macro|text|<arg|text>>><assign|<unquote|<arg|env>>|<\macro|body>
    <surround|<compound|<unquote|<merge|next-|<arg|env>>>>||<style-with|src-compact|none|<compound|<unquote|<arg|render>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-numbered>>|<compound|<unquote|<merge|<arg|env>|-text>>>|<compound|<unquote|<merge|the-|<arg|env>>>>>>|<arg|body>>>>
  </macro>><assign|<unquote|<merge|<arg|env>|*>>|<\macro|body>
    <style-with|src-compact|none|<compound|<unquote|<arg|render>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-unnumbered>>|<compound|<unquote|<merge|<arg|env>|-text>>>>>|<arg|body>>>
  </macro>>>>>>

  <assign|new-theorem|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem>>>

  <assign|new-remark|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-remark>>>

  <assign|new-exercise|<macro|env|name|<new-env|<arg|env>|<arg|name>|exercise-env|render-exercise>>>

  <\active*>
    <\src-comment>
      Defining new algorithmic environments.
    </src-comment>
  </active*>

  <assign|new-algorithm|<macro|env|name|render|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|algorithm-env><assign|<unquote|<merge|<arg|env>|-text>>|<macro|<localize|<unquote|<arg|name>>>>><assign|<unquote|<arg|env>>|<\macro|body>
    <\surround|<compound|<unquote|<merge|next-|<arg|env>>>>|>
      <\render-algorithm|<compound|<unquote|<merge|<arg|env>|-text>>>
      <with|font-shape|right|<compound|<unquote|<merge|the-|<arg|env>>>>>>
        <arg|body>
      </render-algorithm>
    </surround>
  </macro>><assign|<unquote|<merge|<arg|env>|*>>|<\macro|body>
    <\render-algorithm|<compound|<unquote|<merge|<arg|env>|-text>>>>
      <arg|body>
    </render-algorithm>
  </macro>><assign|<unquote|<merge|named-|<arg|env>>>|<\macro|name|body>
    <\render-algorithm|<compound|<unquote|<merge|<arg|env>|-text>>>
    <arg|name>>
      <arg|body>
    </render-algorithm>
  </macro>><assign|<unquote|<merge|specified-|<arg|env>>>|<\macro|intro|body>
    <\surround|<compound|<unquote|<merge|next-|<arg|env>>>>|>
      <\render-specified-algorithm|<compound|<unquote|<merge|<arg|env>|-text>>>
      <with|font-shape|right|<compound|<unquote|<merge|the-|<arg|env>>>>>>
        <arg|intro>
      <|render-specified-algorithm>
        <arg|body>
      </render-specified-algorithm>
    </surround>
  </macro>><assign|<unquote|<merge|specified-|<arg|env>|*>>|<\macro|intro|body>
    <\render-specified-algorithm|<compound|<unquote|<merge|<arg|env>|-text>>>>
      <arg|intro>
    <|render-specified-algorithm>
      <arg|body>
    </render-specified-algorithm>
  </macro>><assign|<unquote|<merge|named-specified-|<arg|env>>>|<\macro|name|intro|body>
    <\render-specified-algorithm|<compound|<unquote|<merge|<arg|env>|-text>>>
    <arg|name>>
      <arg|intro>
    <|render-specified-algorithm>
      <arg|body>
    </render-specified-algorithm>
  </macro>>>>>>

  <\active*>
    <\src-comment>
      Defining new figure-like environments.
    </src-comment>
  </active*>

  <assign|new-figure|<macro|env|name|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|figure-env><assign|<unquote|<merge|<arg|env>|-text>>|<macro|<localize|<unquote|<arg|name>>>>><assign|<unquote|<merge|small-|<arg|env>>>|<macro|body|caption|<style-with|src-compact|none|<compound|<unquote|<merge|next-|<arg|env>>>><style-with|src-compact|none|<render-small-figure|<unquote|<arg|env>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-text>>>
  <compound|<unquote|<merge|the-|<arg|env>>>>>|<arg|body>|<surround|<set-binding|<compound|<unquote|<merge|the-|<arg|env>>>>>||<arg|caption>>>>>>><assign|<unquote|<merge|small-|<arg|env>|*>>|<macro|body|caption|<style-with|src-compact|none|<render-small-figure|<unquote|<arg|env>>|<compound|<unquote|<merge|<arg|env>|-text>>>|<arg|body>|<arg|caption>>>>><assign|<unquote|<merge|big-|<arg|env>>>|<\macro|body|caption>
    <surround|<compound|<unquote|<merge|next-|<arg|env>>>>||<style-with|src-compact|none|<render-big-figure|<unquote|<arg|env>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|env>|-text>>>
    <compound|<unquote|<merge|the-|<arg|env>>>>>|<arg|body>|<surround|<set-binding|<compound|<unquote|<merge|the-|<arg|env>>>>>||<arg|caption>>>>>
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