<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-manage|1.0>

    <\src-purpose>
      Managing groups of environments.
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

  <new-counter-group|figure-env>

  <add-to-counter-group|figure-env|std-env>

  <add-to-counter-group|equation|std-env>

  <add-to-counter-group|footnote|std-en>

  <assign|resetstdenv|<macro|<reset-std-env>>>

  <assign|display-std-env|<macro|nr|<the-prefix><arg|nr>>>

  <\active*>
    <\src-comment>
      Defining new block environments with one parameter.
    </src-comment>
  </active*>

  <assign|new-env|<macro|env|name|group|render|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|<unquote|<arg|group>>><assign|<unquote|<arg|env>>|<\macro|body>
    <surround|<compound|<unquote|<merge|next-|<arg|env>>>>||<style-with|src-compact|none|<compound|<unquote|<arg|render>>|<localize|<unquote|<arg|name>>>
    <compound|<unquote|<merge|the-|<arg|env>>>>|<arg|body>>>>
  </macro>>>>>>

  <assign|new-theorem|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|theorem*>>>

  <assign|new-remark|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|remark*>>>

  <assign|new-exercise|<macro|env|name|<new-env|<arg|env>|<arg|name>|exercise-env|exercise*>>>

  <\active*>
    <\src-comment>
      Defining new figure-like environments.
    </src-comment>
  </active*>

  <assign|new-figure|<macro|env|name|<quasi|<style-with|src-compact|none|<add-to-counter-group|<unquote|<arg|env>>|figure-env><assign|<unquote|<merge|small-|<arg|env>>>|<macro|body|caption|<style-with|src-compact|none|<compound|<unquote|<merge|next-|<arg|env>>>><style-with|src-compact|none|<small-figure*|<unquote|<arg|env>>|<localize|<unquote|<arg|name>>>
  <compound|<unquote|<merge|the-|<arg|env>>>>|<arg|body>|<arg|caption>>>>>><assign|<unquote|<merge|big-|<arg|env>>>|<\macro|body|caption>
    <surround|<compound|<unquote|<merge|next-|<arg|env>>>>||<style-with|src-compact|none|<big-figure*|<unquote|<arg|env>>|<localize|<unquote|<arg|name>>>
    <compound|<unquote|<merge|the-|<arg|env>>>>|<arg|body>|<arg|caption>>>>
  </macro>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>