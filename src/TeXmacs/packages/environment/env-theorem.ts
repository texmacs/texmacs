<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-theorem|1.0>

    <\src-purpose>
      Theorem-like environments.
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

  <assign|theorem-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|exercise-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|theorem-sep|<macro|. >>

  <assign|exercise-sep|<macro|. >>

  \;

  <assign|theorem*|<macro|which|body|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|italic|<arg|body>>>>>

  <assign|remark*|<macro|which|body|<theorem*|<arg|which>|<with|font-shape|right|<arg|body>>>>>

  <assign|exercise*|<macro|which|body|<surround|<vspace*|0.5fn><no-indent>|<right-flush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|1.5fn>|font-size|0.84|<surround|<exercise-name|<arg|which><exercise-sep>>||<arg|body>>>>>>

  <assign|proof*|<macro|which|body|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<space|0.5fn><right-flush><with|mode|math|\<box\>><vspace|1fn>|<arg|body>>>>

  \;

  <assign|dueto|<macro|name|<with|font-shape|right|<theorem-name|(<arg|name>)
  >>>>

  <assign|corollary*|<macro|body|<theorem*|<translate|Corollary|english|<language>>|<arg|body>>>>

  <assign|proof|<\macro|body>
    <proof*|<translate|Proof|english|<language>>|<arg|body>>
  </macro>>

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