<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|framed-envs|1.0>

    <\src-purpose>
      An example style package for fancy frames around environments.
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

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
      Framed theorems
    </src-comment>
  </active*>

  <assign|unframed-render-remark|<value|render-remark>>

  <assign|render-remark|<\macro|which|body>
    <\padded-normal|1fn|1fn>
      <\wide-std-framed-colored|dark blue|pastel yellow>
        <\unframed-render-remark|<arg|which>>
          <arg|body>
        </unframed-render-remark>
      </wide-std-framed-colored>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>