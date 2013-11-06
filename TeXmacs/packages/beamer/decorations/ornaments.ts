<TeXmacs|1.0.7.21>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|ornaments|1.0|ornaments|1.0>

    <\src-purpose>
      Special ornaments for presentations.
    </src-purpose>

    <src-copyright|2007--2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Customizations
    </src-comment>
  </active*>

  <assign|granite|<macro|x|<with|ornament-color|<pattern|granite-dark.png|*3/5|*3/5|#101010>|color|white|strong-color|#f0ffb0|math-color|#ffd4c0|ornament-sunny-color|light
  grey|ornament-shadow-color|dark grey|<ornament|<arg|x>>>>>

  <assign|manila-paper|<macro|x|<with|ornament-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#d0d0c0>|ornament-sunny-color|#e8e8e0|ornament-shadow-color|#acac90|<ornament|<arg|x>>>>>

  <assign|metal|<macro|x|<with|ornament-color|<pattern|metal-brushed-medium.png|*3/5|*3/5|#c0c0d0>|ornament-sunny-color|#e0e0e8|ornament-shadow-color|#9090ac|<ornament|<arg|x>>>>>

  <assign|pine|<macro|x|<with|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>|strong-color|#0c3000|math-color|#500000|ornament-sunny-color|#ffe8c0|ornament-shadow-color|brown|<ornament|<arg|x>>>>>

  <assign|ridged-paper|<macro|x|<with|ornament-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcdc>|ornament-sunny-color|#f0e0e0|ornament-shadow-color|#d0a0a0|<ornament|<arg|x>>>>>

  <assign|rough-paper|<macro|x|<with|ornament-color|<pattern|paper-rough-medium.png|*3/5|*3/5|#dcdcdc>|ornament-sunny-color|#e0e0e0|ornament-shadow-color|#a0a0a0|<ornament|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Ringed ornaments
    </src-comment>
  </active*>

  <assign|ring-width|1fn>

  <assign|ring-indent|1fn>

  <assign|ring-pattern|<pattern|$TEXMACS_PATH/misc/images/ring-binder-1.png|100%||#fff0>>

  <assign|ring-ornamented|<\macro|body>
    <\with|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|ornament-shape|ring|ornament-color|<value|ring-pattern>|ornament-hpadding|<over|<value|ring-width>|2>|ornament-vpadding|0spc>
      <\ornamented>
        <with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-hpadding|<tuple|<plus|<value|old-hpadding>|<over|<value|ring-width>|2>|<value|ring-indent>>|<value|old-hpadding>>|ornament-vpadding|<value|old-vpadding>|<\ornament>
          <\surround||<right-flush>>
            <\with|ornament-hpadding|<value|old-hpadding>>
              <arg|body>
            </with>
          </surround>
        </ornament>>
      </ornamented>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>