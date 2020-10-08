<TeXmacs|1.99.13>

<style|<tuple|source|english>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|automate|1.0|automate|1.0>

    <\src-purpose>
      Style package for automated document generation
    </src-purpose>

    <src-copyright|2015|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(auto-menu)>

  <\active*>
    <\src-comment>
      Style parameters.
    </src-comment>
  </active*>

  <if|<greater|<value|par-par-sep>|0.25fn>|<assign|par-par-sep|0.25fn>>

  \;

  <assign|auto-border|1ln>

  <assign|auto-sep|1ln>

  <assign|auto-lpadding|0.5fn>

  <assign|auto-rpadding|0.5fn>

  <assign|auto-vpadding|0.3333fn>

  <assign|auto-band-width|0.75fn>

  <\active*>
    <\src-comment>
      Simple presentation macros
    </src-comment>
  </active*>

  <assign|auto-keyword|<macro|name|<with|font-series|bold|font-shape|small-caps|<arg|name>>>>

  <\active*>
    <\src-comment>
      Presentation markup for block structures
    </src-comment>
  </active*>

  <assign|block-render|<\macro|bgc|body>
    <\surround|<no-indent>|>
      <\with|prev-shape|<value|ornament-shape>|prev-color|<value|ornament-color>|prev-hpadding|<value|ornament-hpadding>|prev-vpadding|<value|ornament-vpadding>|prev-border|<value|ornament-border>|prev-sunny-color|<value|ornament-sunny-color>|prev-shadow-color|<value|ornament-shadow-color>|ornament-shape|classic|ornament-color|<blend|#ffffffc0|<arg|bgc>>|ornament-hpadding|<tuple|<value|auto-lpadding>|<value|auto-rpadding>>|ornament-vpadding|<value|auto-vpadding>|ornament-border|<value|auto-border>|ornament-sunny-color|<blend|#ffffffe0|<arg|bgc>>|ornament-shadow-color|<blend|#ffffff80|<arg|bgc>>>
        <\ornament>
          <\with|ornament-shape|<value|prev-shape>|ornament-color|<value|prev-color>|ornament-hpadding|<value|prev-hpadding>|ornament-vpadding|<value|prev-vpadding>|ornament-boder|<value|prev-border>|ornament-sunny-color|<value|prev-sunny-color>|ornament-shadow-color|<value|prev-shadow-color>>
            <\surround|<no-indent>|<right-flush>>
              <arg|body>
            </surround>
          </with>
        </ornament>
      </with>
    </surround>
  </macro>>

  <assign|block-indent|<\macro|bgc|body>
    <\surround|<no-indent>|>
      <\with|prev-shape|<value|ornament-shape>|prev-color|<value|ornament-color>|prev-hpadding|<value|ornament-hpadding>|prev-vpadding|<value|ornament-vpadding>|prev-border|<value|ornament-border>|prev-sunny-color|<value|ornament-sunny-color>|prev-shadow-color|<value|ornament-shadow-color>|ornament-shape|band|ornament-color|<blend|#ffffffc0|<arg|bgc>>|ornament-hpadding|<over|<value|auto-band-width>|2>|ornament-vpadding|<tuple|0spc|<value|auto-vpadding>>|ornament-border|<value|auto-border>|ornament-sunny-color|<blend|#ffffffe0|<arg|bgc>>|ornament-shadow-color|<blend|#ffffff80|<arg|bgc>>>
        <\ornament>
          <\with|ornament-shape|<value|prev-shape>|ornament-color|<value|prev-color>|ornament-hpadding|<value|prev-hpadding>|ornament-vpadding|<value|prev-vpadding>|ornament-boder|<value|prev-border>|ornament-sunny-color|<value|prev-sunny-color>|ornament-shadow-color|<value|prev-shadow-color>>
            <\surround||<right-flush>>
              <\ornament-indent|<value|auto-lpadding>|0fn|0fn|0fn>
                <arg|body>
              </ornament-indent>
            </surround>
          </with>
        </ornament>
      </with>
    </surround>
  </macro>>

  <assign|block-unary|<\macro|bgc|body|arg1>
    <\with|old-par-ver-sep|<value|par-ver-sep>|old-par-sep|<value|par-sep>|old-par-par-sep|<value|par-par-sep>|old-par-first|<value|par-first>|par-ver-sep|<value|auto-sep>|par-sep|0fn|par-par-sep|0fn|par-first|0fn>
      <\surround||<vspace|<value|old-par-par-sep>>>
        <\block-render|<arg|bgc>>
          <\with|par-ver-sep|<value|old-par-ver-sep>|par-sep|<value|old-par-sep>|par-par-sep|<value|old-par-par-sep>|par-first|<value|old-par-first>>
            <arg|body>
          </with>
        </block-render>

        <\block-indent|<arg|bgc>>
          <\with|par-ver-sep|<value|old-par-ver-sep>|par-sep|<value|old-par-sep>|par-par-sep|<value|old-par-par-sep>|par-first|<value|old-par-first>>
            <arg|arg1>
          </with>
        </block-indent>
      </surround>
    </with>
  </macro>>

  <assign|block-unary-block-unary|<\macro|bgc|body1|arg1|body2|arg2>
    <\with|old-par-ver-sep|<value|par-ver-sep>|old-par-sep|<value|par-sep>|old-par-par-sep|<value|par-par-sep>|old-par-first|<value|par-first>|par-ver-sep|<value|auto-sep>|par-sep|0fn|par-par-sep|0fn|par-first|0fn>
      <\surround||<vspace|<value|old-par-par-sep>>>
        <\block-render|<arg|bgc>>
          <\with|par-ver-sep|<value|old-par-ver-sep>|par-sep|<value|old-par-sep>|par-par-sep|<value|old-par-par-sep>|par-first|<value|old-par-first>>
            <arg|body1>
          </with>
        </block-render>

        <\block-indent|<arg|bgc>>
          <\with|par-ver-sep|<value|old-par-ver-sep>|par-sep|<value|old-par-sep>|par-par-sep|<value|old-par-par-sep>|par-first|<value|old-par-first>>
            <arg|arg1>
          </with>
        </block-indent>

        <\block-render|<arg|bgc>>
          <\with|par-ver-sep|<value|old-par-ver-sep>|par-sep|<value|old-par-sep>|par-par-sep|<value|old-par-par-sep>|par-first|<value|old-par-first>>
            <arg|body2>
          </with>
        </block-render>

        <\block-indent|<arg|bgc>>
          <\with|par-ver-sep|<value|old-par-ver-sep>|par-sep|<value|old-par-sep>|par-par-sep|<value|old-par-par-sep>|par-first|<value|old-par-first>>
            <arg|arg2>
          </with>
        </block-indent>
      </surround>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Block structures
    </src-comment>
  </active*>

  <assign|block-if|<\macro|cond|body>
    <\block-unary|brown|<auto-keyword|if> <arg|cond>>
      <arg|body>
    </block-unary>
  </macro>>

  <assign|block-if-else|<\macro|cond|body1|body2>
    <\block-unary-block-unary|brown|<auto-keyword|if> <arg|cond>>
      <arg|body1>
    </block-unary-block-unary|<auto-keyword|else>|<arg|body2>>
  </macro>>

  <assign|block-for|<\macro|var|list|body>
    <\block-unary|brown|<auto-keyword|for> <arg|var> <auto-keyword|in>
    <arg|list>>
      <arg|body>
    </block-unary>
  </macro>>

  <assign|block-while|<\macro|cond|body>
    <\block-unary|brown|<auto-keyword|while> <arg|cond>>
      <arg|body>
    </block-unary>
  </macro>>

  <assign|block-assign|<\macro|var|val>
    <block-render|brown|<arg|var> <math|\<longleftarrow\>> <arg|val>>
  </macro>>

  <assign|block-intersperse|<\macro|sep|body>
    <\block-unary|brown|<auto-keyword|intersperse> <arg|sep>>
      <arg|body>
    </block-unary>
  </macro>>

  <assign|block-texmacs-tag-extra|<macro|x|<space|0.2spc>\|<space|0.2spc><arg|x>>>

  <assign|block-texmacs-tag|<\xmacro|args>
    <\block-unary|blue|<auto-keyword|<arg|args|0>><map-args|block-texmacs-tag-extra|concat|args|1|<minus|<get-arity|<quote-arg|args>>|1>>>
      <arg|args|<minus|<get-arity|<quote-arg|args>>|1>>
    </block-unary>
  </xmacro>>

  <drd-props|block-texmacs-tag|arity|<tuple|repeat*|1|2>|accessible|all>

  <\active*>
    <\src-comment>
      Presentation markup for inline structures
    </src-comment>
  </active*>

  <assign|inline-render|<macro|bgc|body|<with|color|<arg|bgc>|<math|\<langle\>><arg|body><math|\<rangle\>>>>>

  <assign|inline-unary|<macro|bgc|body|arg1|<with|color|<arg|bgc>|<math|\<langle\>><arg|body>\|><arg|arg1><with|color|<arg|bgc>|<math|\<rangle\>>>>>

  <assign|inline-unary-inline-unary|<macro|bgc|body1|arg1|body2|arg2|<with|color|<arg|bgc>|<math|\<langle\>><arg|body1>\|><arg|arg1><with|color|<arg|bgc>|\|<arg|body2>\|><arg|arg2><with|color|<arg|bgc>|<math|\<rangle\>>>>>

  <\active*>
    <\src-comment>
      Inline structures
    </src-comment>
  </active*>

  <assign|inline-if|<macro|cond|body|<inline-unary|brown|<auto-keyword|if>
  <arg|cond>|<arg|body>>>>

  <assign|inline-if-else|<macro|cond|body1|body2|<inline-unary-inline-unary|brown|<auto-keyword|if>
  <arg|cond>|<arg|body1>|<auto-keyword|else>|<arg|body2>>>>

  <assign|inline-for|<macro|var|list|body|<inline-unary|brown|<auto-keyword|for>
  <arg|var> <auto-keyword|in> <arg|list>|<arg|body>>>>

  <assign|inline-while|<macro|cond|body|<inline-unary|brown|<auto-keyword|while>
  <arg|cond>|<arg|body>>>>

  <assign|inline-assign|<macro|var|val|<inline-render|brown|<arg|var>
  <math|\<leftarrow\>> <arg|val>>>>

  <assign|inline-intersperse|<macro|sep|body|<with|old-color|<value|color>|<inline-unary|brown|<auto-keyword|intersperse>
  <with|color|<value|old-color>|<arg|sep>>|<arg|body>>>>>

  <assign|inline-texmacs-tag-extra|<macro|x|<space|0.2spc>\|<space|0.2spc><with|color|<value|prev-color>|<arg|x>>>>

  <assign|inline-texmacs-tag|<xmacro|args|<with|prev-color|color|<inline-render|blue|<auto-keyword|<arg|args|0>><map-args|inline-texmacs-tag-extra|concat|args|1>>>>>

  <\active*>
    <\src-comment>
      Variables
    </src-comment>
  </active*>

  <assign|var-render|<macro|bgc|body|<with|ornament-shape|classic|ornament-color|<blend|#ffffffc0|<arg|bgc>>|ornament-hpadding|0.25ex|ornament-vpadding|0.25ex|ornament-border|<value|auto-border>|ornament-sunny-color|<blend|#ffffffe0|<arg|bgc>>|ornament-shadow-color|<blend|#ffffff80|<arg|bgc>>|<resize|<ornament|<arg|body>>||<plus|1b|0.25ex>||<minus|1t|0.25ex>>>>>

  <assign|swell-var|<macro|body|<resize|<arg|body>||<minimum|1b|-0.25ex>||<maximum|1t|1.25ex>>>>

  <assign|output-string|<macro|body|<var-render|green|<swell-var|<arg|body>>>>>

  <assign|block-output|<macro|body|<block-render|cyan|<arg|body>>>>

  <assign|inline-output|<macro|body|<var-render|cyan|<swell-var|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Support for various programming languages
    </src-comment>
  </active*>

  <assign|implied-generic|<macro|body|<arg|body>>>

  <assign|implied-scm|<macro|body|<scm|<arg|body>>>>

  <assign|implied-cpp|<macro|body|<cpp|<arg|body>>>>

  <assign|implied-mmx|<macro|body|<mmx|<arg|body>>>>

  <assign|implied-python|<macro|body|<python|<arg|body>>>>

  <assign|implied-scilab|<macro|body|<scilab|<arg|body>>>>

  <assign|implied-shell|<macro|body|<shell|<arg|body>>>>

  <assign|implied-verbatim|<macro|body|<verbatim|<arg|body>>>>

  \;

  <drd-props|implied-generic|border|no>

  <drd-props|implied-scm|border|no>

  <drd-props|implied-cpp|border|no>

  <drd-props|implied-mmx|border|no>

  <drd-props|implied-python|border|no>

  <drd-props|implied-scilab|border|no>

  <drd-props|implied-shell|border|no>

  <drd-props|implied-verbatim|border|no>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>