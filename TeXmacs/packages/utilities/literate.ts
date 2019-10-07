<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|literate|1.0>

    <\src-purpose>
      Literate programming based on chunks
    </src-purpose>

    <src-copyright|1998--2015|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(utils literate lp-menu)>

  <\active*>
    <\src-comment>
      Presentation macros
    </src-comment>
  </active*>

  <assign|wide-bothlined-named|<macro|top-border|bot-border|top-sep|bot-sep|body|title|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|2|1|1|cell-width|1par>|<cwith|1|2|1|1|cell-lsep|0pt>|<cwith|1|2|1|1|cell-rsep|0pt>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-tborder|<arg|top-border>>|<cwith|2|2|1|1|cell-bborder|<arg|bot-border>>|<cwith|1|2|1|1|cell-tsep|<arg|top-sep>>|<cwith|1|1|1|1|cell-bsep|<arg|top-sep>>|<cwith|2|2|1|1|cell-bsep|<arg|bot-sep>>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-vcorrect|b>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|title>
  </cell>>|<row|<\cell>
    <arg|body>
  </cell>>>>>>>>

  <assign|wide-std-bothlined-named|<macro|body|title|<wide-bothlined-named|<value|overlined-width>|<value|underlined-width>|<value|overlined-sep>|<value|underlined-sep>|<arg|body>|<arg|title>>>>

  <assign|bothlined-named|<\macro|body|title>
    <padded|<wide-std-bothlined-named|<arg|body>|<arg|title>>>
  </macro>>

  <\active*>
    <\src-comment>
      Generic chunks
    </src-comment>
  </active*>

  <assign|chunk-number|<macro|name|<value|<merge|<arg|name>|-nr>>>>

  <assign|chunk-inc|<macro|name|<if|<provides|<merge|<arg|name>|-nr>>|<assign|<merge|<arg|name>|-nr>|<plus|<value|<merge|<arg|name>|-nr>>|1>>|<assign|<merge|<arg|name>|-nr>|1>>>>

  \;

  <assign|chunk-label|<macro|name|<if|<provides|<merge|<arg|name>|-nr>>|<assign|the-label|<arg|name>><label|<merge|chunk-|<arg|name>|-|<value|<merge|<arg|name>|-nr>>>>>>>

  <assign|chunk-ref|<macro|name|<math|\<langle\>><hlink|<arg|name>|<merge|#chunk-|<arg|name>|-1>><math|\<rangle\>>>>

  <assign|chunk-up|<macro|name|<hlink|<math|\<vartriangle\>>|<merge|#chunk-|<arg|name>|-|<minus|<value|<merge|<arg|name>|-nr>>|1>>>>>

  <assign|chunk-down|<macro|name|<hlink|<math|\<triangledown\>>|<merge|#chunk-|<arg|name>|-|<plus|<value|<merge|<arg|name>|-nr>>|1>>>>>

  \;

  <assign|chunk-link|<macro|name|<if|<unequal|<find-file|<arg|name>>|false>|<slink|<arg|name>>|<arg|name>>>>

  <assign|chunk-this|<macro|name|<with|color|#404050|<chunk-link|<arg|name>>>>>

  <assign|chunk-assign|<macro|prev|next|<if|<arg|prev>|<math|+\<equiv\>>|<if|<arg|next>|:<math|\<equiv\>>|<math|\<equiv\>>>>>>

  <assign|chunk-assign|<macro|prev|next|>>

  \;

  <assign|generic-chunk|<\macro|name|prev|next|body>
    <\surround|<if|<arg|prev>|<chunk-inc|<arg|name>>|<assign|<merge|<arg|name>|-nr>|1>>|>
      <\bothlined-named>
        <\compact>
          <surround|<chunk-label|<arg|name>>||<yes-indent*><arg|body>>
        </compact>
      </bothlined-named|<small|<chunk-this|<arg|name>>
      <chunk-assign|<arg|prev>|<arg|next>><htab|5mm><if|<arg|prev>|<chunk-up|<arg|name>>|<phantom|<math|\<vartriangle\>>>>
      <if|<or|<arg|prev>|<arg|next>>|<chunk-number|<arg|name>>>
      <if|<arg|next>|<chunk-down|<arg|name>>|<phantom|<math|\<triangledown\>>>>>>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Chunks for specific programming languages
    </src-comment>
  </active*>

  <assign|scm-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\scm>
      <arg|body>
    </scm>>
  </macro>>

  <assign|cpp-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\cpp>
      <arg|body>
    </cpp>>
  </macro>>

  <assign|java-chunk|<\macro|name|prev|next|body>
    <\generic-chunk|<arg|name>|<arg|prev>|<arg|next>>
      <\java>
        <arg|body>
      </java>
    </generic-chunk>
  </macro>>

  <assign|mmx-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\mmx>
      <arg|body>
    </mmx>>
  </macro>>

  <assign|python-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\python>
      <arg|body>
    </python>>
  </macro>>

  <assign|scala-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\scala>
      <arg|body>
    </scala>>
  </macro>>

  <assign|scilab-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\scilab>
      <arg|body>
    </scilab>>
  </macro>>

  <assign|shell-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\shell>
      <arg|body>
    </shell>>
  </macro>>

  <assign|verbatim-chunk|<\macro|name|prev|next|body>
    <generic-chunk|<arg|name>|<arg|prev>|<arg|next>|<\verbatim>
      <arg|body>
    </verbatim>>
  </macro>>

  <\active*>
    <\src-comment>
      Macros to keep parts of the chunks invisible while editing. This can
      typically be used for adding some newlines during the actual generation
      of code, or inserting some copyright information into the header.
    </src-comment>
  </active*>

  <assign|unfolded-newline-before|<\macro|body>
    \;

    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>

  <assign|folded-newline-before|<\macro|body>
    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>

  <assign|unfolded-opening|<\macro|before|body>
    <\greyed>
      <\wide-normal>
        <arg|before>
      </wide-normal>
    </greyed>

    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>

  <assign|folded-opening|<\macro|before|body>
    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>

  <assign|unfolded-ending|<\macro|body|after>
    <\wide-normal>
      <arg|body>
    </wide-normal>

    <\greyed>
      <\wide-normal>
        <arg|after>
      </wide-normal>
    </greyed>
  </macro>>

  <assign|folded-ending|<\macro|body|after>
    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>