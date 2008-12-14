<TeXmacs|1.0.3.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|allouche|1.0>

    <\src-purpose>
      An example style package used by David Allouche.
    </src-purpose>

    <src-copyright|1998--2004|David Allouche>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <with|font-series|bold|Standard macros for help files>

  <assign|TeXmacs|<macro|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>>>

  <assign|scheme|<with|font-shape|small-caps|Scheme>>

  <assign|tmdef|<macro|concept|<with|font-shape|italic|<arg|concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<block*|<tformat|<table|<row|<cell|<arg|which>>>>>>>>

  <assign|menu|<macro|name|<with|font-family|ss|<arg|name>>>>

  <assign|submenu|<macro|name|sub|<with|font-family|ss|<arg|name><with|mode|math|\<rightarrow\>><arg|sub>>>>

  <assign|subsubmenu|<macro|name|sub|subsub|<with|font-family|ss|<arg|name><with|mode|math|\<rightarrow\>><arg|sub><with|mode|math|\<rightarrow\>><arg|subsub>>>>

  <assign|at|@>

  <assign|underscore|<with|font-family|tt|_>>

  <assign|TeX|<macro|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>

  <assign|LaTeX|<macro|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>

  <assign|pari|<with|font-shape|small-caps|Pari>>

  \;

  <with|font-series|bold|Additional layout and code snippets macros>

  <assign|noparskip|<macro|body|<with|par-par-sep|0cm|<arg|body>><vspace|<value|par-par-sep>>>>

  <assign|indent step|4spc>

  <assign|indent|<macro|body|<with|par-left|<plus|<value|par-left>|<indent\ step>>|<arg|body>>>>

  <assign|code|<macro|c|<indent|<with|par-par-sep|0|<surround|<yes-indent>|<vspace|0.5fn>|<arg|c>>>>>>

  <assign|NULL|<with|font-family|tt|NULL>>

  <assign|visibility|<macro|x|<with|par-left|<minus|<value|par-left>|<value|indent
  step>>|<keyw|<arg|x>:>>>>

  \;

  <with|font-series|bold|Shell and filesystem related macros>

  <assign|sh|<macro|arg1|<with|font-family|tt|<arg|arg1>>>>

  Name of a program that can be typed on the shell prompt or asked about
  using man, or info.

  <assign|bash|<macro|arg1|<with|font-family|tt|<arg|arg1>>>>

  Fragment of a script for the <sh|bash> shell. On a GNU/Linux system, this
  can generally be typed on the command-line prompt.

  <assign|shopt|<macro|text|<with|font-family|tt|<arg|text>>>>

  Option that can be passed to some shell command.

  <assign|file|<macro|name|<with|font-family|tt|<arg|name>>>>

  Name of a source file.

  <assign|dir|<macro|name|<with|font-family|tt|<arg|name>>>>

  Name of a filesystem directory.

  <assign|path|<macro|thedir|thefile|<dir|<arg|thedir>><with|font-family|tt|/><file|<arg|thefile>>>>

  Full path.

  <assign|c++|<group|<with|font-family|ss|C><space|-.75spc>+<space|-.75spc>+>>

  Name of C++ language

  <assign|c|<with|font-family|ss|C>>

  Name of C language

  <\with|font-series|bold>
    \;

    Basic C++ language macros
  </with>

  <assign|var|<macro|name|<with|font-family|tt|<with|color|brown|<arg|name>>>>>

  Name of a variable found in the source.

  <assign|type|<macro|name|<with|font-family|tt|<with|color|green|<arg|name>>>>>

  Name of a data type, structure, class, etc. found in the source.

  <assign|const|<macro|name|<with|font-family|tt|<arg|name>>>>

  Name of a constant value (#define, const variable, etc.)

  <assign|func|<macro|name|<with|font-family|tt|<with|color|blue|<arg|name>>>>>

  Name of a function, a method or a paraterized macro.

  <assign|oper|<macro|op|<if|<equal|<arg|op>|-\<gtr\>>|<with|mode|math|\<rightarrow\>>|<if|<equal|<arg|op>|*>|<with|mode|math|\<ast\>>|<if|<equal|<arg|op>|!=>|<with|mode|math|\<neq\>>|<with|font-family|tt|<arg|op>>>>>>>

  A C++ operator, can pretty print som operators.

  <assign|qmeth|<macro|class|meth|<type|<arg|class>><oper|::><macro|<arg|meth>><macro|>>>

  A qualified method name.

  <assign|ptrmsg|<macro|class|meth|<type|<arg|class>><oper|-\<gtr\>><macro|<arg|meth>><macro|>>>

  A message sent to a object of a given class after dereferencing (with the
  -\<gtr\> operator).

  <assign|keyw|<macro|word|<with|font-family|ss|<arg|word>>>>

  A C++ keyword.

  \;

  <with|font-series|bold|Advanced C++ language macros>

  <assign|module|<macro|name|<with|font-family|tt|<arg|name>>>>

  Name of a gencc module.

  <assign|parameter|<macro|thetype|thename|default|<type|<arg|thetype>><if|<unequal||<arg|thename>>|
  ><var|<arg|thename><if|<unequal|<arg|default>|>|<with|mode|math|=>><with|font-family|tt|<arg|default>>>>>

  Function parameter (utility macro). Separating space is not displayed if
  name is empty

  <assign|funcproto|<macro|r|f|p|<type|<arg|r>><if|<equal|<arg|r>|>||
  ><macro|<arg|f>>(<with|param-sep||param|<macro|t|n|<value|param-sep><type|<arg|t>><if|<equal|<arg|n>|>||
  ><var|<arg|n>><assign|param-sep|, >>|<arg|p>>)>>

  A function prototype without a structred parameter signature

  <assign|funcproto0|<macro|ret|name|type1|name1|<funcproto|<arg|ret>|<arg|name>|>>>

  A function prototype without parameter.

  <assign|funcproto1|<macro|ret|name|type1|name1|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>>>>

  A function prototype with one parameter.

  <assign|funcproto2|<macro|ret|name|type1|name1|type2|name2|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>,
  <parameter|<arg|type2>|<arg|name2>>>>>

  <assign|funcproto3|<macro|ret|name|type1|name1|type2|name2|type3|name3|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>,
  <parameter|<arg|type2>|<arg|name2>>, <parameter|<arg|type3>|<arg|name3>>>>>

  <assign|funcproto4|<macro|ret|name|type1|name1|type2|name2|type3|name3|type4|name4|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>,
  <parameter|<arg|type2>|<arg|name2>>, <parameter|<arg|type3>|<arg|name3>>,
  <parameter|<arg|type4>|<arg|name4>>>>>

  <assign|funcproto5|<macro|ret|name|type1|name1|type2|name2|type3|name3|type4|name4|type5|name5|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>,
  <parameter|<arg|type2>|<arg|name2>>, <parameter|<arg|type3>|<arg|name3>>,
  <parameter|<arg|type4>|<arg|name4>>, <parameter|<arg|type5>|<arg|name5>>>>>

  Idem with two, three and more parameters.

  \;

  <with|font-series|bold|Scheme language macros>

  <assign|svar|<macro|name|<with|font-family|tt|<arg|name>>>>

  Name of a Scheme variable.

  <assign|sfunc|<macro|name|<with|font-family|tt|<arg|name>>>>

  Name of a Scheme function.

  \;

  <with|font-series|bold|Old definitions>

  <assign|cmnt|<macro|x|<htab|5mm><with|font-size|0.84|<arg|x>>>>

  <assign|cpp|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|class|<macro|thename|body|<surround|<assign|theclass|<arg|thename>><subsection|Class
  <type|<theclass>>>||<arg|body>>>>

  <assign|interface|<macro|thelabel|body|<surround|<subsubsection|<arg|thelabel>>|<vspace|1fn>|<with|par-par-sep|0|first
  indent|2fn|<surround|<yes-indent*>||<arg|body>>>>>>

  <assign|members|<macro|body|<interface|Members|<arg|body>>>>

  <assign|funcproto|<macro|r|f|p|<type|<arg|r>><if|<equal|<arg|r>|>||
  ><macro|<arg|f>>(<with|param-sep||param|<macro|t|n|<value|param-sep><type|<arg|t>><if|<equal|<arg|n>|>||
  ><var|<arg|n>><assign|param-sep|, >>|<arg|p>>)>>

  <assign|//|<macro|//|<htab|5mm><with|font-size|0.84|<arg|//>>>>

  \;

  <with|font-series|bold|Definitions which should become obsolete>

  <assign|theparams|><assign|paramsep|><assign|thecomment|><assign|thereturn|>

  <assign|function|<macro|name|body|<assign|theparams|><assign|paramsep|><assign|thecomment|><assign|thereturn|><with|themethod|<arg|name>|paramsep||theparams||thecomment||thereturn||<arg|body><value|thereturn><with|font-family|tt|<with|color|blue|<value|themethod>>>
  (<value|theparams>)<htab|5mm><with|font-size|0.84|<value|thecomment>>>>>

  <assign|constructor|<macro|body|<function|<value|theclass>|<assign|thereturn|><arg|body>>>>

  <assign|data|<macro|thetype|thename|thecomment|<with|font-family|tt|color|green|<arg|thetype>>
  <with|font-family|tt|color|brown|<arg|thename>><htab|5mm><arg|thecomment>>>

  <assign|comment|<macro|text|<assign|thecomment|<arg|text>>>>

  <assign|!param|<macro|x|<assign|theparams|<value|theparams><value|paramsep><arg|x>><assign|paramsep|,
  >>>

  <assign|param|<macro|thetype|thename|<param|<with|font-family|tt|color|green|<arg|thetype>>
  <with|font-family|tt|color|brown|<arg|thename>>>>>

  <assign|optparam|<macro|thetype|thename|thedefault|<param|<with|font-family|tt|color|green|<arg|thetype>>
  <with|font-family|tt|color|brown|<arg|thename>=<arg|thedefault>>>>>

  <assign|return|<macro|thetype|<assign|thereturn|<with|font-family|tt|color|green|<arg|thetype>>
  >>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|1in>
    <associate|page-even|1in>
    <associate|page-odd|1in>
    <associate|page-reduce-bot|0.3in>
    <associate|page-reduce-left|0.7in>
    <associate|page-reduce-right|0.7in>
    <associate|page-reduce-top|0.3in>
    <associate|page-right|1in>
    <associate|page-top|1in>
    <associate|page-type|letter>
    <associate|par-width|6.5in>
    <associate|preamble|true>
  </collection>
</initial>