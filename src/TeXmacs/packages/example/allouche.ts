<TeXmacs|0.3.4.12>

<\body>
  <with|font series|bold|Standard macros for help files>

  <assign|TeXmacs|<macro|T<rsub|<space|-0.4spc><move|<resize|<with|index
  level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><mov\
  e|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>>>

  <assign|scheme|<with|font shape|small-caps|Scheme>>

  <assign|tmdef|<macro|concept|<with|font shape|italic|<arg|concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<expand|block*|<tformat|<table|<row|<cell|<arg|whi\
  ch>>>>>>>>

  <assign|menu|<macro|name|<with|font family|ss|<arg|name>>>>

  <assign|submenu|<macro|name|sub|<with|font
  family|ss|<arg|name><with|mode|math|\<rightarrow\>><arg|sub>>>>

  <assign|subsubmenu|<macro|name|sub|subsub|<with|font
  family|ss|<arg|name><with|mode|math|\<rightarrow\>><arg|sub><with|mode|math\
  |\<rightarrow\>><arg|subsub>>>>

  <assign|at|@>

  <assign|underscore|<with|font family|tt|_>>

  <assign|TeX|<macro|T<rsub|<space|-0.4spc><move|<resize|<with|index
  level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>

  <assign|LaTeX|<macro|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2s\
  pc>T<rsub|<space|-0.4spc><move|<resize|<with|index
  level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>

  <assign|pari|<with|font shape|small-caps|Pari>>

  \;

  <with|font series|bold|Additional layout and code snippets macros>

  <assign|noparskip|<macro|body|<with|interparagraph
  space|0cm|<arg|body>><vspace|<value|interparagraph space>>>>

  <assign|indent step|4spc>

  <assign|indent|<macro|body|<with|left margin|<plus|<apply|left
  margin>|<apply|indent step>>|<arg|body>>>>

  <assign|code|<macro|c|<indent|<with|interparagraph
  space|0|<surround|<format|enable first indentation>|<vspace|0.5fn>|<arg|c>>\
  >>>>

  <assign|NULL|<with|font family|tt|NULL>>

  <assign|visibility|<macro|x|<with|left margin|<minus|<value|left
  margin>|<value|indent step>>|<keyw|<arg|x>:>>>>

  \;

  <with|font series|bold|Shell and filesystem related macros>

  <assign|sh|<macro|arg1|<with|font family|tt|<arg|arg1>>>>

  Name of a program that can be typed on the shell prompt or asked about
  using man, or info.

  <assign|bash|<macro|arg1|<with|font family|tt|<arg|arg1>>>>

  Fragment of a script for the <sh|bash> shell. On a GNU/Linux system, this
  can generally be typed on the command-line prompt.

  <assign|shopt|<macro|text|<with|font family|tt|<arg|text>>>>

  Option that can be passed to some shell command.

  <assign|file|<macro|name|<with|font family|tt|<arg|name>>>>

  Name of a source file.

  <assign|dir|<macro|name|<with|font family|tt|<arg|name>>>>

  Name of a filesystem directory.

  <assign|path|<macro|thedir|thefile|<dir|<arg|thedir>><with|font
  family|tt|/><file|<arg|thefile>>>>

  Full path.

  <assign|c++|<group|<with|font family|ss|C><space|-.75spc>+<space|-.75spc>+>\
  >

  Name of C++ language

  <assign|c|<with|font family|ss|C>>

  Name of C language

  <\with|font series|bold>
    \;

    Basic C++ language macros
  </with>

  <assign|var|<macro|name|<with|font family|tt|<with|color|brown|<arg|name>>>\
  >>

  Name of a variable found in the source.

  <assign|type|<macro|name|<with|font family|tt|<with|color|green|<arg|name>>\
  >>>

  Name of a data type, structure, class, etc. found in the source.

  <assign|const|<macro|name|<with|font family|tt|<arg|name>>>>

  Name of a constant value (#define, const variable, etc.)

  <assign|func|<macro|name|<with|font family|tt|<with|color|blue|<arg|name>>>\
  >>

  Name of a function, a method or a paraterized macro.

  <assign|oper|<macro|op|<if|<equal|<arg|op>|<quote|-\<gtr\>>>|<with|mode|mat\
  h|\<rightarrow\>>|<if|<equal|<arg|op>|<quote|*>>|<with|mode|math|\<ast\>>|<\
  if|<equal|<arg|op>|<quote|!=>>|<with|mode|math|\<neq\>>|<with|font
  family|tt|<arg|op>>>>>>>

  A C++ operator, can pretty print som operators.

  <assign|qmeth|<macro|class|meth|<type|<arg|class>><oper|::><expand|func|<ar\
  g|meth>><expand|func|>>>

  A qualified method name.

  <assign|ptrmsg|<macro|class|meth|<type|<arg|class>><oper|-\<gtr\>><expand|f\
  unc|<arg|meth>><expand|func|>>>

  A message sent to a object of a given class after dereferencing (with the
  -\<gtr\> operator).

  <assign|keyw|<macro|word|<with|font family|ss|<arg|word>>>>

  A C++ keyword.

  \;

  <with|font series|bold|Advanced C++ language macros>

  <assign|module|<macro|name|<with|font family|tt|<arg|name>>>>

  Name of a gencc module.

  <assign|parameter|<macro|thetype|thename|default|<type|<arg|thetype>><if|<u\
  nequal||<arg|thename>>| ><var|<arg|thename><if|<unequal|<arg|default>|>|<wi\
  th|mode|math|=>><with|font family|tt|<arg|default>>>>>

  Function parameter (utility macro). Separating space is not displayed if
  name is empty

  <assign|funcproto|<macro|r|f|p|<type|<arg|r>><if|<equal|<arg|r>|>||
  ><expand|func|<arg|f>>(<with|param-sep||param|<macro|t|n|<value|param-sep><\
  type|<arg|t>><if|<equal|<arg|n>|>|| ><var|<arg|n>><assign|param-sep|,
  >>|<arg|p>>)>>

  A function prototype without a structred parameter signature

  <assign|funcproto0|<macro|ret|name|type1|name1|<funcproto|<arg|ret>|<arg|na\
  me>|>>>

  A function prototype without parameter.

  <assign|funcproto1|<macro|ret|name|type1|name1|<funcproto|<arg|ret>|<arg|na\
  me>|<parameter|<arg|type1>|<arg|name1>>>>>

  A function prototype with one parameter.

  <assign|funcproto2|<macro|ret|name|type1|name1|type2|name2|<funcproto|<arg|\
  ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>,
  <parameter|<arg|type2>|<arg|name2>>>>>

  <assign|funcproto3|<macro|ret|name|type1|name1|type2|name2|type3|name3|<fun\
  cproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>,
  <parameter|<arg|type2>|<arg|name2>>, <parameter|<arg|type3>|<arg|name3>>>>>

  <assign|funcproto4|<macro|ret|name|type1|name1|type2|name2|type3|name3|type\
  4|name4|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|<arg|name1>>\
  , <parameter|<arg|type2>|<arg|name2>>, <parameter|<arg|type3>|<arg|name3>>,
  <parameter|<arg|type4>|<arg|name4>>>>>

  <assign|funcproto5|<macro|ret|name|type1|name1|type2|name2|type3|name3|type\
  4|name4|type5|name5|<funcproto|<arg|ret>|<arg|name>|<parameter|<arg|type1>|\
  <arg|name1>>, <parameter|<arg|type2>|<arg|name2>>,
  <parameter|<arg|type3>|<arg|name3>>, <parameter|<arg|type4>|<arg|name4>>,
  <parameter|<arg|type5>|<arg|name5>>>>>

  Idem with two, three and more parameters.

  \;

  <with|font series|bold|Scheme language macros>

  <assign|svar|<macro|name|<with|font family|tt|<arg|name>>>>

  Name of a Scheme variable.

  <assign|sfunc|<macro|name|<with|font family|tt|<arg|name>>>>

  Name of a Scheme function.

  \;

  <with|font series|bold|Old definitions>

  <assign|cmnt|<macro|x|<htab|5mm><with|font size|0.84|<arg|x>>>>

  <assign|cpp|<macro|x|<with|font family|tt|<arg|x>>>>

  <assign|class|<macro|thename|body|<surround|<assign|theclass|<arg|thename>>\
  <subsection|Class <apply|type|<apply|theclass>>>||<arg|body>>>>

  <assign|interface|<macro|thelabel|body|<surround|<subsubsection|<arg|thelab\
  el>>|<vspace|1fn>|<with|interparagraph space|0|first
  indent|2fn|<surround|<format|enable indentation after>||<arg|body>>>>>>

  <assign|members|<macro|body|<interface|Members|<arg|body>>>>

  <assign|funcproto|<macro|r|f|p|<type|<arg|r>><if|<equal|<arg|r>|>||
  ><expand|func|<arg|f>>(<with|param-sep||param|<macro|t|n|<value|param-sep><\
  type|<arg|t>><if|<equal|<arg|n>|>|| ><var|<arg|n>><assign|param-sep|,
  >>|<arg|p>>)>>

  <assign|//|<macro|//|<htab|5mm><with|font size|0.84|<arg|//>>>>

  \;

  <with|font series|bold|Definitions which should become obsolete>

  <assign|theparams|><assign|paramsep|><assign|thecomment|><assign|thereturn|\
  >

  <assign|function|<func|name|body|<assign|theparams|><assign|paramsep|><assi\
  gn|thecomment|><assign|thereturn|><with|themethod|<value|name>|paramsep||th\
  eparams||thecomment||thereturn||<value|body><value|thereturn><with|font
  family|tt|<with|color|blue|<value|themethod>>>
  (<value|theparams>)<htab|5mm><with|font size|0.84|<value|thecomment>>>>>

  <assign|constructor|<func|body|<apply|function|<value|theclass>|<assign|the\
  return|><value|body>>>>

  <assign|data|<func|thetype|thename|thecomment|<with|font
  family|tt|color|green|<value|thetype>> <with|font
  family|tt|color|brown|<value|thename>><htab|5mm><value|thecomment>>>

  <assign|comment|<func|text|<assign|thecomment|<value|text>>>>

  <assign|!param|<func|x|<assign|theparams|<value|theparams><value|paramsep><\
  value|x>><assign|paramsep|, >>>

  <assign|param|<func|thetype|thename|<apply|!param|<with|font
  family|tt|color|green|<value|thetype>> <with|font
  family|tt|color|brown|<value|thename>>>>>

  <assign|optparam|<func|thetype|thename|thedefault|<apply|!param|<with|font
  family|tt|color|green|<value|thetype>> <with|font
  family|tt|color|brown|<value|thename>=<value|thedefault>>>>>

  <assign|return|<func|thetype|<assign|thereturn|<with|font
  family|tt|color|green|<value|thetype>> >>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|odd page margin|1in>
    <associate|paragraph width|6.5in>
    <associate|page right margin|1in>
    <associate|page top margin|1in>
    <associate|reduction page right margin|0.7in>
    <associate|reduction page bottom margin|0.3in>
    <associate|page type|letter>
    <associate|reduction page left margin|0.7in>
    <associate|even page margin|1in>
    <associate|page bottom margin|1in>
    <associate|reduction page top margin|0.3in>
  </collection>
</initial>
