<TeXmacs|1.0.1.22>

<\body>
  <\with|mode|math>
    <assign|A|\<cal-A\>>

    <assign|B|\<cal-B\>>

    <assign|D|\<cal-D\>>

    <assign|I|<with|formula style|false|<big|int>>>

    <assign|L|\<cal-L\>>

    <assign|R|\<cal-R\>>

    <assign|T|\<cal-T\>>

    <assign|FS|<with|math font|cal*|F>>

    <assign|GS|<with|math font|cal*|G>>

    <assign|PS|<with|math font|cal*|P>>

    <assign|Lin|<with|math font|cal*|L>>

    <assign|Gb|<with|math font|cal*|G>>

    <assign|Mlin|<with|math font|cal*|M>>

    <assign|Mlt|<with|math font|cal*|T>>

    <assign|Set|<with|math font|cal*|S>>

    \;

    <assign|gb|<macro|x|<with|math font family|mt|[<space|-0.6spc>[><arg|x><with|math
    font family|mt|]<space|-0.6spc>]>>>

    <assign|cgb|<macro|x|<with|math font family|mt|{<space|-0.6spc>{><arg|x><with|math
    font family|mt|}<space|-0.6spc>}>>>

    <assign|gbt|<macro|x|<with|math font family|mt|[<space|-0.6spc>[<space|-0.6spc>[><arg|x><with|math
    font family|mt|]<space|-0.6spc>]<space|-0.6spc>]>>>

    <assign|cgbt|<macro|x|<with|math font
    family|mt|{<space|-0.6spc>{<space|-0.6spc>{><arg|x><with|math font
    family|mt|}<space|-0.6spc>}<space|-0.6spc>}>>>

    \;

    <assign|head|<macro|x|<arg|x><rsup|\<sharp\>>>>

    <assign|tail|<macro|x|<arg|x><rsup|\<flat\>>>>

    <assign|hm|<macro|i|x|\<sharp\><rsub|<arg|i>> <arg|x>>>

    <assign|tm|<macro|i|x|\<flat\><rsub|<arg|i>> <arg|x>>>

    \;
  </with>

  <assign|recall|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell
  background|pastel red>|<table|<row|<cell|<arg|x>>>>>>>>

  <assign|fold|<macro|x|y|<surround||<apply|hflush><with|color|blue|<with|mode|math|\<cdots\>>>|<arg|x>>>>

  <assign|unfold|<\macro|x|y>
    <arg|x>

    <arg|y>
  </macro>>

  <assign|solution|<\macro|x>
    <expand|exercise*|<apply|localize|Solution>|<arg|x>>
  </macro>>

  <assign|annote|<macro|i|<with|font family|ss|<with|color|brown|[><with|color|dark
  blue|<arg|i>><with|color|brown|]>>>>

  <assign|correct|<macro|old|new|<with|color|brown|[><with|color|dark
  blue|<arg|old>><with|color|brown|<with|mode|math|\<rightarrow\>>><with|color|dark
  blue|<arg|new>><with|color|brown|]>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|odd page margin|30mm>
    <associate|paragraph width|150mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
  </collection>
</initial>
