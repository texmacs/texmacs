<TeXmacs|1.0.2.9>

<\body>
  <\with|mode|math>
    <assign|A|\<cal-A\>>

    <assign|B|\<cal-B\>>

    <assign|D|\<cal-D\>>

    <assign|I|<with|math-display|false|<big|int>>>

    <assign|L|\<cal-L\>>

    <assign|R|\<cal-R\>>

    <assign|T|\<cal-T\>>

    <assign|FS|<with|math-font|cal*|F>>

    <assign|GS|<with|math-font|cal*|G>>

    <assign|PS|<with|math-font|cal*|P>>

    <assign|Lin|<with|math-font|cal*|L>>

    <assign|Gb|<with|math-font|cal*|G>>

    <assign|Mlin|<with|math-font|cal*|M>>

    <assign|Mlt|<with|math-font|cal*|T>>

    <assign|Set|<with|math-font|cal*|S>>

    <assign|xor|<space|0.6spc>\<triangledown\><space|0.6spc>>

    \;

    <assign|gb|<macro|x|<with|math-font-family|mt|[<space|-0.6spc>[><arg|x><with|math-font-family|mt|]<space|-0.6spc>]>>>

    <assign|cgb|<macro|x|<with|math-font-family|mt|{<space|-0.6spc>{><arg|x><with|math-font-family|mt|}<space|-0.6spc>}>>>

    <assign|gbt|<macro|x|<with|math-font-family|mt|[<space|-0.6spc>[<space|-0.6spc>[><arg|x><with|math-font-family|mt|]<space|-0.6spc>]<space|-0.6spc>]>>>

    <assign|cgbt|<macro|x|<with|math-font-family|mt|{<space|-0.6spc>{<space|-0.6spc>{><arg|x><with|math-font-family|mt|}<space|-0.6spc>}<space|-0.6spc>}>>>

    \;

    <assign|head|<macro|x|<arg|x><rsup|\<sharp\>>>>

    <assign|tail|<macro|x|<arg|x><rsup|\<flat\>>>>

    <assign|hm|<macro|i|x|\<sharp\><rsub|<arg|i>> <arg|x>>>

    <assign|tm|<macro|i|x|\<flat\><rsub|<arg|i>> <arg|x>>>

    <assign|lcut|<macro|x|<wide|<arg|x>|\<wide-varleftarrow\>>>>

    <assign|rcut|<macro|x|<wide|<arg|x>|\<wide-varrightarrow\>>>>

    \;
  </with>

  <assign|recall|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<table|<row|<cell|<arg|x>>>>>>>>

  <assign|fold|<macro|x|y|<surround||<hflush><with|color|blue|<with|mode|math|\<cdots\>>>|<arg|x>>>>

  <assign|unfold|<\macro|x|y>
    <arg|x>

    <arg|y>
  </macro>>

  <assign|solution|<\macro|x>
    <exercise*|<localize|Solution>|<arg|x>>
  </macro>>

  <assign|annote|<macro|i|<with|font-family|ss|<with|color|brown|[><with|color|dark
  blue|<arg|i>><with|color|brown|]>>>>

  <assign|correct|<macro|old|new|<with|color|brown|[><with|color|dark
  blue|<arg|old>><with|color|brown|<with|mode|math|\<rightarrow\>>><with|color|dark
  blue|<arg|new>><with|color|brown|]>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>