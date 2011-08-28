<TeXmacs|1.0.1.20>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\expand|tmdoc-title>
    Using GNU R sessions inside <TeXmacs>
  </expand>

  GNU <name|R> is an integrated suit of software facilities for data
  manipulation and graphical display. Many people use R as a statistics
  system. But the developers prefer to think of it as an implement within
  which many classical and modern statistical techniques have been
  implemented. You may download <name|R> from

  <\verbatim>
    \ \ \ \ http://www.r-project.org
  </verbatim>

  In order to launch an <name|R> session inside <TeXmacs>, use
  <apply|menu|Insert|Session|R>. The <verbatim|v()> function can be used in
  order to include the contents of the <name|R> graphics window inside your
  worksheet.

  <\session|r|default>
    <\output>
      \<gtr\> Welcome to the TeXmacs interface to R.

      To put the current graph in the TeXmacs buffer as an eps use v().

      The functions plotv(), linesv(), and pointsv() are provided as a
      convenience.

      They do the regular function, and then insert the graph.

      \;

      To change the size of the graph that is inserted to TeXmacs,just adjust
      the size of the X11 window.
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      qnorm(1/2,mean=0,sd=1)
    </input>

    <\output>
      [1] 0
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      x\<less\>-1:100
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      y\<less\>-rnorm(x)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      plot(x,y)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      v()
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 0 0 504
      504\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
      .ps.prolog\n/gs \ { gsave } def\n/gr \ { grestore } def\n/ep \ {
      showpage gr gr } def\n/m \ \ { moveto } def\n/l \ { rlineto } def\n/np
      \ { newpath } def\n/cp \ { closepath } def\n/f \ \ { fill } def\n/o
      \ \ { stroke } def\n/c \ \ { newpath 0 360 arc } def\n/r \ \ { 4 2 roll
      moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch
      rlineto closepath } def\n/p1 \ { stroke } def\n/p2 \ { gsave bg
      setrgbcolor fill grestore newpath } def\n/p3 \ { gsave bg setrgbcolor
      fill grestore stroke } def\n/t \ \ { 6 -2 roll moveto gsave rotate\n
      \ \ \ \ \ \ ps mul neg 0 2 1 roll rmoveto\n \ \ \ \ \ \ 1 index
      stringwidth pop\n \ \ \ \ \ \ mul neg 0 rmoveto show grestore }
      def\n/cl \ { grestore gsave newpath 3 index 3 index moveto 1 index\n
      \ \ \ \ \ \ 4 -1 roll lineto \ exch 1 index lineto lineto\n
      \ \ \ \ \ \ closepath clip newpath } def\n/rgb { setrgbcolor } def\n/s
      \ \ { scalefont setfont } def\n/R \ \ { /Font1 findfont } def\n/B \ \ {
      /Font2 findfont } def\n/I \ \ { /Font3 findfont } def\n/BI \ { /Font4
      findfont } def\n/S \ \ { /Font5 findfont } def\n1 setlinecap 1
      setlinejoin\n% end \ \ .ps.prolog\n%%IncludeResource: font
      Helvetica\n/Helvetica findfont\ndup length dict begin\n \ {1 index /FID
      ne {def} {pop pop} ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n
      \ currentdict\n \ end\n/Font1 exch definefont pop\n%%IncludeResource:
      font Helvetica-Bold\n/Helvetica-Bold findfont\ndup length dict begin\n
      \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font2 exch definefont
      pop\n%%IncludeResource: font Helvetica-Oblique\n/Helvetica-Oblique
      findfont\ndup length dict begin\n \ {1 index /FID ne {def} {pop pop}
      ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n \ currentdict\n
      \ end\n/Font3 exch definefont pop\n%%IncludeResource: font
      Helvetica-BoldOblique\n/Helvetica-BoldOblique findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font4 exch definefont
      pop\n%%IncludeResource: font Symbol\n/Symbol findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n
      \ currentdict\n \ end\n/Font5 exch definefont pop\n%%EndProlog\n%%Page:
      1 1\nbp\n34.44 42.84 486.00 469.31 cl\n34.44 42.84 486.00 469.31 cl\n0
      0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n51.16 275.86 1.58 c p1\n55.39
      122.88 1.58 c p1\n59.61 185.12 1.58 c p1\n63.83 309.09 1.58 c p1\n68.06
      286.38 1.58 c p1\n72.28 194.27 1.58 c p1\n76.50 348.93 1.58 c p1\n80.73
      188.74 1.58 c p1\n84.95 183.40 1.58 c p1\n89.17 411.44 1.58 c p1\n93.40
      99.89 1.58 c p1\n97.62 280.18 1.58 c p1\n101.84 187.84 1.58 c
      p1\n106.07 323.91 1.58 c p1\n110.29 193.11 1.58 c p1\n114.51 135.72
      1.58 c p1\n118.74 296.82 1.58 c p1\n122.96 155.47 1.58 c p1\n127.18
      248.65 1.58 c p1\n131.41 58.64 1.58 c p1\n135.63 307.87 1.58 c
      p1\n139.85 198.36 1.58 c p1\n144.08 135.28 1.58 c p1\n148.30 303.15
      1.58 c p1\n152.52 406.84 1.58 c p1\n156.75 284.12 1.58 c p1\n160.97
      409.03 1.58 c p1\n165.19 259.07 1.58 c p1\n169.42 355.92 1.58 c
      p1\n173.64 162.72 1.58 c p1\n177.86 217.28 1.58 c p1\n182.09 394.53
      1.58 c p1\n186.31 293.09 1.58 c p1\n190.53 255.75 1.58 c p1\n194.76
      278.35 1.58 c p1\n198.98 401.37 1.58 c p1\n203.20 99.91 1.58 c
      p1\n207.43 184.08 1.58 c p1\n211.65 453.51 1.58 c p1\n215.87 195.74
      1.58 c p1\n220.10 361.49 1.58 c p1\n224.32 351.51 1.58 c p1\n228.54
      241.03 1.58 c p1\n232.77 169.00 1.58 c p1\n236.99 331.28 1.58 c
      p1\n241.21 164.97 1.58 c p1\n245.44 342.69 1.58 c p1\n249.66 332.54
      1.58 c p1\n253.88 418.10 1.58 c p1\n258.11 281.46 1.58 c p1\n262.33
      238.42 1.58 c p1\n266.55 251.98 1.58 c p1\n270.78 381.79 1.58 c
      p1\n275.00 399.95 1.58 c p1\n279.22 191.56 1.58 c p1\n283.45 107.38
      1.58 c p1\n287.67 295.07 1.58 c p1\n291.89 379.61 1.58 c p1\n296.12
      167.68 1.58 c p1\n300.34 269.31 1.58 c p1\n304.56 425.28 1.58 c
      p1\n308.79 116.53 1.58 c p1\n313.01 214.11 1.58 c p1\n317.23 254.73
      1.58 c p1\n321.46 245.88 1.58 c p1\n325.68 259.42 1.58 c p1\n329.90
      232.22 1.58 c p1\n334.13 245.46 1.58 c p1\n338.35 386.76 1.58 c
      p1\n342.57 201.53 1.58 c p1\n346.80 277.30 1.58 c p1\n351.02 286.30
      1.58 c p1\n355.24 181.19 1.58 c p1\n359.47 215.51 1.58 c p1\n363.69
      356.82 1.58 c p1\n367.91 434.80 1.58 c p1\n372.14 320.17 1.58 c
      p1\n376.36 247.88 1.58 c p1\n380.58 293.99 1.58 c p1\n384.81 218.00
      1.58 c p1\n389.03 101.31 1.58 c p1\n393.25 190.99 1.58 c p1\n397.48
      248.45 1.58 c p1\n401.70 297.11 1.58 c p1\n405.92 115.64 1.58 c
      p1\n410.15 333.06 1.58 c p1\n414.37 396.91 1.58 c p1\n418.59 363.95
      1.58 c p1\n422.82 128.76 1.58 c p1\n427.04 270.74 1.58 c p1\n431.26
      240.07 1.58 c p1\n435.49 430.33 1.58 c p1\n439.71 294.83 1.58 c
      p1\n443.93 231.24 1.58 c p1\n448.16 305.87 1.58 c p1\n452.38 353.77
      1.58 c p1\n456.60 219.88 1.58 c p1\n460.83 270.96 1.58 c p1\n465.05
      385.81 1.58 c p1\n469.27 286.48 1.58 c p1\n0.00 0.00 503.64 503.75
      cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n46.94 42.84
      m\n422.33 0.00 l\no\nnp\n46.94 42.84 m\n-0.00 -4.20 l\no\nnp\n131.41
      42.84 m\n-0.00 -4.20 l\no\nnp\n215.87 42.84 m\n0.00 -4.20
      l\no\nnp\n300.34 42.84 m\n-0.00 -4.20 l\no\nnp\n384.81 42.84 m\n0.00
      -4.20 l\no\nnp\n469.27 42.84 m\n-0.00 -4.20 l\no\n/ps 12 def R 12
      s\n46.94 27.72 (0) .5 0 0 t\n131.41 27.72 (20) .5 0 0 t\n215.87 27.72
      (40) .5 0 0 t\n300.34 27.72 (60) .5 0 0 t\n384.81 27.72 (80) .5 0 0
      t\n469.27 27.72 (100) .5 0 0 t\nnp\n34.44 43.77 m\n-0.00 376.13
      l\no\nnp\n34.44 43.77 m\n-4.20 0.00 l\no\nnp\n34.44 119.00 m\n-4.20 0
      l\no\nnp\n34.44 194.22 m\n-4.20 -0.00 l\no\nnp\n34.44 269.45 m\n-4.20
      -0.00 l\no\nnp\n34.44 344.67 m\n-4.20 0.00 l\no\nnp\n34.44 419.90
      m\n-4.20 -0.00 l\no\n24.36 43.77 (-3) .5 0 90 t\n24.36 119.00 (-2) .5 0
      90 t\n24.36 194.22 (-1) .5 0 90 t\n24.36 269.45 (0) .5 0 90 t\n24.36
      344.67 (1) .5 0 90 t\n24.36 419.90 (2) .5 0 90 t\nnp\n34.44 42.84
      m\n451.56 0.00 l\n0 426.47 l\n-451.56 0.00 l\n-0.00 -426.47 l\no\n0.00
      0.00 503.64 503.75 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n260.22 10.92 (x)
      .5 0 0 t\n7.56 256.07 (y) .5 0 90 t\nep\n%%Trailer\n%%Pages:
      1\n%%EOF\n>|ps>|/2|/2||||>
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      plot(x)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      v()
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 0 0 504
      504\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
      .ps.prolog\n/gs \ { gsave } def\n/gr \ { grestore } def\n/ep \ {
      showpage gr gr } def\n/m \ \ { moveto } def\n/l \ { rlineto } def\n/np
      \ { newpath } def\n/cp \ { closepath } def\n/f \ \ { fill } def\n/o
      \ \ { stroke } def\n/c \ \ { newpath 0 360 arc } def\n/r \ \ { 4 2 roll
      moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch
      rlineto closepath } def\n/p1 \ { stroke } def\n/p2 \ { gsave bg
      setrgbcolor fill grestore newpath } def\n/p3 \ { gsave bg setrgbcolor
      fill grestore stroke } def\n/t \ \ { 6 -2 roll moveto gsave rotate\n
      \ \ \ \ \ \ ps mul neg 0 2 1 roll rmoveto\n \ \ \ \ \ \ 1 index
      stringwidth pop\n \ \ \ \ \ \ mul neg 0 rmoveto show grestore }
      def\n/cl \ { grestore gsave newpath 3 index 3 index moveto 1 index\n
      \ \ \ \ \ \ 4 -1 roll lineto \ exch 1 index lineto lineto\n
      \ \ \ \ \ \ closepath clip newpath } def\n/rgb { setrgbcolor } def\n/s
      \ \ { scalefont setfont } def\n/R \ \ { /Font1 findfont } def\n/B \ \ {
      /Font2 findfont } def\n/I \ \ { /Font3 findfont } def\n/BI \ { /Font4
      findfont } def\n/S \ \ { /Font5 findfont } def\n1 setlinecap 1
      setlinejoin\n% end \ \ .ps.prolog\n%%IncludeResource: font
      Helvetica\n/Helvetica findfont\ndup length dict begin\n \ {1 index /FID
      ne {def} {pop pop} ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n
      \ currentdict\n \ end\n/Font1 exch definefont pop\n%%IncludeResource:
      font Helvetica-Bold\n/Helvetica-Bold findfont\ndup length dict begin\n
      \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font2 exch definefont
      pop\n%%IncludeResource: font Helvetica-Oblique\n/Helvetica-Oblique
      findfont\ndup length dict begin\n \ {1 index /FID ne {def} {pop pop}
      ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n \ currentdict\n
      \ end\n/Font3 exch definefont pop\n%%IncludeResource: font
      Helvetica-BoldOblique\n/Helvetica-BoldOblique findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font4 exch definefont
      pop\n%%IncludeResource: font Symbol\n/Symbol findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n
      \ currentdict\n \ end\n/Font5 exch definefont pop\n%%EndProlog\n%%Page:
      1 1\nbp\n34.44 42.84 486.00 469.31 cl\n34.44 42.84 486.00 469.31 cl\n0
      0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n51.16 58.64 1.58 c p1\n55.39
      62.62 1.58 c p1\n59.61 66.61 1.58 c p1\n63.83 70.60 1.58 c p1\n68.06
      74.59 1.58 c p1\n72.28 78.58 1.58 c p1\n76.50 82.57 1.58 c p1\n80.73
      86.56 1.58 c p1\n84.95 90.54 1.58 c p1\n89.17 94.53 1.58 c p1\n93.40
      98.52 1.58 c p1\n97.62 102.51 1.58 c p1\n101.84 106.50 1.58 c
      p1\n106.07 110.49 1.58 c p1\n110.29 114.48 1.58 c p1\n114.51 118.46
      1.58 c p1\n118.74 122.45 1.58 c p1\n122.96 126.44 1.58 c p1\n127.18
      130.43 1.58 c p1\n131.41 134.42 1.58 c p1\n135.63 138.41 1.58 c
      p1\n139.85 142.40 1.58 c p1\n144.08 146.39 1.58 c p1\n148.30 150.37
      1.58 c p1\n152.52 154.36 1.58 c p1\n156.75 158.35 1.58 c p1\n160.97
      162.34 1.58 c p1\n165.19 166.33 1.58 c p1\n169.42 170.32 1.58 c
      p1\n173.64 174.31 1.58 c p1\n177.86 178.29 1.58 c p1\n182.09 182.28
      1.58 c p1\n186.31 186.27 1.58 c p1\n190.53 190.26 1.58 c p1\n194.76
      194.25 1.58 c p1\n198.98 198.24 1.58 c p1\n203.20 202.23 1.58 c
      p1\n207.43 206.21 1.58 c p1\n211.65 210.20 1.58 c p1\n215.87 214.19
      1.58 c p1\n220.10 218.18 1.58 c p1\n224.32 222.17 1.58 c p1\n228.54
      226.16 1.58 c p1\n232.77 230.15 1.58 c p1\n236.99 234.14 1.58 c
      p1\n241.21 238.12 1.58 c p1\n245.44 242.11 1.58 c p1\n249.66 246.10
      1.58 c p1\n253.88 250.09 1.58 c p1\n258.11 254.08 1.58 c p1\n262.33
      258.07 1.58 c p1\n266.55 262.06 1.58 c p1\n270.78 266.04 1.58 c
      p1\n275.00 270.03 1.58 c p1\n279.22 274.02 1.58 c p1\n283.45 278.01
      1.58 c p1\n287.67 282.00 1.58 c p1\n291.89 285.99 1.58 c p1\n296.12
      289.98 1.58 c p1\n300.34 293.96 1.58 c p1\n304.56 297.95 1.58 c
      p1\n308.79 301.94 1.58 c p1\n313.01 305.93 1.58 c p1\n317.23 309.92
      1.58 c p1\n321.46 313.91 1.58 c p1\n325.68 317.90 1.58 c p1\n329.90
      321.89 1.58 c p1\n334.13 325.87 1.58 c p1\n338.35 329.86 1.58 c
      p1\n342.57 333.85 1.58 c p1\n346.80 337.84 1.58 c p1\n351.02 341.83
      1.58 c p1\n355.24 345.82 1.58 c p1\n359.47 349.81 1.58 c p1\n363.69
      353.79 1.58 c p1\n367.91 357.78 1.58 c p1\n372.14 361.77 1.58 c
      p1\n376.36 365.76 1.58 c p1\n380.58 369.75 1.58 c p1\n384.81 373.74
      1.58 c p1\n389.03 377.73 1.58 c p1\n393.25 381.71 1.58 c p1\n397.48
      385.70 1.58 c p1\n401.70 389.69 1.58 c p1\n405.92 393.68 1.58 c
      p1\n410.15 397.67 1.58 c p1\n414.37 401.66 1.58 c p1\n418.59 405.65
      1.58 c p1\n422.82 409.64 1.58 c p1\n427.04 413.62 1.58 c p1\n431.26
      417.61 1.58 c p1\n435.49 421.60 1.58 c p1\n439.71 425.59 1.58 c
      p1\n443.93 429.58 1.58 c p1\n448.16 433.57 1.58 c p1\n452.38 437.56
      1.58 c p1\n456.60 441.54 1.58 c p1\n460.83 445.53 1.58 c p1\n465.05
      449.52 1.58 c p1\n469.27 453.51 1.58 c p1\n0.00 0.00 503.64 503.75
      cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n46.94 42.84
      m\n422.33 0.00 l\no\nnp\n46.94 42.84 m\n-0.00 -4.20 l\no\nnp\n131.41
      42.84 m\n-0.00 -4.20 l\no\nnp\n215.87 42.84 m\n0.00 -4.20
      l\no\nnp\n300.34 42.84 m\n-0.00 -4.20 l\no\nnp\n384.81 42.84 m\n0.00
      -4.20 l\no\nnp\n469.27 42.84 m\n-0.00 -4.20 l\no\n/ps 12 def R 12
      s\n46.94 27.72 (0) .5 0 0 t\n131.41 27.72 (20) .5 0 0 t\n215.87 27.72
      (40) .5 0 0 t\n300.34 27.72 (60) .5 0 0 t\n384.81 27.72 (80) .5 0 0
      t\n469.27 27.72 (100) .5 0 0 t\nnp\n34.44 54.65 m\n-0.00 398.86
      l\no\nnp\n34.44 54.65 m\n-4.20 -0.00 l\no\nnp\n34.44 134.42 m\n-4.20
      -0.00 l\no\nnp\n34.44 214.19 m\n-4.20 -0.00 l\no\nnp\n34.44 293.96
      m\n-4.20 -0.00 l\no\nnp\n34.44 373.74 m\n-4.20 0.00 l\no\nnp\n34.44
      453.51 m\n-4.20 -0.00 l\no\n24.36 54.65 (0) .5 0 90 t\n24.36 134.42
      (20) .5 0 90 t\n24.36 214.19 (40) .5 0 90 t\n24.36 293.96 (60) .5 0 90
      t\n24.36 373.74 (80) .5 0 90 t\n24.36 453.51 (100) .5 0 90 t\nnp\n34.44
      42.84 m\n451.56 0.00 l\n0 426.47 l\n-451.56 0.00 l\n-0.00 -426.47
      l\no\n0.00 0.00 503.64 503.75 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n260.22
      10.92 (Index) .5 0 0 t\n7.56 256.07 (x) .5 0 90
      t\nep\n%%Trailer\n%%Pages: 1\n%%EOF\n>|ps>|/2|/2||||>
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      z\<less\>-sort(y)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      plot(z)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      v()
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 0 0 504
      504\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
      .ps.prolog\n/gs \ { gsave } def\n/gr \ { grestore } def\n/ep \ {
      showpage gr gr } def\n/m \ \ { moveto } def\n/l \ { rlineto } def\n/np
      \ { newpath } def\n/cp \ { closepath } def\n/f \ \ { fill } def\n/o
      \ \ { stroke } def\n/c \ \ { newpath 0 360 arc } def\n/r \ \ { 4 2 roll
      moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch
      rlineto closepath } def\n/p1 \ { stroke } def\n/p2 \ { gsave bg
      setrgbcolor fill grestore newpath } def\n/p3 \ { gsave bg setrgbcolor
      fill grestore stroke } def\n/t \ \ { 6 -2 roll moveto gsave rotate\n
      \ \ \ \ \ \ ps mul neg 0 2 1 roll rmoveto\n \ \ \ \ \ \ 1 index
      stringwidth pop\n \ \ \ \ \ \ mul neg 0 rmoveto show grestore }
      def\n/cl \ { grestore gsave newpath 3 index 3 index moveto 1 index\n
      \ \ \ \ \ \ 4 -1 roll lineto \ exch 1 index lineto lineto\n
      \ \ \ \ \ \ closepath clip newpath } def\n/rgb { setrgbcolor } def\n/s
      \ \ { scalefont setfont } def\n/R \ \ { /Font1 findfont } def\n/B \ \ {
      /Font2 findfont } def\n/I \ \ { /Font3 findfont } def\n/BI \ { /Font4
      findfont } def\n/S \ \ { /Font5 findfont } def\n1 setlinecap 1
      setlinejoin\n% end \ \ .ps.prolog\n%%IncludeResource: font
      Helvetica\n/Helvetica findfont\ndup length dict begin\n \ {1 index /FID
      ne {def} {pop pop} ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n
      \ currentdict\n \ end\n/Font1 exch definefont pop\n%%IncludeResource:
      font Helvetica-Bold\n/Helvetica-Bold findfont\ndup length dict begin\n
      \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font2 exch definefont
      pop\n%%IncludeResource: font Helvetica-Oblique\n/Helvetica-Oblique
      findfont\ndup length dict begin\n \ {1 index /FID ne {def} {pop pop}
      ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n \ currentdict\n
      \ end\n/Font3 exch definefont pop\n%%IncludeResource: font
      Helvetica-BoldOblique\n/Helvetica-BoldOblique findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font4 exch definefont
      pop\n%%IncludeResource: font Symbol\n/Symbol findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n
      \ currentdict\n \ end\n/Font5 exch definefont pop\n%%EndProlog\n%%Page:
      1 1\nbp\n34.44 42.84 486.00 469.31 cl\n34.44 42.84 486.00 469.31 cl\n0
      0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n51.16 58.64 1.58 c p1\n55.39
      99.89 1.58 c p1\n59.61 99.91 1.58 c p1\n63.83 101.31 1.58 c p1\n68.06
      107.38 1.58 c p1\n72.28 115.64 1.58 c p1\n76.50 116.53 1.58 c p1\n80.73
      122.88 1.58 c p1\n84.95 128.76 1.58 c p1\n89.17 135.28 1.58 c p1\n93.40
      135.72 1.58 c p1\n97.62 155.47 1.58 c p1\n101.84 162.72 1.58 c
      p1\n106.07 164.97 1.58 c p1\n110.29 167.68 1.58 c p1\n114.51 169.00
      1.58 c p1\n118.74 181.19 1.58 c p1\n122.96 183.40 1.58 c p1\n127.18
      184.08 1.58 c p1\n131.41 185.12 1.58 c p1\n135.63 187.84 1.58 c
      p1\n139.85 188.74 1.58 c p1\n144.08 190.99 1.58 c p1\n148.30 191.56
      1.58 c p1\n152.52 193.11 1.58 c p1\n156.75 194.27 1.58 c p1\n160.97
      195.74 1.58 c p1\n165.19 198.36 1.58 c p1\n169.42 201.53 1.58 c
      p1\n173.64 214.11 1.58 c p1\n177.86 215.51 1.58 c p1\n182.09 217.28
      1.58 c p1\n186.31 218.00 1.58 c p1\n190.53 219.88 1.58 c p1\n194.76
      231.24 1.58 c p1\n198.98 232.22 1.58 c p1\n203.20 238.42 1.58 c
      p1\n207.43 240.07 1.58 c p1\n211.65 241.03 1.58 c p1\n215.87 245.46
      1.58 c p1\n220.10 245.88 1.58 c p1\n224.32 247.88 1.58 c p1\n228.54
      248.45 1.58 c p1\n232.77 248.65 1.58 c p1\n236.99 251.98 1.58 c
      p1\n241.21 254.73 1.58 c p1\n245.44 255.75 1.58 c p1\n249.66 259.07
      1.58 c p1\n253.88 259.42 1.58 c p1\n258.11 269.31 1.58 c p1\n262.33
      270.74 1.58 c p1\n266.55 270.96 1.58 c p1\n270.78 275.86 1.58 c
      p1\n275.00 277.30 1.58 c p1\n279.22 278.35 1.58 c p1\n283.45 280.18
      1.58 c p1\n287.67 281.46 1.58 c p1\n291.89 284.12 1.58 c p1\n296.12
      286.30 1.58 c p1\n300.34 286.38 1.58 c p1\n304.56 286.48 1.58 c
      p1\n308.79 293.09 1.58 c p1\n313.01 293.99 1.58 c p1\n317.23 294.83
      1.58 c p1\n321.46 295.07 1.58 c p1\n325.68 296.82 1.58 c p1\n329.90
      297.11 1.58 c p1\n334.13 303.15 1.58 c p1\n338.35 305.87 1.58 c
      p1\n342.57 307.87 1.58 c p1\n346.80 309.09 1.58 c p1\n351.02 320.17
      1.58 c p1\n355.24 323.91 1.58 c p1\n359.47 331.28 1.58 c p1\n363.69
      332.54 1.58 c p1\n367.91 333.06 1.58 c p1\n372.14 342.69 1.58 c
      p1\n376.36 348.93 1.58 c p1\n380.58 351.51 1.58 c p1\n384.81 353.77
      1.58 c p1\n389.03 355.92 1.58 c p1\n393.25 356.82 1.58 c p1\n397.48
      361.49 1.58 c p1\n401.70 363.95 1.58 c p1\n405.92 379.61 1.58 c
      p1\n410.15 381.79 1.58 c p1\n414.37 385.81 1.58 c p1\n418.59 386.76
      1.58 c p1\n422.82 394.53 1.58 c p1\n427.04 396.91 1.58 c p1\n431.26
      399.95 1.58 c p1\n435.49 401.37 1.58 c p1\n439.71 406.84 1.58 c
      p1\n443.93 409.03 1.58 c p1\n448.16 411.44 1.58 c p1\n452.38 418.10
      1.58 c p1\n456.60 425.28 1.58 c p1\n460.83 430.33 1.58 c p1\n465.05
      434.80 1.58 c p1\n469.27 453.51 1.58 c p1\n0.00 0.00 503.64 503.75
      cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n46.94 42.84
      m\n422.33 0.00 l\no\nnp\n46.94 42.84 m\n-0.00 -4.20 l\no\nnp\n131.41
      42.84 m\n-0.00 -4.20 l\no\nnp\n215.87 42.84 m\n0.00 -4.20
      l\no\nnp\n300.34 42.84 m\n-0.00 -4.20 l\no\nnp\n384.81 42.84 m\n0.00
      -4.20 l\no\nnp\n469.27 42.84 m\n-0.00 -4.20 l\no\n/ps 12 def R 12
      s\n46.94 27.72 (0) .5 0 0 t\n131.41 27.72 (20) .5 0 0 t\n215.87 27.72
      (40) .5 0 0 t\n300.34 27.72 (60) .5 0 0 t\n384.81 27.72 (80) .5 0 0
      t\n469.27 27.72 (100) .5 0 0 t\nnp\n34.44 43.77 m\n-0.00 376.13
      l\no\nnp\n34.44 43.77 m\n-4.20 0.00 l\no\nnp\n34.44 119.00 m\n-4.20 0
      l\no\nnp\n34.44 194.22 m\n-4.20 -0.00 l\no\nnp\n34.44 269.45 m\n-4.20
      -0.00 l\no\nnp\n34.44 344.67 m\n-4.20 0.00 l\no\nnp\n34.44 419.90
      m\n-4.20 -0.00 l\no\n24.36 43.77 (-3) .5 0 90 t\n24.36 119.00 (-2) .5 0
      90 t\n24.36 194.22 (-1) .5 0 90 t\n24.36 269.45 (0) .5 0 90 t\n24.36
      344.67 (1) .5 0 90 t\n24.36 419.90 (2) .5 0 90 t\nnp\n34.44 42.84
      m\n451.56 0.00 l\n0 426.47 l\n-451.56 0.00 l\n-0.00 -426.47 l\no\n0.00
      0.00 503.64 503.75 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n260.22 10.92
      (Index) .5 0 0 t\n7.56 256.07 (z) .5 0 90 t\nep\n%%Trailer\n%%Pages:
      1\n%%EOF\n>|ps>|/2|/2||||>
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      plot(z,sort(y))
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      v()
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 0 0 504
      504\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
      .ps.prolog\n/gs \ { gsave } def\n/gr \ { grestore } def\n/ep \ {
      showpage gr gr } def\n/m \ \ { moveto } def\n/l \ { rlineto } def\n/np
      \ { newpath } def\n/cp \ { closepath } def\n/f \ \ { fill } def\n/o
      \ \ { stroke } def\n/c \ \ { newpath 0 360 arc } def\n/r \ \ { 4 2 roll
      moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch
      rlineto closepath } def\n/p1 \ { stroke } def\n/p2 \ { gsave bg
      setrgbcolor fill grestore newpath } def\n/p3 \ { gsave bg setrgbcolor
      fill grestore stroke } def\n/t \ \ { 6 -2 roll moveto gsave rotate\n
      \ \ \ \ \ \ ps mul neg 0 2 1 roll rmoveto\n \ \ \ \ \ \ 1 index
      stringwidth pop\n \ \ \ \ \ \ mul neg 0 rmoveto show grestore }
      def\n/cl \ { grestore gsave newpath 3 index 3 index moveto 1 index\n
      \ \ \ \ \ \ 4 -1 roll lineto \ exch 1 index lineto lineto\n
      \ \ \ \ \ \ closepath clip newpath } def\n/rgb { setrgbcolor } def\n/s
      \ \ { scalefont setfont } def\n/R \ \ { /Font1 findfont } def\n/B \ \ {
      /Font2 findfont } def\n/I \ \ { /Font3 findfont } def\n/BI \ { /Font4
      findfont } def\n/S \ \ { /Font5 findfont } def\n1 setlinecap 1
      setlinejoin\n% end \ \ .ps.prolog\n%%IncludeResource: font
      Helvetica\n/Helvetica findfont\ndup length dict begin\n \ {1 index /FID
      ne {def} {pop pop} ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n
      \ currentdict\n \ end\n/Font1 exch definefont pop\n%%IncludeResource:
      font Helvetica-Bold\n/Helvetica-Bold findfont\ndup length dict begin\n
      \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font2 exch definefont
      pop\n%%IncludeResource: font Helvetica-Oblique\n/Helvetica-Oblique
      findfont\ndup length dict begin\n \ {1 index /FID ne {def} {pop pop}
      ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n \ currentdict\n
      \ end\n/Font3 exch definefont pop\n%%IncludeResource: font
      Helvetica-BoldOblique\n/Helvetica-BoldOblique findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font4 exch definefont
      pop\n%%IncludeResource: font Symbol\n/Symbol findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n
      \ currentdict\n \ end\n/Font5 exch definefont pop\n%%EndProlog\n%%Page:
      1 1\nbp\n34.44 42.84 486.00 469.31 cl\n34.44 42.84 486.00 469.31 cl\n0
      0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n51.16 58.64 1.58 c p1\n94.85
      99.89 1.58 c p1\n94.87 99.91 1.58 c p1\n96.35 101.31 1.58 c p1\n102.78
      107.38 1.58 c p1\n111.52 115.64 1.58 c p1\n112.46 116.53 1.58 c
      p1\n119.19 122.88 1.58 c p1\n125.42 128.76 1.58 c p1\n132.31 135.28
      1.58 c p1\n132.79 135.72 1.58 c p1\n153.70 155.47 1.58 c p1\n161.38
      162.72 1.58 c p1\n163.76 164.97 1.58 c p1\n166.62 167.68 1.58 c
      p1\n168.02 169.00 1.58 c p1\n180.93 181.19 1.58 c p1\n183.27 183.40
      1.58 c p1\n183.99 184.08 1.58 c p1\n185.09 185.12 1.58 c p1\n187.98
      187.84 1.58 c p1\n188.92 188.74 1.58 c p1\n191.31 190.99 1.58 c
      p1\n191.91 191.56 1.58 c p1\n193.55 193.11 1.58 c p1\n194.78 194.27
      1.58 c p1\n196.33 195.74 1.58 c p1\n199.11 198.36 1.58 c p1\n202.47
      201.53 1.58 c p1\n215.79 214.11 1.58 c p1\n217.27 215.51 1.58 c
      p1\n219.14 217.28 1.58 c p1\n219.91 218.00 1.58 c p1\n221.89 219.88
      1.58 c p1\n233.92 231.24 1.58 c p1\n234.96 232.22 1.58 c p1\n241.53
      238.42 1.58 c p1\n243.28 240.07 1.58 c p1\n244.29 241.03 1.58 c
      p1\n248.98 245.46 1.58 c p1\n249.43 245.88 1.58 c p1\n251.55 247.88
      1.58 c p1\n252.14 248.45 1.58 c p1\n252.36 248.65 1.58 c p1\n255.89
      251.98 1.58 c p1\n258.79 254.73 1.58 c p1\n259.88 255.75 1.58 c
      p1\n263.40 259.07 1.58 c p1\n263.76 259.42 1.58 c p1\n274.23 269.31
      1.58 c p1\n275.75 270.74 1.58 c p1\n275.98 270.96 1.58 c p1\n281.17
      275.86 1.58 c p1\n282.70 277.30 1.58 c p1\n283.81 278.35 1.58 c
      p1\n285.74 280.18 1.58 c p1\n287.10 281.46 1.58 c p1\n289.91 284.12
      1.58 c p1\n292.23 286.30 1.58 c p1\n292.31 286.38 1.58 c p1\n292.42
      286.48 1.58 c p1\n299.41 293.09 1.58 c p1\n300.37 293.99 1.58 c
      p1\n301.26 294.83 1.58 c p1\n301.51 295.07 1.58 c p1\n303.36 296.82
      1.58 c p1\n303.67 297.11 1.58 c p1\n310.07 303.15 1.58 c p1\n312.95
      305.87 1.58 c p1\n315.06 307.87 1.58 c p1\n316.35 309.09 1.58 c
      p1\n328.09 320.17 1.58 c p1\n332.05 323.91 1.58 c p1\n339.85 331.28
      1.58 c p1\n341.19 332.54 1.58 c p1\n341.74 333.06 1.58 c p1\n351.93
      342.69 1.58 c p1\n358.54 348.93 1.58 c p1\n361.27 351.51 1.58 c
      p1\n363.66 353.77 1.58 c p1\n365.94 355.92 1.58 c p1\n366.89 356.82
      1.58 c p1\n371.84 361.49 1.58 c p1\n374.44 363.95 1.58 c p1\n391.03
      379.61 1.58 c p1\n393.34 381.79 1.58 c p1\n397.59 385.81 1.58 c
      p1\n398.59 386.76 1.58 c p1\n406.83 394.53 1.58 c p1\n409.34 396.91
      1.58 c p1\n412.57 399.95 1.58 c p1\n414.06 401.37 1.58 c p1\n419.86
      406.84 1.58 c p1\n422.18 409.03 1.58 c p1\n424.72 411.44 1.58 c
      p1\n431.78 418.10 1.58 c p1\n439.38 425.28 1.58 c p1\n444.73 430.33
      1.58 c p1\n449.46 434.80 1.58 c p1\n469.27 453.51 1.58 c p1\n0.00 0.00
      503.64 503.75 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n35.43
      42.84 m\n398.25 0.00 l\no\nnp\n35.43 42.84 m\n-0.00 -4.20
      l\no\nnp\n115.08 42.84 m\n-0.00 -4.20 l\no\nnp\n194.73 42.84 m\n-0.00
      -4.20 l\no\nnp\n274.38 42.84 m\n-0.00 -4.20 l\no\nnp\n354.03 42.84
      m\n-0.00 -4.20 l\no\nnp\n433.68 42.84 m\n0.00 -4.20 l\no\n/ps 12 def R
      12 s\n35.43 27.72 (-3) .5 0 0 t\n115.08 27.72 (-2) .5 0 0 t\n194.73
      27.72 (-1) .5 0 0 t\n274.38 27.72 (0) .5 0 0 t\n354.03 27.72 (1) .5 0 0
      t\n433.68 27.72 (2) .5 0 0 t\nnp\n34.44 43.77 m\n-0.00 376.13
      l\no\nnp\n34.44 43.77 m\n-4.20 0.00 l\no\nnp\n34.44 119.00 m\n-4.20 0
      l\no\nnp\n34.44 194.22 m\n-4.20 -0.00 l\no\nnp\n34.44 269.45 m\n-4.20
      -0.00 l\no\nnp\n34.44 344.67 m\n-4.20 0.00 l\no\nnp\n34.44 419.90
      m\n-4.20 -0.00 l\no\n24.36 43.77 (-3) .5 0 90 t\n24.36 119.00 (-2) .5 0
      90 t\n24.36 194.22 (-1) .5 0 90 t\n24.36 269.45 (0) .5 0 90 t\n24.36
      344.67 (1) .5 0 90 t\n24.36 419.90 (2) .5 0 90 t\nnp\n34.44 42.84
      m\n451.56 0.00 l\n0 426.47 l\n-451.56 0.00 l\n-0.00 -426.47 l\no\n0.00
      0.00 503.64 503.75 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n260.22 10.92 (z)
      .5 0 0 t\n7.56 256.07 (sort\\(y\\)) .5 0 90 t\nep\n%%Trailer\n%%Pages:
      1\n%%EOF\n>|ps>|/2|/2||||>
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      hist(z)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      v()
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 0 0 504
      504\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
      .ps.prolog\n/gs \ { gsave } def\n/gr \ { grestore } def\n/ep \ {
      showpage gr gr } def\n/m \ \ { moveto } def\n/l \ { rlineto } def\n/np
      \ { newpath } def\n/cp \ { closepath } def\n/f \ \ { fill } def\n/o
      \ \ { stroke } def\n/c \ \ { newpath 0 360 arc } def\n/r \ \ { 4 2 roll
      moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch
      rlineto closepath } def\n/p1 \ { stroke } def\n/p2 \ { gsave bg
      setrgbcolor fill grestore newpath } def\n/p3 \ { gsave bg setrgbcolor
      fill grestore stroke } def\n/t \ \ { 6 -2 roll moveto gsave rotate\n
      \ \ \ \ \ \ ps mul neg 0 2 1 roll rmoveto\n \ \ \ \ \ \ 1 index
      stringwidth pop\n \ \ \ \ \ \ mul neg 0 rmoveto show grestore }
      def\n/cl \ { grestore gsave newpath 3 index 3 index moveto 1 index\n
      \ \ \ \ \ \ 4 -1 roll lineto \ exch 1 index lineto lineto\n
      \ \ \ \ \ \ closepath clip newpath } def\n/rgb { setrgbcolor } def\n/s
      \ \ { scalefont setfont } def\n/R \ \ { /Font1 findfont } def\n/B \ \ {
      /Font2 findfont } def\n/I \ \ { /Font3 findfont } def\n/BI \ { /Font4
      findfont } def\n/S \ \ { /Font5 findfont } def\n1 setlinecap 1
      setlinejoin\n% end \ \ .ps.prolog\n%%IncludeResource: font
      Helvetica\n/Helvetica findfont\ndup length dict begin\n \ {1 index /FID
      ne {def} {pop pop} ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n
      \ currentdict\n \ end\n/Font1 exch definefont pop\n%%IncludeResource:
      font Helvetica-Bold\n/Helvetica-Bold findfont\ndup length dict begin\n
      \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font2 exch definefont
      pop\n%%IncludeResource: font Helvetica-Oblique\n/Helvetica-Oblique
      findfont\ndup length dict begin\n \ {1 index /FID ne {def} {pop pop}
      ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n \ currentdict\n
      \ end\n/Font3 exch definefont pop\n%%IncludeResource: font
      Helvetica-BoldOblique\n/Helvetica-BoldOblique findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font4 exch definefont
      pop\n%%IncludeResource: font Symbol\n/Symbol findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n
      \ currentdict\n \ end\n/Font5 exch definefont pop\n%%EndProlog\n%%Page:
      1 1\nbp\n34.44 42.84 486.00 469.31 cl\n0.00 0.00 503.64 503.75 cl\n/ps
      14 def B 14 s\n0 0 0 rgb\n260.22 481.50 (Histogram of z) .5 0 0 t\n/ps
      12 def R 12 s\n260.22 10.92 (z) .5 0 0 t\n7.56 256.07 (Frequency) .5 0
      90 t\n0.00 0.00 503.64 503.75 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n51.16 42.84 m\n380.10 0.00 l\no\nnp\n51.16 42.84 m\n-0.00
      -4.20 l\no\nnp\n127.18 42.84 m\n0.00 -4.20 l\no\nnp\n203.20 42.84
      m\n-0.00 -4.20 l\no\nnp\n279.22 42.84 m\n0.00 -4.20 l\no\nnp\n355.24
      42.84 m\n0.00 -4.20 l\no\nnp\n431.26 42.84 m\n-0.00 -4.20 l\no\n/ps 12
      def R 12 s\n51.16 27.72 (-3) .5 0 0 t\n127.18 27.72 (-2) .5 0 0
      t\n203.20 27.72 (-1) .5 0 0 t\n279.22 27.72 (0) .5 0 0 t\n355.24 27.72
      (1) .5 0 0 t\n431.26 27.72 (2) .5 0 0 t\nnp\n34.44 58.64 m\n-0.00
      311.74 l\no\nnp\n34.44 58.64 m\n-4.20 0.00 l\no\nnp\n34.44 162.55
      m\n-4.20 0.00 l\no\nnp\n34.44 266.46 m\n-4.20 -0.00 l\no\nnp\n34.44
      370.38 m\n-4.20 -0.00 l\no\n24.36 58.64 (0) .5 0 90 t\n24.36 162.55 (5)
      .5 0 90 t\n24.36 266.46 (10) .5 0 90 t\n24.36 370.38 (15) .5 0 90
      t\n34.44 42.84 486.00 469.31 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n51.16 58.64 38.01 20.78 r p1\n89.17 58.64 38.01 124.70 r
      p1\n127.18 58.64 38.01 103.91 r p1\n165.19 58.64 38.01 270.18 r
      p1\n203.20 58.64 38.01 207.83 r p1\n241.21 58.64 38.01 311.74 r
      p1\n279.22 58.64 38.01 394.88 r p1\n317.23 58.64 38.01 166.26 r
      p1\n355.24 58.64 38.01 187.05 r p1\n393.25 58.64 38.01 207.83 r
      p1\n431.26 58.64 38.01 83.13 r p1\nep\n%%Trailer\n%%Pages:
      1\n%%EOF\n>|ps>|/2|/2||||>
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      summary(z)
    </input>

    <\output>
      \ \ \ \ \ Min. \ \ 1st Qu. \ \ \ Median \ \ \ \ \ Mean \ \ 3rd Qu.
      \ \ \ \ \ Max.\ 

      -2.802000 -1.003000 \ 0.007691 -0.040620 \ 0.840500 \ 2.447000\ 
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      hist(y,seq(-4,3.5,0.2),prob=TRUE)
    </input>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      v()
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 0 0 504
      504\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
      .ps.prolog\n/gs \ { gsave } def\n/gr \ { grestore } def\n/ep \ {
      showpage gr gr } def\n/m \ \ { moveto } def\n/l \ { rlineto } def\n/np
      \ { newpath } def\n/cp \ { closepath } def\n/f \ \ { fill } def\n/o
      \ \ { stroke } def\n/c \ \ { newpath 0 360 arc } def\n/r \ \ { 4 2 roll
      moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch
      rlineto closepath } def\n/p1 \ { stroke } def\n/p2 \ { gsave bg
      setrgbcolor fill grestore newpath } def\n/p3 \ { gsave bg setrgbcolor
      fill grestore stroke } def\n/t \ \ { 6 -2 roll moveto gsave rotate\n
      \ \ \ \ \ \ ps mul neg 0 2 1 roll rmoveto\n \ \ \ \ \ \ 1 index
      stringwidth pop\n \ \ \ \ \ \ mul neg 0 rmoveto show grestore }
      def\n/cl \ { grestore gsave newpath 3 index 3 index moveto 1 index\n
      \ \ \ \ \ \ 4 -1 roll lineto \ exch 1 index lineto lineto\n
      \ \ \ \ \ \ closepath clip newpath } def\n/rgb { setrgbcolor } def\n/s
      \ \ { scalefont setfont } def\n/R \ \ { /Font1 findfont } def\n/B \ \ {
      /Font2 findfont } def\n/I \ \ { /Font3 findfont } def\n/BI \ { /Font4
      findfont } def\n/S \ \ { /Font5 findfont } def\n1 setlinecap 1
      setlinejoin\n% end \ \ .ps.prolog\n%%IncludeResource: font
      Helvetica\n/Helvetica findfont\ndup length dict begin\n \ {1 index /FID
      ne {def} {pop pop} ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n
      \ currentdict\n \ end\n/Font1 exch definefont pop\n%%IncludeResource:
      font Helvetica-Bold\n/Helvetica-Bold findfont\ndup length dict begin\n
      \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font2 exch definefont
      pop\n%%IncludeResource: font Helvetica-Oblique\n/Helvetica-Oblique
      findfont\ndup length dict begin\n \ {1 index /FID ne {def} {pop pop}
      ifelse} forall\n \ /Encoding ISOLatin1Encoding def\n \ currentdict\n
      \ end\n/Font3 exch definefont pop\n%%IncludeResource: font
      Helvetica-BoldOblique\n/Helvetica-BoldOblique findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n \ /Encoding
      ISOLatin1Encoding def\n \ currentdict\n \ end\n/Font4 exch definefont
      pop\n%%IncludeResource: font Symbol\n/Symbol findfont\ndup length dict
      begin\n \ {1 index /FID ne {def} {pop pop} ifelse} forall\n
      \ currentdict\n \ end\n/Font5 exch definefont pop\n%%EndProlog\n%%Page:
      1 1\nbp\n34.44 42.84 486.00 469.31 cl\n0.00 0.00 503.64 503.75 cl\n/ps
      14 def B 14 s\n0 0 0 rgb\n260.22 481.50 (Histogram of y) .5 0 0 t\n/ps
      12 def R 12 s\n260.22 10.92 (y) .5 0 0 t\n7.56 256.07 (Density) .5 0 90
      t\n0.00 0.00 503.64 503.75 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n51.16 42.84 m\n339.01 0.00 l\no\nnp\n51.16 42.84 m\n-0.00
      -4.20 l\no\nnp\n164.17 42.84 m\n-0.00 -4.20 l\no\nnp\n277.17 42.84
      m\n0.00 -4.20 l\no\nnp\n390.17 42.84 m\n0.00 -4.20 l\no\n/ps 12 def R
      12 s\n51.16 27.72 (-4) .5 0 0 t\n164.17 27.72 (-2) .5 0 0 t\n277.17
      27.72 (0) .5 0 0 t\n390.17 27.72 (2) .5 0 0 t\nnp\n34.44 58.64 m\n-0.00
      351.00 l\no\nnp\n34.44 58.64 m\n-4.20 0.00 l\no\nnp\n34.44 146.39
      m\n-4.20 -0.00 l\no\nnp\n34.44 234.14 m\n-4.20 -0.00 l\no\nnp\n34.44
      321.89 m\n-4.20 -0.00 l\no\nnp\n34.44 409.64 m\n-4.20 -0.00 l\no\n24.36
      58.64 (0.0) .5 0 90 t\n24.36 146.39 (0.1) .5 0 90 t\n24.36 234.14 (0.2)
      .5 0 90 t\n24.36 321.89 (0.3) .5 0 90 t\n24.36 409.64 (0.4) .5 0 90
      t\n34.44 42.84 486.00 469.31 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n51.16 58.64 11.30 0.00 r p1\n62.46 58.64 11.30 0.00 r
      p1\n73.76 58.64 11.30 0.00 r p1\n85.06 58.64 11.30 0.00 r p1\n96.37
      58.64 11.30 0.00 r p1\n107.67 58.64 11.30 43.88 r p1\n118.97 58.64
      11.30 0.00 r p1\n130.27 58.64 11.30 0.00 r p1\n141.57 58.64 11.30
      131.63 r p1\n152.87 58.64 11.30 131.63 r p1\n164.17 58.64 11.30 87.75 r
      p1\n175.47 58.64 11.30 87.75 r p1\n186.77 58.64 11.30 87.75 r
      p1\n198.07 58.64 11.30 131.63 r p1\n209.37 58.64 11.30 394.88 r
      p1\n220.67 58.64 11.30 175.50 r p1\n231.97 58.64 11.30 219.38 r
      p1\n243.27 58.64 11.30 131.63 r p1\n254.57 58.64 11.30 351.00 r
      p1\n265.87 58.64 11.30 219.38 r p1\n277.17 58.64 11.30 351.00 r
      p1\n288.47 58.64 11.30 394.88 r p1\n299.77 58.64 11.30 175.50 r
      p1\n311.07 58.64 11.30 87.75 r p1\n322.37 58.64 11.30 175.50 r
      p1\n333.67 58.64 11.30 219.38 r p1\n344.97 58.64 11.30 87.75 r
      p1\n356.27 58.64 11.30 175.50 r p1\n367.57 58.64 11.30 175.50 r
      p1\n378.87 58.64 11.30 175.50 r p1\n390.17 58.64 11.30 131.63 r
      p1\n401.47 58.64 11.30 0.00 r p1\n412.77 58.64 11.30 43.88 r p1\n424.07
      58.64 11.30 0.00 r p1\n435.37 58.64 11.30 0.00 r p1\n446.67 58.64 11.30
      0.00 r p1\n457.97 58.64 11.30 0.00 r p1\nep\n%%Trailer\n%%Pages:
      1\n%%EOF\n>|ps>|/2|/2||||>
    </output>

    <\input|<\with|color|red>
      \<gtr\> <with|color|black|>
    </with>>
      \;
    </input>
  </session>

  <apply|tmdoc-copyright|2003|Chu-Ching Huang|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-10|<tuple|8.2|?>>
    <associate|toc-11|<tuple|8.3|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|toc-12|<tuple|8.4|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|toc-13|<tuple|8.5|?>>
    <associate|idx-3|<tuple|3|?>>
    <associate|gly-4|<tuple|4|?>>
    <associate|toc-14|<tuple|8.6|?>>
    <associate|idx-4|<tuple|7|?>>
    <associate|gly-5|<tuple|5|?>>
    <associate|toc-15|<tuple|8.7|?>>
    <associate|idx-5|<tuple|8|?>>
    <associate|gly-6|<tuple|6|?>>
    <associate|toc-16|<tuple|8.8|?>>
    <associate|gly-7|<tuple|7|?>>
    <associate|gly-8|<tuple|8|?>>
    <associate|gly-9|<tuple|9|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|7|?>>
    <associate|toc-8|<tuple|8|?>>
    <associate|toc-9|<tuple|8.1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|figure>
      <tuple|normal||<pageref|gly-1>>

      <tuple|normal||<pageref|gly-2>>

      <tuple|normal||<pageref|gly-3>>

      <tuple|normal||<pageref|gly-4>>

      <tuple|normal||<pageref|gly-5>>

      <tuple|normal||<pageref|gly-6>>
    </associate>
  </collection>
</auxiliary>
