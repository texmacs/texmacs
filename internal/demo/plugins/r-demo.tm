<TeXmacs|1.0.1.23>

<style|generic>

<\body>
  <with|prog language|r|this session|default|<\session>
    <\output>
      \ source("/home/vdhoeven/texmacs/distr/plugins/r/r/texmacs_init.R")

      \;

      R : Copyright 2003, The R Development Core Team

      Version 1.7.1 \ (2003-06-16)

      \;

      R is free software and comes with ABSOLUTELY NO WARRANTY.

      You are welcome to redistribute it under certain conditions.

      Type `license()' or `licence()' for distribution details.

      \;

      R is a collaborative project with many contributors.

      Type `contributors()' for more information.

      \;

      Type `demo()' for some demos, `help()' for on-line help, or

      `help.start()' for a HTML browser interface to help.

      Type `q()' to quit R.

      \;

      \<gtr\>\ 
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      if (dev.cur() \<less\>= 1) get(getOption("device"))();

      opar \<less\>- par(ask = interactive() && (.Device %in% c("X11",

      \ \ \ \ "GTK", "gnome", "windows", "Macintosh")));

      x \<less\>- rnorm(50);

      opar \<less\>- c(opar, par(bg = "white"));

      plot(x, ann = FALSE, type = "n");

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 1 1 1 } def\n0.00 0.00 595.28 841.89 r p2\n153.64 276.94
      441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n216.28 350.38 m\n187.76 -0.00 l\no\nnp\n216.28 350.38
      m\n0.00 -7.20 l\no\nnp\n253.83 350.38 m\n0.00 -7.20 l\no\nnp\n291.38
      350.38 m\n-0.00 -7.20 l\no\nnp\n328.94 350.38 m\n-0.00 -7.20
      l\no\nnp\n366.49 350.38 m\n0.00 -7.20 l\no\nnp\n404.04 350.38 m\n0.00
      -7.20 l\no\n/ps 12 def R 12 s\n216.28 324.46 (0) .5 0 0 t\n253.83
      324.46 (10) .5 0 0 t\n291.38 324.46 (20) .5 0 0 t\n328.94 324.46 (30)
      .5 0 0 t\n366.49 324.46 (40) .5 0 0 t\n404.04 324.46 (50) .5 0 0
      t\nnp\n212.68 370.70 m\n0.00 127.08 l\no\nnp\n212.68 370.70 m\n-7.20
      -0.00 l\no\nnp\n212.68 402.47 m\n-7.20 0.00 l\no\nnp\n212.68 434.24
      m\n-7.20 0.00 l\no\nnp\n212.68 466.01 m\n-7.20 -0.00 l\no\nnp\n212.68
      497.78 m\n-7.20 -0.00 l\no\n195.40 370.70 (-1) .5 0 90 t\n195.40 402.47
      (0) .5 0 90 t\n195.40 434.24 (1) .5 0 90 t\n195.40 466.01 (2) .5 0 90
      t\n195.40 497.78 (3) .5 0 90 t\nnp\n212.68 350.38 m\n198.72 -0.00
      l\n-0.00 155.52 l\n-198.72 -0.00 l\n0.00 -155.52
      l\no\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      abline(h = 0, col = gray(0.9));

      lines(x, col = "green4", lty = "dotted");

      points(x, bg = "limegreen", pch = 21);

      title(main = "Simple Use of Color In a Plot",

      \ \ \ \ \ \ xlab = "Just a Whisper of a Label",

      \ \ \ \ \ \ col.main = "blue",

      \ \ \ \ \ \ col.lab = gray(0.8),

      \ \ \ \ \ \ cex.main = 1.2, cex.lab = 1,

      \ \ \ \ \ \ font.main = 4, font.lab = 3);

      par(bg = "gray");

      pie(rep(1, 24), col = rainbow(24), radius = 0.9);

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 0.7451 0.7451 0.7451 } def\n0.00 0.00 595.28 841.89 r
      p2\n212.68 350.38 411.40 505.90 cl\n212.68 350.38 411.40 505.90 cl\n/bg
      { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 376.84
      428.14 m\n-0.05 2.43 l\n-0.13 2.42 l\n-0.23 2.41 l\n-0.32 2.40 l\n-0.40
      2.39 l\n-0.50 2.37 l\n-0.58 2.36 l\n-62.59 -16.78 l\ncp p3\nnp\n376.28
      436.60 m\n3.22 0.43 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R
      12 s\n0 0 0 rgb\n382.71 433.23 (1) 0 0 0 t\n212.68 350.38 411.40 505.90
      cl\n/bg { 1 0.2510 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 374.63 444.92 m\n-0.67 2.33 l\n-0.76 2.30 l\n-0.84 2.27
      l\n-0.93 2.24 l\n-1.01 2.20 l\n-1.09 2.16 l\n-1.17 2.12 l\n-56.12
      -32.40 l\ncp p3\nnp\n371.91 452.94 m\n2.99 1.24 l\no\n153.64 276.94
      441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n377.89 451.20 (2) 0 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 1 0.5020 0 } def\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 368.16 460.54 m\n-1.25 2.08
      l\n-1.33 2.03 l\n-1.40 1.97 l\n-1.48 1.93 l\n-1.55 1.86 l\n-1.61 1.81
      l\n-1.68 1.75 l\n-45.82 -45.83 l\ncp p3\nnp\n363.45 467.59 m\n2.57 1.98
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n368.59 467.43 (3) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg { 1
      0.7490 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 357.86
      473.97 m\n-1.75 1.68 l\n-1.80 1.61 l\n-1.87 1.55 l\n-1.92 1.47 l\n-1.98
      1.41 l\n-2.03 1.32 l\n-2.07 1.25 l\n-32.40 -56.12 l\ncp p3\nnp\n351.49
      479.55 m\n1.97 2.57 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R
      12 s\n0 0 0 rgb\n355.43 480.48 (4) 0 0 0 t\n212.68 350.38 411.40 505.90
      cl\n/bg { 1 1 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n
      344.44 484.26 m\n-2.12 1.18 l\n-2.17 1.09 l\n-2.20 1.01 l\n-2.24 0.93
      l\n-2.27 0.84 l\n-2.30 0.76 l\n-2.33 0.67 l\n-16.77 -62.60 l\ncp
      p3\nnp\n336.84 488.01 m\n1.24 3.00 l\no\n153.64 276.94 441.64 564.94
      cl\n/ps 12 def R 12 s\n0 0 0 rgb\n339.32 489.99 (5) 0 0 0 t\n212.68
      350.38 411.40 505.90 cl\n/bg { 0.7490 1 0 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n 328.81 490.74 m\n-2.35 0.58 l\n-2.38
      0.50 l\n-2.38 0.40 l\n-2.41 0.32 l\n-2.41 0.22 l\n-2.42 0.14 l\n-2.42
      0.04 l\n0.00 -64.80 l\ncp p3\nnp\n320.50 492.39 m\n0.42 3.21
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n321.34 494.71 (6) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg {
      0.5020 1 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n
      312.04 492.94 m\n-2.43 -0.04 l\n-2.41 -0.14 l\n-2.42 -0.22 l\n-2.40
      -0.32 l\n-2.39 -0.40 l\n-2.37 -0.50 l\n-2.35 -0.58 l\n16.77 -62.60
      l\ncp p3\nnp\n303.58 492.39 m\n-0.42 3.21 l\no\n153.64 276.94 441.64
      564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n302.73 494.69 (7) 1 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 0.2510 1 0 } def\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 295.27 490.74 m\n-2.33 -0.67
      l\n-2.30 -0.76 l\n-2.28 -0.84 l\n-2.24 -0.93 l\n-2.20 -1.01 l\n-2.16
      -1.09 l\n-2.12 -1.18 l\n32.40 -56.12 l\ncp p3\nnp\n287.24 488.01
      m\n-1.24 3.00 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12
      s\n0 0 0 rgb\n284.76 489.90 (8) 1 0 0 t\n212.68 350.38 411.40 505.90
      cl\n/bg { 0 1 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n
      279.64 484.26 m\n-2.08 -1.25 l\n-2.03 -1.32 l\n-1.97 -1.41 l\n-1.92
      -1.47 l\n-1.87 -1.55 l\n-1.81 -1.61 l\n-1.74 -1.68 l\n45.82 -45.83
      l\ncp p3\nnp\n272.59 479.55 m\n-1.97 2.57 l\no\n153.64 276.94 441.64
      564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n268.65 480.59 (9) 1 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 0 1 0.2510 } def\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 266.22 473.97 m\n-1.68 -1.75
      l\n-1.62 -1.81 l\n-1.54 -1.86 l\n-1.48 -1.93 l\n-1.40 -1.97 l\n-1.33
      -2.03 l\n-1.25 -2.08 l\n56.12 -32.40 l\ncp p3\nnp\n260.63 467.59
      m\n-2.57 1.98 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12
      s\n0 0 0 rgb\n255.49 467.43 (10) 1 0 0 t\n212.68 350.38 411.40 505.90
      cl\n/bg { 0 1 0.5020 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 255.92 460.54 m\n-1.17 -2.12 l\n-1.09 -2.16 l\n-1.02
      -2.20 l\n-0.92 -2.24 l\n-0.85 -2.27 l\n-0.75 -2.30 l\n-0.67 -2.33
      l\n62.59 -16.78 l\ncp p3\nnp\n252.17 452.94 m\n-2.99 1.24 l\no\n153.64
      276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n246.18 451.20
      (11) 1 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg { 0 1 0.7490 } def\n0
      0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 249.45 444.92 m\n-0.59
      -2.36 l\n-0.49 -2.37 l\n-0.41 -2.39 l\n-0.31 -2.40 l\n-0.23 -2.41
      l\n-0.14 -2.42 l\n-0.04 -2.43 l\n64.80 -0.00 l\ncp p3\nnp\n247.79
      436.60 m\n-3.21 0.43 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R
      12 s\n0 0 0 rgb\n241.37 433.23 (12) 1 0 0 t\n212.68 350.38 411.40
      505.90 cl\n/bg { 0 1 1 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 247.24 428.14 m\n0.04 -2.42 l\n0.14 -2.42 l\n0.23 -2.41
      l\n0.31 -2.40 l\n0.41 -2.39 l\n0.49 -2.37 l\n0.59 -2.36 l\n62.59 16.77
      l\ncp p3\nnp\n247.79 419.69 m\n-3.21 -0.43 l\no\n153.64 276.94 441.64
      564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n241.37 414.74 (13) 1 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 0 0.7490 1 } def\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 249.45 411.37 m\n0.67 -2.33
      l\n0.75 -2.30 l\n0.85 -2.27 l\n0.92 -2.24 l\n1.02 -2.20 l\n1.09 -2.16
      l\n1.17 -2.13 l\n56.12 32.40 l\ncp p3\nnp\n252.17 403.35 m\n-2.99 -1.24
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n246.18 396.65 (14) 1 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg {
      0 0.5020 1 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n
      255.92 395.74 m\n1.25 -2.07 l\n1.33 -2.03 l\n1.40 -1.97 l\n1.48 -1.93
      l\n1.54 -1.86 l\n1.62 -1.81 l\n1.68 -1.75 l\n45.82 45.82 l\ncp
      p3\nnp\n260.63 388.70 m\n-2.57 -1.98 l\no\n153.64 276.94 441.64 564.94
      cl\n/ps 12 def R 12 s\n0 0 0 rgb\n255.49 380.65 (15) 1 0 0 t\n212.68
      350.38 411.40 505.90 cl\n/bg { 0 0.2510 1 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n 266.22 382.32 m\n1.74 -1.68 l\n1.81
      -1.61 l\n1.87 -1.55 l\n1.92 -1.47 l\n1.97 -1.41 l\n2.03 -1.32 l\n2.08
      -1.25 l\n32.40 56.11 l\ncp p3\nnp\n272.59 376.74 m\n-1.97 -2.57
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n268.65 367.49 (16) 1 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg {
      0 0 1 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 279.64
      372.03 m\n2.12 -1.18 l\n2.16 -1.09 l\n2.20 -1.01 l\n2.24 -0.93 l\n2.28
      -0.84 l\n2.30 -0.76 l\n2.33 -0.67 l\n16.77 62.59 l\ncp p3\nnp\n287.24
      368.28 m\n-1.24 -3.00 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def
      R 12 s\n0 0 0 rgb\n284.76 358.07 (17) 1 0 0 t\n212.68 350.38 411.40
      505.90 cl\n/bg { 0.2510 0 1 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 295.27 365.55 m\n2.35 -0.58 l\n2.37 -0.50 l\n2.39 -0.40
      l\n2.40 -0.32 l\n2.42 -0.22 l\n2.41 -0.14 l\n2.43 -0.05 l\n0.00 64.80
      l\ncp p3\nnp\n303.58 363.90 m\n-0.42 -3.21 l\no\n153.64 276.94 441.64
      564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n302.73 353.37 (18) 1 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 0.5020 0 1 } def\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 312.04 363.34 m\n2.42 0.05
      l\n2.42 0.14 l\n2.41 0.22 l\n2.41 0.32 l\n2.38 0.40 l\n2.38 0.50
      l\n2.35 0.58 l\n-16.77 62.59 l\ncp p3\nnp\n320.50 363.90 m\n0.42 -3.21
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n321.34 353.37 (19) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg {
      0.7490 0 1 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n
      328.81 365.55 m\n2.33 0.67 l\n2.30 0.76 l\n2.27 0.84 l\n2.24 0.93
      l\n2.20 1.01 l\n2.17 1.09 l\n2.12 1.18 l\n-32.40 56.11 l\ncp
      p3\nnp\n336.84 368.28 m\n1.24 -3.00 l\no\n153.64 276.94 441.64 564.94
      cl\n/ps 12 def R 12 s\n0 0 0 rgb\n339.32 358.19 (20) 0 0 0 t\n212.68
      350.38 411.40 505.90 cl\n/bg { 1 0 1 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n 344.44 372.03 m\n2.07 1.25 l\n2.03
      1.32 l\n1.98 1.41 l\n1.92 1.47 l\n1.87 1.55 l\n1.80 1.61 l\n1.75 1.68
      l\n-45.82 45.82 l\ncp p3\nnp\n351.49 376.74 m\n1.97 -2.57 l\no\n153.64
      276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n355.43 367.38
      (21) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg { 1 0 0.7490 } def\n0
      0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 357.86 382.32 m\n1.68
      1.75 l\n1.61 1.81 l\n1.55 1.86 l\n1.48 1.93 l\n1.40 1.97 l\n1.33 2.03
      l\n1.25 2.07 l\n-56.12 32.40 l\ncp p3\nnp\n363.45 388.70 m\n2.57 -1.98
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n368.59 380.53 (22) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg {
      1 0 0.5020 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n
      368.16 395.74 m\n1.17 2.13 l\n1.09 2.16 l\n1.01 2.20 l\n0.93 2.24
      l\n0.84 2.27 l\n0.76 2.30 l\n0.67 2.33 l\n-62.59 16.77 l\ncp
      p3\nnp\n371.91 403.35 m\n2.99 -1.24 l\no\n153.64 276.94 441.64 564.94
      cl\n/ps 12 def R 12 s\n0 0 0 rgb\n377.89 396.76 (23) 0 0 0 t\n212.68
      350.38 411.40 505.90 cl\n/bg { 1 0 0.2510 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n 374.63 411.37 m\n0.58 2.36 l\n0.50
      2.37 l\n0.40 2.39 l\n0.32 2.40 l\n0.23 2.41 l\n0.13 2.42 l\n0.05 2.42
      l\n-64.80 -0.00 l\ncp p3\nnp\n376.28 419.69 m\n3.22 -0.43 l\no\n153.64
      276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n382.71 414.62
      (24) 0 0 0 t\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      title(main = "A Sample Color Wheel", cex.main = 1.4,

      \ \ \ \ font.main = 3);

      title(xlab = "(Use this as a test of monitor linearity)",

      \ \ \ \ cex.lab = 0.8, font.lab = 3);

      pie.sales \<less\>- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12);

      names(pie.sales) \<less\>- c("Blueberry", "Cherry", "Apple",

      \ \ \ \ "Boston Cream", "Other", "Vanilla Cream");

      pie(pie.sales, col = c("purple", "violetred1", "green3",

      \ \ \ \ "cornsilk", "cyan", "white"));

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 0.7451 0.7451 0.7451 } def\n0.00 0.00 595.28 841.89 r
      p2\n212.68 350.38 411.40 505.90 cl\n212.68 350.38 411.40 505.90 cl\n/bg
      { 0.6275 0.1255 0.9412 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 369.64 428.14 m\n-0.03 1.89 l\n-0.10 1.89 l\n-0.15 1.88
      l\n-0.22 1.88 l\n-0.27 1.86 l\n-0.34 1.86 l\n-0.40 1.85 l\n-0.46 1.83
      l\n-0.52 1.81 l\n-0.58 1.80 l\n-0.64 1.78 l\n-0.69 1.75 l\n-0.75 1.74
      l\n-0.81 1.70 l\n-0.87 1.68 l\n-0.92 1.65 l\n-0.97 1.62 l\n-1.02 1.58
      l\n-1.08 1.56 l\n-1.12 1.51 l\n-1.18 1.48 l\n-1.22 1.44 l\n-1.27 1.39
      l\n-41.99 -39.43 l\ncp p3\nnp\n365.59 449.35 m\n2.68 1.06 l\no\n153.64
      276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n370.95 448.45
      (Blueberry) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg { 1 0.2431
      0.5882 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 354.03
      467.57 m\n-1.28 1.33 l\n-1.33 1.28 l\n-1.36 1.23 l\n-1.40 1.19 l\n-1.44
      1.15 l\n-1.47 1.10 l\n-1.51 1.06 l\n-1.55 1.00 l\n-1.57 0.95 l\n-1.60
      0.91 l\n-1.63 0.85 l\n-1.66 0.80 l\n-1.68 0.74 l\n-1.71 0.70 l\n-1.72
      0.63 l\n-1.75 0.59 l\n-1.76 0.52 l\n-1.78 0.47 l\n-1.80 0.41 l\n-1.80
      0.36 l\n-1.82 0.29 l\n-1.82 0.24 l\n-1.83 0.18 l\n-1.84 0.12 l\n-1.84
      0.07 l\n-1.84 0.00 l\n-1.84 -0.06 l\n-1.84 -0.11 l\n-1.83 -0.17
      l\n-1.82 -0.23 l\n-1.82 -0.29 l\n-1.81 -0.35 l\n-1.79 -0.40 l\n-1.78
      -0.46 l\n-1.77 -0.52 l\n-1.75 -0.58 l\n-1.73 -0.63 l\n-1.71 -0.68
      l\n-1.68 -0.74 l\n-1.66 -0.79 l\n-1.64 -0.85 l\n-1.60 -0.89 l\n-1.58
      -0.95 l\n-1.55 -1.00 l\n-1.51 -1.04 l\n-1.48 -1.10 l\n-1.44 -1.14
      l\n-1.41 -1.19 l\n-1.37 -1.23 l\n-1.33 -1.27 l\n-1.28 -1.32 l\n-1.25
      -1.35 l\n-1.20 -1.40 l\n-1.15 -1.43 l\n-1.11 -1.47 l\n-1.06 -1.50
      l\n-1.01 -1.54 l\n-0.97 -1.57 l\n-0.91 -1.60 l\n50.48 -27.75 l\ncp
      p3\nnp\n304.82 485.29 m\n-0.36 2.86 l\no\n153.64 276.94 441.64 564.94
      cl\n/ps 12 def R 12 s\n0 0 0 rgb\n304.10 487.87 (Cherry) 1 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 0 0.8039 0 } def\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 261.56 455.89 m\n-0.88 -1.66
      l\n-0.82 -1.69 l\n-0.77 -1.72 l\n-0.71 -1.74 l\n-0.66 -1.76 l\n-0.60
      -1.79 l\n-0.53 -1.80 l\n-0.48 -1.82 l\n-0.42 -1.84 l\n-0.36 -1.84
      l\n-0.30 -1.86 l\n-0.24 -1.87 l\n-0.18 -1.87 l\n-0.11 -1.88 l\n-0.06
      -1.88 l\n0.01 -1.88 l\n0.07 -1.88 l\n0.13 -1.88 l\n0.19 -1.87 l\n0.25
      -1.87 l\n0.31 -1.85 l\n0.38 -1.85 l\n0.43 -1.83 l\n0.49 -1.82 l\n0.56
      -1.79 l\n0.61 -1.78 l\n0.67 -1.76 l\n0.72 -1.74 l\n0.78 -1.71 l\n0.84
      -1.69 l\n0.89 -1.65 l\n0.95 -1.63 l\n1.00 -1.60 l\n1.05 -1.56 l\n1.10
      -1.52 l\n1.15 -1.49 l\n1.19 -1.46 l\n1.25 -1.41 l\n1.29 -1.37 l\n1.33
      -1.32 l\n1.38 -1.29 l\n1.42 -1.24 l\n1.46 -1.19 l\n1.49 -1.14 l\n1.53
      -1.09 l\n1.57 -1.04 l\n1.60 -0.99 l\n1.63 -0.94 l\n1.66 -0.88 l\n1.69
      -0.83 l\n24.53 52.11 l\ncp p3\nnp\n257.26 410.35 m\n-2.74 -0.89
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n251.78 405.50 (Apple) 1 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg
      { 1 0.9725 0.8627 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 287.51 376.03 m\n1.71 -0.77 l\n1.72 -0.71 l\n1.75 -0.66
      l\n1.77 -0.60 l\n1.79 -0.54 l\n1.80 -0.48 l\n1.82 -0.43 l\n1.83 -0.36
      l\n1.85 -0.31 l\n1.85 -0.24 l\n1.86 -0.19 l\n1.86 -0.12 l\n1.87 -0.07
      l\n1.86 0.00 l\n1.87 0.06 l\n1.87 0.11 l\n1.86 0.18 l\n1.85 0.24
      l\n1.84 0.30 l\n1.84 0.36 l\n1.82 0.41 l\n1.80 0.48 l\n1.79 0.53
      l\n1.77 0.59 l\n1.76 0.65 l\n1.73 0.71 l\n1.70 0.76 l\n1.68 0.81
      l\n1.65 0.87 l\n1.63 0.93 l\n1.59 0.97 l\n-30.86 48.63 l\ncp
      p3\nnp\n315.65 370.66 m\n0.19 -2.88 l\no\n153.64 276.94 441.64 564.94
      cl\n/ps 12 def R 12 s\n0 0 0 rgb\n316.02 360.60 (Boston Cream) 0 0 0
      t\n212.68 350.38 411.40 505.90 cl\n/bg { 0 1 1 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n 342.90 379.51 m\n1.73 1.14 l\n1.68
      1.20 l\n1.64 1.26 l\n1.59 1.32 l\n1.55 1.37 l\n1.49 1.43 l\n1.45 1.48
      l\n-41.99 39.43 l\ncp p3\nnp\n348.75 383.76 m\n1.84 -2.22 l\no\n153.64
      276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0 rgb\n352.42 375.02
      (Other) 0 0 0 t\n212.68 350.38 411.40 505.90 cl\n/bg { 1 1 1 } def\n0 0
      0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n 354.03 388.71 m\n1.27 1.40
      l\n1.22 1.44 l\n1.18 1.48 l\n1.12 1.51 l\n1.08 1.55 l\n1.02 1.59
      l\n0.97 1.62 l\n0.92 1.65 l\n0.87 1.68 l\n0.81 1.70 l\n0.75 1.74
      l\n0.69 1.75 l\n0.64 1.78 l\n0.58 1.80 l\n0.52 1.81 l\n0.46 1.83
      l\n0.40 1.85 l\n0.34 1.86 l\n0.27 1.86 l\n0.22 1.88 l\n0.15 1.88
      l\n0.10 1.89 l\n0.03 1.88 l\n-57.60 -0.00 l\ncp p3\nnp\n365.59 406.94
      m\n2.68 -1.06 l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12
      s\n0 0 0 rgb\n370.95 400.51 (Vanilla Cream) 0 0 0
      t\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      title(main = "January Pie Sales", cex.main = 1.8,

      \ \ \ \ font.main = 1);

      title(xlab = "(Don't try this at home kids)", cex.lab = 0.8,

      \ \ \ \ font.lab = 3);

      par(bg = "cornsilk");

      n \<less\>- 10;

      g \<less\>- gl(n, 100, n * 100);

      x \<less\>- rnorm(n * 100) + sqrt(codes(g));

      boxplot(split(x, g), col = "lavender", notch = TRUE);

      v()
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 1 0.9725 0.8627 } def\n0.00 0.00 595.28 841.89 r
      p2\n212.68 350.38 411.40 505.90 cl\n212.68 350.38 411.40 505.90 cl\n/bg
      { 0.9020 0.9020 0.9804 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n 221.88 389.50 m\n14.72 0 l\n-0.00 6.76 l\n-3.68 3.50
      l\n3.68 3.50 l\n-0.00 8.41 l\n-14.72 0.00 l\n-0.00 -8.41 l\n3.68 -3.50
      l\n-3.68 -3.50 l\ncp p3\nnp\n225.56 399.76 m\n7.36 -0.00 l\no\n0.75
      setlinewidth\n[ 3.00 5.00] 0 setdash\nnp\n229.24 365.75 m\n0.00 23.75
      l\no\nnp\n229.24 442.31 m\n0.00 -30.64 l\no\n0.75 setlinewidth\n[] 0
      setdash\nnp\n225.56 365.75 m\n7.36 0 l\no\nnp\n225.56 442.31 m\n7.36
      0.00 l\no\n229.24 356.14 2.70 c p1\n229.24 448.06 2.70 c p1\n229.24
      454.44 2.70 c p1\nnp\n 240.28 407.49 m\n14.72 0.00 l\n0 6.71 l\n-3.68
      3.06 l\n3.68 3.06 l\n0 6.54 l\n-14.72 0.00 l\n0.00 -6.54 l\n3.68 -3.06
      l\n-3.68 -3.06 l\ncp p3\nnp\n243.96 417.26 m\n7.36 -0.00 l\no\n0.75
      setlinewidth\n[ 3.00 5.00] 0 setdash\nnp\n247.64 383.76 m\n-0.00 23.73
      l\no\nnp\n247.64 453.08 m\n-0.00 -26.22 l\no\n0.75 setlinewidth\n[] 0
      setdash\nnp\n243.96 383.76 m\n7.36 -0.00 l\no\nnp\n243.96 453.08
      m\n7.36 -0.00 l\no\n247.64 459.04 2.70 c p1\n247.64 457.14 2.70 c
      p1\nnp\n 258.68 412.78 m\n14.72 -0.00 l\n-0.00 4.30 l\n-3.68 3.52
      l\n3.68 3.52 l\n-0.00 10.94 l\n-14.72 0.00 l\n0.00 -10.94 l\n3.68 -3.52
      l\n-3.68 -3.52 l\ncp p3\nnp\n262.36 420.60 m\n7.36 0.00 l\no\n0.75
      setlinewidth\n[ 3.00 5.00] 0 setdash\nnp\n266.04 385.84 m\n0.00 26.94
      l\no\nnp\n266.04 466.45 m\n0.00 -31.39 l\no\n0.75 setlinewidth\n[] 0
      setdash\nnp\n262.36 385.84 m\n7.36 -0.00 l\no\nnp\n262.36 466.45
      m\n7.36 -0.00 l\no\n266.04 379.37 2.70 c p1\n266.04 375.96 2.70 c
      p1\n266.04 477.19 2.70 c p1\nnp\n 277.08 412.30 m\n14.72 0.00 l\n0.00
      7.30 l\n-3.68 3.56 l\n3.68 3.56 l\n0.00 8.11 l\n-14.72 -0.00 l\n-0.00
      -8.11 l\n3.68 -3.56 l\n-3.68 -3.56 l\ncp p3\nnp\n280.76 423.16 m\n7.36
      0.00 l\no\n0.75 setlinewidth\n[ 3.00 5.00] 0 setdash\nnp\n284.44 389.42
      m\n-0.00 22.88 l\no\nnp\n284.44 462.84 m\n-0.00 -28.01 l\no\n0.75
      setlinewidth\n[] 0 setdash\nnp\n280.76 389.42 m\n7.36 0.00
      l\no\nnp\n280.76 462.84 m\n7.36 -0.00 l\no\nnp\n 295.48 413.30 m\n14.72
      0.00 l\n-0.00 9.87 l\n-3.68 4.62 l\n3.68 4.62 l\n-0.00 10.11 l\n-14.72
      -0.00 l\n0.00 -10.11 l\n3.68 -4.62 l\n-3.68 -4.62 l\ncp p3\nnp\n299.16
      427.79 m\n7.36 0.00 l\no\n0.75 setlinewidth\n[ 3.00 5.00] 0
      setdash\nnp\n302.84 385.79 m\n-0.00 27.51 l\no\nnp\n302.84 477.95
      m\n-0.00 -35.43 l\no\n0.75 setlinewidth\n[] 0 setdash\nnp\n299.16
      385.79 m\n7.36 0.00 l\no\nnp\n299.16 477.95 m\n7.36 -0.00 l\no\n302.84
      488.54 2.70 c p1\nnp\n 313.88 417.68 m\n14.72 0.00 l\n0.00 8.47
      l\n-3.68 3.98 l\n3.68 3.99 l\n0.00 8.76 l\n-14.72 -0.00 l\n-0.00 -8.76
      l\n3.68 -3.99 l\n-3.68 -3.98 l\ncp p3\nnp\n317.56 430.13 m\n7.36 -0.00
      l\no\n0.75 setlinewidth\n[ 3.00 5.00] 0 setdash\nnp\n321.24 390.28
      m\n0.00 27.40 l\no\nnp\n321.24 468.24 m\n0.00 -25.36 l\no\n0.75
      setlinewidth\n[] 0 setdash\nnp\n317.56 390.28 m\n7.36 -0.00
      l\no\nnp\n317.56 468.24 m\n7.36 0.00 l\no\nnp\n 332.28 423.91 m\n14.72
      0.00 l\n0 5.46 l\n-3.68 3.53 l\n3.68 3.53 l\n0 9.84 l\n-14.72 -0.00
      l\n-0.00 -9.84 l\n3.68 -3.53 l\n-3.68 -3.53 l\ncp p3\nnp\n335.96 432.90
      m\n7.36 -0.00 l\no\n0.75 setlinewidth\n[ 3.00 5.00] 0
      setdash\nnp\n339.64 396.99 m\n-0.00 26.92 l\no\nnp\n339.64 473.04
      m\n-0.00 -26.77 l\no\n0.75 setlinewidth\n[] 0 setdash\nnp\n335.96
      396.99 m\n7.36 0.00 l\no\nnp\n335.96 473.04 m\n7.36 0.00 l\no\nnp\n
      350.68 426.40 m\n14.72 -0.00 l\n-0.00 6.69 l\n-3.68 2.91 l\n3.68 2.91
      l\n-0.00 5.92 l\n-14.72 -0.00 l\n0.00 -5.92 l\n3.68 -2.91 l\n-3.68
      -2.91 l\ncp p3\nnp\n354.36 436.00 m\n7.36 0 l\no\n0.75 setlinewidth\n[
      3.00 5.00] 0 setdash\nnp\n358.04 399.82 m\n0.00 26.58 l\no\nnp\n358.04
      467.75 m\n0.00 -22.92 l\no\n0.75 setlinewidth\n[] 0 setdash\nnp\n354.36
      399.82 m\n7.36 -0.00 l\no\nnp\n354.36 467.75 m\n7.36 0 l\no\nnp\n
      369.08 426.44 m\n14.72 -0.00 l\n0.00 11.40 l\n-3.68 4.59 l\n3.68 4.60
      l\n0.00 8.48 l\n-14.72 -0.00 l\n-0.00 -8.48 l\n3.68 -4.60 l\n-3.68
      -4.59 l\ncp p3\nnp\n372.76 442.43 m\n7.36 0.00 l\no\n0.75
      setlinewidth\n[ 3.00 5.00] 0 setdash\nnp\n376.44 400.62 m\n-0.00 25.82
      l\no\nnp\n376.44 479.28 m\n-0.00 -23.77 l\no\n0.75 setlinewidth\n[] 0
      setdash\nnp\n372.76 400.62 m\n7.36 0.00 l\no\nnp\n372.76 479.28 m\n7.36
      -0.00 l\no\n376.44 500.14 2.70 c p1\nnp\n 387.48 394.01 m\n14.72 -0.00
      l\n-0.00 9.78 l\n-3.68 4.11 l\n3.68 4.11 l\n-0.00 8.01 l\n-14.72 -0.00
      l\n0.00 -8.01 l\n3.68 -4.11 l\n-3.68 -4.11 l\ncp p3\nnp\n391.16 407.90
      m\n7.36 -0.00 l\no\n0.75 setlinewidth\n[ 3.00 5.00] 0
      setdash\nnp\n394.84 364.75 m\n-0.00 29.26 l\no\nnp\n394.84 454.71
      m\n-0.00 -34.69 l\no\n0.75 setlinewidth\n[] 0 setdash\nnp\n391.16
      364.75 m\n7.36 0 l\no\nnp\n391.16 454.71 m\n7.36 -0.00 l\no\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n229.24 350.38 m\n165.60 -0.00 l\no\nnp\n229.24 350.38
      m\n0.00 -7.20 l\no\nnp\n247.64 350.38 m\n-0.00 -7.20 l\no\nnp\n266.04
      350.38 m\n0.00 -7.20 l\no\nnp\n284.44 350.38 m\n-0.00 -7.20
      l\no\nnp\n302.84 350.38 m\n-0.00 -7.20 l\no\nnp\n321.24 350.38 m\n0.00
      -7.20 l\no\nnp\n339.64 350.38 m\n-0.00 -7.20 l\no\nnp\n358.04 350.38
      m\n0.00 -7.20 l\no\nnp\n376.44 350.38 m\n-0.00 -7.20 l\no\nnp\n394.84
      350.38 m\n-0.00 -7.20 l\no\n/ps 12 def R 12 s\n229.24 324.46 (1) .5 0 0
      t\n247.64 324.46 (2) .5 0 0 t\n266.04 324.46 (3) .5 0 0 t\n284.44
      324.46 (4) .5 0 0 t\n302.84 324.46 (5) .5 0 0 t\n321.24 324.46 (6) .5 0
      0 t\n339.64 324.46 (7) .5 0 0 t\n358.04 324.46 (8) .5 0 0 t\n376.44
      324.46 (9) .5 0 0 t\nnp\n212.68 351.45 m\n0.00 136.93 l\no\nnp\n212.68
      351.45 m\n-7.20 -0.00 l\no\nnp\n212.68 385.68 m\n-7.20 0.00
      l\no\nnp\n212.68 419.92 m\n-7.20 0.00 l\no\nnp\n212.68 454.15 m\n-7.20
      -0.00 l\no\nnp\n212.68 488.38 m\n-7.20 -0.00 l\no\n195.40 351.45 (-2)
      .5 0 90 t\n195.40 385.68 (0) .5 0 90 t\n195.40 419.92 (2) .5 0 90
      t\n195.40 454.15 (4) .5 0 90 t\n195.40 488.38 (6) .5 0 90 t\nnp\n212.68
      350.38 m\n198.72 -0.00 l\n-0.00 155.52 l\n-198.72 -0.00 l\n0.00 -155.52
      l\no\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      title(main = "Notched Boxplots", xlab = "Group", font.main = 4,

      \ \ \ \ font.lab = 1);

      par(bg = "white");

      n \<less\>- 100;

      x \<less\>- c(0, cumsum(rnorm(n)));

      y \<less\>- c(0, cumsum(rnorm(n)));

      xx \<less\>- c(0:n, n:0);

      yy \<less\>- c(x, rev(y));

      plot(xx, yy, type = "n", xlab = "Time", ylab = "Distance");

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 1 1 1 } def\n0.00 0.00 595.28 841.89 r p2\n212.68 350.38
      411.40 505.90 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n220.04 350.38 m\n184.00 -0.00
      l\no\nnp\n220.04 350.38 m\n-0.00 -7.20 l\no\nnp\n256.84 350.38 m\n-0.00
      -7.20 l\no\nnp\n293.64 350.38 m\n-0.00 -7.20 l\no\nnp\n330.44 350.38
      m\n-0.00 -7.20 l\no\nnp\n367.24 350.38 m\n0.00 -7.20 l\no\nnp\n404.04
      350.38 m\n0.00 -7.20 l\no\n/ps 12 def R 12 s\n220.04 324.46 (0) .5 0 0
      t\n256.84 324.46 (20) .5 0 0 t\n293.64 324.46 (40) .5 0 0 t\n330.44
      324.46 (60) .5 0 0 t\n367.24 324.46 (80) .5 0 0 t\n404.04 324.46 (100)
      .5 0 0 t\nnp\n212.68 372.93 m\n0.00 128.66 l\no\nnp\n212.68 372.93
      m\n-7.20 0.00 l\no\nnp\n212.68 415.82 m\n-7.20 -0.00 l\no\nnp\n212.68
      458.71 m\n-7.20 -0.00 l\no\nnp\n212.68 501.59 m\n-7.20 -0.00
      l\no\n195.40 372.93 (-5) .5 0 90 t\n195.40 415.82 (0) .5 0 90 t\n195.40
      458.71 (5) .5 0 90 t\n195.40 501.59 (10) .5 0 90 t\nnp\n212.68 350.38
      m\n198.72 -0.00 l\n-0.00 155.52 l\n-198.72 -0.00 l\n0.00 -155.52
      l\no\n153.64 276.94 441.64 564.94 cl\n/ps 12 def R 12 s\n0 0 0
      rgb\n312.04 295.66 (Time) .5 0 0 t\n166.60 428.14 (Distance) .5 0 90
      t\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      polygon(xx, yy, col = "gray");

      title("Distance Between Brownian Motions");

      x \<less\>- c(0, 0.4, 0.86, 0.85, 0.69, 0.48, 0.54, 1.09,

      \ \ \ \ 1.11, 1.73, 2.05, 2.02);

      par(bg = "lightgray");

      plot(x, type = "n", axes = FALSE, ann = FALSE);

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 0.8275 0.8275 0.8275 } def\n0.00 0.00 595.28 841.89 r
      p2\n212.68 350.38 411.40 505.90 cl\nep\n%%Trailer\n%%Pages:
      2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      usr \<less\>- par("usr");

      rect(usr[1], usr[3], usr[2], usr[4], col = "cornsilk",

      \ \ \ \ border = "black");

      lines(x, col = "blue");

      points(x, pch = 21, bg = "lightcyan", cex = 1.25);

      axis(2, col.axis = "blue", las = 1);

      axis(1, at = 1:12, lab = month.abb, col.axis = "blue");

      box();

      title(main = "The Level of Interest in R", font.main = 4,

      \ \ \ \ col.main = "red");

      title(xlab = "1996", col.lab = "red");

      par(bg = "cornsilk");

      x \<less\>- rnorm(1000);

      hist(x, xlim = range(-4, 4, x), col = "lavender",

      \ \ \ \ main = "");

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 1 0.9725 0.8627 } def\n0.00 0.00 595.28 841.89 r
      p2\n212.68 350.38 411.40 505.90 cl\n153.64 276.94 441.64 564.94 cl\n/ps
      12 def R 12 s\n0 0 0 rgb\n312.04 295.66 (x) .5 0 0 t\n166.60 428.14
      (Frequency) .5 0 90 t\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n220.04 350.38 m\n184.00 -0.00
      l\no\nnp\n220.04 350.38 m\n-0.00 -7.20 l\no\nnp\n266.04 350.38 m\n0.00
      -7.20 l\no\nnp\n312.04 350.38 m\n0.00 -7.20 l\no\nnp\n358.04 350.38
      m\n0.00 -7.20 l\no\nnp\n404.04 350.38 m\n0.00 -7.20 l\no\n/ps 12 def R
      12 s\n220.04 324.46 (-4) .5 0 0 t\n266.04 324.46 (-2) .5 0 0 t\n312.04
      324.46 (0) .5 0 0 t\n358.04 324.46 (2) .5 0 0 t\n404.04 324.46 (4) .5 0
      0 t\nnp\n212.68 356.14 m\n0.00 137.80 l\no\nnp\n212.68 356.14 m\n-7.20
      -0.00 l\no\nnp\n212.68 390.59 m\n-7.20 -0.00 l\no\nnp\n212.68 425.04
      m\n-7.20 0.00 l\no\nnp\n212.68 459.49 m\n-7.20 0.00 l\no\nnp\n212.68
      493.94 m\n-7.20 -0.00 l\no\n195.40 356.14 (0) .5 0 90 t\n195.40 390.59
      (50) .5 0 90 t\n195.40 425.04 (100) .5 0 90 t\n195.40 459.49 (150) .5 0
      90 t\n195.40 493.94 (200) .5 0 90 t\n212.68 350.38 411.40 505.90
      cl\n/bg { 0.9020 0.9020 0.9804 } def\n0 0 0 rgb\n0.75 setlinewidth\n[]
      0 setdash\n231.54 356.14 11.50 0.69 r p3\n243.04 356.14 11.50 2.76 r
      p3\n254.54 356.14 11.50 10.33 r p3\n266.04 356.14 11.50 37.89 r
      p3\n277.54 356.14 11.50 59.94 r p3\n289.04 356.14 11.50 94.39 r
      p3\n300.54 356.14 11.50 140.56 r p3\n312.04 356.14 11.50 144.00 r
      p3\n323.54 356.14 11.50 102.66 r p3\n335.04 356.14 11.50 59.25 r
      p3\n346.54 356.14 11.50 24.11 r p3\n358.04 356.14 11.50 9.65 r
      p3\n369.54 356.14 11.50 2.07 r p3\n381.04 356.14 11.50 0.69 r
      p3\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      title(main = "1000 Normal Random Variates", font.main = 3);

      data("iris");

      pairs(iris[1:4], main = "Edgar Anderson's Iris Data",

      \ \ \ \ font.main = 4, pch = 19);

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 1 0.9725 0.8627 } def\n0.00 0.00 595.28 841.89 r
      p2\n196.41 464.43 239.89 503.17 cl\n153.64 276.94 441.64 564.94 cl\n0 0
      0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n196.41 464.43 m\n43.48 0.00
      l\n-0.00 38.74 l\n-43.48 0.00 l\n-0.00 -38.74 l\no\n196.41 464.43
      239.89 503.17 cl\n/ps 16 def R 16 s\n0 0 0 rgb\n218.15 479.66
      (Sepal.Length) .5 0 0 t\n249.40 464.43 292.89 503.17 cl\n153.64 276.94
      441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 464.43 m\n43.49 0.00 l\n-0.00 38.74 l\n-43.49 0.00
      l\n0.00 -38.74 l\no\nnp\n251.01 503.17 m\n33.55 0.00 l\no\nnp\n251.01
      503.17 m\n-0.00 4.75 l\no\nnp\n259.40 503.17 m\n-0.00 4.75
      l\no\nnp\n267.79 503.17 m\n0.00 4.75 l\no\nnp\n276.18 503.17 m\n0.00
      4.75 l\no\nnp\n284.56 503.17 m\n0.00 4.75 l\no\n/ps 8 def R 8 s\n251.01
      514.57 (2.0) .5 0 0 t\n276.18 514.57 (3.5) .5 0 0 t\n249.40 464.43
      292.89 503.17 cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n276.18 473.84 1.78 c p3\n267.79 471.85 1.78 c p3\n271.14
      469.85 1.78 c p3\n269.46 468.86 1.78 c p3\n277.85 472.84 1.78 c
      p3\n282.89 476.83 1.78 c p3\n274.50 468.86 1.78 c p3\n274.50 472.84
      1.78 c p3\n266.11 466.86 1.78 c p3\n269.46 471.85 1.78 c p3\n279.53
      476.83 1.78 c p3\n274.50 470.85 1.78 c p3\n267.79 470.85 1.78 c
      p3\n267.79 465.87 1.78 c p3\n284.56 480.81 1.78 c p3\n291.28 479.82
      1.78 c p3\n282.89 476.83 1.78 c p3\n276.18 473.84 1.78 c p3\n281.21
      479.82 1.78 c p3\n281.21 473.84 1.78 c p3\n274.50 476.83 1.78 c
      p3\n279.53 473.84 1.78 c p3\n277.85 468.86 1.78 c p3\n272.82 473.84
      1.78 c p3\n274.50 470.85 1.78 c p3\n267.79 472.84 1.78 c p3\n274.50
      472.84 1.78 c p3\n276.18 474.83 1.78 c p3\n274.50 474.83 1.78 c
      p3\n271.14 469.85 1.78 c p3\n269.46 470.85 1.78 c p3\n274.50 476.83
      1.78 c p3\n286.24 474.83 1.78 c p3\n287.92 477.82 1.78 c p3\n269.46
      471.85 1.78 c p3\n271.14 472.84 1.78 c p3\n276.18 477.82 1.78 c
      p3\n277.85 471.85 1.78 c p3\n267.79 466.86 1.78 c p3\n274.50 473.84
      1.78 c p3\n276.18 472.84 1.78 c p3\n256.04 467.86 1.78 c p3\n271.14
      466.86 1.78 c p3\n276.18 472.84 1.78 c p3\n281.21 473.84 1.78 c
      p3\n267.79 470.85 1.78 c p3\n281.21 473.84 1.78 c p3\n271.14 468.86
      1.78 c p3\n279.53 475.83 1.78 c p3\n272.82 472.84 1.78 c p3\n271.14
      492.77 1.78 c p3\n271.14 486.79 1.78 c p3\n269.46 491.77 1.78 c
      p3\n256.04 477.82 1.78 c p3\n264.43 487.79 1.78 c p3\n264.43 479.82
      1.78 c p3\n272.82 485.79 1.78 c p3\n257.72 471.85 1.78 c p3\n266.11
      488.78 1.78 c p3\n262.75 474.83 1.78 c p3\n251.01 472.84 1.78 c
      p3\n267.79 481.81 1.78 c p3\n254.36 482.80 1.78 c p3\n266.11 483.80
      1.78 c p3\n266.11 478.82 1.78 c p3\n269.46 489.78 1.78 c p3\n267.79
      478.82 1.78 c p3\n262.75 480.81 1.78 c p3\n254.36 484.80 1.78 c
      p3\n259.40 478.82 1.78 c p3\n271.14 481.81 1.78 c p3\n264.43 483.80
      1.78 c p3\n259.40 485.79 1.78 c p3\n264.43 483.80 1.78 c p3\n266.11
      486.79 1.78 c p3\n267.79 488.78 1.78 c p3\n264.43 490.77 1.78 c
      p3\n267.79 489.78 1.78 c p3\n266.11 482.80 1.78 c p3\n261.08 479.82
      1.78 c p3\n257.72 477.82 1.78 c p3\n257.72 477.82 1.78 c p3\n262.75
      480.81 1.78 c p3\n262.75 482.80 1.78 c p3\n267.79 476.83 1.78 c
      p3\n274.50 482.80 1.78 c p3\n269.46 489.78 1.78 c p3\n256.04 485.79
      1.78 c p3\n267.79 478.82 1.78 c p3\n259.40 477.82 1.78 c p3\n261.08
      477.82 1.78 c p3\n267.79 483.80 1.78 c p3\n261.08 480.81 1.78 c
      p3\n256.04 472.84 1.78 c p3\n262.75 478.82 1.78 c p3\n267.79 479.82
      1.78 c p3\n266.11 479.82 1.78 c p3\n266.11 484.80 1.78 c p3\n259.40
      473.84 1.78 c p3\n264.43 479.82 1.78 c p3\n272.82 485.79 1.78 c
      p3\n262.75 480.81 1.78 c p3\n267.79 493.76 1.78 c p3\n266.11 485.79
      1.78 c p3\n267.79 487.79 1.78 c p3\n267.79 498.75 1.78 c p3\n259.40
      471.85 1.78 c p3\n266.11 495.76 1.78 c p3\n259.40 489.78 1.78 c
      p3\n277.85 494.76 1.78 c p3\n271.14 487.79 1.78 c p3\n262.75 486.79
      1.78 c p3\n267.79 490.77 1.78 c p3\n259.40 479.82 1.78 c p3\n264.43
      480.81 1.78 c p3\n271.14 486.79 1.78 c p3\n267.79 487.79 1.78 c
      p3\n281.21 499.74 1.78 c p3\n261.08 499.74 1.78 c p3\n254.36 482.80
      1.78 c p3\n271.14 491.77 1.78 c p3\n264.43 478.82 1.78 c p3\n264.43
      499.74 1.78 c p3\n262.75 485.79 1.78 c p3\n272.82 489.78 1.78 c
      p3\n271.14 494.76 1.78 c p3\n264.43 484.80 1.78 c p3\n267.79 483.80
      1.78 c p3\n264.43 486.79 1.78 c p3\n267.79 494.76 1.78 c p3\n264.43
      496.75 1.78 c p3\n281.21 501.73 1.78 c p3\n264.43 486.79 1.78 c
      p3\n264.43 485.79 1.78 c p3\n261.08 483.80 1.78 c p3\n267.79 499.74
      1.78 c p3\n274.50 485.79 1.78 c p3\n269.46 486.79 1.78 c p3\n267.79
      482.80 1.78 c p3\n269.46 491.77 1.78 c p3\n269.46 489.78 1.78 c
      p3\n269.46 491.77 1.78 c p3\n262.75 480.81 1.78 c p3\n271.14 490.77
      1.78 c p3\n272.82 489.78 1.78 c p3\n267.79 489.78 1.78 c p3\n259.40
      485.79 1.78 c p3\n267.79 487.79 1.78 c p3\n274.50 484.80 1.78 c
      p3\n267.79 481.81 1.78 c p3\n302.39 464.43 345.88 503.17 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n302.39 464.43 m\n43.49 0.00 l\n-0.00 38.74 l\n-43.49 0.00
      l\n-0.00 -38.74 l\no\n302.39 464.43 345.88 503.17 cl\n/bg { 0 0 0 }
      def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n306.73 473.84 1.78 c
      p3\n306.73 471.85 1.78 c p3\n306.05 469.85 1.78 c p3\n307.41 468.86
      1.78 c p3\n306.73 472.84 1.78 c p3\n308.78 476.83 1.78 c p3\n306.73
      468.86 1.78 c p3\n307.41 472.84 1.78 c p3\n306.73 466.86 1.78 c
      p3\n307.41 471.85 1.78 c p3\n307.41 476.83 1.78 c p3\n308.10 470.85
      1.78 c p3\n306.73 470.85 1.78 c p3\n304.68 465.87 1.78 c p3\n305.37
      480.81 1.78 c p3\n307.41 479.82 1.78 c p3\n306.05 476.83 1.78 c
      p3\n306.73 473.84 1.78 c p3\n308.78 479.82 1.78 c p3\n307.41 473.84
      1.78 c p3\n308.78 476.83 1.78 c p3\n307.41 473.84 1.78 c p3\n304.00
      468.86 1.78 c p3\n308.78 473.84 1.78 c p3\n310.14 470.85 1.78 c
      p3\n308.10 472.84 1.78 c p3\n308.10 472.84 1.78 c p3\n307.41 474.83
      1.78 c p3\n306.73 474.83 1.78 c p3\n308.10 469.85 1.78 c p3\n308.10
      470.85 1.78 c p3\n307.41 476.83 1.78 c p3\n307.41 474.83 1.78 c
      p3\n306.73 477.82 1.78 c p3\n307.41 471.85 1.78 c p3\n305.37 472.84
      1.78 c p3\n306.05 477.82 1.78 c p3\n306.73 471.85 1.78 c p3\n306.05
      466.86 1.78 c p3\n307.41 473.84 1.78 c p3\n306.05 472.84 1.78 c
      p3\n306.05 467.86 1.78 c p3\n306.05 466.86 1.78 c p3\n308.10 472.84
      1.78 c p3\n310.14 473.84 1.78 c p3\n306.73 470.85 1.78 c p3\n308.10
      473.84 1.78 c p3\n306.73 468.86 1.78 c p3\n307.41 475.83 1.78 c
      p3\n306.73 472.84 1.78 c p3\n329.25 492.77 1.78 c p3\n327.89 486.79
      1.78 c p3\n330.62 491.77 1.78 c p3\n324.48 477.82 1.78 c p3\n328.57
      487.79 1.78 c p3\n327.89 479.82 1.78 c p3\n329.25 485.79 1.78 c
      p3\n319.70 471.85 1.78 c p3\n328.57 488.78 1.78 c p3\n323.79 474.83
      1.78 c p3\n321.06 472.84 1.78 c p3\n325.84 481.81 1.78 c p3\n324.48
      482.80 1.78 c p3\n329.25 483.80 1.78 c p3\n321.75 478.82 1.78 c
      p3\n327.20 489.78 1.78 c p3\n327.89 478.82 1.78 c p3\n325.16 480.81
      1.78 c p3\n327.89 484.80 1.78 c p3\n323.79 478.82 1.78 c p3\n329.93
      481.81 1.78 c p3\n324.48 483.80 1.78 c p3\n330.62 485.79 1.78 c
      p3\n329.25 483.80 1.78 c p3\n326.52 486.79 1.78 c p3\n327.20 488.78
      1.78 c p3\n329.93 490.77 1.78 c p3\n331.30 489.78 1.78 c p3\n327.89
      482.80 1.78 c p3\n321.06 479.82 1.78 c p3\n323.11 477.82 1.78 c
      p3\n322.43 477.82 1.78 c p3\n323.79 480.81 1.78 c p3\n331.98 482.80
      1.78 c p3\n327.89 476.83 1.78 c p3\n327.89 482.80 1.78 c p3\n329.25
      489.78 1.78 c p3\n327.20 485.79 1.78 c p3\n325.16 478.82 1.78 c
      p3\n324.48 477.82 1.78 c p3\n327.20 477.82 1.78 c p3\n328.57 483.80
      1.78 c p3\n324.48 480.81 1.78 c p3\n319.70 472.84 1.78 c p3\n325.84
      478.82 1.78 c p3\n325.84 479.82 1.78 c p3\n325.84 479.82 1.78 c
      p3\n326.52 484.80 1.78 c p3\n317.65 473.84 1.78 c p3\n325.16 479.82
      1.78 c p3\n338.12 485.79 1.78 c p3\n331.98 480.81 1.78 c p3\n337.44
      493.76 1.78 c p3\n335.39 485.79 1.78 c p3\n336.76 487.79 1.78 c
      p3\n342.22 498.75 1.78 c p3\n327.89 471.85 1.78 c p3\n340.17 495.76
      1.78 c p3\n336.76 489.78 1.78 c p3\n338.81 494.76 1.78 c p3\n331.98
      487.79 1.78 c p3\n333.35 486.79 1.78 c p3\n334.71 490.77 1.78 c
      p3\n331.30 479.82 1.78 c p3\n331.98 480.81 1.78 c p3\n333.35 486.79
      1.78 c p3\n334.71 487.79 1.78 c p3\n342.90 499.74 1.78 c p3\n344.27
      499.74 1.78 c p3\n331.30 482.80 1.78 c p3\n336.08 491.77 1.78 c
      p3\n330.62 478.82 1.78 c p3\n342.90 499.74 1.78 c p3\n330.62 485.79
      1.78 c p3\n336.08 489.78 1.78 c p3\n338.12 494.76 1.78 c p3\n329.93
      484.80 1.78 c p3\n330.62 483.80 1.78 c p3\n335.39 486.79 1.78 c
      p3\n336.76 494.76 1.78 c p3\n338.81 496.75 1.78 c p3\n340.85 501.73
      1.78 c p3\n335.39 486.79 1.78 c p3\n331.98 485.79 1.78 c p3\n335.39
      483.80 1.78 c p3\n338.81 499.74 1.78 c p3\n335.39 485.79 1.78 c
      p3\n334.71 486.79 1.78 c p3\n329.93 482.80 1.78 c p3\n334.03 491.77
      1.78 c p3\n335.39 489.78 1.78 c p3\n331.98 491.77 1.78 c p3\n331.98
      480.81 1.78 c p3\n337.44 490.77 1.78 c p3\n336.08 489.78 1.78 c
      p3\n332.66 489.78 1.78 c p3\n331.30 485.79 1.78 c p3\n332.66 487.79
      1.78 c p3\n334.03 484.80 1.78 c p3\n331.98 481.81 1.78 c p3\n355.38
      464.43 398.87 503.17 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n355.38 464.43 m\n43.49 0.00
      l\n0.00 38.74 l\n-43.49 0.00 l\n-0.00 -38.74 l\no\nnp\n363.70 503.17
      m\n33.56 0.00 l\no\nnp\n363.70 503.17 m\n-0.00 4.75 l\no\nnp\n372.09
      503.17 m\n-0.00 4.75 l\no\nnp\n380.48 503.17 m\n0.00 4.75
      l\no\nnp\n388.87 503.17 m\n0.00 4.75 l\no\nnp\n397.26 503.17 m\n-0.00
      4.75 l\no\n/ps 8 def R 8 s\n363.70 514.57 (0.5) .5 0 0 t\n388.87 514.57
      (2.0) .5 0 0 t\nnp\n398.87 467.86 m\n0.00 34.87 l\no\nnp\n398.87 467.86
      m\n4.75 0.00 l\no\nnp\n398.87 472.84 m\n4.75 -0.00 l\no\nnp\n398.87
      477.82 m\n4.75 -0.00 l\no\nnp\n398.87 482.80 m\n4.75 0.00
      l\no\nnp\n398.87 487.79 m\n4.75 0.00 l\no\nnp\n398.87 492.77 m\n4.75
      -0.00 l\no\nnp\n398.87 497.75 m\n4.75 0 l\no\nnp\n398.87 502.73 m\n4.75
      0.00 l\no\n415.98 467.86 (4.5) .5 0 90 t\n415.98 487.79 (6.5) .5 0 90
      t\n355.38 464.43 398.87 503.17 cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\n358.67 473.84 1.78 c p3\n358.67 471.85 1.78
      c p3\n358.67 469.85 1.78 c p3\n358.67 468.86 1.78 c p3\n358.67 472.84
      1.78 c p3\n362.03 476.83 1.78 c p3\n360.35 468.86 1.78 c p3\n358.67
      472.84 1.78 c p3\n358.67 466.86 1.78 c p3\n356.99 471.85 1.78 c
      p3\n358.67 476.83 1.78 c p3\n358.67 470.85 1.78 c p3\n356.99 470.85
      1.78 c p3\n356.99 465.87 1.78 c p3\n358.67 480.81 1.78 c p3\n362.03
      479.82 1.78 c p3\n362.03 476.83 1.78 c p3\n360.35 473.84 1.78 c
      p3\n360.35 479.82 1.78 c p3\n360.35 473.84 1.78 c p3\n358.67 476.83
      1.78 c p3\n362.03 473.84 1.78 c p3\n358.67 468.86 1.78 c p3\n363.70
      473.84 1.78 c p3\n358.67 470.85 1.78 c p3\n358.67 472.84 1.78 c
      p3\n362.03 472.84 1.78 c p3\n358.67 474.83 1.78 c p3\n358.67 474.83
      1.78 c p3\n358.67 469.85 1.78 c p3\n358.67 470.85 1.78 c p3\n362.03
      476.83 1.78 c p3\n356.99 474.83 1.78 c p3\n358.67 477.82 1.78 c
      p3\n358.67 471.85 1.78 c p3\n358.67 472.84 1.78 c p3\n358.67 477.82
      1.78 c p3\n356.99 471.85 1.78 c p3\n358.67 466.86 1.78 c p3\n358.67
      473.84 1.78 c p3\n360.35 472.84 1.78 c p3\n360.35 467.86 1.78 c
      p3\n358.67 466.86 1.78 c p3\n365.38 472.84 1.78 c p3\n362.03 473.84
      1.78 c p3\n360.35 470.85 1.78 c p3\n358.67 473.84 1.78 c p3\n358.67
      468.86 1.78 c p3\n358.67 475.83 1.78 c p3\n358.67 472.84 1.78 c
      p3\n378.80 492.77 1.78 c p3\n380.48 486.79 1.78 c p3\n380.48 491.77
      1.78 c p3\n377.13 477.82 1.78 c p3\n380.48 487.79 1.78 c p3\n377.13
      479.82 1.78 c p3\n382.16 485.79 1.78 c p3\n372.09 471.85 1.78 c
      p3\n377.13 488.78 1.78 c p3\n378.80 474.83 1.78 c p3\n372.09 472.84
      1.78 c p3\n380.48 481.81 1.78 c p3\n372.09 482.80 1.78 c p3\n378.80
      483.80 1.78 c p3\n377.13 478.82 1.78 c p3\n378.80 489.78 1.78 c
      p3\n380.48 478.82 1.78 c p3\n372.09 480.81 1.78 c p3\n380.48 484.80
      1.78 c p3\n373.77 478.82 1.78 c p3\n385.51 481.81 1.78 c p3\n377.13
      483.80 1.78 c p3\n380.48 485.79 1.78 c p3\n375.45 483.80 1.78 c
      p3\n377.13 486.79 1.78 c p3\n378.80 488.78 1.78 c p3\n378.80 490.77
      1.78 c p3\n383.84 489.78 1.78 c p3\n380.48 482.80 1.78 c p3\n372.09
      479.82 1.78 c p3\n373.77 477.82 1.78 c p3\n372.09 477.82 1.78 c
      p3\n375.45 480.81 1.78 c p3\n382.16 482.80 1.78 c p3\n380.48 476.83
      1.78 c p3\n382.16 482.80 1.78 c p3\n380.48 489.78 1.78 c p3\n377.13
      485.79 1.78 c p3\n377.13 478.82 1.78 c p3\n377.13 477.82 1.78 c
      p3\n375.45 477.82 1.78 c p3\n378.80 483.80 1.78 c p3\n375.45 480.81
      1.78 c p3\n372.09 472.84 1.78 c p3\n377.13 478.82 1.78 c p3\n375.45
      479.82 1.78 c p3\n377.13 479.82 1.78 c p3\n377.13 484.80 1.78 c
      p3\n373.77 473.84 1.78 c p3\n377.13 479.82 1.78 c p3\n397.26 485.79
      1.78 c p3\n387.19 480.81 1.78 c p3\n390.55 493.76 1.78 c p3\n385.51
      485.79 1.78 c p3\n392.23 487.79 1.78 c p3\n390.55 498.75 1.78 c
      p3\n383.84 471.85 1.78 c p3\n385.51 495.76 1.78 c p3\n385.51 489.78
      1.78 c p3\n397.26 494.76 1.78 c p3\n388.87 487.79 1.78 c p3\n387.19
      486.79 1.78 c p3\n390.55 490.77 1.78 c p3\n388.87 479.82 1.78 c
      p3\n395.58 480.81 1.78 c p3\n393.90 486.79 1.78 c p3\n385.51 487.79
      1.78 c p3\n392.23 499.74 1.78 c p3\n393.90 499.74 1.78 c p3\n380.48
      482.80 1.78 c p3\n393.90 491.77 1.78 c p3\n388.87 478.82 1.78 c
      p3\n388.87 499.74 1.78 c p3\n385.51 485.79 1.78 c p3\n390.55 489.78
      1.78 c p3\n385.51 494.76 1.78 c p3\n385.51 484.80 1.78 c p3\n385.51
      483.80 1.78 c p3\n390.55 486.79 1.78 c p3\n382.16 494.76 1.78 c
      p3\n387.19 496.75 1.78 c p3\n388.87 501.73 1.78 c p3\n392.23 486.79
      1.78 c p3\n380.48 485.79 1.78 c p3\n378.80 483.80 1.78 c p3\n393.90
      499.74 1.78 c p3\n395.58 485.79 1.78 c p3\n385.51 486.79 1.78 c
      p3\n385.51 482.80 1.78 c p3\n390.55 491.77 1.78 c p3\n395.58 489.78
      1.78 c p3\n393.90 491.77 1.78 c p3\n387.19 480.81 1.78 c p3\n393.90
      490.77 1.78 c p3\n397.26 489.78 1.78 c p3\n393.90 489.78 1.78 c
      p3\n387.19 485.79 1.78 c p3\n388.87 487.79 1.78 c p3\n393.90 484.80
      1.78 c p3\n385.51 481.81 1.78 c p3\n196.41 416.19 239.89 454.93
      cl\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n196.41 416.19 m\n43.48 -0.00 l\n-0.00 38.74 l\n-43.48 0.00
      l\n-0.00 -38.74 l\no\nnp\n196.41 417.63 m\n-0.00 29.89 l\no\nnp\n196.41
      417.63 m\n-4.76 -0.00 l\no\nnp\n196.41 425.10 m\n-4.76 0.00
      l\no\nnp\n196.41 432.57 m\n-4.76 -0.00 l\no\nnp\n196.41 440.04 m\n-4.76
      0.00 l\no\nnp\n196.41 447.52 m\n-4.76 -0.00 l\no\n/ps 8 def R 8
      s\n185.00 417.63 (2.0) .5 0 90 t\n185.00 440.04 (3.5) .5 0 90 t\n196.41
      416.19 239.89 454.93 cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\n206.96 440.04 1.78 c p3\n204.73 432.57 1.78
      c p3\n202.49 435.56 1.78 c p3\n201.37 434.07 1.78 c p3\n205.85 441.54
      1.78 c p3\n210.32 446.02 1.78 c p3\n201.37 438.55 1.78 c p3\n205.85
      438.55 1.78 c p3\n199.13 431.08 1.78 c p3\n204.73 434.07 1.78 c
      p3\n210.32 443.03 1.78 c p3\n203.61 438.55 1.78 c p3\n203.61 432.57
      1.78 c p3\n198.02 432.57 1.78 c p3\n214.79 447.52 1.78 c p3\n213.68
      453.49 1.78 c p3\n210.32 446.02 1.78 c p3\n206.96 440.04 1.78 c
      p3\n213.68 444.53 1.78 c p3\n206.96 444.53 1.78 c p3\n210.32 438.55
      1.78 c p3\n206.96 443.03 1.78 c p3\n201.37 441.54 1.78 c p3\n206.96
      437.06 1.78 c p3\n203.61 438.55 1.78 c p3\n205.85 432.57 1.78 c
      p3\n205.85 438.55 1.78 c p3\n208.08 440.04 1.78 c p3\n208.08 438.55
      1.78 c p3\n202.49 435.56 1.78 c p3\n203.61 434.07 1.78 c p3\n210.32
      438.55 1.78 c p3\n208.08 449.01 1.78 c p3\n211.44 450.51 1.78 c
      p3\n204.73 434.07 1.78 c p3\n205.85 435.56 1.78 c p3\n211.44 440.04
      1.78 c p3\n204.73 441.54 1.78 c p3\n199.13 432.57 1.78 c p3\n206.96
      438.55 1.78 c p3\n205.85 440.04 1.78 c p3\n200.25 422.11 1.78 c
      p3\n199.13 435.56 1.78 c p3\n205.85 440.04 1.78 c p3\n206.96 444.53
      1.78 c p3\n203.61 432.57 1.78 c p3\n206.96 444.53 1.78 c p3\n201.37
      435.56 1.78 c p3\n209.20 443.03 1.78 c p3\n205.85 437.06 1.78 c
      p3\n228.22 435.56 1.78 c p3\n221.51 435.56 1.78 c p3\n227.10 434.07
      1.78 c p3\n211.44 422.11 1.78 c p3\n222.62 429.58 1.78 c p3\n213.68
      429.58 1.78 c p3\n220.39 437.06 1.78 c p3\n204.73 423.61 1.78 c
      p3\n223.74 431.08 1.78 c p3\n208.08 428.09 1.78 c p3\n205.85 417.63
      1.78 c p3\n215.91 432.57 1.78 c p3\n217.03 420.62 1.78 c p3\n218.15
      431.08 1.78 c p3\n212.56 431.08 1.78 c p3\n224.86 434.07 1.78 c
      p3\n212.56 432.57 1.78 c p3\n214.79 428.09 1.78 c p3\n219.27 420.62
      1.78 c p3\n212.56 425.10 1.78 c p3\n215.91 435.56 1.78 c p3\n218.15
      429.58 1.78 c p3\n220.39 425.10 1.78 c p3\n218.15 429.58 1.78 c
      p3\n221.51 431.08 1.78 c p3\n223.74 432.57 1.78 c p3\n225.98 429.58
      1.78 c p3\n224.86 432.57 1.78 c p3\n217.03 431.08 1.78 c p3\n213.68
      426.59 1.78 c p3\n211.44 423.61 1.78 c p3\n211.44 423.61 1.78 c
      p3\n214.79 428.09 1.78 c p3\n217.03 428.09 1.78 c p3\n210.32 432.57
      1.78 c p3\n217.03 438.55 1.78 c p3\n224.86 434.07 1.78 c p3\n220.39
      422.11 1.78 c p3\n212.56 432.57 1.78 c p3\n211.44 425.10 1.78 c
      p3\n211.44 426.59 1.78 c p3\n218.15 432.57 1.78 c p3\n214.79 426.59
      1.78 c p3\n205.85 422.11 1.78 c p3\n212.56 428.09 1.78 c p3\n213.68
      432.57 1.78 c p3\n213.68 431.08 1.78 c p3\n219.27 431.08 1.78 c
      p3\n206.96 425.10 1.78 c p3\n213.68 429.58 1.78 c p3\n220.39 437.06
      1.78 c p3\n214.79 428.09 1.78 c p3\n229.33 432.57 1.78 c p3\n220.39
      431.08 1.78 c p3\n222.62 432.57 1.78 c p3\n234.93 432.57 1.78 c
      p3\n204.73 425.10 1.78 c p3\n231.57 431.08 1.78 c p3\n224.86 425.10
      1.78 c p3\n230.45 441.54 1.78 c p3\n222.62 435.56 1.78 c p3\n221.51
      428.09 1.78 c p3\n225.98 432.57 1.78 c p3\n213.68 425.10 1.78 c
      p3\n214.79 429.58 1.78 c p3\n221.51 435.56 1.78 c p3\n222.62 432.57
      1.78 c p3\n236.05 444.53 1.78 c p3\n236.05 426.59 1.78 c p3\n217.03
      420.62 1.78 c p3\n227.10 435.56 1.78 c p3\n212.56 429.58 1.78 c
      p3\n236.05 429.58 1.78 c p3\n220.39 428.09 1.78 c p3\n224.86 437.06
      1.78 c p3\n230.45 435.56 1.78 c p3\n219.27 429.58 1.78 c p3\n218.15
      432.57 1.78 c p3\n221.51 429.58 1.78 c p3\n230.45 432.57 1.78 c
      p3\n232.69 429.58 1.78 c p3\n238.28 444.53 1.78 c p3\n221.51 429.58
      1.78 c p3\n220.39 429.58 1.78 c p3\n218.15 426.59 1.78 c p3\n236.05
      432.57 1.78 c p3\n220.39 438.55 1.78 c p3\n221.51 434.07 1.78 c
      p3\n217.03 432.57 1.78 c p3\n227.10 434.07 1.78 c p3\n224.86 434.07
      1.78 c p3\n227.10 434.07 1.78 c p3\n214.79 428.09 1.78 c p3\n225.98
      435.56 1.78 c p3\n224.86 437.06 1.78 c p3\n224.86 432.57 1.78 c
      p3\n220.39 425.10 1.78 c p3\n222.62 432.57 1.78 c p3\n219.27 438.55
      1.78 c p3\n215.91 432.57 1.78 c p3\n249.40 416.19 292.89 454.93
      cl\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 416.19 m\n43.49 -0.00 l\n-0.00 38.74 l\n-43.49 0.00
      l\n0.00 -38.74 l\no\n249.40 416.19 292.89 454.93 cl\n/ps 16 def R 16
      s\n0 0 0 rgb\n271.14 431.32 (Sepal.Width) .5 0 0 t\n302.39 416.19
      345.88 454.93 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n302.39 416.19 m\n43.49 -0.00 l\n-0.00
      38.74 l\n-43.49 0.00 l\n-0.00 -38.74 l\no\n302.39 416.19 345.88 454.93
      cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n306.73 440.04 1.78 c p3\n306.73 432.57 1.78 c p3\n306.05
      435.56 1.78 c p3\n307.41 434.07 1.78 c p3\n306.73 441.54 1.78 c
      p3\n308.78 446.02 1.78 c p3\n306.73 438.55 1.78 c p3\n307.41 438.55
      1.78 c p3\n306.73 431.08 1.78 c p3\n307.41 434.07 1.78 c p3\n307.41
      443.03 1.78 c p3\n308.10 438.55 1.78 c p3\n306.73 432.57 1.78 c
      p3\n304.68 432.57 1.78 c p3\n305.37 447.52 1.78 c p3\n307.41 453.49
      1.78 c p3\n306.05 446.02 1.78 c p3\n306.73 440.04 1.78 c p3\n308.78
      444.53 1.78 c p3\n307.41 444.53 1.78 c p3\n308.78 438.55 1.78 c
      p3\n307.41 443.03 1.78 c p3\n304.00 441.54 1.78 c p3\n308.78 437.06
      1.78 c p3\n310.14 438.55 1.78 c p3\n308.10 432.57 1.78 c p3\n308.10
      438.55 1.78 c p3\n307.41 440.04 1.78 c p3\n306.73 438.55 1.78 c
      p3\n308.10 435.56 1.78 c p3\n308.10 434.07 1.78 c p3\n307.41 438.55
      1.78 c p3\n307.41 449.01 1.78 c p3\n306.73 450.51 1.78 c p3\n307.41
      434.07 1.78 c p3\n305.37 435.56 1.78 c p3\n306.05 440.04 1.78 c
      p3\n306.73 441.54 1.78 c p3\n306.05 432.57 1.78 c p3\n307.41 438.55
      1.78 c p3\n306.05 440.04 1.78 c p3\n306.05 422.11 1.78 c p3\n306.05
      435.56 1.78 c p3\n308.10 440.04 1.78 c p3\n310.14 444.53 1.78 c
      p3\n306.73 432.57 1.78 c p3\n308.10 444.53 1.78 c p3\n306.73 435.56
      1.78 c p3\n307.41 443.03 1.78 c p3\n306.73 437.06 1.78 c p3\n329.25
      435.56 1.78 c p3\n327.89 435.56 1.78 c p3\n330.62 434.07 1.78 c
      p3\n324.48 422.11 1.78 c p3\n328.57 429.58 1.78 c p3\n327.89 429.58
      1.78 c p3\n329.25 437.06 1.78 c p3\n319.70 423.61 1.78 c p3\n328.57
      431.08 1.78 c p3\n323.79 428.09 1.78 c p3\n321.06 417.63 1.78 c
      p3\n325.84 432.57 1.78 c p3\n324.48 420.62 1.78 c p3\n329.25 431.08
      1.78 c p3\n321.75 431.08 1.78 c p3\n327.20 434.07 1.78 c p3\n327.89
      432.57 1.78 c p3\n325.16 428.09 1.78 c p3\n327.89 420.62 1.78 c
      p3\n323.79 425.10 1.78 c p3\n329.93 435.56 1.78 c p3\n324.48 429.58
      1.78 c p3\n330.62 425.10 1.78 c p3\n329.25 429.58 1.78 c p3\n326.52
      431.08 1.78 c p3\n327.20 432.57 1.78 c p3\n329.93 429.58 1.78 c
      p3\n331.30 432.57 1.78 c p3\n327.89 431.08 1.78 c p3\n321.06 426.59
      1.78 c p3\n323.11 423.61 1.78 c p3\n322.43 423.61 1.78 c p3\n323.79
      428.09 1.78 c p3\n331.98 428.09 1.78 c p3\n327.89 432.57 1.78 c
      p3\n327.89 438.55 1.78 c p3\n329.25 434.07 1.78 c p3\n327.20 422.11
      1.78 c p3\n325.16 432.57 1.78 c p3\n324.48 425.10 1.78 c p3\n327.20
      426.59 1.78 c p3\n328.57 432.57 1.78 c p3\n324.48 426.59 1.78 c
      p3\n319.70 422.11 1.78 c p3\n325.84 428.09 1.78 c p3\n325.84 432.57
      1.78 c p3\n325.84 431.08 1.78 c p3\n326.52 431.08 1.78 c p3\n317.65
      425.10 1.78 c p3\n325.16 429.58 1.78 c p3\n338.12 437.06 1.78 c
      p3\n331.98 428.09 1.78 c p3\n337.44 432.57 1.78 c p3\n335.39 431.08
      1.78 c p3\n336.76 432.57 1.78 c p3\n342.22 432.57 1.78 c p3\n327.89
      425.10 1.78 c p3\n340.17 431.08 1.78 c p3\n336.76 425.10 1.78 c
      p3\n338.81 441.54 1.78 c p3\n331.98 435.56 1.78 c p3\n333.35 428.09
      1.78 c p3\n334.71 432.57 1.78 c p3\n331.30 425.10 1.78 c p3\n331.98
      429.58 1.78 c p3\n333.35 435.56 1.78 c p3\n334.71 432.57 1.78 c
      p3\n342.90 444.53 1.78 c p3\n344.27 426.59 1.78 c p3\n331.30 420.62
      1.78 c p3\n336.08 435.56 1.78 c p3\n330.62 429.58 1.78 c p3\n342.90
      429.58 1.78 c p3\n330.62 428.09 1.78 c p3\n336.08 437.06 1.78 c
      p3\n338.12 435.56 1.78 c p3\n329.93 429.58 1.78 c p3\n330.62 432.57
      1.78 c p3\n335.39 429.58 1.78 c p3\n336.76 432.57 1.78 c p3\n338.81
      429.58 1.78 c p3\n340.85 444.53 1.78 c p3\n335.39 429.58 1.78 c
      p3\n331.98 429.58 1.78 c p3\n335.39 426.59 1.78 c p3\n338.81 432.57
      1.78 c p3\n335.39 438.55 1.78 c p3\n334.71 434.07 1.78 c p3\n329.93
      432.57 1.78 c p3\n334.03 434.07 1.78 c p3\n335.39 434.07 1.78 c
      p3\n331.98 434.07 1.78 c p3\n331.98 428.09 1.78 c p3\n337.44 435.56
      1.78 c p3\n336.08 437.06 1.78 c p3\n332.66 432.57 1.78 c p3\n331.30
      425.10 1.78 c p3\n332.66 432.57 1.78 c p3\n334.03 438.55 1.78 c
      p3\n331.98 432.57 1.78 c p3\n355.38 416.19 398.87 454.93 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n355.38 416.19 m\n43.49 -0.00 l\n0.00 38.74 l\n-43.49 0.00
      l\n-0.00 -38.74 l\no\n355.38 416.19 398.87 454.93 cl\n/bg { 0 0 0 }
      def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n358.67 440.04 1.78 c
      p3\n358.67 432.57 1.78 c p3\n358.67 435.56 1.78 c p3\n358.67 434.07
      1.78 c p3\n358.67 441.54 1.78 c p3\n362.03 446.02 1.78 c p3\n360.35
      438.55 1.78 c p3\n358.67 438.55 1.78 c p3\n358.67 431.08 1.78 c
      p3\n356.99 434.07 1.78 c p3\n358.67 443.03 1.78 c p3\n358.67 438.55
      1.78 c p3\n356.99 432.57 1.78 c p3\n356.99 432.57 1.78 c p3\n358.67
      447.52 1.78 c p3\n362.03 453.49 1.78 c p3\n362.03 446.02 1.78 c
      p3\n360.35 440.04 1.78 c p3\n360.35 444.53 1.78 c p3\n360.35 444.53
      1.78 c p3\n358.67 438.55 1.78 c p3\n362.03 443.03 1.78 c p3\n358.67
      441.54 1.78 c p3\n363.70 437.06 1.78 c p3\n358.67 438.55 1.78 c
      p3\n358.67 432.57 1.78 c p3\n362.03 438.55 1.78 c p3\n358.67 440.04
      1.78 c p3\n358.67 438.55 1.78 c p3\n358.67 435.56 1.78 c p3\n358.67
      434.07 1.78 c p3\n362.03 438.55 1.78 c p3\n356.99 449.01 1.78 c
      p3\n358.67 450.51 1.78 c p3\n358.67 434.07 1.78 c p3\n358.67 435.56
      1.78 c p3\n358.67 440.04 1.78 c p3\n356.99 441.54 1.78 c p3\n358.67
      432.57 1.78 c p3\n358.67 438.55 1.78 c p3\n360.35 440.04 1.78 c
      p3\n360.35 422.11 1.78 c p3\n358.67 435.56 1.78 c p3\n365.38 440.04
      1.78 c p3\n362.03 444.53 1.78 c p3\n360.35 432.57 1.78 c p3\n358.67
      444.53 1.78 c p3\n358.67 435.56 1.78 c p3\n358.67 443.03 1.78 c
      p3\n358.67 437.06 1.78 c p3\n378.80 435.56 1.78 c p3\n380.48 435.56
      1.78 c p3\n380.48 434.07 1.78 c p3\n377.13 422.11 1.78 c p3\n380.48
      429.58 1.78 c p3\n377.13 429.58 1.78 c p3\n382.16 437.06 1.78 c
      p3\n372.09 423.61 1.78 c p3\n377.13 431.08 1.78 c p3\n378.80 428.09
      1.78 c p3\n372.09 417.63 1.78 c p3\n380.48 432.57 1.78 c p3\n372.09
      420.62 1.78 c p3\n378.80 431.08 1.78 c p3\n377.13 431.08 1.78 c
      p3\n378.80 434.07 1.78 c p3\n380.48 432.57 1.78 c p3\n372.09 428.09
      1.78 c p3\n380.48 420.62 1.78 c p3\n373.77 425.10 1.78 c p3\n385.51
      435.56 1.78 c p3\n377.13 429.58 1.78 c p3\n380.48 425.10 1.78 c
      p3\n375.45 429.58 1.78 c p3\n377.13 431.08 1.78 c p3\n378.80 432.57
      1.78 c p3\n378.80 429.58 1.78 c p3\n383.84 432.57 1.78 c p3\n380.48
      431.08 1.78 c p3\n372.09 426.59 1.78 c p3\n373.77 423.61 1.78 c
      p3\n372.09 423.61 1.78 c p3\n375.45 428.09 1.78 c p3\n382.16 428.09
      1.78 c p3\n380.48 432.57 1.78 c p3\n382.16 438.55 1.78 c p3\n380.48
      434.07 1.78 c p3\n377.13 422.11 1.78 c p3\n377.13 432.57 1.78 c
      p3\n377.13 425.10 1.78 c p3\n375.45 426.59 1.78 c p3\n378.80 432.57
      1.78 c p3\n375.45 426.59 1.78 c p3\n372.09 422.11 1.78 c p3\n377.13
      428.09 1.78 c p3\n375.45 432.57 1.78 c p3\n377.13 431.08 1.78 c
      p3\n377.13 431.08 1.78 c p3\n373.77 425.10 1.78 c p3\n377.13 429.58
      1.78 c p3\n397.26 437.06 1.78 c p3\n387.19 428.09 1.78 c p3\n390.55
      432.57 1.78 c p3\n385.51 431.08 1.78 c p3\n392.23 432.57 1.78 c
      p3\n390.55 432.57 1.78 c p3\n383.84 425.10 1.78 c p3\n385.51 431.08
      1.78 c p3\n385.51 425.10 1.78 c p3\n397.26 441.54 1.78 c p3\n388.87
      435.56 1.78 c p3\n387.19 428.09 1.78 c p3\n390.55 432.57 1.78 c
      p3\n388.87 425.10 1.78 c p3\n395.58 429.58 1.78 c p3\n393.90 435.56
      1.78 c p3\n385.51 432.57 1.78 c p3\n392.23 444.53 1.78 c p3\n393.90
      426.59 1.78 c p3\n380.48 420.62 1.78 c p3\n393.90 435.56 1.78 c
      p3\n388.87 429.58 1.78 c p3\n388.87 429.58 1.78 c p3\n385.51 428.09
      1.78 c p3\n390.55 437.06 1.78 c p3\n385.51 435.56 1.78 c p3\n385.51
      429.58 1.78 c p3\n385.51 432.57 1.78 c p3\n390.55 429.58 1.78 c
      p3\n382.16 432.57 1.78 c p3\n387.19 429.58 1.78 c p3\n388.87 444.53
      1.78 c p3\n392.23 429.58 1.78 c p3\n380.48 429.58 1.78 c p3\n378.80
      426.59 1.78 c p3\n393.90 432.57 1.78 c p3\n395.58 438.55 1.78 c
      p3\n385.51 434.07 1.78 c p3\n385.51 432.57 1.78 c p3\n390.55 434.07
      1.78 c p3\n395.58 434.07 1.78 c p3\n393.90 434.07 1.78 c p3\n387.19
      428.09 1.78 c p3\n393.90 435.56 1.78 c p3\n397.26 437.06 1.78 c
      p3\n393.90 432.57 1.78 c p3\n387.19 425.10 1.78 c p3\n388.87 432.57
      1.78 c p3\n393.90 438.55 1.78 c p3\n385.51 432.57 1.78 c p3\n196.41
      367.95 239.89 406.69 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n196.41 367.95 m\n43.48 -0.00
      l\n-0.00 38.74 l\n-43.48 -0.00 l\n-0.00 -38.74 l\no\n196.41 367.95
      239.89 406.69 cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n206.96 371.82 1.78 c p3\n204.73 371.82 1.78 c p3\n202.49
      371.21 1.78 c p3\n201.37 372.43 1.78 c p3\n205.85 371.82 1.78 c
      p3\n210.32 373.64 1.78 c p3\n201.37 371.82 1.78 c p3\n205.85 372.43
      1.78 c p3\n199.13 371.82 1.78 c p3\n204.73 372.43 1.78 c p3\n210.32
      372.43 1.78 c p3\n203.61 373.04 1.78 c p3\n203.61 371.82 1.78 c
      p3\n198.02 370.00 1.78 c p3\n214.79 370.60 1.78 c p3\n213.68 372.43
      1.78 c p3\n210.32 371.21 1.78 c p3\n206.96 371.82 1.78 c p3\n213.68
      373.64 1.78 c p3\n206.96 372.43 1.78 c p3\n210.32 373.64 1.78 c
      p3\n206.96 372.43 1.78 c p3\n201.37 369.39 1.78 c p3\n206.96 373.64
      1.78 c p3\n203.61 374.86 1.78 c p3\n205.85 373.04 1.78 c p3\n205.85
      373.04 1.78 c p3\n208.08 372.43 1.78 c p3\n208.08 371.82 1.78 c
      p3\n202.49 373.04 1.78 c p3\n203.61 373.04 1.78 c p3\n210.32 372.43
      1.78 c p3\n208.08 372.43 1.78 c p3\n211.44 371.82 1.78 c p3\n204.73
      372.43 1.78 c p3\n205.85 370.60 1.78 c p3\n211.44 371.21 1.78 c
      p3\n204.73 371.82 1.78 c p3\n199.13 371.21 1.78 c p3\n206.96 372.43
      1.78 c p3\n205.85 371.21 1.78 c p3\n200.25 371.21 1.78 c p3\n199.13
      371.21 1.78 c p3\n205.85 373.04 1.78 c p3\n206.96 374.86 1.78 c
      p3\n203.61 371.82 1.78 c p3\n206.96 373.04 1.78 c p3\n201.37 371.82
      1.78 c p3\n209.20 372.43 1.78 c p3\n205.85 371.82 1.78 c p3\n228.22
      391.88 1.78 c p3\n221.51 390.66 1.78 c p3\n227.10 393.10 1.78 c
      p3\n211.44 387.62 1.78 c p3\n222.62 391.27 1.78 c p3\n213.68 390.66
      1.78 c p3\n220.39 391.88 1.78 c p3\n204.73 383.37 1.78 c p3\n223.74
      391.27 1.78 c p3\n208.08 387.02 1.78 c p3\n205.85 384.59 1.78 c
      p3\n215.91 388.84 1.78 c p3\n217.03 387.62 1.78 c p3\n218.15 391.88
      1.78 c p3\n212.56 385.19 1.78 c p3\n224.86 390.06 1.78 c p3\n212.56
      390.66 1.78 c p3\n214.79 388.23 1.78 c p3\n219.27 390.66 1.78 c
      p3\n212.56 387.02 1.78 c p3\n215.91 392.49 1.78 c p3\n218.15 387.62
      1.78 c p3\n220.39 393.10 1.78 c p3\n218.15 391.88 1.78 c p3\n221.51
      389.45 1.78 c p3\n223.74 390.06 1.78 c p3\n225.98 392.49 1.78 c
      p3\n224.86 393.70 1.78 c p3\n217.03 390.66 1.78 c p3\n213.68 384.59
      1.78 c p3\n211.44 386.41 1.78 c p3\n211.44 385.80 1.78 c p3\n214.79
      387.02 1.78 c p3\n217.03 394.31 1.78 c p3\n210.32 390.66 1.78 c
      p3\n217.03 390.66 1.78 c p3\n224.86 391.88 1.78 c p3\n220.39 390.06
      1.78 c p3\n212.56 388.23 1.78 c p3\n211.44 387.62 1.78 c p3\n211.44
      390.06 1.78 c p3\n218.15 391.27 1.78 c p3\n214.79 387.62 1.78 c
      p3\n205.85 383.37 1.78 c p3\n212.56 388.84 1.78 c p3\n213.68 388.84
      1.78 c p3\n213.68 388.84 1.78 c p3\n219.27 389.45 1.78 c p3\n206.96
      381.55 1.78 c p3\n213.68 388.23 1.78 c p3\n220.39 399.78 1.78 c
      p3\n214.79 394.31 1.78 c p3\n229.33 399.18 1.78 c p3\n220.39 397.35
      1.78 c p3\n222.62 398.57 1.78 c p3\n234.93 403.43 1.78 c p3\n204.73
      390.66 1.78 c p3\n231.57 401.61 1.78 c p3\n224.86 398.57 1.78 c
      p3\n230.45 400.39 1.78 c p3\n222.62 394.31 1.78 c p3\n221.51 395.53
      1.78 c p3\n225.98 396.74 1.78 c p3\n213.68 393.70 1.78 c p3\n214.79
      394.31 1.78 c p3\n221.51 395.53 1.78 c p3\n222.62 396.74 1.78 c
      p3\n236.05 404.04 1.78 c p3\n236.05 405.25 1.78 c p3\n217.03 393.70
      1.78 c p3\n227.10 397.96 1.78 c p3\n212.56 393.10 1.78 c p3\n236.05
      404.04 1.78 c p3\n220.39 393.10 1.78 c p3\n224.86 397.96 1.78 c
      p3\n230.45 399.78 1.78 c p3\n219.27 392.49 1.78 c p3\n218.15 393.10
      1.78 c p3\n221.51 397.35 1.78 c p3\n230.45 398.57 1.78 c p3\n232.69
      400.39 1.78 c p3\n238.28 402.21 1.78 c p3\n221.51 397.35 1.78 c
      p3\n220.39 394.31 1.78 c p3\n218.15 397.35 1.78 c p3\n236.05 400.39
      1.78 c p3\n220.39 397.35 1.78 c p3\n221.51 396.74 1.78 c p3\n217.03
      392.49 1.78 c p3\n227.10 396.14 1.78 c p3\n224.86 397.35 1.78 c
      p3\n227.10 394.31 1.78 c p3\n214.79 394.31 1.78 c p3\n225.98 399.18
      1.78 c p3\n224.86 397.96 1.78 c p3\n224.86 394.92 1.78 c p3\n220.39
      393.70 1.78 c p3\n222.62 394.92 1.78 c p3\n219.27 396.14 1.78 c
      p3\n215.91 394.31 1.78 c p3\n249.40 367.95 292.89 406.69 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 367.95 m\n43.49 -0.00 l\n-0.00 38.74 l\n-43.49
      -0.00 l\n0.00 -38.74 l\no\n249.40 367.95 292.89 406.69 cl\n/bg { 0 0 0
      } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n276.18 371.82 1.78 c
      p3\n267.79 371.82 1.78 c p3\n271.14 371.21 1.78 c p3\n269.46 372.43
      1.78 c p3\n277.85 371.82 1.78 c p3\n282.89 373.64 1.78 c p3\n274.50
      371.82 1.78 c p3\n274.50 372.43 1.78 c p3\n266.11 371.82 1.78 c
      p3\n269.46 372.43 1.78 c p3\n279.53 372.43 1.78 c p3\n274.50 373.04
      1.78 c p3\n267.79 371.82 1.78 c p3\n267.79 370.00 1.78 c p3\n284.56
      370.60 1.78 c p3\n291.28 372.43 1.78 c p3\n282.89 371.21 1.78 c
      p3\n276.18 371.82 1.78 c p3\n281.21 373.64 1.78 c p3\n281.21 372.43
      1.78 c p3\n274.50 373.64 1.78 c p3\n279.53 372.43 1.78 c p3\n277.85
      369.39 1.78 c p3\n272.82 373.64 1.78 c p3\n274.50 374.86 1.78 c
      p3\n267.79 373.04 1.78 c p3\n274.50 373.04 1.78 c p3\n276.18 372.43
      1.78 c p3\n274.50 371.82 1.78 c p3\n271.14 373.04 1.78 c p3\n269.46
      373.04 1.78 c p3\n274.50 372.43 1.78 c p3\n286.24 372.43 1.78 c
      p3\n287.92 371.82 1.78 c p3\n269.46 372.43 1.78 c p3\n271.14 370.60
      1.78 c p3\n276.18 371.21 1.78 c p3\n277.85 371.82 1.78 c p3\n267.79
      371.21 1.78 c p3\n274.50 372.43 1.78 c p3\n276.18 371.21 1.78 c
      p3\n256.04 371.21 1.78 c p3\n271.14 371.21 1.78 c p3\n276.18 373.04
      1.78 c p3\n281.21 374.86 1.78 c p3\n267.79 371.82 1.78 c p3\n281.21
      373.04 1.78 c p3\n271.14 371.82 1.78 c p3\n279.53 372.43 1.78 c
      p3\n272.82 371.82 1.78 c p3\n271.14 391.88 1.78 c p3\n271.14 390.66
      1.78 c p3\n269.46 393.10 1.78 c p3\n256.04 387.62 1.78 c p3\n264.43
      391.27 1.78 c p3\n264.43 390.66 1.78 c p3\n272.82 391.88 1.78 c
      p3\n257.72 383.37 1.78 c p3\n266.11 391.27 1.78 c p3\n262.75 387.02
      1.78 c p3\n251.01 384.59 1.78 c p3\n267.79 388.84 1.78 c p3\n254.36
      387.62 1.78 c p3\n266.11 391.88 1.78 c p3\n266.11 385.19 1.78 c
      p3\n269.46 390.06 1.78 c p3\n267.79 390.66 1.78 c p3\n262.75 388.23
      1.78 c p3\n254.36 390.66 1.78 c p3\n259.40 387.02 1.78 c p3\n271.14
      392.49 1.78 c p3\n264.43 387.62 1.78 c p3\n259.40 393.10 1.78 c
      p3\n264.43 391.88 1.78 c p3\n266.11 389.45 1.78 c p3\n267.79 390.06
      1.78 c p3\n264.43 392.49 1.78 c p3\n267.79 393.70 1.78 c p3\n266.11
      390.66 1.78 c p3\n261.08 384.59 1.78 c p3\n257.72 386.41 1.78 c
      p3\n257.72 385.80 1.78 c p3\n262.75 387.02 1.78 c p3\n262.75 394.31
      1.78 c p3\n267.79 390.66 1.78 c p3\n274.50 390.66 1.78 c p3\n269.46
      391.88 1.78 c p3\n256.04 390.06 1.78 c p3\n267.79 388.23 1.78 c
      p3\n259.40 387.62 1.78 c p3\n261.08 390.06 1.78 c p3\n267.79 391.27
      1.78 c p3\n261.08 387.62 1.78 c p3\n256.04 383.37 1.78 c p3\n262.75
      388.84 1.78 c p3\n267.79 388.84 1.78 c p3\n266.11 388.84 1.78 c
      p3\n266.11 389.45 1.78 c p3\n259.40 381.55 1.78 c p3\n264.43 388.23
      1.78 c p3\n272.82 399.78 1.78 c p3\n262.75 394.31 1.78 c p3\n267.79
      399.18 1.78 c p3\n266.11 397.35 1.78 c p3\n267.79 398.57 1.78 c
      p3\n267.79 403.43 1.78 c p3\n259.40 390.66 1.78 c p3\n266.11 401.61
      1.78 c p3\n259.40 398.57 1.78 c p3\n277.85 400.39 1.78 c p3\n271.14
      394.31 1.78 c p3\n262.75 395.53 1.78 c p3\n267.79 396.74 1.78 c
      p3\n259.40 393.70 1.78 c p3\n264.43 394.31 1.78 c p3\n271.14 395.53
      1.78 c p3\n267.79 396.74 1.78 c p3\n281.21 404.04 1.78 c p3\n261.08
      405.25 1.78 c p3\n254.36 393.70 1.78 c p3\n271.14 397.96 1.78 c
      p3\n264.43 393.10 1.78 c p3\n264.43 404.04 1.78 c p3\n262.75 393.10
      1.78 c p3\n272.82 397.96 1.78 c p3\n271.14 399.78 1.78 c p3\n264.43
      392.49 1.78 c p3\n267.79 393.10 1.78 c p3\n264.43 397.35 1.78 c
      p3\n267.79 398.57 1.78 c p3\n264.43 400.39 1.78 c p3\n281.21 402.21
      1.78 c p3\n264.43 397.35 1.78 c p3\n264.43 394.31 1.78 c p3\n261.08
      397.35 1.78 c p3\n267.79 400.39 1.78 c p3\n274.50 397.35 1.78 c
      p3\n269.46 396.74 1.78 c p3\n267.79 392.49 1.78 c p3\n269.46 396.14
      1.78 c p3\n269.46 397.35 1.78 c p3\n269.46 394.31 1.78 c p3\n262.75
      394.31 1.78 c p3\n271.14 399.18 1.78 c p3\n272.82 397.96 1.78 c
      p3\n267.79 394.92 1.78 c p3\n259.40 393.70 1.78 c p3\n267.79 394.92
      1.78 c p3\n274.50 396.14 1.78 c p3\n267.79 394.31 1.78 c p3\n302.39
      367.95 345.88 406.69 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n302.39 367.95 m\n43.49 -0.00
      l\n-0.00 38.74 l\n-43.49 -0.00 l\n-0.00 -38.74 l\no\n302.39 367.95
      345.88 406.69 cl\n/ps 16 def R 16 s\n0 0 0 rgb\n324.13 383.34
      (Petal.Length) .5 0 0 t\n355.38 367.95 398.87 406.69 cl\n153.64 276.94
      441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n355.38 367.95 m\n43.49 -0.00 l\n0.00 38.74 l\n-43.49 -0.00
      l\n-0.00 -38.74 l\no\nnp\n398.87 369.39 m\n0.00 36.47 l\no\nnp\n398.87
      369.39 m\n4.75 -0.00 l\no\nnp\n398.87 375.47 m\n4.75 0.00
      l\no\nnp\n398.87 381.55 m\n4.75 0.00 l\no\nnp\n398.87 387.62 m\n4.75
      0.00 l\no\nnp\n398.87 393.70 m\n4.75 -0.00 l\no\nnp\n398.87 399.78
      m\n4.75 -0.00 l\no\nnp\n398.87 405.86 m\n4.75 0.00 l\no\n/ps 8 def R 8
      s\n415.98 369.39 (1) .5 0 90 t\n415.98 381.55 (3) .5 0 90 t\n415.98
      393.70 (5) .5 0 90 t\n415.98 405.86 (7) .5 0 90 t\n355.38 367.95 398.87
      406.69 cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n358.67 371.82 1.78 c p3\n358.67 371.82 1.78 c p3\n358.67
      371.21 1.78 c p3\n358.67 372.43 1.78 c p3\n358.67 371.82 1.78 c
      p3\n362.03 373.64 1.78 c p3\n360.35 371.82 1.78 c p3\n358.67 372.43
      1.78 c p3\n358.67 371.82 1.78 c p3\n356.99 372.43 1.78 c p3\n358.67
      372.43 1.78 c p3\n358.67 373.04 1.78 c p3\n356.99 371.82 1.78 c
      p3\n356.99 370.00 1.78 c p3\n358.67 370.60 1.78 c p3\n362.03 372.43
      1.78 c p3\n362.03 371.21 1.78 c p3\n360.35 371.82 1.78 c p3\n360.35
      373.64 1.78 c p3\n360.35 372.43 1.78 c p3\n358.67 373.64 1.78 c
      p3\n362.03 372.43 1.78 c p3\n358.67 369.39 1.78 c p3\n363.70 373.64
      1.78 c p3\n358.67 374.86 1.78 c p3\n358.67 373.04 1.78 c p3\n362.03
      373.04 1.78 c p3\n358.67 372.43 1.78 c p3\n358.67 371.82 1.78 c
      p3\n358.67 373.04 1.78 c p3\n358.67 373.04 1.78 c p3\n362.03 372.43
      1.78 c p3\n356.99 372.43 1.78 c p3\n358.67 371.82 1.78 c p3\n358.67
      372.43 1.78 c p3\n358.67 370.60 1.78 c p3\n358.67 371.21 1.78 c
      p3\n356.99 371.82 1.78 c p3\n358.67 371.21 1.78 c p3\n358.67 372.43
      1.78 c p3\n360.35 371.21 1.78 c p3\n360.35 371.21 1.78 c p3\n358.67
      371.21 1.78 c p3\n365.38 373.04 1.78 c p3\n362.03 374.86 1.78 c
      p3\n360.35 371.82 1.78 c p3\n358.67 373.04 1.78 c p3\n358.67 371.82
      1.78 c p3\n358.67 372.43 1.78 c p3\n358.67 371.82 1.78 c p3\n378.80
      391.88 1.78 c p3\n380.48 390.66 1.78 c p3\n380.48 393.10 1.78 c
      p3\n377.13 387.62 1.78 c p3\n380.48 391.27 1.78 c p3\n377.13 390.66
      1.78 c p3\n382.16 391.88 1.78 c p3\n372.09 383.37 1.78 c p3\n377.13
      391.27 1.78 c p3\n378.80 387.02 1.78 c p3\n372.09 384.59 1.78 c
      p3\n380.48 388.84 1.78 c p3\n372.09 387.62 1.78 c p3\n378.80 391.88
      1.78 c p3\n377.13 385.19 1.78 c p3\n378.80 390.06 1.78 c p3\n380.48
      390.66 1.78 c p3\n372.09 388.23 1.78 c p3\n380.48 390.66 1.78 c
      p3\n373.77 387.02 1.78 c p3\n385.51 392.49 1.78 c p3\n377.13 387.62
      1.78 c p3\n380.48 393.10 1.78 c p3\n375.45 391.88 1.78 c p3\n377.13
      389.45 1.78 c p3\n378.80 390.06 1.78 c p3\n378.80 392.49 1.78 c
      p3\n383.84 393.70 1.78 c p3\n380.48 390.66 1.78 c p3\n372.09 384.59
      1.78 c p3\n373.77 386.41 1.78 c p3\n372.09 385.80 1.78 c p3\n375.45
      387.02 1.78 c p3\n382.16 394.31 1.78 c p3\n380.48 390.66 1.78 c
      p3\n382.16 390.66 1.78 c p3\n380.48 391.88 1.78 c p3\n377.13 390.06
      1.78 c p3\n377.13 388.23 1.78 c p3\n377.13 387.62 1.78 c p3\n375.45
      390.06 1.78 c p3\n378.80 391.27 1.78 c p3\n375.45 387.62 1.78 c
      p3\n372.09 383.37 1.78 c p3\n377.13 388.84 1.78 c p3\n375.45 388.84
      1.78 c p3\n377.13 388.84 1.78 c p3\n377.13 389.45 1.78 c p3\n373.77
      381.55 1.78 c p3\n377.13 388.23 1.78 c p3\n397.26 399.78 1.78 c
      p3\n387.19 394.31 1.78 c p3\n390.55 399.18 1.78 c p3\n385.51 397.35
      1.78 c p3\n392.23 398.57 1.78 c p3\n390.55 403.43 1.78 c p3\n383.84
      390.66 1.78 c p3\n385.51 401.61 1.78 c p3\n385.51 398.57 1.78 c
      p3\n397.26 400.39 1.78 c p3\n388.87 394.31 1.78 c p3\n387.19 395.53
      1.78 c p3\n390.55 396.74 1.78 c p3\n388.87 393.70 1.78 c p3\n395.58
      394.31 1.78 c p3\n393.90 395.53 1.78 c p3\n385.51 396.74 1.78 c
      p3\n392.23 404.04 1.78 c p3\n393.90 405.25 1.78 c p3\n380.48 393.70
      1.78 c p3\n393.90 397.96 1.78 c p3\n388.87 393.10 1.78 c p3\n388.87
      404.04 1.78 c p3\n385.51 393.10 1.78 c p3\n390.55 397.96 1.78 c
      p3\n385.51 399.78 1.78 c p3\n385.51 392.49 1.78 c p3\n385.51 393.10
      1.78 c p3\n390.55 397.35 1.78 c p3\n382.16 398.57 1.78 c p3\n387.19
      400.39 1.78 c p3\n388.87 402.21 1.78 c p3\n392.23 397.35 1.78 c
      p3\n380.48 394.31 1.78 c p3\n378.80 397.35 1.78 c p3\n393.90 400.39
      1.78 c p3\n395.58 397.35 1.78 c p3\n385.51 396.74 1.78 c p3\n385.51
      392.49 1.78 c p3\n390.55 396.14 1.78 c p3\n395.58 397.35 1.78 c
      p3\n393.90 394.31 1.78 c p3\n387.19 394.31 1.78 c p3\n393.90 399.18
      1.78 c p3\n397.26 397.96 1.78 c p3\n393.90 394.92 1.78 c p3\n387.19
      393.70 1.78 c p3\n388.87 394.92 1.78 c p3\n393.90 396.14 1.78 c
      p3\n385.51 394.31 1.78 c p3\n196.41 319.71 239.89 358.45 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n196.41 319.71 m\n43.48 -0.00 l\n-0.00 38.74 l\n-43.48
      -0.00 l\n-0.00 -38.74 l\no\nnp\n200.25 319.71 m\n39.15 -0.00
      l\no\nnp\n200.25 319.71 m\n0 -4.75 l\no\nnp\n205.85 319.71 m\n-0.00
      -4.75 l\no\nnp\n211.44 319.71 m\n-0.00 -4.75 l\no\nnp\n217.03 319.71
      m\n0.00 -4.75 l\no\nnp\n222.62 319.71 m\n0.00 -4.75 l\no\nnp\n228.22
      319.71 m\n-0.00 -4.75 l\no\nnp\n233.81 319.71 m\n0.00 -4.75
      l\no\nnp\n239.40 319.71 m\n0.00 -4.75 l\no\n/ps 8 def R 8 s\n200.25
      302.61 (4.5) .5 0 0 t\n222.62 302.61 (6.5) .5 0 0 t\nnp\n196.41 327.13
      m\n-0.00 29.88 l\no\nnp\n196.41 327.13 m\n-4.76 -0.00 l\no\nnp\n196.41
      334.60 m\n-4.76 0.00 l\no\nnp\n196.41 342.07 m\n-4.76 -0.00
      l\no\nnp\n196.41 349.54 m\n-4.76 0.00 l\no\nnp\n196.41 357.01 m\n-4.76
      -0.00 l\no\n185.00 327.13 (0.5) .5 0 90 t\n185.00 349.54 (2.0) .5 0 90
      t\n196.41 319.71 239.89 358.45 cl\n/bg { 0 0 0 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\n206.96 322.64 1.78 c p3\n204.73 322.64 1.78
      c p3\n202.49 322.64 1.78 c p3\n201.37 322.64 1.78 c p3\n205.85 322.64
      1.78 c p3\n210.32 325.63 1.78 c p3\n201.37 324.14 1.78 c p3\n205.85
      322.64 1.78 c p3\n199.13 322.64 1.78 c p3\n204.73 321.15 1.78 c
      p3\n210.32 322.64 1.78 c p3\n203.61 322.64 1.78 c p3\n203.61 321.15
      1.78 c p3\n198.02 321.15 1.78 c p3\n214.79 322.64 1.78 c p3\n213.68
      325.63 1.78 c p3\n210.32 325.63 1.78 c p3\n206.96 324.14 1.78 c
      p3\n213.68 324.14 1.78 c p3\n206.96 324.14 1.78 c p3\n210.32 322.64
      1.78 c p3\n206.96 325.63 1.78 c p3\n201.37 322.64 1.78 c p3\n206.96
      327.13 1.78 c p3\n203.61 322.64 1.78 c p3\n205.85 322.64 1.78 c
      p3\n205.85 325.63 1.78 c p3\n208.08 322.64 1.78 c p3\n208.08 322.64
      1.78 c p3\n202.49 322.64 1.78 c p3\n203.61 322.64 1.78 c p3\n210.32
      325.63 1.78 c p3\n208.08 321.15 1.78 c p3\n211.44 322.64 1.78 c
      p3\n204.73 322.64 1.78 c p3\n205.85 322.64 1.78 c p3\n211.44 322.64
      1.78 c p3\n204.73 321.15 1.78 c p3\n199.13 322.64 1.78 c p3\n206.96
      322.64 1.78 c p3\n205.85 324.14 1.78 c p3\n200.25 324.14 1.78 c
      p3\n199.13 322.64 1.78 c p3\n205.85 328.62 1.78 c p3\n206.96 325.63
      1.78 c p3\n203.61 324.14 1.78 c p3\n206.96 322.64 1.78 c p3\n201.37
      322.64 1.78 c p3\n209.20 322.64 1.78 c p3\n205.85 322.64 1.78 c
      p3\n228.22 340.58 1.78 c p3\n221.51 342.07 1.78 c p3\n227.10 342.07
      1.78 c p3\n211.44 339.08 1.78 c p3\n222.62 342.07 1.78 c p3\n213.68
      339.08 1.78 c p3\n220.39 343.56 1.78 c p3\n204.73 334.60 1.78 c
      p3\n223.74 339.08 1.78 c p3\n208.08 340.58 1.78 c p3\n205.85 334.60
      1.78 c p3\n215.91 342.07 1.78 c p3\n217.03 334.60 1.78 c p3\n218.15
      340.58 1.78 c p3\n212.56 339.08 1.78 c p3\n224.86 340.58 1.78 c
      p3\n212.56 342.07 1.78 c p3\n214.79 334.60 1.78 c p3\n219.27 342.07
      1.78 c p3\n212.56 336.09 1.78 c p3\n215.91 346.55 1.78 c p3\n218.15
      339.08 1.78 c p3\n220.39 342.07 1.78 c p3\n218.15 337.59 1.78 c
      p3\n221.51 339.08 1.78 c p3\n223.74 340.58 1.78 c p3\n225.98 340.58
      1.78 c p3\n224.86 345.06 1.78 c p3\n217.03 342.07 1.78 c p3\n213.68
      334.60 1.78 c p3\n211.44 336.09 1.78 c p3\n211.44 334.60 1.78 c
      p3\n214.79 337.59 1.78 c p3\n217.03 343.56 1.78 c p3\n210.32 342.07
      1.78 c p3\n217.03 343.56 1.78 c p3\n224.86 342.07 1.78 c p3\n220.39
      339.08 1.78 c p3\n212.56 339.08 1.78 c p3\n211.44 339.08 1.78 c
      p3\n211.44 337.59 1.78 c p3\n218.15 340.58 1.78 c p3\n214.79 337.59
      1.78 c p3\n205.85 334.60 1.78 c p3\n212.56 339.08 1.78 c p3\n213.68
      337.59 1.78 c p3\n213.68 339.08 1.78 c p3\n219.27 339.08 1.78 c
      p3\n206.96 336.09 1.78 c p3\n213.68 339.08 1.78 c p3\n220.39 357.01
      1.78 c p3\n214.79 348.05 1.78 c p3\n229.33 351.04 1.78 c p3\n220.39
      346.55 1.78 c p3\n222.62 352.53 1.78 c p3\n234.93 351.04 1.78 c
      p3\n204.73 345.06 1.78 c p3\n231.57 346.55 1.78 c p3\n224.86 346.55
      1.78 c p3\n230.45 357.01 1.78 c p3\n222.62 349.54 1.78 c p3\n221.51
      348.05 1.78 c p3\n225.98 351.04 1.78 c p3\n213.68 349.54 1.78 c
      p3\n214.79 355.52 1.78 c p3\n221.51 354.03 1.78 c p3\n222.62 346.55
      1.78 c p3\n236.05 352.53 1.78 c p3\n236.05 354.03 1.78 c p3\n217.03
      342.07 1.78 c p3\n227.10 354.03 1.78 c p3\n212.56 349.54 1.78 c
      p3\n236.05 349.54 1.78 c p3\n220.39 346.55 1.78 c p3\n224.86 351.04
      1.78 c p3\n230.45 346.55 1.78 c p3\n219.27 346.55 1.78 c p3\n218.15
      346.55 1.78 c p3\n221.51 351.04 1.78 c p3\n230.45 343.56 1.78 c
      p3\n232.69 348.05 1.78 c p3\n238.28 349.54 1.78 c p3\n221.51 352.53
      1.78 c p3\n220.39 342.07 1.78 c p3\n218.15 340.58 1.78 c p3\n236.05
      354.03 1.78 c p3\n220.39 355.52 1.78 c p3\n221.51 346.55 1.78 c
      p3\n217.03 346.55 1.78 c p3\n227.10 351.04 1.78 c p3\n224.86 355.52
      1.78 c p3\n227.10 354.03 1.78 c p3\n214.79 348.05 1.78 c p3\n225.98
      354.03 1.78 c p3\n224.86 357.01 1.78 c p3\n224.86 354.03 1.78 c
      p3\n220.39 348.05 1.78 c p3\n222.62 349.54 1.78 c p3\n219.27 354.03
      1.78 c p3\n215.91 346.55 1.78 c p3\n249.40 319.71 292.89 358.45
      cl\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 319.71 m\n43.49 -0.00 l\n-0.00 38.74 l\n-43.49
      -0.00 l\n0.00 -38.74 l\no\n249.40 319.71 292.89 358.45 cl\n/bg { 0 0 0
      } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n276.18 322.64 1.78 c
      p3\n267.79 322.64 1.78 c p3\n271.14 322.64 1.78 c p3\n269.46 322.64
      1.78 c p3\n277.85 322.64 1.78 c p3\n282.89 325.63 1.78 c p3\n274.50
      324.14 1.78 c p3\n274.50 322.64 1.78 c p3\n266.11 322.64 1.78 c
      p3\n269.46 321.15 1.78 c p3\n279.53 322.64 1.78 c p3\n274.50 322.64
      1.78 c p3\n267.79 321.15 1.78 c p3\n267.79 321.15 1.78 c p3\n284.56
      322.64 1.78 c p3\n291.28 325.63 1.78 c p3\n282.89 325.63 1.78 c
      p3\n276.18 324.14 1.78 c p3\n281.21 324.14 1.78 c p3\n281.21 324.14
      1.78 c p3\n274.50 322.64 1.78 c p3\n279.53 325.63 1.78 c p3\n277.85
      322.64 1.78 c p3\n272.82 327.13 1.78 c p3\n274.50 322.64 1.78 c
      p3\n267.79 322.64 1.78 c p3\n274.50 325.63 1.78 c p3\n276.18 322.64
      1.78 c p3\n274.50 322.64 1.78 c p3\n271.14 322.64 1.78 c p3\n269.46
      322.64 1.78 c p3\n274.50 325.63 1.78 c p3\n286.24 321.15 1.78 c
      p3\n287.92 322.64 1.78 c p3\n269.46 322.64 1.78 c p3\n271.14 322.64
      1.78 c p3\n276.18 322.64 1.78 c p3\n277.85 321.15 1.78 c p3\n267.79
      322.64 1.78 c p3\n274.50 322.64 1.78 c p3\n276.18 324.14 1.78 c
      p3\n256.04 324.14 1.78 c p3\n271.14 322.64 1.78 c p3\n276.18 328.62
      1.78 c p3\n281.21 325.63 1.78 c p3\n267.79 324.14 1.78 c p3\n281.21
      322.64 1.78 c p3\n271.14 322.64 1.78 c p3\n279.53 322.64 1.78 c
      p3\n272.82 322.64 1.78 c p3\n271.14 340.58 1.78 c p3\n271.14 342.07
      1.78 c p3\n269.46 342.07 1.78 c p3\n256.04 339.08 1.78 c p3\n264.43
      342.07 1.78 c p3\n264.43 339.08 1.78 c p3\n272.82 343.56 1.78 c
      p3\n257.72 334.60 1.78 c p3\n266.11 339.08 1.78 c p3\n262.75 340.58
      1.78 c p3\n251.01 334.60 1.78 c p3\n267.79 342.07 1.78 c p3\n254.36
      334.60 1.78 c p3\n266.11 340.58 1.78 c p3\n266.11 339.08 1.78 c
      p3\n269.46 340.58 1.78 c p3\n267.79 342.07 1.78 c p3\n262.75 334.60
      1.78 c p3\n254.36 342.07 1.78 c p3\n259.40 336.09 1.78 c p3\n271.14
      346.55 1.78 c p3\n264.43 339.08 1.78 c p3\n259.40 342.07 1.78 c
      p3\n264.43 337.59 1.78 c p3\n266.11 339.08 1.78 c p3\n267.79 340.58
      1.78 c p3\n264.43 340.58 1.78 c p3\n267.79 345.06 1.78 c p3\n266.11
      342.07 1.78 c p3\n261.08 334.60 1.78 c p3\n257.72 336.09 1.78 c
      p3\n257.72 334.60 1.78 c p3\n262.75 337.59 1.78 c p3\n262.75 343.56
      1.78 c p3\n267.79 342.07 1.78 c p3\n274.50 343.56 1.78 c p3\n269.46
      342.07 1.78 c p3\n256.04 339.08 1.78 c p3\n267.79 339.08 1.78 c
      p3\n259.40 339.08 1.78 c p3\n261.08 337.59 1.78 c p3\n267.79 340.58
      1.78 c p3\n261.08 337.59 1.78 c p3\n256.04 334.60 1.78 c p3\n262.75
      339.08 1.78 c p3\n267.79 337.59 1.78 c p3\n266.11 339.08 1.78 c
      p3\n266.11 339.08 1.78 c p3\n259.40 336.09 1.78 c p3\n264.43 339.08
      1.78 c p3\n272.82 357.01 1.78 c p3\n262.75 348.05 1.78 c p3\n267.79
      351.04 1.78 c p3\n266.11 346.55 1.78 c p3\n267.79 352.53 1.78 c
      p3\n267.79 351.04 1.78 c p3\n259.40 345.06 1.78 c p3\n266.11 346.55
      1.78 c p3\n259.40 346.55 1.78 c p3\n277.85 357.01 1.78 c p3\n271.14
      349.54 1.78 c p3\n262.75 348.05 1.78 c p3\n267.79 351.04 1.78 c
      p3\n259.40 349.54 1.78 c p3\n264.43 355.52 1.78 c p3\n271.14 354.03
      1.78 c p3\n267.79 346.55 1.78 c p3\n281.21 352.53 1.78 c p3\n261.08
      354.03 1.78 c p3\n254.36 342.07 1.78 c p3\n271.14 354.03 1.78 c
      p3\n264.43 349.54 1.78 c p3\n264.43 349.54 1.78 c p3\n262.75 346.55
      1.78 c p3\n272.82 351.04 1.78 c p3\n271.14 346.55 1.78 c p3\n264.43
      346.55 1.78 c p3\n267.79 346.55 1.78 c p3\n264.43 351.04 1.78 c
      p3\n267.79 343.56 1.78 c p3\n264.43 348.05 1.78 c p3\n281.21 349.54
      1.78 c p3\n264.43 352.53 1.78 c p3\n264.43 342.07 1.78 c p3\n261.08
      340.58 1.78 c p3\n267.79 354.03 1.78 c p3\n274.50 355.52 1.78 c
      p3\n269.46 346.55 1.78 c p3\n267.79 346.55 1.78 c p3\n269.46 351.04
      1.78 c p3\n269.46 355.52 1.78 c p3\n269.46 354.03 1.78 c p3\n262.75
      348.05 1.78 c p3\n271.14 354.03 1.78 c p3\n272.82 357.01 1.78 c
      p3\n267.79 354.03 1.78 c p3\n259.40 348.05 1.78 c p3\n267.79 349.54
      1.78 c p3\n274.50 354.03 1.78 c p3\n267.79 346.55 1.78 c p3\n302.39
      319.71 345.88 358.45 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n302.39 319.71 m\n43.49 -0.00
      l\n-0.00 38.74 l\n-43.49 -0.00 l\n-0.00 -38.74 l\no\nnp\n304.00 319.71
      m\n40.95 -0.00 l\no\nnp\n304.00 319.71 m\n0 -4.75 l\no\nnp\n310.83
      319.71 m\n-0.00 -4.75 l\no\nnp\n317.65 319.71 m\n-0.00 -4.75
      l\no\nnp\n324.48 319.71 m\n0.00 -4.75 l\no\nnp\n331.30 319.71 m\n0.00
      -4.75 l\no\nnp\n338.12 319.71 m\n0.00 -4.75 l\no\nnp\n344.95 319.71
      m\n-0.00 -4.75 l\no\n/ps 8 def R 8 s\n304.00 302.61 (1) .5 0 0
      t\n317.65 302.61 (3) .5 0 0 t\n331.30 302.61 (5) .5 0 0 t\n344.95
      302.61 (7) .5 0 0 t\n302.39 319.71 345.88 358.45 cl\n/bg { 0 0 0 }
      def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n306.73 322.64 1.78 c
      p3\n306.73 322.64 1.78 c p3\n306.05 322.64 1.78 c p3\n307.41 322.64
      1.78 c p3\n306.73 322.64 1.78 c p3\n308.78 325.63 1.78 c p3\n306.73
      324.14 1.78 c p3\n307.41 322.64 1.78 c p3\n306.73 322.64 1.78 c
      p3\n307.41 321.15 1.78 c p3\n307.41 322.64 1.78 c p3\n308.10 322.64
      1.78 c p3\n306.73 321.15 1.78 c p3\n304.68 321.15 1.78 c p3\n305.37
      322.64 1.78 c p3\n307.41 325.63 1.78 c p3\n306.05 325.63 1.78 c
      p3\n306.73 324.14 1.78 c p3\n308.78 324.14 1.78 c p3\n307.41 324.14
      1.78 c p3\n308.78 322.64 1.78 c p3\n307.41 325.63 1.78 c p3\n304.00
      322.64 1.78 c p3\n308.78 327.13 1.78 c p3\n310.14 322.64 1.78 c
      p3\n308.10 322.64 1.78 c p3\n308.10 325.63 1.78 c p3\n307.41 322.64
      1.78 c p3\n306.73 322.64 1.78 c p3\n308.10 322.64 1.78 c p3\n308.10
      322.64 1.78 c p3\n307.41 325.63 1.78 c p3\n307.41 321.15 1.78 c
      p3\n306.73 322.64 1.78 c p3\n307.41 322.64 1.78 c p3\n305.37 322.64
      1.78 c p3\n306.05 322.64 1.78 c p3\n306.73 321.15 1.78 c p3\n306.05
      322.64 1.78 c p3\n307.41 322.64 1.78 c p3\n306.05 324.14 1.78 c
      p3\n306.05 324.14 1.78 c p3\n306.05 322.64 1.78 c p3\n308.10 328.62
      1.78 c p3\n310.14 325.63 1.78 c p3\n306.73 324.14 1.78 c p3\n308.10
      322.64 1.78 c p3\n306.73 322.64 1.78 c p3\n307.41 322.64 1.78 c
      p3\n306.73 322.64 1.78 c p3\n329.25 340.58 1.78 c p3\n327.89 342.07
      1.78 c p3\n330.62 342.07 1.78 c p3\n324.48 339.08 1.78 c p3\n328.57
      342.07 1.78 c p3\n327.89 339.08 1.78 c p3\n329.25 343.56 1.78 c
      p3\n319.70 334.60 1.78 c p3\n328.57 339.08 1.78 c p3\n323.79 340.58
      1.78 c p3\n321.06 334.60 1.78 c p3\n325.84 342.07 1.78 c p3\n324.48
      334.60 1.78 c p3\n329.25 340.58 1.78 c p3\n321.75 339.08 1.78 c
      p3\n327.20 340.58 1.78 c p3\n327.89 342.07 1.78 c p3\n325.16 334.60
      1.78 c p3\n327.89 342.07 1.78 c p3\n323.79 336.09 1.78 c p3\n329.93
      346.55 1.78 c p3\n324.48 339.08 1.78 c p3\n330.62 342.07 1.78 c
      p3\n329.25 337.59 1.78 c p3\n326.52 339.08 1.78 c p3\n327.20 340.58
      1.78 c p3\n329.93 340.58 1.78 c p3\n331.30 345.06 1.78 c p3\n327.89
      342.07 1.78 c p3\n321.06 334.60 1.78 c p3\n323.11 336.09 1.78 c
      p3\n322.43 334.60 1.78 c p3\n323.79 337.59 1.78 c p3\n331.98 343.56
      1.78 c p3\n327.89 342.07 1.78 c p3\n327.89 343.56 1.78 c p3\n329.25
      342.07 1.78 c p3\n327.20 339.08 1.78 c p3\n325.16 339.08 1.78 c
      p3\n324.48 339.08 1.78 c p3\n327.20 337.59 1.78 c p3\n328.57 340.58
      1.78 c p3\n324.48 337.59 1.78 c p3\n319.70 334.60 1.78 c p3\n325.84
      339.08 1.78 c p3\n325.84 337.59 1.78 c p3\n325.84 339.08 1.78 c
      p3\n326.52 339.08 1.78 c p3\n317.65 336.09 1.78 c p3\n325.16 339.08
      1.78 c p3\n338.12 357.01 1.78 c p3\n331.98 348.05 1.78 c p3\n337.44
      351.04 1.78 c p3\n335.39 346.55 1.78 c p3\n336.76 352.53 1.78 c
      p3\n342.22 351.04 1.78 c p3\n327.89 345.06 1.78 c p3\n340.17 346.55
      1.78 c p3\n336.76 346.55 1.78 c p3\n338.81 357.01 1.78 c p3\n331.98
      349.54 1.78 c p3\n333.35 348.05 1.78 c p3\n334.71 351.04 1.78 c
      p3\n331.30 349.54 1.78 c p3\n331.98 355.52 1.78 c p3\n333.35 354.03
      1.78 c p3\n334.71 346.55 1.78 c p3\n342.90 352.53 1.78 c p3\n344.27
      354.03 1.78 c p3\n331.30 342.07 1.78 c p3\n336.08 354.03 1.78 c
      p3\n330.62 349.54 1.78 c p3\n342.90 349.54 1.78 c p3\n330.62 346.55
      1.78 c p3\n336.08 351.04 1.78 c p3\n338.12 346.55 1.78 c p3\n329.93
      346.55 1.78 c p3\n330.62 346.55 1.78 c p3\n335.39 351.04 1.78 c
      p3\n336.76 343.56 1.78 c p3\n338.81 348.05 1.78 c p3\n340.85 349.54
      1.78 c p3\n335.39 352.53 1.78 c p3\n331.98 342.07 1.78 c p3\n335.39
      340.58 1.78 c p3\n338.81 354.03 1.78 c p3\n335.39 355.52 1.78 c
      p3\n334.71 346.55 1.78 c p3\n329.93 346.55 1.78 c p3\n334.03 351.04
      1.78 c p3\n335.39 355.52 1.78 c p3\n331.98 354.03 1.78 c p3\n331.98
      348.05 1.78 c p3\n337.44 354.03 1.78 c p3\n336.08 357.01 1.78 c
      p3\n332.66 354.03 1.78 c p3\n331.30 348.05 1.78 c p3\n332.66 349.54
      1.78 c p3\n334.03 354.03 1.78 c p3\n331.98 346.55 1.78 c p3\n355.38
      319.71 398.87 358.45 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n355.38 319.71 m\n43.49 -0.00
      l\n0.00 38.74 l\n-43.49 -0.00 l\n-0.00 -38.74 l\no\n355.38 319.71
      398.87 358.45 cl\n/ps 16 def R 16 s\n0 0 0 rgb\n377.13 333.46
      (Petal.Width) .5 0 0 t\n153.64 276.94 441.64 564.94 cl\n/ps 14 def BI
      14 s\n0 0 0 rgb\n297.64 538.33 (Edgar Anderson's Iris Data) .5 0 0
      t\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      pairs(iris[1:4], main = "Edgar Anderson's Iris Data",

      \ \ \ \ pch = 21, bg = c("red", "green3",
      "blue")[codes(iris$Species)]);

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 1 0.9725 0.8627 } def\n0.00 0.00 595.28 841.89 r
      p2\n196.41 464.43 239.89 503.17 cl\n153.64 276.94 441.64 564.94 cl\n0 0
      0 rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n196.41 464.43 m\n43.48 0.00
      l\n-0.00 38.74 l\n-43.48 0.00 l\n-0.00 -38.74 l\no\n196.41 464.43
      239.89 503.17 cl\n/ps 16 def R 16 s\n0 0 0 rgb\n218.15 479.66
      (Sepal.Length) .5 0 0 t\n249.40 464.43 292.89 503.17 cl\n153.64 276.94
      441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 464.43 m\n43.49 0.00 l\n-0.00 38.74 l\n-43.49 0.00
      l\n0.00 -38.74 l\no\nnp\n251.01 503.17 m\n33.55 0.00 l\no\nnp\n251.01
      503.17 m\n-0.00 4.75 l\no\nnp\n259.40 503.17 m\n-0.00 4.75
      l\no\nnp\n267.79 503.17 m\n0.00 4.75 l\no\nnp\n276.18 503.17 m\n0.00
      4.75 l\no\nnp\n284.56 503.17 m\n0.00 4.75 l\no\n/ps 8 def R 8 s\n251.01
      514.57 (2.0) .5 0 0 t\n276.18 514.57 (3.5) .5 0 0 t\n249.40 464.43
      292.89 503.17 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n276.18 473.84 1.78 c p3\n267.79 471.85 1.78 c p3\n271.14
      469.85 1.78 c p3\n269.46 468.86 1.78 c p3\n277.85 472.84 1.78 c
      p3\n282.89 476.83 1.78 c p3\n274.50 468.86 1.78 c p3\n274.50 472.84
      1.78 c p3\n266.11 466.86 1.78 c p3\n269.46 471.85 1.78 c p3\n279.53
      476.83 1.78 c p3\n274.50 470.85 1.78 c p3\n267.79 470.85 1.78 c
      p3\n267.79 465.87 1.78 c p3\n284.56 480.81 1.78 c p3\n291.28 479.82
      1.78 c p3\n282.89 476.83 1.78 c p3\n276.18 473.84 1.78 c p3\n281.21
      479.82 1.78 c p3\n281.21 473.84 1.78 c p3\n274.50 476.83 1.78 c
      p3\n279.53 473.84 1.78 c p3\n277.85 468.86 1.78 c p3\n272.82 473.84
      1.78 c p3\n274.50 470.85 1.78 c p3\n267.79 472.84 1.78 c p3\n274.50
      472.84 1.78 c p3\n276.18 474.83 1.78 c p3\n274.50 474.83 1.78 c
      p3\n271.14 469.85 1.78 c p3\n269.46 470.85 1.78 c p3\n274.50 476.83
      1.78 c p3\n286.24 474.83 1.78 c p3\n287.92 477.82 1.78 c p3\n269.46
      471.85 1.78 c p3\n271.14 472.84 1.78 c p3\n276.18 477.82 1.78 c
      p3\n277.85 471.85 1.78 c p3\n267.79 466.86 1.78 c p3\n274.50 473.84
      1.78 c p3\n276.18 472.84 1.78 c p3\n256.04 467.86 1.78 c p3\n271.14
      466.86 1.78 c p3\n276.18 472.84 1.78 c p3\n281.21 473.84 1.78 c
      p3\n267.79 470.85 1.78 c p3\n281.21 473.84 1.78 c p3\n271.14 468.86
      1.78 c p3\n279.53 475.83 1.78 c p3\n272.82 472.84 1.78 c p3\n/bg { 0
      0.8039 0 } def\n271.14 492.77 1.78 c p3\n271.14 486.79 1.78 c
      p3\n269.46 491.77 1.78 c p3\n256.04 477.82 1.78 c p3\n264.43 487.79
      1.78 c p3\n264.43 479.82 1.78 c p3\n272.82 485.79 1.78 c p3\n257.72
      471.85 1.78 c p3\n266.11 488.78 1.78 c p3\n262.75 474.83 1.78 c
      p3\n251.01 472.84 1.78 c p3\n267.79 481.81 1.78 c p3\n254.36 482.80
      1.78 c p3\n266.11 483.80 1.78 c p3\n266.11 478.82 1.78 c p3\n269.46
      489.78 1.78 c p3\n267.79 478.82 1.78 c p3\n262.75 480.81 1.78 c
      p3\n254.36 484.80 1.78 c p3\n259.40 478.82 1.78 c p3\n271.14 481.81
      1.78 c p3\n264.43 483.80 1.78 c p3\n259.40 485.79 1.78 c p3\n264.43
      483.80 1.78 c p3\n266.11 486.79 1.78 c p3\n267.79 488.78 1.78 c
      p3\n264.43 490.77 1.78 c p3\n267.79 489.78 1.78 c p3\n266.11 482.80
      1.78 c p3\n261.08 479.82 1.78 c p3\n257.72 477.82 1.78 c p3\n257.72
      477.82 1.78 c p3\n262.75 480.81 1.78 c p3\n262.75 482.80 1.78 c
      p3\n267.79 476.83 1.78 c p3\n274.50 482.80 1.78 c p3\n269.46 489.78
      1.78 c p3\n256.04 485.79 1.78 c p3\n267.79 478.82 1.78 c p3\n259.40
      477.82 1.78 c p3\n261.08 477.82 1.78 c p3\n267.79 483.80 1.78 c
      p3\n261.08 480.81 1.78 c p3\n256.04 472.84 1.78 c p3\n262.75 478.82
      1.78 c p3\n267.79 479.82 1.78 c p3\n266.11 479.82 1.78 c p3\n266.11
      484.80 1.78 c p3\n259.40 473.84 1.78 c p3\n264.43 479.82 1.78 c p3\n/bg
      { 0 0 1 } def\n272.82 485.79 1.78 c p3\n262.75 480.81 1.78 c p3\n267.79
      493.76 1.78 c p3\n266.11 485.79 1.78 c p3\n267.79 487.79 1.78 c
      p3\n267.79 498.75 1.78 c p3\n259.40 471.85 1.78 c p3\n266.11 495.76
      1.78 c p3\n259.40 489.78 1.78 c p3\n277.85 494.76 1.78 c p3\n271.14
      487.79 1.78 c p3\n262.75 486.79 1.78 c p3\n267.79 490.77 1.78 c
      p3\n259.40 479.82 1.78 c p3\n264.43 480.81 1.78 c p3\n271.14 486.79
      1.78 c p3\n267.79 487.79 1.78 c p3\n281.21 499.74 1.78 c p3\n261.08
      499.74 1.78 c p3\n254.36 482.80 1.78 c p3\n271.14 491.77 1.78 c
      p3\n264.43 478.82 1.78 c p3\n264.43 499.74 1.78 c p3\n262.75 485.79
      1.78 c p3\n272.82 489.78 1.78 c p3\n271.14 494.76 1.78 c p3\n264.43
      484.80 1.78 c p3\n267.79 483.80 1.78 c p3\n264.43 486.79 1.78 c
      p3\n267.79 494.76 1.78 c p3\n264.43 496.75 1.78 c p3\n281.21 501.73
      1.78 c p3\n264.43 486.79 1.78 c p3\n264.43 485.79 1.78 c p3\n261.08
      483.80 1.78 c p3\n267.79 499.74 1.78 c p3\n274.50 485.79 1.78 c
      p3\n269.46 486.79 1.78 c p3\n267.79 482.80 1.78 c p3\n269.46 491.77
      1.78 c p3\n269.46 489.78 1.78 c p3\n269.46 491.77 1.78 c p3\n262.75
      480.81 1.78 c p3\n271.14 490.77 1.78 c p3\n272.82 489.78 1.78 c
      p3\n267.79 489.78 1.78 c p3\n259.40 485.79 1.78 c p3\n267.79 487.79
      1.78 c p3\n274.50 484.80 1.78 c p3\n267.79 481.81 1.78 c p3\n302.39
      464.43 345.88 503.17 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n302.39 464.43 m\n43.49 0.00
      l\n-0.00 38.74 l\n-43.49 0.00 l\n-0.00 -38.74 l\no\n302.39 464.43
      345.88 503.17 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n306.73 473.84 1.78 c p3\n306.73 471.85 1.78 c p3\n306.05
      469.85 1.78 c p3\n307.41 468.86 1.78 c p3\n306.73 472.84 1.78 c
      p3\n308.78 476.83 1.78 c p3\n306.73 468.86 1.78 c p3\n307.41 472.84
      1.78 c p3\n306.73 466.86 1.78 c p3\n307.41 471.85 1.78 c p3\n307.41
      476.83 1.78 c p3\n308.10 470.85 1.78 c p3\n306.73 470.85 1.78 c
      p3\n304.68 465.87 1.78 c p3\n305.37 480.81 1.78 c p3\n307.41 479.82
      1.78 c p3\n306.05 476.83 1.78 c p3\n306.73 473.84 1.78 c p3\n308.78
      479.82 1.78 c p3\n307.41 473.84 1.78 c p3\n308.78 476.83 1.78 c
      p3\n307.41 473.84 1.78 c p3\n304.00 468.86 1.78 c p3\n308.78 473.84
      1.78 c p3\n310.14 470.85 1.78 c p3\n308.10 472.84 1.78 c p3\n308.10
      472.84 1.78 c p3\n307.41 474.83 1.78 c p3\n306.73 474.83 1.78 c
      p3\n308.10 469.85 1.78 c p3\n308.10 470.85 1.78 c p3\n307.41 476.83
      1.78 c p3\n307.41 474.83 1.78 c p3\n306.73 477.82 1.78 c p3\n307.41
      471.85 1.78 c p3\n305.37 472.84 1.78 c p3\n306.05 477.82 1.78 c
      p3\n306.73 471.85 1.78 c p3\n306.05 466.86 1.78 c p3\n307.41 473.84
      1.78 c p3\n306.05 472.84 1.78 c p3\n306.05 467.86 1.78 c p3\n306.05
      466.86 1.78 c p3\n308.10 472.84 1.78 c p3\n310.14 473.84 1.78 c
      p3\n306.73 470.85 1.78 c p3\n308.10 473.84 1.78 c p3\n306.73 468.86
      1.78 c p3\n307.41 475.83 1.78 c p3\n306.73 472.84 1.78 c p3\n/bg { 0
      0.8039 0 } def\n329.25 492.77 1.78 c p3\n327.89 486.79 1.78 c
      p3\n330.62 491.77 1.78 c p3\n324.48 477.82 1.78 c p3\n328.57 487.79
      1.78 c p3\n327.89 479.82 1.78 c p3\n329.25 485.79 1.78 c p3\n319.70
      471.85 1.78 c p3\n328.57 488.78 1.78 c p3\n323.79 474.83 1.78 c
      p3\n321.06 472.84 1.78 c p3\n325.84 481.81 1.78 c p3\n324.48 482.80
      1.78 c p3\n329.25 483.80 1.78 c p3\n321.75 478.82 1.78 c p3\n327.20
      489.78 1.78 c p3\n327.89 478.82 1.78 c p3\n325.16 480.81 1.78 c
      p3\n327.89 484.80 1.78 c p3\n323.79 478.82 1.78 c p3\n329.93 481.81
      1.78 c p3\n324.48 483.80 1.78 c p3\n330.62 485.79 1.78 c p3\n329.25
      483.80 1.78 c p3\n326.52 486.79 1.78 c p3\n327.20 488.78 1.78 c
      p3\n329.93 490.77 1.78 c p3\n331.30 489.78 1.78 c p3\n327.89 482.80
      1.78 c p3\n321.06 479.82 1.78 c p3\n323.11 477.82 1.78 c p3\n322.43
      477.82 1.78 c p3\n323.79 480.81 1.78 c p3\n331.98 482.80 1.78 c
      p3\n327.89 476.83 1.78 c p3\n327.89 482.80 1.78 c p3\n329.25 489.78
      1.78 c p3\n327.20 485.79 1.78 c p3\n325.16 478.82 1.78 c p3\n324.48
      477.82 1.78 c p3\n327.20 477.82 1.78 c p3\n328.57 483.80 1.78 c
      p3\n324.48 480.81 1.78 c p3\n319.70 472.84 1.78 c p3\n325.84 478.82
      1.78 c p3\n325.84 479.82 1.78 c p3\n325.84 479.82 1.78 c p3\n326.52
      484.80 1.78 c p3\n317.65 473.84 1.78 c p3\n325.16 479.82 1.78 c p3\n/bg
      { 0 0 1 } def\n338.12 485.79 1.78 c p3\n331.98 480.81 1.78 c p3\n337.44
      493.76 1.78 c p3\n335.39 485.79 1.78 c p3\n336.76 487.79 1.78 c
      p3\n342.22 498.75 1.78 c p3\n327.89 471.85 1.78 c p3\n340.17 495.76
      1.78 c p3\n336.76 489.78 1.78 c p3\n338.81 494.76 1.78 c p3\n331.98
      487.79 1.78 c p3\n333.35 486.79 1.78 c p3\n334.71 490.77 1.78 c
      p3\n331.30 479.82 1.78 c p3\n331.98 480.81 1.78 c p3\n333.35 486.79
      1.78 c p3\n334.71 487.79 1.78 c p3\n342.90 499.74 1.78 c p3\n344.27
      499.74 1.78 c p3\n331.30 482.80 1.78 c p3\n336.08 491.77 1.78 c
      p3\n330.62 478.82 1.78 c p3\n342.90 499.74 1.78 c p3\n330.62 485.79
      1.78 c p3\n336.08 489.78 1.78 c p3\n338.12 494.76 1.78 c p3\n329.93
      484.80 1.78 c p3\n330.62 483.80 1.78 c p3\n335.39 486.79 1.78 c
      p3\n336.76 494.76 1.78 c p3\n338.81 496.75 1.78 c p3\n340.85 501.73
      1.78 c p3\n335.39 486.79 1.78 c p3\n331.98 485.79 1.78 c p3\n335.39
      483.80 1.78 c p3\n338.81 499.74 1.78 c p3\n335.39 485.79 1.78 c
      p3\n334.71 486.79 1.78 c p3\n329.93 482.80 1.78 c p3\n334.03 491.77
      1.78 c p3\n335.39 489.78 1.78 c p3\n331.98 491.77 1.78 c p3\n331.98
      480.81 1.78 c p3\n337.44 490.77 1.78 c p3\n336.08 489.78 1.78 c
      p3\n332.66 489.78 1.78 c p3\n331.30 485.79 1.78 c p3\n332.66 487.79
      1.78 c p3\n334.03 484.80 1.78 c p3\n331.98 481.81 1.78 c p3\n355.38
      464.43 398.87 503.17 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n355.38 464.43 m\n43.49 0.00
      l\n0.00 38.74 l\n-43.49 0.00 l\n-0.00 -38.74 l\no\nnp\n363.70 503.17
      m\n33.56 0.00 l\no\nnp\n363.70 503.17 m\n-0.00 4.75 l\no\nnp\n372.09
      503.17 m\n-0.00 4.75 l\no\nnp\n380.48 503.17 m\n0.00 4.75
      l\no\nnp\n388.87 503.17 m\n0.00 4.75 l\no\nnp\n397.26 503.17 m\n-0.00
      4.75 l\no\n/ps 8 def R 8 s\n363.70 514.57 (0.5) .5 0 0 t\n388.87 514.57
      (2.0) .5 0 0 t\nnp\n398.87 467.86 m\n0.00 34.87 l\no\nnp\n398.87 467.86
      m\n4.75 0.00 l\no\nnp\n398.87 472.84 m\n4.75 -0.00 l\no\nnp\n398.87
      477.82 m\n4.75 -0.00 l\no\nnp\n398.87 482.80 m\n4.75 0.00
      l\no\nnp\n398.87 487.79 m\n4.75 0.00 l\no\nnp\n398.87 492.77 m\n4.75
      -0.00 l\no\nnp\n398.87 497.75 m\n4.75 0 l\no\nnp\n398.87 502.73 m\n4.75
      0.00 l\no\n415.98 467.86 (4.5) .5 0 90 t\n415.98 487.79 (6.5) .5 0 90
      t\n355.38 464.43 398.87 503.17 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\n358.67 473.84 1.78 c p3\n358.67 471.85 1.78
      c p3\n358.67 469.85 1.78 c p3\n358.67 468.86 1.78 c p3\n358.67 472.84
      1.78 c p3\n362.03 476.83 1.78 c p3\n360.35 468.86 1.78 c p3\n358.67
      472.84 1.78 c p3\n358.67 466.86 1.78 c p3\n356.99 471.85 1.78 c
      p3\n358.67 476.83 1.78 c p3\n358.67 470.85 1.78 c p3\n356.99 470.85
      1.78 c p3\n356.99 465.87 1.78 c p3\n358.67 480.81 1.78 c p3\n362.03
      479.82 1.78 c p3\n362.03 476.83 1.78 c p3\n360.35 473.84 1.78 c
      p3\n360.35 479.82 1.78 c p3\n360.35 473.84 1.78 c p3\n358.67 476.83
      1.78 c p3\n362.03 473.84 1.78 c p3\n358.67 468.86 1.78 c p3\n363.70
      473.84 1.78 c p3\n358.67 470.85 1.78 c p3\n358.67 472.84 1.78 c
      p3\n362.03 472.84 1.78 c p3\n358.67 474.83 1.78 c p3\n358.67 474.83
      1.78 c p3\n358.67 469.85 1.78 c p3\n358.67 470.85 1.78 c p3\n362.03
      476.83 1.78 c p3\n356.99 474.83 1.78 c p3\n358.67 477.82 1.78 c
      p3\n358.67 471.85 1.78 c p3\n358.67 472.84 1.78 c p3\n358.67 477.82
      1.78 c p3\n356.99 471.85 1.78 c p3\n358.67 466.86 1.78 c p3\n358.67
      473.84 1.78 c p3\n360.35 472.84 1.78 c p3\n360.35 467.86 1.78 c
      p3\n358.67 466.86 1.78 c p3\n365.38 472.84 1.78 c p3\n362.03 473.84
      1.78 c p3\n360.35 470.85 1.78 c p3\n358.67 473.84 1.78 c p3\n358.67
      468.86 1.78 c p3\n358.67 475.83 1.78 c p3\n358.67 472.84 1.78 c p3\n/bg
      { 0 0.8039 0 } def\n378.80 492.77 1.78 c p3\n380.48 486.79 1.78 c
      p3\n380.48 491.77 1.78 c p3\n377.13 477.82 1.78 c p3\n380.48 487.79
      1.78 c p3\n377.13 479.82 1.78 c p3\n382.16 485.79 1.78 c p3\n372.09
      471.85 1.78 c p3\n377.13 488.78 1.78 c p3\n378.80 474.83 1.78 c
      p3\n372.09 472.84 1.78 c p3\n380.48 481.81 1.78 c p3\n372.09 482.80
      1.78 c p3\n378.80 483.80 1.78 c p3\n377.13 478.82 1.78 c p3\n378.80
      489.78 1.78 c p3\n380.48 478.82 1.78 c p3\n372.09 480.81 1.78 c
      p3\n380.48 484.80 1.78 c p3\n373.77 478.82 1.78 c p3\n385.51 481.81
      1.78 c p3\n377.13 483.80 1.78 c p3\n380.48 485.79 1.78 c p3\n375.45
      483.80 1.78 c p3\n377.13 486.79 1.78 c p3\n378.80 488.78 1.78 c
      p3\n378.80 490.77 1.78 c p3\n383.84 489.78 1.78 c p3\n380.48 482.80
      1.78 c p3\n372.09 479.82 1.78 c p3\n373.77 477.82 1.78 c p3\n372.09
      477.82 1.78 c p3\n375.45 480.81 1.78 c p3\n382.16 482.80 1.78 c
      p3\n380.48 476.83 1.78 c p3\n382.16 482.80 1.78 c p3\n380.48 489.78
      1.78 c p3\n377.13 485.79 1.78 c p3\n377.13 478.82 1.78 c p3\n377.13
      477.82 1.78 c p3\n375.45 477.82 1.78 c p3\n378.80 483.80 1.78 c
      p3\n375.45 480.81 1.78 c p3\n372.09 472.84 1.78 c p3\n377.13 478.82
      1.78 c p3\n375.45 479.82 1.78 c p3\n377.13 479.82 1.78 c p3\n377.13
      484.80 1.78 c p3\n373.77 473.84 1.78 c p3\n377.13 479.82 1.78 c p3\n/bg
      { 0 0 1 } def\n397.26 485.79 1.78 c p3\n387.19 480.81 1.78 c p3\n390.55
      493.76 1.78 c p3\n385.51 485.79 1.78 c p3\n392.23 487.79 1.78 c
      p3\n390.55 498.75 1.78 c p3\n383.84 471.85 1.78 c p3\n385.51 495.76
      1.78 c p3\n385.51 489.78 1.78 c p3\n397.26 494.76 1.78 c p3\n388.87
      487.79 1.78 c p3\n387.19 486.79 1.78 c p3\n390.55 490.77 1.78 c
      p3\n388.87 479.82 1.78 c p3\n395.58 480.81 1.78 c p3\n393.90 486.79
      1.78 c p3\n385.51 487.79 1.78 c p3\n392.23 499.74 1.78 c p3\n393.90
      499.74 1.78 c p3\n380.48 482.80 1.78 c p3\n393.90 491.77 1.78 c
      p3\n388.87 478.82 1.78 c p3\n388.87 499.74 1.78 c p3\n385.51 485.79
      1.78 c p3\n390.55 489.78 1.78 c p3\n385.51 494.76 1.78 c p3\n385.51
      484.80 1.78 c p3\n385.51 483.80 1.78 c p3\n390.55 486.79 1.78 c
      p3\n382.16 494.76 1.78 c p3\n387.19 496.75 1.78 c p3\n388.87 501.73
      1.78 c p3\n392.23 486.79 1.78 c p3\n380.48 485.79 1.78 c p3\n378.80
      483.80 1.78 c p3\n393.90 499.74 1.78 c p3\n395.58 485.79 1.78 c
      p3\n385.51 486.79 1.78 c p3\n385.51 482.80 1.78 c p3\n390.55 491.77
      1.78 c p3\n395.58 489.78 1.78 c p3\n393.90 491.77 1.78 c p3\n387.19
      480.81 1.78 c p3\n393.90 490.77 1.78 c p3\n397.26 489.78 1.78 c
      p3\n393.90 489.78 1.78 c p3\n387.19 485.79 1.78 c p3\n388.87 487.79
      1.78 c p3\n393.90 484.80 1.78 c p3\n385.51 481.81 1.78 c p3\n196.41
      416.19 239.89 454.93 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n196.41 416.19 m\n43.48 -0.00
      l\n-0.00 38.74 l\n-43.48 0.00 l\n-0.00 -38.74 l\no\nnp\n196.41 417.63
      m\n-0.00 29.89 l\no\nnp\n196.41 417.63 m\n-4.76 -0.00 l\no\nnp\n196.41
      425.10 m\n-4.76 0.00 l\no\nnp\n196.41 432.57 m\n-4.76 -0.00
      l\no\nnp\n196.41 440.04 m\n-4.76 0.00 l\no\nnp\n196.41 447.52 m\n-4.76
      -0.00 l\no\n/ps 8 def R 8 s\n185.00 417.63 (2.0) .5 0 90 t\n185.00
      440.04 (3.5) .5 0 90 t\n196.41 416.19 239.89 454.93 cl\n/bg { 1 0 0 }
      def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n206.96 440.04 1.78 c
      p3\n204.73 432.57 1.78 c p3\n202.49 435.56 1.78 c p3\n201.37 434.07
      1.78 c p3\n205.85 441.54 1.78 c p3\n210.32 446.02 1.78 c p3\n201.37
      438.55 1.78 c p3\n205.85 438.55 1.78 c p3\n199.13 431.08 1.78 c
      p3\n204.73 434.07 1.78 c p3\n210.32 443.03 1.78 c p3\n203.61 438.55
      1.78 c p3\n203.61 432.57 1.78 c p3\n198.02 432.57 1.78 c p3\n214.79
      447.52 1.78 c p3\n213.68 453.49 1.78 c p3\n210.32 446.02 1.78 c
      p3\n206.96 440.04 1.78 c p3\n213.68 444.53 1.78 c p3\n206.96 444.53
      1.78 c p3\n210.32 438.55 1.78 c p3\n206.96 443.03 1.78 c p3\n201.37
      441.54 1.78 c p3\n206.96 437.06 1.78 c p3\n203.61 438.55 1.78 c
      p3\n205.85 432.57 1.78 c p3\n205.85 438.55 1.78 c p3\n208.08 440.04
      1.78 c p3\n208.08 438.55 1.78 c p3\n202.49 435.56 1.78 c p3\n203.61
      434.07 1.78 c p3\n210.32 438.55 1.78 c p3\n208.08 449.01 1.78 c
      p3\n211.44 450.51 1.78 c p3\n204.73 434.07 1.78 c p3\n205.85 435.56
      1.78 c p3\n211.44 440.04 1.78 c p3\n204.73 441.54 1.78 c p3\n199.13
      432.57 1.78 c p3\n206.96 438.55 1.78 c p3\n205.85 440.04 1.78 c
      p3\n200.25 422.11 1.78 c p3\n199.13 435.56 1.78 c p3\n205.85 440.04
      1.78 c p3\n206.96 444.53 1.78 c p3\n203.61 432.57 1.78 c p3\n206.96
      444.53 1.78 c p3\n201.37 435.56 1.78 c p3\n209.20 443.03 1.78 c
      p3\n205.85 437.06 1.78 c p3\n/bg { 0 0.8039 0 } def\n228.22 435.56 1.78
      c p3\n221.51 435.56 1.78 c p3\n227.10 434.07 1.78 c p3\n211.44 422.11
      1.78 c p3\n222.62 429.58 1.78 c p3\n213.68 429.58 1.78 c p3\n220.39
      437.06 1.78 c p3\n204.73 423.61 1.78 c p3\n223.74 431.08 1.78 c
      p3\n208.08 428.09 1.78 c p3\n205.85 417.63 1.78 c p3\n215.91 432.57
      1.78 c p3\n217.03 420.62 1.78 c p3\n218.15 431.08 1.78 c p3\n212.56
      431.08 1.78 c p3\n224.86 434.07 1.78 c p3\n212.56 432.57 1.78 c
      p3\n214.79 428.09 1.78 c p3\n219.27 420.62 1.78 c p3\n212.56 425.10
      1.78 c p3\n215.91 435.56 1.78 c p3\n218.15 429.58 1.78 c p3\n220.39
      425.10 1.78 c p3\n218.15 429.58 1.78 c p3\n221.51 431.08 1.78 c
      p3\n223.74 432.57 1.78 c p3\n225.98 429.58 1.78 c p3\n224.86 432.57
      1.78 c p3\n217.03 431.08 1.78 c p3\n213.68 426.59 1.78 c p3\n211.44
      423.61 1.78 c p3\n211.44 423.61 1.78 c p3\n214.79 428.09 1.78 c
      p3\n217.03 428.09 1.78 c p3\n210.32 432.57 1.78 c p3\n217.03 438.55
      1.78 c p3\n224.86 434.07 1.78 c p3\n220.39 422.11 1.78 c p3\n212.56
      432.57 1.78 c p3\n211.44 425.10 1.78 c p3\n211.44 426.59 1.78 c
      p3\n218.15 432.57 1.78 c p3\n214.79 426.59 1.78 c p3\n205.85 422.11
      1.78 c p3\n212.56 428.09 1.78 c p3\n213.68 432.57 1.78 c p3\n213.68
      431.08 1.78 c p3\n219.27 431.08 1.78 c p3\n206.96 425.10 1.78 c
      p3\n213.68 429.58 1.78 c p3\n/bg { 0 0 1 } def\n220.39 437.06 1.78 c
      p3\n214.79 428.09 1.78 c p3\n229.33 432.57 1.78 c p3\n220.39 431.08
      1.78 c p3\n222.62 432.57 1.78 c p3\n234.93 432.57 1.78 c p3\n204.73
      425.10 1.78 c p3\n231.57 431.08 1.78 c p3\n224.86 425.10 1.78 c
      p3\n230.45 441.54 1.78 c p3\n222.62 435.56 1.78 c p3\n221.51 428.09
      1.78 c p3\n225.98 432.57 1.78 c p3\n213.68 425.10 1.78 c p3\n214.79
      429.58 1.78 c p3\n221.51 435.56 1.78 c p3\n222.62 432.57 1.78 c
      p3\n236.05 444.53 1.78 c p3\n236.05 426.59 1.78 c p3\n217.03 420.62
      1.78 c p3\n227.10 435.56 1.78 c p3\n212.56 429.58 1.78 c p3\n236.05
      429.58 1.78 c p3\n220.39 428.09 1.78 c p3\n224.86 437.06 1.78 c
      p3\n230.45 435.56 1.78 c p3\n219.27 429.58 1.78 c p3\n218.15 432.57
      1.78 c p3\n221.51 429.58 1.78 c p3\n230.45 432.57 1.78 c p3\n232.69
      429.58 1.78 c p3\n238.28 444.53 1.78 c p3\n221.51 429.58 1.78 c
      p3\n220.39 429.58 1.78 c p3\n218.15 426.59 1.78 c p3\n236.05 432.57
      1.78 c p3\n220.39 438.55 1.78 c p3\n221.51 434.07 1.78 c p3\n217.03
      432.57 1.78 c p3\n227.10 434.07 1.78 c p3\n224.86 434.07 1.78 c
      p3\n227.10 434.07 1.78 c p3\n214.79 428.09 1.78 c p3\n225.98 435.56
      1.78 c p3\n224.86 437.06 1.78 c p3\n224.86 432.57 1.78 c p3\n220.39
      425.10 1.78 c p3\n222.62 432.57 1.78 c p3\n219.27 438.55 1.78 c
      p3\n215.91 432.57 1.78 c p3\n249.40 416.19 292.89 454.93 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 416.19 m\n43.49 -0.00 l\n-0.00 38.74 l\n-43.49 0.00
      l\n0.00 -38.74 l\no\n249.40 416.19 292.89 454.93 cl\n/ps 16 def R 16
      s\n0 0 0 rgb\n271.14 431.32 (Sepal.Width) .5 0 0 t\n302.39 416.19
      345.88 454.93 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\nnp\n302.39 416.19 m\n43.49 -0.00 l\n-0.00
      38.74 l\n-43.49 0.00 l\n-0.00 -38.74 l\no\n302.39 416.19 345.88 454.93
      cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n306.73 440.04 1.78 c p3\n306.73 432.57 1.78 c p3\n306.05
      435.56 1.78 c p3\n307.41 434.07 1.78 c p3\n306.73 441.54 1.78 c
      p3\n308.78 446.02 1.78 c p3\n306.73 438.55 1.78 c p3\n307.41 438.55
      1.78 c p3\n306.73 431.08 1.78 c p3\n307.41 434.07 1.78 c p3\n307.41
      443.03 1.78 c p3\n308.10 438.55 1.78 c p3\n306.73 432.57 1.78 c
      p3\n304.68 432.57 1.78 c p3\n305.37 447.52 1.78 c p3\n307.41 453.49
      1.78 c p3\n306.05 446.02 1.78 c p3\n306.73 440.04 1.78 c p3\n308.78
      444.53 1.78 c p3\n307.41 444.53 1.78 c p3\n308.78 438.55 1.78 c
      p3\n307.41 443.03 1.78 c p3\n304.00 441.54 1.78 c p3\n308.78 437.06
      1.78 c p3\n310.14 438.55 1.78 c p3\n308.10 432.57 1.78 c p3\n308.10
      438.55 1.78 c p3\n307.41 440.04 1.78 c p3\n306.73 438.55 1.78 c
      p3\n308.10 435.56 1.78 c p3\n308.10 434.07 1.78 c p3\n307.41 438.55
      1.78 c p3\n307.41 449.01 1.78 c p3\n306.73 450.51 1.78 c p3\n307.41
      434.07 1.78 c p3\n305.37 435.56 1.78 c p3\n306.05 440.04 1.78 c
      p3\n306.73 441.54 1.78 c p3\n306.05 432.57 1.78 c p3\n307.41 438.55
      1.78 c p3\n306.05 440.04 1.78 c p3\n306.05 422.11 1.78 c p3\n306.05
      435.56 1.78 c p3\n308.10 440.04 1.78 c p3\n310.14 444.53 1.78 c
      p3\n306.73 432.57 1.78 c p3\n308.10 444.53 1.78 c p3\n306.73 435.56
      1.78 c p3\n307.41 443.03 1.78 c p3\n306.73 437.06 1.78 c p3\n/bg { 0
      0.8039 0 } def\n329.25 435.56 1.78 c p3\n327.89 435.56 1.78 c
      p3\n330.62 434.07 1.78 c p3\n324.48 422.11 1.78 c p3\n328.57 429.58
      1.78 c p3\n327.89 429.58 1.78 c p3\n329.25 437.06 1.78 c p3\n319.70
      423.61 1.78 c p3\n328.57 431.08 1.78 c p3\n323.79 428.09 1.78 c
      p3\n321.06 417.63 1.78 c p3\n325.84 432.57 1.78 c p3\n324.48 420.62
      1.78 c p3\n329.25 431.08 1.78 c p3\n321.75 431.08 1.78 c p3\n327.20
      434.07 1.78 c p3\n327.89 432.57 1.78 c p3\n325.16 428.09 1.78 c
      p3\n327.89 420.62 1.78 c p3\n323.79 425.10 1.78 c p3\n329.93 435.56
      1.78 c p3\n324.48 429.58 1.78 c p3\n330.62 425.10 1.78 c p3\n329.25
      429.58 1.78 c p3\n326.52 431.08 1.78 c p3\n327.20 432.57 1.78 c
      p3\n329.93 429.58 1.78 c p3\n331.30 432.57 1.78 c p3\n327.89 431.08
      1.78 c p3\n321.06 426.59 1.78 c p3\n323.11 423.61 1.78 c p3\n322.43
      423.61 1.78 c p3\n323.79 428.09 1.78 c p3\n331.98 428.09 1.78 c
      p3\n327.89 432.57 1.78 c p3\n327.89 438.55 1.78 c p3\n329.25 434.07
      1.78 c p3\n327.20 422.11 1.78 c p3\n325.16 432.57 1.78 c p3\n324.48
      425.10 1.78 c p3\n327.20 426.59 1.78 c p3\n328.57 432.57 1.78 c
      p3\n324.48 426.59 1.78 c p3\n319.70 422.11 1.78 c p3\n325.84 428.09
      1.78 c p3\n325.84 432.57 1.78 c p3\n325.84 431.08 1.78 c p3\n326.52
      431.08 1.78 c p3\n317.65 425.10 1.78 c p3\n325.16 429.58 1.78 c p3\n/bg
      { 0 0 1 } def\n338.12 437.06 1.78 c p3\n331.98 428.09 1.78 c p3\n337.44
      432.57 1.78 c p3\n335.39 431.08 1.78 c p3\n336.76 432.57 1.78 c
      p3\n342.22 432.57 1.78 c p3\n327.89 425.10 1.78 c p3\n340.17 431.08
      1.78 c p3\n336.76 425.10 1.78 c p3\n338.81 441.54 1.78 c p3\n331.98
      435.56 1.78 c p3\n333.35 428.09 1.78 c p3\n334.71 432.57 1.78 c
      p3\n331.30 425.10 1.78 c p3\n331.98 429.58 1.78 c p3\n333.35 435.56
      1.78 c p3\n334.71 432.57 1.78 c p3\n342.90 444.53 1.78 c p3\n344.27
      426.59 1.78 c p3\n331.30 420.62 1.78 c p3\n336.08 435.56 1.78 c
      p3\n330.62 429.58 1.78 c p3\n342.90 429.58 1.78 c p3\n330.62 428.09
      1.78 c p3\n336.08 437.06 1.78 c p3\n338.12 435.56 1.78 c p3\n329.93
      429.58 1.78 c p3\n330.62 432.57 1.78 c p3\n335.39 429.58 1.78 c
      p3\n336.76 432.57 1.78 c p3\n338.81 429.58 1.78 c p3\n340.85 444.53
      1.78 c p3\n335.39 429.58 1.78 c p3\n331.98 429.58 1.78 c p3\n335.39
      426.59 1.78 c p3\n338.81 432.57 1.78 c p3\n335.39 438.55 1.78 c
      p3\n334.71 434.07 1.78 c p3\n329.93 432.57 1.78 c p3\n334.03 434.07
      1.78 c p3\n335.39 434.07 1.78 c p3\n331.98 434.07 1.78 c p3\n331.98
      428.09 1.78 c p3\n337.44 435.56 1.78 c p3\n336.08 437.06 1.78 c
      p3\n332.66 432.57 1.78 c p3\n331.30 425.10 1.78 c p3\n332.66 432.57
      1.78 c p3\n334.03 438.55 1.78 c p3\n331.98 432.57 1.78 c p3\n355.38
      416.19 398.87 454.93 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n355.38 416.19 m\n43.49 -0.00
      l\n0.00 38.74 l\n-43.49 0.00 l\n-0.00 -38.74 l\no\n355.38 416.19 398.87
      454.93 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n358.67 440.04 1.78 c p3\n358.67 432.57 1.78 c p3\n358.67
      435.56 1.78 c p3\n358.67 434.07 1.78 c p3\n358.67 441.54 1.78 c
      p3\n362.03 446.02 1.78 c p3\n360.35 438.55 1.78 c p3\n358.67 438.55
      1.78 c p3\n358.67 431.08 1.78 c p3\n356.99 434.07 1.78 c p3\n358.67
      443.03 1.78 c p3\n358.67 438.55 1.78 c p3\n356.99 432.57 1.78 c
      p3\n356.99 432.57 1.78 c p3\n358.67 447.52 1.78 c p3\n362.03 453.49
      1.78 c p3\n362.03 446.02 1.78 c p3\n360.35 440.04 1.78 c p3\n360.35
      444.53 1.78 c p3\n360.35 444.53 1.78 c p3\n358.67 438.55 1.78 c
      p3\n362.03 443.03 1.78 c p3\n358.67 441.54 1.78 c p3\n363.70 437.06
      1.78 c p3\n358.67 438.55 1.78 c p3\n358.67 432.57 1.78 c p3\n362.03
      438.55 1.78 c p3\n358.67 440.04 1.78 c p3\n358.67 438.55 1.78 c
      p3\n358.67 435.56 1.78 c p3\n358.67 434.07 1.78 c p3\n362.03 438.55
      1.78 c p3\n356.99 449.01 1.78 c p3\n358.67 450.51 1.78 c p3\n358.67
      434.07 1.78 c p3\n358.67 435.56 1.78 c p3\n358.67 440.04 1.78 c
      p3\n356.99 441.54 1.78 c p3\n358.67 432.57 1.78 c p3\n358.67 438.55
      1.78 c p3\n360.35 440.04 1.78 c p3\n360.35 422.11 1.78 c p3\n358.67
      435.56 1.78 c p3\n365.38 440.04 1.78 c p3\n362.03 444.53 1.78 c
      p3\n360.35 432.57 1.78 c p3\n358.67 444.53 1.78 c p3\n358.67 435.56
      1.78 c p3\n358.67 443.03 1.78 c p3\n358.67 437.06 1.78 c p3\n/bg { 0
      0.8039 0 } def\n378.80 435.56 1.78 c p3\n380.48 435.56 1.78 c
      p3\n380.48 434.07 1.78 c p3\n377.13 422.11 1.78 c p3\n380.48 429.58
      1.78 c p3\n377.13 429.58 1.78 c p3\n382.16 437.06 1.78 c p3\n372.09
      423.61 1.78 c p3\n377.13 431.08 1.78 c p3\n378.80 428.09 1.78 c
      p3\n372.09 417.63 1.78 c p3\n380.48 432.57 1.78 c p3\n372.09 420.62
      1.78 c p3\n378.80 431.08 1.78 c p3\n377.13 431.08 1.78 c p3\n378.80
      434.07 1.78 c p3\n380.48 432.57 1.78 c p3\n372.09 428.09 1.78 c
      p3\n380.48 420.62 1.78 c p3\n373.77 425.10 1.78 c p3\n385.51 435.56
      1.78 c p3\n377.13 429.58 1.78 c p3\n380.48 425.10 1.78 c p3\n375.45
      429.58 1.78 c p3\n377.13 431.08 1.78 c p3\n378.80 432.57 1.78 c
      p3\n378.80 429.58 1.78 c p3\n383.84 432.57 1.78 c p3\n380.48 431.08
      1.78 c p3\n372.09 426.59 1.78 c p3\n373.77 423.61 1.78 c p3\n372.09
      423.61 1.78 c p3\n375.45 428.09 1.78 c p3\n382.16 428.09 1.78 c
      p3\n380.48 432.57 1.78 c p3\n382.16 438.55 1.78 c p3\n380.48 434.07
      1.78 c p3\n377.13 422.11 1.78 c p3\n377.13 432.57 1.78 c p3\n377.13
      425.10 1.78 c p3\n375.45 426.59 1.78 c p3\n378.80 432.57 1.78 c
      p3\n375.45 426.59 1.78 c p3\n372.09 422.11 1.78 c p3\n377.13 428.09
      1.78 c p3\n375.45 432.57 1.78 c p3\n377.13 431.08 1.78 c p3\n377.13
      431.08 1.78 c p3\n373.77 425.10 1.78 c p3\n377.13 429.58 1.78 c p3\n/bg
      { 0 0 1 } def\n397.26 437.06 1.78 c p3\n387.19 428.09 1.78 c p3\n390.55
      432.57 1.78 c p3\n385.51 431.08 1.78 c p3\n392.23 432.57 1.78 c
      p3\n390.55 432.57 1.78 c p3\n383.84 425.10 1.78 c p3\n385.51 431.08
      1.78 c p3\n385.51 425.10 1.78 c p3\n397.26 441.54 1.78 c p3\n388.87
      435.56 1.78 c p3\n387.19 428.09 1.78 c p3\n390.55 432.57 1.78 c
      p3\n388.87 425.10 1.78 c p3\n395.58 429.58 1.78 c p3\n393.90 435.56
      1.78 c p3\n385.51 432.57 1.78 c p3\n392.23 444.53 1.78 c p3\n393.90
      426.59 1.78 c p3\n380.48 420.62 1.78 c p3\n393.90 435.56 1.78 c
      p3\n388.87 429.58 1.78 c p3\n388.87 429.58 1.78 c p3\n385.51 428.09
      1.78 c p3\n390.55 437.06 1.78 c p3\n385.51 435.56 1.78 c p3\n385.51
      429.58 1.78 c p3\n385.51 432.57 1.78 c p3\n390.55 429.58 1.78 c
      p3\n382.16 432.57 1.78 c p3\n387.19 429.58 1.78 c p3\n388.87 444.53
      1.78 c p3\n392.23 429.58 1.78 c p3\n380.48 429.58 1.78 c p3\n378.80
      426.59 1.78 c p3\n393.90 432.57 1.78 c p3\n395.58 438.55 1.78 c
      p3\n385.51 434.07 1.78 c p3\n385.51 432.57 1.78 c p3\n390.55 434.07
      1.78 c p3\n395.58 434.07 1.78 c p3\n393.90 434.07 1.78 c p3\n387.19
      428.09 1.78 c p3\n393.90 435.56 1.78 c p3\n397.26 437.06 1.78 c
      p3\n393.90 432.57 1.78 c p3\n387.19 425.10 1.78 c p3\n388.87 432.57
      1.78 c p3\n393.90 438.55 1.78 c p3\n385.51 432.57 1.78 c p3\n196.41
      367.95 239.89 406.69 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n196.41 367.95 m\n43.48 -0.00
      l\n-0.00 38.74 l\n-43.48 -0.00 l\n-0.00 -38.74 l\no\n196.41 367.95
      239.89 406.69 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n206.96 371.82 1.78 c p3\n204.73 371.82 1.78 c p3\n202.49
      371.21 1.78 c p3\n201.37 372.43 1.78 c p3\n205.85 371.82 1.78 c
      p3\n210.32 373.64 1.78 c p3\n201.37 371.82 1.78 c p3\n205.85 372.43
      1.78 c p3\n199.13 371.82 1.78 c p3\n204.73 372.43 1.78 c p3\n210.32
      372.43 1.78 c p3\n203.61 373.04 1.78 c p3\n203.61 371.82 1.78 c
      p3\n198.02 370.00 1.78 c p3\n214.79 370.60 1.78 c p3\n213.68 372.43
      1.78 c p3\n210.32 371.21 1.78 c p3\n206.96 371.82 1.78 c p3\n213.68
      373.64 1.78 c p3\n206.96 372.43 1.78 c p3\n210.32 373.64 1.78 c
      p3\n206.96 372.43 1.78 c p3\n201.37 369.39 1.78 c p3\n206.96 373.64
      1.78 c p3\n203.61 374.86 1.78 c p3\n205.85 373.04 1.78 c p3\n205.85
      373.04 1.78 c p3\n208.08 372.43 1.78 c p3\n208.08 371.82 1.78 c
      p3\n202.49 373.04 1.78 c p3\n203.61 373.04 1.78 c p3\n210.32 372.43
      1.78 c p3\n208.08 372.43 1.78 c p3\n211.44 371.82 1.78 c p3\n204.73
      372.43 1.78 c p3\n205.85 370.60 1.78 c p3\n211.44 371.21 1.78 c
      p3\n204.73 371.82 1.78 c p3\n199.13 371.21 1.78 c p3\n206.96 372.43
      1.78 c p3\n205.85 371.21 1.78 c p3\n200.25 371.21 1.78 c p3\n199.13
      371.21 1.78 c p3\n205.85 373.04 1.78 c p3\n206.96 374.86 1.78 c
      p3\n203.61 371.82 1.78 c p3\n206.96 373.04 1.78 c p3\n201.37 371.82
      1.78 c p3\n209.20 372.43 1.78 c p3\n205.85 371.82 1.78 c p3\n/bg { 0
      0.8039 0 } def\n228.22 391.88 1.78 c p3\n221.51 390.66 1.78 c
      p3\n227.10 393.10 1.78 c p3\n211.44 387.62 1.78 c p3\n222.62 391.27
      1.78 c p3\n213.68 390.66 1.78 c p3\n220.39 391.88 1.78 c p3\n204.73
      383.37 1.78 c p3\n223.74 391.27 1.78 c p3\n208.08 387.02 1.78 c
      p3\n205.85 384.59 1.78 c p3\n215.91 388.84 1.78 c p3\n217.03 387.62
      1.78 c p3\n218.15 391.88 1.78 c p3\n212.56 385.19 1.78 c p3\n224.86
      390.06 1.78 c p3\n212.56 390.66 1.78 c p3\n214.79 388.23 1.78 c
      p3\n219.27 390.66 1.78 c p3\n212.56 387.02 1.78 c p3\n215.91 392.49
      1.78 c p3\n218.15 387.62 1.78 c p3\n220.39 393.10 1.78 c p3\n218.15
      391.88 1.78 c p3\n221.51 389.45 1.78 c p3\n223.74 390.06 1.78 c
      p3\n225.98 392.49 1.78 c p3\n224.86 393.70 1.78 c p3\n217.03 390.66
      1.78 c p3\n213.68 384.59 1.78 c p3\n211.44 386.41 1.78 c p3\n211.44
      385.80 1.78 c p3\n214.79 387.02 1.78 c p3\n217.03 394.31 1.78 c
      p3\n210.32 390.66 1.78 c p3\n217.03 390.66 1.78 c p3\n224.86 391.88
      1.78 c p3\n220.39 390.06 1.78 c p3\n212.56 388.23 1.78 c p3\n211.44
      387.62 1.78 c p3\n211.44 390.06 1.78 c p3\n218.15 391.27 1.78 c
      p3\n214.79 387.62 1.78 c p3\n205.85 383.37 1.78 c p3\n212.56 388.84
      1.78 c p3\n213.68 388.84 1.78 c p3\n213.68 388.84 1.78 c p3\n219.27
      389.45 1.78 c p3\n206.96 381.55 1.78 c p3\n213.68 388.23 1.78 c p3\n/bg
      { 0 0 1 } def\n220.39 399.78 1.78 c p3\n214.79 394.31 1.78 c p3\n229.33
      399.18 1.78 c p3\n220.39 397.35 1.78 c p3\n222.62 398.57 1.78 c
      p3\n234.93 403.43 1.78 c p3\n204.73 390.66 1.78 c p3\n231.57 401.61
      1.78 c p3\n224.86 398.57 1.78 c p3\n230.45 400.39 1.78 c p3\n222.62
      394.31 1.78 c p3\n221.51 395.53 1.78 c p3\n225.98 396.74 1.78 c
      p3\n213.68 393.70 1.78 c p3\n214.79 394.31 1.78 c p3\n221.51 395.53
      1.78 c p3\n222.62 396.74 1.78 c p3\n236.05 404.04 1.78 c p3\n236.05
      405.25 1.78 c p3\n217.03 393.70 1.78 c p3\n227.10 397.96 1.78 c
      p3\n212.56 393.10 1.78 c p3\n236.05 404.04 1.78 c p3\n220.39 393.10
      1.78 c p3\n224.86 397.96 1.78 c p3\n230.45 399.78 1.78 c p3\n219.27
      392.49 1.78 c p3\n218.15 393.10 1.78 c p3\n221.51 397.35 1.78 c
      p3\n230.45 398.57 1.78 c p3\n232.69 400.39 1.78 c p3\n238.28 402.21
      1.78 c p3\n221.51 397.35 1.78 c p3\n220.39 394.31 1.78 c p3\n218.15
      397.35 1.78 c p3\n236.05 400.39 1.78 c p3\n220.39 397.35 1.78 c
      p3\n221.51 396.74 1.78 c p3\n217.03 392.49 1.78 c p3\n227.10 396.14
      1.78 c p3\n224.86 397.35 1.78 c p3\n227.10 394.31 1.78 c p3\n214.79
      394.31 1.78 c p3\n225.98 399.18 1.78 c p3\n224.86 397.96 1.78 c
      p3\n224.86 394.92 1.78 c p3\n220.39 393.70 1.78 c p3\n222.62 394.92
      1.78 c p3\n219.27 396.14 1.78 c p3\n215.91 394.31 1.78 c p3\n249.40
      367.95 292.89 406.69 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n249.40 367.95 m\n43.49 -0.00
      l\n-0.00 38.74 l\n-43.49 -0.00 l\n0.00 -38.74 l\no\n249.40 367.95
      292.89 406.69 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n276.18 371.82 1.78 c p3\n267.79 371.82 1.78 c p3\n271.14
      371.21 1.78 c p3\n269.46 372.43 1.78 c p3\n277.85 371.82 1.78 c
      p3\n282.89 373.64 1.78 c p3\n274.50 371.82 1.78 c p3\n274.50 372.43
      1.78 c p3\n266.11 371.82 1.78 c p3\n269.46 372.43 1.78 c p3\n279.53
      372.43 1.78 c p3\n274.50 373.04 1.78 c p3\n267.79 371.82 1.78 c
      p3\n267.79 370.00 1.78 c p3\n284.56 370.60 1.78 c p3\n291.28 372.43
      1.78 c p3\n282.89 371.21 1.78 c p3\n276.18 371.82 1.78 c p3\n281.21
      373.64 1.78 c p3\n281.21 372.43 1.78 c p3\n274.50 373.64 1.78 c
      p3\n279.53 372.43 1.78 c p3\n277.85 369.39 1.78 c p3\n272.82 373.64
      1.78 c p3\n274.50 374.86 1.78 c p3\n267.79 373.04 1.78 c p3\n274.50
      373.04 1.78 c p3\n276.18 372.43 1.78 c p3\n274.50 371.82 1.78 c
      p3\n271.14 373.04 1.78 c p3\n269.46 373.04 1.78 c p3\n274.50 372.43
      1.78 c p3\n286.24 372.43 1.78 c p3\n287.92 371.82 1.78 c p3\n269.46
      372.43 1.78 c p3\n271.14 370.60 1.78 c p3\n276.18 371.21 1.78 c
      p3\n277.85 371.82 1.78 c p3\n267.79 371.21 1.78 c p3\n274.50 372.43
      1.78 c p3\n276.18 371.21 1.78 c p3\n256.04 371.21 1.78 c p3\n271.14
      371.21 1.78 c p3\n276.18 373.04 1.78 c p3\n281.21 374.86 1.78 c
      p3\n267.79 371.82 1.78 c p3\n281.21 373.04 1.78 c p3\n271.14 371.82
      1.78 c p3\n279.53 372.43 1.78 c p3\n272.82 371.82 1.78 c p3\n/bg { 0
      0.8039 0 } def\n271.14 391.88 1.78 c p3\n271.14 390.66 1.78 c
      p3\n269.46 393.10 1.78 c p3\n256.04 387.62 1.78 c p3\n264.43 391.27
      1.78 c p3\n264.43 390.66 1.78 c p3\n272.82 391.88 1.78 c p3\n257.72
      383.37 1.78 c p3\n266.11 391.27 1.78 c p3\n262.75 387.02 1.78 c
      p3\n251.01 384.59 1.78 c p3\n267.79 388.84 1.78 c p3\n254.36 387.62
      1.78 c p3\n266.11 391.88 1.78 c p3\n266.11 385.19 1.78 c p3\n269.46
      390.06 1.78 c p3\n267.79 390.66 1.78 c p3\n262.75 388.23 1.78 c
      p3\n254.36 390.66 1.78 c p3\n259.40 387.02 1.78 c p3\n271.14 392.49
      1.78 c p3\n264.43 387.62 1.78 c p3\n259.40 393.10 1.78 c p3\n264.43
      391.88 1.78 c p3\n266.11 389.45 1.78 c p3\n267.79 390.06 1.78 c
      p3\n264.43 392.49 1.78 c p3\n267.79 393.70 1.78 c p3\n266.11 390.66
      1.78 c p3\n261.08 384.59 1.78 c p3\n257.72 386.41 1.78 c p3\n257.72
      385.80 1.78 c p3\n262.75 387.02 1.78 c p3\n262.75 394.31 1.78 c
      p3\n267.79 390.66 1.78 c p3\n274.50 390.66 1.78 c p3\n269.46 391.88
      1.78 c p3\n256.04 390.06 1.78 c p3\n267.79 388.23 1.78 c p3\n259.40
      387.62 1.78 c p3\n261.08 390.06 1.78 c p3\n267.79 391.27 1.78 c
      p3\n261.08 387.62 1.78 c p3\n256.04 383.37 1.78 c p3\n262.75 388.84
      1.78 c p3\n267.79 388.84 1.78 c p3\n266.11 388.84 1.78 c p3\n266.11
      389.45 1.78 c p3\n259.40 381.55 1.78 c p3\n264.43 388.23 1.78 c p3\n/bg
      { 0 0 1 } def\n272.82 399.78 1.78 c p3\n262.75 394.31 1.78 c p3\n267.79
      399.18 1.78 c p3\n266.11 397.35 1.78 c p3\n267.79 398.57 1.78 c
      p3\n267.79 403.43 1.78 c p3\n259.40 390.66 1.78 c p3\n266.11 401.61
      1.78 c p3\n259.40 398.57 1.78 c p3\n277.85 400.39 1.78 c p3\n271.14
      394.31 1.78 c p3\n262.75 395.53 1.78 c p3\n267.79 396.74 1.78 c
      p3\n259.40 393.70 1.78 c p3\n264.43 394.31 1.78 c p3\n271.14 395.53
      1.78 c p3\n267.79 396.74 1.78 c p3\n281.21 404.04 1.78 c p3\n261.08
      405.25 1.78 c p3\n254.36 393.70 1.78 c p3\n271.14 397.96 1.78 c
      p3\n264.43 393.10 1.78 c p3\n264.43 404.04 1.78 c p3\n262.75 393.10
      1.78 c p3\n272.82 397.96 1.78 c p3\n271.14 399.78 1.78 c p3\n264.43
      392.49 1.78 c p3\n267.79 393.10 1.78 c p3\n264.43 397.35 1.78 c
      p3\n267.79 398.57 1.78 c p3\n264.43 400.39 1.78 c p3\n281.21 402.21
      1.78 c p3\n264.43 397.35 1.78 c p3\n264.43 394.31 1.78 c p3\n261.08
      397.35 1.78 c p3\n267.79 400.39 1.78 c p3\n274.50 397.35 1.78 c
      p3\n269.46 396.74 1.78 c p3\n267.79 392.49 1.78 c p3\n269.46 396.14
      1.78 c p3\n269.46 397.35 1.78 c p3\n269.46 394.31 1.78 c p3\n262.75
      394.31 1.78 c p3\n271.14 399.18 1.78 c p3\n272.82 397.96 1.78 c
      p3\n267.79 394.92 1.78 c p3\n259.40 393.70 1.78 c p3\n267.79 394.92
      1.78 c p3\n274.50 396.14 1.78 c p3\n267.79 394.31 1.78 c p3\n302.39
      367.95 345.88 406.69 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n302.39 367.95 m\n43.49 -0.00
      l\n-0.00 38.74 l\n-43.49 -0.00 l\n-0.00 -38.74 l\no\n302.39 367.95
      345.88 406.69 cl\n/ps 16 def R 16 s\n0 0 0 rgb\n324.13 383.34
      (Petal.Length) .5 0 0 t\n355.38 367.95 398.87 406.69 cl\n153.64 276.94
      441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n355.38 367.95 m\n43.49 -0.00 l\n0.00 38.74 l\n-43.49 -0.00
      l\n-0.00 -38.74 l\no\nnp\n398.87 369.39 m\n0.00 36.47 l\no\nnp\n398.87
      369.39 m\n4.75 -0.00 l\no\nnp\n398.87 375.47 m\n4.75 0.00
      l\no\nnp\n398.87 381.55 m\n4.75 0.00 l\no\nnp\n398.87 387.62 m\n4.75
      0.00 l\no\nnp\n398.87 393.70 m\n4.75 -0.00 l\no\nnp\n398.87 399.78
      m\n4.75 -0.00 l\no\nnp\n398.87 405.86 m\n4.75 0.00 l\no\n/ps 8 def R 8
      s\n415.98 369.39 (1) .5 0 90 t\n415.98 381.55 (3) .5 0 90 t\n415.98
      393.70 (5) .5 0 90 t\n415.98 405.86 (7) .5 0 90 t\n355.38 367.95 398.87
      406.69 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\n358.67 371.82 1.78 c p3\n358.67 371.82 1.78 c p3\n358.67
      371.21 1.78 c p3\n358.67 372.43 1.78 c p3\n358.67 371.82 1.78 c
      p3\n362.03 373.64 1.78 c p3\n360.35 371.82 1.78 c p3\n358.67 372.43
      1.78 c p3\n358.67 371.82 1.78 c p3\n356.99 372.43 1.78 c p3\n358.67
      372.43 1.78 c p3\n358.67 373.04 1.78 c p3\n356.99 371.82 1.78 c
      p3\n356.99 370.00 1.78 c p3\n358.67 370.60 1.78 c p3\n362.03 372.43
      1.78 c p3\n362.03 371.21 1.78 c p3\n360.35 371.82 1.78 c p3\n360.35
      373.64 1.78 c p3\n360.35 372.43 1.78 c p3\n358.67 373.64 1.78 c
      p3\n362.03 372.43 1.78 c p3\n358.67 369.39 1.78 c p3\n363.70 373.64
      1.78 c p3\n358.67 374.86 1.78 c p3\n358.67 373.04 1.78 c p3\n362.03
      373.04 1.78 c p3\n358.67 372.43 1.78 c p3\n358.67 371.82 1.78 c
      p3\n358.67 373.04 1.78 c p3\n358.67 373.04 1.78 c p3\n362.03 372.43
      1.78 c p3\n356.99 372.43 1.78 c p3\n358.67 371.82 1.78 c p3\n358.67
      372.43 1.78 c p3\n358.67 370.60 1.78 c p3\n358.67 371.21 1.78 c
      p3\n356.99 371.82 1.78 c p3\n358.67 371.21 1.78 c p3\n358.67 372.43
      1.78 c p3\n360.35 371.21 1.78 c p3\n360.35 371.21 1.78 c p3\n358.67
      371.21 1.78 c p3\n365.38 373.04 1.78 c p3\n362.03 374.86 1.78 c
      p3\n360.35 371.82 1.78 c p3\n358.67 373.04 1.78 c p3\n358.67 371.82
      1.78 c p3\n358.67 372.43 1.78 c p3\n358.67 371.82 1.78 c p3\n/bg { 0
      0.8039 0 } def\n378.80 391.88 1.78 c p3\n380.48 390.66 1.78 c
      p3\n380.48 393.10 1.78 c p3\n377.13 387.62 1.78 c p3\n380.48 391.27
      1.78 c p3\n377.13 390.66 1.78 c p3\n382.16 391.88 1.78 c p3\n372.09
      383.37 1.78 c p3\n377.13 391.27 1.78 c p3\n378.80 387.02 1.78 c
      p3\n372.09 384.59 1.78 c p3\n380.48 388.84 1.78 c p3\n372.09 387.62
      1.78 c p3\n378.80 391.88 1.78 c p3\n377.13 385.19 1.78 c p3\n378.80
      390.06 1.78 c p3\n380.48 390.66 1.78 c p3\n372.09 388.23 1.78 c
      p3\n380.48 390.66 1.78 c p3\n373.77 387.02 1.78 c p3\n385.51 392.49
      1.78 c p3\n377.13 387.62 1.78 c p3\n380.48 393.10 1.78 c p3\n375.45
      391.88 1.78 c p3\n377.13 389.45 1.78 c p3\n378.80 390.06 1.78 c
      p3\n378.80 392.49 1.78 c p3\n383.84 393.70 1.78 c p3\n380.48 390.66
      1.78 c p3\n372.09 384.59 1.78 c p3\n373.77 386.41 1.78 c p3\n372.09
      385.80 1.78 c p3\n375.45 387.02 1.78 c p3\n382.16 394.31 1.78 c
      p3\n380.48 390.66 1.78 c p3\n382.16 390.66 1.78 c p3\n380.48 391.88
      1.78 c p3\n377.13 390.06 1.78 c p3\n377.13 388.23 1.78 c p3\n377.13
      387.62 1.78 c p3\n375.45 390.06 1.78 c p3\n378.80 391.27 1.78 c
      p3\n375.45 387.62 1.78 c p3\n372.09 383.37 1.78 c p3\n377.13 388.84
      1.78 c p3\n375.45 388.84 1.78 c p3\n377.13 388.84 1.78 c p3\n377.13
      389.45 1.78 c p3\n373.77 381.55 1.78 c p3\n377.13 388.23 1.78 c p3\n/bg
      { 0 0 1 } def\n397.26 399.78 1.78 c p3\n387.19 394.31 1.78 c p3\n390.55
      399.18 1.78 c p3\n385.51 397.35 1.78 c p3\n392.23 398.57 1.78 c
      p3\n390.55 403.43 1.78 c p3\n383.84 390.66 1.78 c p3\n385.51 401.61
      1.78 c p3\n385.51 398.57 1.78 c p3\n397.26 400.39 1.78 c p3\n388.87
      394.31 1.78 c p3\n387.19 395.53 1.78 c p3\n390.55 396.74 1.78 c
      p3\n388.87 393.70 1.78 c p3\n395.58 394.31 1.78 c p3\n393.90 395.53
      1.78 c p3\n385.51 396.74 1.78 c p3\n392.23 404.04 1.78 c p3\n393.90
      405.25 1.78 c p3\n380.48 393.70 1.78 c p3\n393.90 397.96 1.78 c
      p3\n388.87 393.10 1.78 c p3\n388.87 404.04 1.78 c p3\n385.51 393.10
      1.78 c p3\n390.55 397.96 1.78 c p3\n385.51 399.78 1.78 c p3\n385.51
      392.49 1.78 c p3\n385.51 393.10 1.78 c p3\n390.55 397.35 1.78 c
      p3\n382.16 398.57 1.78 c p3\n387.19 400.39 1.78 c p3\n388.87 402.21
      1.78 c p3\n392.23 397.35 1.78 c p3\n380.48 394.31 1.78 c p3\n378.80
      397.35 1.78 c p3\n393.90 400.39 1.78 c p3\n395.58 397.35 1.78 c
      p3\n385.51 396.74 1.78 c p3\n385.51 392.49 1.78 c p3\n390.55 396.14
      1.78 c p3\n395.58 397.35 1.78 c p3\n393.90 394.31 1.78 c p3\n387.19
      394.31 1.78 c p3\n393.90 399.18 1.78 c p3\n397.26 397.96 1.78 c
      p3\n393.90 394.92 1.78 c p3\n387.19 393.70 1.78 c p3\n388.87 394.92
      1.78 c p3\n393.90 396.14 1.78 c p3\n385.51 394.31 1.78 c p3\n196.41
      319.71 239.89 358.45 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n196.41 319.71 m\n43.48 -0.00
      l\n-0.00 38.74 l\n-43.48 -0.00 l\n-0.00 -38.74 l\no\nnp\n200.25 319.71
      m\n39.15 -0.00 l\no\nnp\n200.25 319.71 m\n0 -4.75 l\no\nnp\n205.85
      319.71 m\n-0.00 -4.75 l\no\nnp\n211.44 319.71 m\n-0.00 -4.75
      l\no\nnp\n217.03 319.71 m\n0.00 -4.75 l\no\nnp\n222.62 319.71 m\n0.00
      -4.75 l\no\nnp\n228.22 319.71 m\n-0.00 -4.75 l\no\nnp\n233.81 319.71
      m\n0.00 -4.75 l\no\nnp\n239.40 319.71 m\n0.00 -4.75 l\no\n/ps 8 def R 8
      s\n200.25 302.61 (4.5) .5 0 0 t\n222.62 302.61 (6.5) .5 0 0
      t\nnp\n196.41 327.13 m\n-0.00 29.88 l\no\nnp\n196.41 327.13 m\n-4.76
      -0.00 l\no\nnp\n196.41 334.60 m\n-4.76 0.00 l\no\nnp\n196.41 342.07
      m\n-4.76 -0.00 l\no\nnp\n196.41 349.54 m\n-4.76 0.00 l\no\nnp\n196.41
      357.01 m\n-4.76 -0.00 l\no\n185.00 327.13 (0.5) .5 0 90 t\n185.00
      349.54 (2.0) .5 0 90 t\n196.41 319.71 239.89 358.45 cl\n/bg { 1 0 0 }
      def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n206.96 322.64 1.78 c
      p3\n204.73 322.64 1.78 c p3\n202.49 322.64 1.78 c p3\n201.37 322.64
      1.78 c p3\n205.85 322.64 1.78 c p3\n210.32 325.63 1.78 c p3\n201.37
      324.14 1.78 c p3\n205.85 322.64 1.78 c p3\n199.13 322.64 1.78 c
      p3\n204.73 321.15 1.78 c p3\n210.32 322.64 1.78 c p3\n203.61 322.64
      1.78 c p3\n203.61 321.15 1.78 c p3\n198.02 321.15 1.78 c p3\n214.79
      322.64 1.78 c p3\n213.68 325.63 1.78 c p3\n210.32 325.63 1.78 c
      p3\n206.96 324.14 1.78 c p3\n213.68 324.14 1.78 c p3\n206.96 324.14
      1.78 c p3\n210.32 322.64 1.78 c p3\n206.96 325.63 1.78 c p3\n201.37
      322.64 1.78 c p3\n206.96 327.13 1.78 c p3\n203.61 322.64 1.78 c
      p3\n205.85 322.64 1.78 c p3\n205.85 325.63 1.78 c p3\n208.08 322.64
      1.78 c p3\n208.08 322.64 1.78 c p3\n202.49 322.64 1.78 c p3\n203.61
      322.64 1.78 c p3\n210.32 325.63 1.78 c p3\n208.08 321.15 1.78 c
      p3\n211.44 322.64 1.78 c p3\n204.73 322.64 1.78 c p3\n205.85 322.64
      1.78 c p3\n211.44 322.64 1.78 c p3\n204.73 321.15 1.78 c p3\n199.13
      322.64 1.78 c p3\n206.96 322.64 1.78 c p3\n205.85 324.14 1.78 c
      p3\n200.25 324.14 1.78 c p3\n199.13 322.64 1.78 c p3\n205.85 328.62
      1.78 c p3\n206.96 325.63 1.78 c p3\n203.61 324.14 1.78 c p3\n206.96
      322.64 1.78 c p3\n201.37 322.64 1.78 c p3\n209.20 322.64 1.78 c
      p3\n205.85 322.64 1.78 c p3\n/bg { 0 0.8039 0 } def\n228.22 340.58 1.78
      c p3\n221.51 342.07 1.78 c p3\n227.10 342.07 1.78 c p3\n211.44 339.08
      1.78 c p3\n222.62 342.07 1.78 c p3\n213.68 339.08 1.78 c p3\n220.39
      343.56 1.78 c p3\n204.73 334.60 1.78 c p3\n223.74 339.08 1.78 c
      p3\n208.08 340.58 1.78 c p3\n205.85 334.60 1.78 c p3\n215.91 342.07
      1.78 c p3\n217.03 334.60 1.78 c p3\n218.15 340.58 1.78 c p3\n212.56
      339.08 1.78 c p3\n224.86 340.58 1.78 c p3\n212.56 342.07 1.78 c
      p3\n214.79 334.60 1.78 c p3\n219.27 342.07 1.78 c p3\n212.56 336.09
      1.78 c p3\n215.91 346.55 1.78 c p3\n218.15 339.08 1.78 c p3\n220.39
      342.07 1.78 c p3\n218.15 337.59 1.78 c p3\n221.51 339.08 1.78 c
      p3\n223.74 340.58 1.78 c p3\n225.98 340.58 1.78 c p3\n224.86 345.06
      1.78 c p3\n217.03 342.07 1.78 c p3\n213.68 334.60 1.78 c p3\n211.44
      336.09 1.78 c p3\n211.44 334.60 1.78 c p3\n214.79 337.59 1.78 c
      p3\n217.03 343.56 1.78 c p3\n210.32 342.07 1.78 c p3\n217.03 343.56
      1.78 c p3\n224.86 342.07 1.78 c p3\n220.39 339.08 1.78 c p3\n212.56
      339.08 1.78 c p3\n211.44 339.08 1.78 c p3\n211.44 337.59 1.78 c
      p3\n218.15 340.58 1.78 c p3\n214.79 337.59 1.78 c p3\n205.85 334.60
      1.78 c p3\n212.56 339.08 1.78 c p3\n213.68 337.59 1.78 c p3\n213.68
      339.08 1.78 c p3\n219.27 339.08 1.78 c p3\n206.96 336.09 1.78 c
      p3\n213.68 339.08 1.78 c p3\n/bg { 0 0 1 } def\n220.39 357.01 1.78 c
      p3\n214.79 348.05 1.78 c p3\n229.33 351.04 1.78 c p3\n220.39 346.55
      1.78 c p3\n222.62 352.53 1.78 c p3\n234.93 351.04 1.78 c p3\n204.73
      345.06 1.78 c p3\n231.57 346.55 1.78 c p3\n224.86 346.55 1.78 c
      p3\n230.45 357.01 1.78 c p3\n222.62 349.54 1.78 c p3\n221.51 348.05
      1.78 c p3\n225.98 351.04 1.78 c p3\n213.68 349.54 1.78 c p3\n214.79
      355.52 1.78 c p3\n221.51 354.03 1.78 c p3\n222.62 346.55 1.78 c
      p3\n236.05 352.53 1.78 c p3\n236.05 354.03 1.78 c p3\n217.03 342.07
      1.78 c p3\n227.10 354.03 1.78 c p3\n212.56 349.54 1.78 c p3\n236.05
      349.54 1.78 c p3\n220.39 346.55 1.78 c p3\n224.86 351.04 1.78 c
      p3\n230.45 346.55 1.78 c p3\n219.27 346.55 1.78 c p3\n218.15 346.55
      1.78 c p3\n221.51 351.04 1.78 c p3\n230.45 343.56 1.78 c p3\n232.69
      348.05 1.78 c p3\n238.28 349.54 1.78 c p3\n221.51 352.53 1.78 c
      p3\n220.39 342.07 1.78 c p3\n218.15 340.58 1.78 c p3\n236.05 354.03
      1.78 c p3\n220.39 355.52 1.78 c p3\n221.51 346.55 1.78 c p3\n217.03
      346.55 1.78 c p3\n227.10 351.04 1.78 c p3\n224.86 355.52 1.78 c
      p3\n227.10 354.03 1.78 c p3\n214.79 348.05 1.78 c p3\n225.98 354.03
      1.78 c p3\n224.86 357.01 1.78 c p3\n224.86 354.03 1.78 c p3\n220.39
      348.05 1.78 c p3\n222.62 349.54 1.78 c p3\n219.27 354.03 1.78 c
      p3\n215.91 346.55 1.78 c p3\n249.40 319.71 292.89 358.45 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n249.40 319.71 m\n43.49 -0.00 l\n-0.00 38.74 l\n-43.49
      -0.00 l\n0.00 -38.74 l\no\n249.40 319.71 292.89 358.45 cl\n/bg { 1 0 0
      } def\n0 0 0 rgb\n0.75 setlinewidth\n[] 0 setdash\n276.18 322.64 1.78 c
      p3\n267.79 322.64 1.78 c p3\n271.14 322.64 1.78 c p3\n269.46 322.64
      1.78 c p3\n277.85 322.64 1.78 c p3\n282.89 325.63 1.78 c p3\n274.50
      324.14 1.78 c p3\n274.50 322.64 1.78 c p3\n266.11 322.64 1.78 c
      p3\n269.46 321.15 1.78 c p3\n279.53 322.64 1.78 c p3\n274.50 322.64
      1.78 c p3\n267.79 321.15 1.78 c p3\n267.79 321.15 1.78 c p3\n284.56
      322.64 1.78 c p3\n291.28 325.63 1.78 c p3\n282.89 325.63 1.78 c
      p3\n276.18 324.14 1.78 c p3\n281.21 324.14 1.78 c p3\n281.21 324.14
      1.78 c p3\n274.50 322.64 1.78 c p3\n279.53 325.63 1.78 c p3\n277.85
      322.64 1.78 c p3\n272.82 327.13 1.78 c p3\n274.50 322.64 1.78 c
      p3\n267.79 322.64 1.78 c p3\n274.50 325.63 1.78 c p3\n276.18 322.64
      1.78 c p3\n274.50 322.64 1.78 c p3\n271.14 322.64 1.78 c p3\n269.46
      322.64 1.78 c p3\n274.50 325.63 1.78 c p3\n286.24 321.15 1.78 c
      p3\n287.92 322.64 1.78 c p3\n269.46 322.64 1.78 c p3\n271.14 322.64
      1.78 c p3\n276.18 322.64 1.78 c p3\n277.85 321.15 1.78 c p3\n267.79
      322.64 1.78 c p3\n274.50 322.64 1.78 c p3\n276.18 324.14 1.78 c
      p3\n256.04 324.14 1.78 c p3\n271.14 322.64 1.78 c p3\n276.18 328.62
      1.78 c p3\n281.21 325.63 1.78 c p3\n267.79 324.14 1.78 c p3\n281.21
      322.64 1.78 c p3\n271.14 322.64 1.78 c p3\n279.53 322.64 1.78 c
      p3\n272.82 322.64 1.78 c p3\n/bg { 0 0.8039 0 } def\n271.14 340.58 1.78
      c p3\n271.14 342.07 1.78 c p3\n269.46 342.07 1.78 c p3\n256.04 339.08
      1.78 c p3\n264.43 342.07 1.78 c p3\n264.43 339.08 1.78 c p3\n272.82
      343.56 1.78 c p3\n257.72 334.60 1.78 c p3\n266.11 339.08 1.78 c
      p3\n262.75 340.58 1.78 c p3\n251.01 334.60 1.78 c p3\n267.79 342.07
      1.78 c p3\n254.36 334.60 1.78 c p3\n266.11 340.58 1.78 c p3\n266.11
      339.08 1.78 c p3\n269.46 340.58 1.78 c p3\n267.79 342.07 1.78 c
      p3\n262.75 334.60 1.78 c p3\n254.36 342.07 1.78 c p3\n259.40 336.09
      1.78 c p3\n271.14 346.55 1.78 c p3\n264.43 339.08 1.78 c p3\n259.40
      342.07 1.78 c p3\n264.43 337.59 1.78 c p3\n266.11 339.08 1.78 c
      p3\n267.79 340.58 1.78 c p3\n264.43 340.58 1.78 c p3\n267.79 345.06
      1.78 c p3\n266.11 342.07 1.78 c p3\n261.08 334.60 1.78 c p3\n257.72
      336.09 1.78 c p3\n257.72 334.60 1.78 c p3\n262.75 337.59 1.78 c
      p3\n262.75 343.56 1.78 c p3\n267.79 342.07 1.78 c p3\n274.50 343.56
      1.78 c p3\n269.46 342.07 1.78 c p3\n256.04 339.08 1.78 c p3\n267.79
      339.08 1.78 c p3\n259.40 339.08 1.78 c p3\n261.08 337.59 1.78 c
      p3\n267.79 340.58 1.78 c p3\n261.08 337.59 1.78 c p3\n256.04 334.60
      1.78 c p3\n262.75 339.08 1.78 c p3\n267.79 337.59 1.78 c p3\n266.11
      339.08 1.78 c p3\n266.11 339.08 1.78 c p3\n259.40 336.09 1.78 c
      p3\n264.43 339.08 1.78 c p3\n/bg { 0 0 1 } def\n272.82 357.01 1.78 c
      p3\n262.75 348.05 1.78 c p3\n267.79 351.04 1.78 c p3\n266.11 346.55
      1.78 c p3\n267.79 352.53 1.78 c p3\n267.79 351.04 1.78 c p3\n259.40
      345.06 1.78 c p3\n266.11 346.55 1.78 c p3\n259.40 346.55 1.78 c
      p3\n277.85 357.01 1.78 c p3\n271.14 349.54 1.78 c p3\n262.75 348.05
      1.78 c p3\n267.79 351.04 1.78 c p3\n259.40 349.54 1.78 c p3\n264.43
      355.52 1.78 c p3\n271.14 354.03 1.78 c p3\n267.79 346.55 1.78 c
      p3\n281.21 352.53 1.78 c p3\n261.08 354.03 1.78 c p3\n254.36 342.07
      1.78 c p3\n271.14 354.03 1.78 c p3\n264.43 349.54 1.78 c p3\n264.43
      349.54 1.78 c p3\n262.75 346.55 1.78 c p3\n272.82 351.04 1.78 c
      p3\n271.14 346.55 1.78 c p3\n264.43 346.55 1.78 c p3\n267.79 346.55
      1.78 c p3\n264.43 351.04 1.78 c p3\n267.79 343.56 1.78 c p3\n264.43
      348.05 1.78 c p3\n281.21 349.54 1.78 c p3\n264.43 352.53 1.78 c
      p3\n264.43 342.07 1.78 c p3\n261.08 340.58 1.78 c p3\n267.79 354.03
      1.78 c p3\n274.50 355.52 1.78 c p3\n269.46 346.55 1.78 c p3\n267.79
      346.55 1.78 c p3\n269.46 351.04 1.78 c p3\n269.46 355.52 1.78 c
      p3\n269.46 354.03 1.78 c p3\n262.75 348.05 1.78 c p3\n271.14 354.03
      1.78 c p3\n272.82 357.01 1.78 c p3\n267.79 354.03 1.78 c p3\n259.40
      348.05 1.78 c p3\n267.79 349.54 1.78 c p3\n274.50 354.03 1.78 c
      p3\n267.79 346.55 1.78 c p3\n302.39 319.71 345.88 358.45 cl\n153.64
      276.94 441.64 564.94 cl\n0 0 0 rgb\n0.75 setlinewidth\n[] 0
      setdash\nnp\n302.39 319.71 m\n43.49 -0.00 l\n-0.00 38.74 l\n-43.49
      -0.00 l\n-0.00 -38.74 l\no\nnp\n304.00 319.71 m\n40.95 -0.00
      l\no\nnp\n304.00 319.71 m\n0 -4.75 l\no\nnp\n310.83 319.71 m\n-0.00
      -4.75 l\no\nnp\n317.65 319.71 m\n-0.00 -4.75 l\no\nnp\n324.48 319.71
      m\n0.00 -4.75 l\no\nnp\n331.30 319.71 m\n0.00 -4.75 l\no\nnp\n338.12
      319.71 m\n0.00 -4.75 l\no\nnp\n344.95 319.71 m\n-0.00 -4.75 l\no\n/ps 8
      def R 8 s\n304.00 302.61 (1) .5 0 0 t\n317.65 302.61 (3) .5 0 0
      t\n331.30 302.61 (5) .5 0 0 t\n344.95 302.61 (7) .5 0 0 t\n302.39
      319.71 345.88 358.45 cl\n/bg { 1 0 0 } def\n0 0 0 rgb\n0.75
      setlinewidth\n[] 0 setdash\n306.73 322.64 1.78 c p3\n306.73 322.64 1.78
      c p3\n306.05 322.64 1.78 c p3\n307.41 322.64 1.78 c p3\n306.73 322.64
      1.78 c p3\n308.78 325.63 1.78 c p3\n306.73 324.14 1.78 c p3\n307.41
      322.64 1.78 c p3\n306.73 322.64 1.78 c p3\n307.41 321.15 1.78 c
      p3\n307.41 322.64 1.78 c p3\n308.10 322.64 1.78 c p3\n306.73 321.15
      1.78 c p3\n304.68 321.15 1.78 c p3\n305.37 322.64 1.78 c p3\n307.41
      325.63 1.78 c p3\n306.05 325.63 1.78 c p3\n306.73 324.14 1.78 c
      p3\n308.78 324.14 1.78 c p3\n307.41 324.14 1.78 c p3\n308.78 322.64
      1.78 c p3\n307.41 325.63 1.78 c p3\n304.00 322.64 1.78 c p3\n308.78
      327.13 1.78 c p3\n310.14 322.64 1.78 c p3\n308.10 322.64 1.78 c
      p3\n308.10 325.63 1.78 c p3\n307.41 322.64 1.78 c p3\n306.73 322.64
      1.78 c p3\n308.10 322.64 1.78 c p3\n308.10 322.64 1.78 c p3\n307.41
      325.63 1.78 c p3\n307.41 321.15 1.78 c p3\n306.73 322.64 1.78 c
      p3\n307.41 322.64 1.78 c p3\n305.37 322.64 1.78 c p3\n306.05 322.64
      1.78 c p3\n306.73 321.15 1.78 c p3\n306.05 322.64 1.78 c p3\n307.41
      322.64 1.78 c p3\n306.05 324.14 1.78 c p3\n306.05 324.14 1.78 c
      p3\n306.05 322.64 1.78 c p3\n308.10 328.62 1.78 c p3\n310.14 325.63
      1.78 c p3\n306.73 324.14 1.78 c p3\n308.10 322.64 1.78 c p3\n306.73
      322.64 1.78 c p3\n307.41 322.64 1.78 c p3\n306.73 322.64 1.78 c p3\n/bg
      { 0 0.8039 0 } def\n329.25 340.58 1.78 c p3\n327.89 342.07 1.78 c
      p3\n330.62 342.07 1.78 c p3\n324.48 339.08 1.78 c p3\n328.57 342.07
      1.78 c p3\n327.89 339.08 1.78 c p3\n329.25 343.56 1.78 c p3\n319.70
      334.60 1.78 c p3\n328.57 339.08 1.78 c p3\n323.79 340.58 1.78 c
      p3\n321.06 334.60 1.78 c p3\n325.84 342.07 1.78 c p3\n324.48 334.60
      1.78 c p3\n329.25 340.58 1.78 c p3\n321.75 339.08 1.78 c p3\n327.20
      340.58 1.78 c p3\n327.89 342.07 1.78 c p3\n325.16 334.60 1.78 c
      p3\n327.89 342.07 1.78 c p3\n323.79 336.09 1.78 c p3\n329.93 346.55
      1.78 c p3\n324.48 339.08 1.78 c p3\n330.62 342.07 1.78 c p3\n329.25
      337.59 1.78 c p3\n326.52 339.08 1.78 c p3\n327.20 340.58 1.78 c
      p3\n329.93 340.58 1.78 c p3\n331.30 345.06 1.78 c p3\n327.89 342.07
      1.78 c p3\n321.06 334.60 1.78 c p3\n323.11 336.09 1.78 c p3\n322.43
      334.60 1.78 c p3\n323.79 337.59 1.78 c p3\n331.98 343.56 1.78 c
      p3\n327.89 342.07 1.78 c p3\n327.89 343.56 1.78 c p3\n329.25 342.07
      1.78 c p3\n327.20 339.08 1.78 c p3\n325.16 339.08 1.78 c p3\n324.48
      339.08 1.78 c p3\n327.20 337.59 1.78 c p3\n328.57 340.58 1.78 c
      p3\n324.48 337.59 1.78 c p3\n319.70 334.60 1.78 c p3\n325.84 339.08
      1.78 c p3\n325.84 337.59 1.78 c p3\n325.84 339.08 1.78 c p3\n326.52
      339.08 1.78 c p3\n317.65 336.09 1.78 c p3\n325.16 339.08 1.78 c p3\n/bg
      { 0 0 1 } def\n338.12 357.01 1.78 c p3\n331.98 348.05 1.78 c p3\n337.44
      351.04 1.78 c p3\n335.39 346.55 1.78 c p3\n336.76 352.53 1.78 c
      p3\n342.22 351.04 1.78 c p3\n327.89 345.06 1.78 c p3\n340.17 346.55
      1.78 c p3\n336.76 346.55 1.78 c p3\n338.81 357.01 1.78 c p3\n331.98
      349.54 1.78 c p3\n333.35 348.05 1.78 c p3\n334.71 351.04 1.78 c
      p3\n331.30 349.54 1.78 c p3\n331.98 355.52 1.78 c p3\n333.35 354.03
      1.78 c p3\n334.71 346.55 1.78 c p3\n342.90 352.53 1.78 c p3\n344.27
      354.03 1.78 c p3\n331.30 342.07 1.78 c p3\n336.08 354.03 1.78 c
      p3\n330.62 349.54 1.78 c p3\n342.90 349.54 1.78 c p3\n330.62 346.55
      1.78 c p3\n336.08 351.04 1.78 c p3\n338.12 346.55 1.78 c p3\n329.93
      346.55 1.78 c p3\n330.62 346.55 1.78 c p3\n335.39 351.04 1.78 c
      p3\n336.76 343.56 1.78 c p3\n338.81 348.05 1.78 c p3\n340.85 349.54
      1.78 c p3\n335.39 352.53 1.78 c p3\n331.98 342.07 1.78 c p3\n335.39
      340.58 1.78 c p3\n338.81 354.03 1.78 c p3\n335.39 355.52 1.78 c
      p3\n334.71 346.55 1.78 c p3\n329.93 346.55 1.78 c p3\n334.03 351.04
      1.78 c p3\n335.39 355.52 1.78 c p3\n331.98 354.03 1.78 c p3\n331.98
      348.05 1.78 c p3\n337.44 354.03 1.78 c p3\n336.08 357.01 1.78 c
      p3\n332.66 354.03 1.78 c p3\n331.30 348.05 1.78 c p3\n332.66 349.54
      1.78 c p3\n334.03 354.03 1.78 c p3\n331.98 346.55 1.78 c p3\n355.38
      319.71 398.87 358.45 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n355.38 319.71 m\n43.49 -0.00
      l\n0.00 38.74 l\n-43.49 -0.00 l\n-0.00 -38.74 l\no\n355.38 319.71
      398.87 358.45 cl\n/ps 16 def R 16 s\n0 0 0 rgb\n377.13 333.46
      (Petal.Width) .5 0 0 t\n153.64 276.94 441.64 564.94 cl\n/ps 14 def B 14
      s\n0 0 0 rgb\n297.64 538.33 (Edgar Anderson's Iris Data) .5 0 0
      t\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      data("volcano");

      x \<less\>- 10 * 1:nrow(volcano);

      y \<less\>- 10 * 1:ncol(volcano);

      l \<less\>- pretty(range(volcano), 10);

      par(bg = "lightcyan");

      pin \<less\>- par("pin");

      xdelta \<less\>- diff(range(x));

      ydelta \<less\>- diff(range(y));

      xscale \<less\>- pin[1]/xdelta;

      yscale \<less\>- pin[2]/ydelta;

      scale \<less\>- if (xscale \<less\> yscale) xscale else yscale;

      xadd \<less\>- 0.5 * (pin[1]/scale - xdelta);

      yadd \<less\>- 0.5 * (pin[2]/scale - ydelta);

      plot(numeric(0), numeric(0), xlim = range(x) + c(-1,

      \ \ \ \ 1) * xadd, ylim = range(y) + c(-1, 1) * yadd, type = "n",

      \ \ \ \ ann = FALSE);

      v();
    </input>

    <\output>
      \ \ 
    </output>

    <\input| <\with|color|red>
      Hit \<less\>Return\<gtr\> to see next plot: <with|color|black|>
    </with> >
      \;
    </input>

    <\output>
      \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-3.0
      EPSF-3.0\n%%DocumentNeededResources: font Helvetica\n%%+ font
      Helvetica-Bold\n%%+ font Helvetica-Oblique\n%%+ font
      Helvetica-BoldOblique\n%%+ font Symbol\n%%Title: R Graphics
      Output\n%%Creator: R Software\n%%Pages: (atend)\n%%BoundingBox: 154 277
      442 565\n%%EndComments\n%%BeginProlog\n/bp \ { gs gs } def\n% begin
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
      2 2\nbp\n/bg { 0.8784 1 1 } def\n0.00 0.00 595.28 841.89 r p2\n212.68
      350.38 411.40 505.90 cl\n153.64 276.94 441.64 564.94 cl\n0 0 0
      rgb\n0.75 setlinewidth\n[] 0 setdash\nnp\n217.90 350.38 m\n171.16 -0.00
      l\no\nnp\n217.90 350.38 m\n0.00 -7.20 l\no\nnp\n260.69 350.38 m\n-0.00
      -7.20 l\no\nnp\n303.48 350.38 m\n0.00 -7.20 l\no\nnp\n346.27 350.38
      m\n-0.00 -7.20 l\no\nnp\n389.06 350.38 m\n0.00 -7.20 l\no\n/ps 12 def R
      12 s\n217.90 324.46 (0) .5 0 0 t\n260.69 324.46 (200) .5 0 0 t\n303.48
      324.46 (400) .5 0 0 t\n346.27 324.46 (600) .5 0 0 t\n389.06 324.46
      (800) .5 0 0 t\nnp\n212.68 371.87 m\n0.00 108.92 l\no\nnp\n212.68
      371.87 m\n-7.20 0.00 l\no\nnp\n212.68 408.18 m\n-7.20 0.00
      l\no\nnp\n212.68 444.48 m\n-7.20 0.00 l\no\nnp\n212.68 480.79 m\n-7.20
      0.00 l\no\n195.40 371.87 (0) .5 0 90 t\n195.40 408.18 (200) .5 0 90
      t\n195.40 444.48 (400) .5 0 90 t\n195.40 480.79 (600) .5 0 90
      t\nnp\n212.68 350.38 m\n198.72 -0.00 l\n-0.00 155.52 l\n-198.72 -0.00
      l\n0.00 -155.52 l\no\nep\n%%Trailer\n%%Pages: 2\n%%EOF\n>|ps>||||||>

      \;
    </output>

    <\input| <\with|color|red>
      \<gtr\> <with|color|black|>
    </with> >
      \;
    </input>
  </session>>

  \;
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
