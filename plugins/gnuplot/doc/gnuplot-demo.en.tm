<TeXmacs|1.0.1.20>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\expand|tmdoc-title>
    Using Gnuplot sessions inside <TeXmacs>
  </expand>

  <name|Gnuplot> is a flexible and powerful application for drawing graphs of
  functions, which can be downloaded from

  <\verbatim>
    \ \ \ \ http://gnuplot.sf.net
  </verbatim>

  \ Press <apply|menu|Insert|Session|Gnuplot> in order to invoke
  <name|Gnuplot>. Here follows an example session:

  <\session|gnuplot|default>
    <\output>
      This is a TeXmacs interface for GNUplot.

      \;
    </output>

    <\input|GNUplot] >
      plot [-10:10][-10:10] x+sin(x)
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-2.0 EPSF-2.0\n%%Title:
      temp.eps\n%%Creator: gnuplot 3.8h patchlevel 0\n%%CreationDate: Thu Jul
      10 07:01:04 2003\n%%DocumentFonts: (atend)\n%%BoundingBox: 50 50 410
      302\n%%Orientation: Portrait\n%%EndComments\n/gnudict 256 dict
      def\ngnudict begin\n/Color false def\n/Solid false def\n/gnulinewidth
      5.000 def\n/userlinewidth gnulinewidth def\n/vshift -46 def\n/dl {10.0
      mul} def\n/hpt_ 31.5 def\n/vpt_ 31.5 def\n/hpt hpt_ def\n/vpt vpt_
      def\n/M {moveto} bind def\n/L {lineto} bind def\n/R {rmoveto} bind
      def\n/V {rlineto} bind def\n/N {newpath moveto} bind def\n/f {rlineto
      fill} bind def\n/vpt2 vpt 2 mul def\n/hpt2 hpt 2 mul def\n/Lshow {
      currentpoint stroke M\n \ 0 vshift R show } def\n/Rshow { currentpoint
      stroke M\n \ dup stringwidth pop neg vshift R show } def\n/Cshow {
      currentpoint stroke M\n \ dup stringwidth pop -2 div vshift R show }
      def\n/UP { dup vpt_ mul /vpt exch def hpt_ mul /hpt exch def\n \ /hpt2
      hpt 2 mul def /vpt2 vpt 2 mul def } def\n/DL { Color {setrgbcolor Solid
      {pop []} if 0 setdash }\n {pop pop pop Solid {pop []} if 0 setdash}
      ifelse } def\n/BL { stroke userlinewidth 2 mul setlinewidth } def\n/AL
      { stroke userlinewidth 2 div setlinewidth } def\n/UL { dup gnulinewidth
      mul /userlinewidth exch def\n \ \ \ \ \ dup 1 lt {pop 1} if 10 mul /udl
      exch def } def\n/PL { stroke userlinewidth setlinewidth } def\n/LTb {
      BL [] 0 0 0 DL } def\n/LTa { AL [1 udl mul 2 udl mul] 0 setdash 0 0 0
      setrgbcolor } def\n/LT0 { PL [] 1 0 0 DL } def\n/LT1 { PL [4 dl 2 dl] 0
      1 0 DL } def\n/LT2 { PL [2 dl 3 dl] 0 0 1 DL } def\n/LT3 { PL [1 dl 1.5
      dl] 1 0 1 DL } def\n/LT4 { PL [5 dl 2 dl 1 dl 2 dl] 0 1 1 DL }
      def\n/LT5 { PL [4 dl 3 dl 1 dl 3 dl] 1 1 0 DL } def\n/LT6 { PL [2 dl 2
      dl 2 dl 4 dl] 0 0 0 DL } def\n/LT7 { PL [2 dl 2 dl 2 dl 2 dl 2 dl 4 dl]
      1 0.3 0 DL } def\n/LT8 { PL [2 dl 2 dl 2 dl 2 dl 2 dl 2 dl 2 dl 4 dl]
      0.5 0.5 0.5 DL } def\n/Pnt { stroke [] 0 setdash\n \ \ gsave 1
      setlinecap M 0 0 V stroke grestore } def\n/Dia { stroke [] 0 setdash 2
      copy vpt add M\n \ hpt neg vpt neg V hpt vpt neg V\n \ hpt vpt V hpt
      neg vpt V closepath stroke\n \ Pnt } def\n/Pls { stroke [] 0 setdash
      vpt sub M 0 vpt2 V\n \ currentpoint stroke M\n \ hpt neg vpt neg R hpt2
      0 V stroke\n \ } def\n/Box { stroke [] 0 setdash 2 copy exch hpt sub
      exch vpt add M\n \ 0 vpt2 neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V
      closepath stroke\n \ Pnt } def\n/Crs { stroke [] 0 setdash exch hpt sub
      exch vpt add M\n \ hpt2 vpt2 neg V currentpoint stroke M\n \ hpt2 neg 0
      R hpt2 vpt2 V stroke } def\n/TriU { stroke [] 0 setdash 2 copy vpt 1.12
      mul add M\n \ hpt neg vpt -1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt
      1.62 mul V closepath stroke\n \ Pnt \ } def\n/Star { 2 copy Pls Crs }
      def\n/BoxF { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V \ hpt2 0 V \ 0 vpt2 V\n \ hpt2 neg 0 V \ closepath fill }
      def\n/TriUF { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt
      -1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath fill
      } def\n/TriD { stroke [] 0 setdash 2 copy vpt 1.12 mul sub M\n \ hpt
      neg vpt 1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V
      closepath stroke\n \ Pnt \ } def\n/TriDF { stroke [] 0 setdash vpt 1.12
      mul sub M\n \ hpt neg vpt 1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt
      -1.62 mul V closepath fill} def\n/DiaF { stroke [] 0 setdash vpt add
      M\n \ hpt neg vpt neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V
      closepath fill } def\n/Pent { stroke [] 0 setdash 2 copy gsave\n
      \ translate 0 hpt M 4 {72 rotate 0 hpt L} repeat\n \ closepath stroke
      grestore Pnt } def\n/PentF { stroke [] 0 setdash gsave\n \ translate 0
      hpt M 4 {72 rotate 0 hpt L} repeat\n \ closepath fill grestore }
      def\n/Circle { stroke [] 0 setdash 2 copy\n \ hpt 0 360 arc stroke Pnt
      } def\n/CircleF { stroke [] 0 setdash hpt 0 360 arc fill } def\n/C0 {
      BL [] 0 setdash 2 copy moveto vpt 90 450 \ arc } bind def\n/C1 { BL []
      0 setdash 2 copy \ \ \ \ \ \ \ moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 90
      arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C2 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 90 180 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C3 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 180 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath }
      bind def\n/C4 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy
      \ vpt 180 270 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
      360 arc closepath } bind def\n/C5 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 0 90 arc\n \ \ \ \ \ \ 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 180 270 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind def\n/C6 { BL [] 0
      setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 90 270 arc closepath
      fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind
      def\n/C7 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 0 270
      arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath
      } bind def\n/C8 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2 copy vpt
      270 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C9 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2
      copy \ vpt 270 450 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
      360 arc closepath } bind def\n/C10 { BL [] 0 setdash 2 copy 2 copy
      moveto vpt 270 360 arc closepath fill\n \ \ \ \ \ \ 2 copy moveto\n
      \ \ \ \ \ \ 2 copy vpt 90 180 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C11 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 180 arc
      closepath fill\n \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt
      270 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C12 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 180 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C13 {
      BL [] 0 setdash \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 90 arc
      closepath fill\n \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt
      180 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C14 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 90 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind def\n/C15 { BL [] 0
      setdash 2 copy vpt 0 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/Rec
      \ \ { newpath 4 2 roll moveto 1 index 0 rlineto 0 exch rlineto\n
      \ \ \ \ \ \ neg 0 rlineto closepath } bind def\n/Square { dup Rec }
      bind def\n/Bsquare { vpt sub exch vpt sub exch vpt2 Square } bind
      def\n/S0 { BL [] 0 setdash 2 copy moveto 0 vpt rlineto BL Bsquare }
      bind def\n/S1 { BL [] 0 setdash 2 copy vpt Square fill Bsquare } bind
      def\n/S2 { BL [] 0 setdash 2 copy exch vpt sub exch vpt Square fill
      Bsquare } bind def\n/S3 { BL [] 0 setdash 2 copy exch vpt sub exch vpt2
      vpt Rec fill Bsquare } bind def\n/S4 { BL [] 0 setdash 2 copy exch vpt
      sub exch vpt sub vpt Square fill Bsquare } bind def\n/S5 { BL [] 0
      setdash 2 copy 2 copy vpt Square fill\n \ \ \ \ \ \ exch vpt sub exch
      vpt sub vpt Square fill Bsquare } bind def\n/S6 { BL [] 0 setdash 2
      copy exch vpt sub exch vpt sub vpt vpt2 Rec fill Bsquare } bind
      def\n/S7 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt vpt2
      Rec fill\n \ \ \ \ \ \ 2 copy vpt Square fill\n \ \ \ \ \ \ Bsquare }
      bind def\n/S8 { BL [] 0 setdash 2 copy vpt sub vpt Square fill Bsquare
      } bind def\n/S9 { BL [] 0 setdash 2 copy vpt sub vpt vpt2 Rec fill
      Bsquare } bind def\n/S10 { BL [] 0 setdash 2 copy vpt sub vpt Square
      fill 2 copy exch vpt sub exch vpt Square fill\n \ \ \ \ \ \ Bsquare }
      bind def\n/S11 { BL [] 0 setdash 2 copy vpt sub vpt Square fill 2 copy
      exch vpt sub exch vpt2 vpt Rec fill\n \ \ \ \ \ \ Bsquare } bind
      def\n/S12 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt2 vpt
      Rec fill Bsquare } bind def\n/S13 { BL [] 0 setdash 2 copy exch vpt sub
      exch vpt sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2 copy vpt Square fill
      Bsquare } bind def\n/S14 { BL [] 0 setdash 2 copy exch vpt sub exch vpt
      sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2 copy exch vpt sub exch vpt Square
      fill Bsquare } bind def\n/S15 { BL [] 0 setdash 2 copy Bsquare fill
      Bsquare } bind def\n/D0 { gsave translate 45 rotate 0 0 S0 stroke
      grestore } bind def\n/D1 { gsave translate 45 rotate 0 0 S1 stroke
      grestore } bind def\n/D2 { gsave translate 45 rotate 0 0 S2 stroke
      grestore } bind def\n/D3 { gsave translate 45 rotate 0 0 S3 stroke
      grestore } bind def\n/D4 { gsave translate 45 rotate 0 0 S4 stroke
      grestore } bind def\n/D5 { gsave translate 45 rotate 0 0 S5 stroke
      grestore } bind def\n/D6 { gsave translate 45 rotate 0 0 S6 stroke
      grestore } bind def\n/D7 { gsave translate 45 rotate 0 0 S7 stroke
      grestore } bind def\n/D8 { gsave translate 45 rotate 0 0 S8 stroke
      grestore } bind def\n/D9 { gsave translate 45 rotate 0 0 S9 stroke
      grestore } bind def\n/D10 { gsave translate 45 rotate 0 0 S10 stroke
      grestore } bind def\n/D11 { gsave translate 45 rotate 0 0 S11 stroke
      grestore } bind def\n/D12 { gsave translate 45 rotate 0 0 S12 stroke
      grestore } bind def\n/D13 { gsave translate 45 rotate 0 0 S13 stroke
      grestore } bind def\n/D14 { gsave translate 45 rotate 0 0 S14 stroke
      grestore } bind def\n/D15 { gsave translate 45 rotate 0 0 S15 stroke
      grestore } bind def\n/DiaE { stroke [] 0 setdash vpt add M\n \ hpt neg
      vpt neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V closepath stroke }
      def\n/BoxE { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V closepath stroke } def\n/TriUE
      { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath stroke }
      def\n/TriDE { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt
      1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath
      stroke } def\n/PentE { stroke [] 0 setdash gsave\n \ translate 0 hpt M
      4 {72 rotate 0 hpt L} repeat\n \ closepath stroke grestore }
      def\n/CircE { stroke [] 0 setdash \n \ hpt 0 360 arc stroke }
      def\n/Opaque { gsave closepath 1 setgray fill grestore 0 setgray
      closepath } def\n/DiaW { stroke [] 0 setdash vpt add M\n \ hpt neg vpt
      neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V Opaque stroke }
      def\n/BoxW { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V Opaque stroke } def\n/TriUW {
      stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V Opaque stroke } def\n/TriDW
      { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt 1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V Opaque stroke } def\n/PentW
      { stroke [] 0 setdash gsave\n \ translate 0 hpt M 4 {72 rotate 0 hpt L}
      repeat\n \ Opaque stroke grestore } def\n/CircW { stroke [] 0 setdash
      \n \ hpt 0 360 arc Opaque stroke } def\n/BoxFill { gsave Rec 1 setgray
      fill grestore } def\n/Symbol-Oblique /Symbol findfont [1 0 .167 1 0 0]
      makefont\ndup length dict begin {1 index /FID eq {pop pop} {def}
      ifelse} forall\ncurrentdict end definefont pop\n/MFshow {{dup dup 0 get
      findfont exch 1 get scalefont setfont\n \ \ \ \ [ currentpoint ] exch
      dup 2 get 0 exch R dup 5 get 2 ne {dup dup 6\n \ \ \ \ get exch 4 get
      {show} {stringwidth pop 0 R} ifelse }if dup 5 get 0 eq\n \ \ \ \ {dup 3
      get {2 get neg 0 exch R pop} {pop aload pop M} ifelse} {dup 5\n
      \ \ \ \ get 1 eq {dup 2 get exch dup 3 get exch 6 get stringwidth pop
      -2 div\n \ \ \ \ dup 0 R} {dup 6 get stringwidth pop -2 div 0 R 6 get\n
      \ \ \ \ show 2 index {aload pop M neg 3 -1 roll neg R pop pop} {pop pop
      pop\n \ \ \ \ pop aload pop M} ifelse }ifelse }ifelse } forall} bind
      def\n/MFwidth {0 exch {dup 3 get{dup dup 0 get findfont exch 1 get
      scalefont\n \ \ \ \ setfont 6 get stringwidth pop add} {pop} ifelse}
      forall} bind def\n/MLshow { currentpoint stroke M\n \ 0 exch R MFshow }
      bind def\n/MRshow { currentpoint stroke M\n \ exch dup MFwidth neg 3 -1
      roll R MFshow } def\n/MCshow { currentpoint stroke M\n \ exch dup
      MFwidth -2 div 3 -1 roll R MFshow } def\nend\n%%EndProlog\ngnudict
      begin\ngsave\n50 50 translate\n0.050 0.050 scale\n0
      setgray\nnewpath\n(Helvetica) findfont 140 scalefont setfont\n1.000
      UL\nLTb\n490 280 M\n63 0 V\n6409 0 R\n-63 0 V\n stroke\n406 280
      M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0 true true 0 (-10)]\n]
      -46.7 MRshow\ngrestore\n490 1428 M\n63 0 V\n6409 0 R\n-63 0 V\n
      stroke\n406 1428 M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0 true true
      0 (-5)]\n] -46.7 MRshow\ngrestore\n490 2576 M\n63 0 V\n6409 0 R\n-63 0
      V\n stroke\n406 2576 M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0 true
      true 0 ( 0)]\n] -46.7 MRshow\ngrestore\n490 3724 M\n63 0 V\n6409 0
      R\n-63 0 V\n stroke\n406 3724 M\ngsave 0 setgray\n[ [(Helvetica) 140.0
      0.0 true true 0 ( 5)]\n] -46.7 MRshow\ngrestore\n490 4872 M\n63 0
      V\n6409 0 R\n-63 0 V\n stroke\n406 4872 M\ngsave 0 setgray\n[
      [(Helvetica) 140.0 0.0 true true 0 ( 10)]\n] -46.7
      MRshow\ngrestore\n490 280 M\n0 63 V\n0 4529 R\n0 -63 V\n stroke\n490
      140 M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0 true true 0 (-10)]\n]
      -46.7 MCshow\ngrestore\n2108 280 M\n0 63 V\n0 4529 R\n0 -63 V\n
      stroke\n2108 140 M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0 true true
      0 (-5)]\n] -46.7 MCshow\ngrestore\n3726 280 M\n0 63 V\n0 4529 R\n0 -63
      V\n stroke\n3726 140 M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0 true
      true 0 ( 0)]\n] -46.7 MCshow\ngrestore\n5344 280 M\n0 63 V\n0 4529 R\n0
      -63 V\n stroke\n5344 140 M\ngsave 0 setgray\n[ [(Helvetica) 140.0 0.0
      true true 0 ( 5)]\n] -46.7 MCshow\ngrestore\n6962 280 M\n0 63 V\n0 4529
      R\n0 -63 V\n stroke\n6962 140 M\ngsave 0 setgray\n[ [(Helvetica) 140.0
      0.0 true true 0 ( 10)]\n] -46.7 MCshow\ngrestore\n1.000 UL\nLTb\n490
      280 M\n6472 0 V\n0 4592 V\n-6472 0 V\n490 280 L\n1.000 UP\n1.000
      UL\nLT0\n6311 4739 M\ngsave 0 setgray\n(x+sin\\(x\\))
      Rshow\ngrestore\n6395 4739 M\n399 0 V\n490 405 M\n65 5 V\n66 2 V\n65 0
      V\n65 1 V\n66 2 V\n65 7 V\n66 12 V\n65 19 V\n65 27 V\n66 36 V\n65 45
      V\n65 55 V\n66 63 V\n65 72 V\n66 79 V\n65 85 V\n65 90 V\n66 91 V\n65 93
      V\n65 92 V\n66 88 V\n65 84 V\n66 78 V\n65 70 V\n65 62 V\n66 52 V\n65 44
      V\n65 34 V\n66 25 V\n65 18 V\n66 11 V\n65 5 V\n65 2 V\n66 1 V\n65 0
      V\n65 2 V\n66 6 V\n65 12 V\n66 18 V\n65 27 V\n65 35 V\n66 44 V\n65 53
      V\n65 63 V\n66 71 V\n65 79 V\n66 84 V\n65 89 V\n65 92 V\n66 92 V\n65 92
      V\n65 89 V\n66 84 V\n65 79 V\n66 71 V\n65 63 V\n65 53 V\n66 44 V\n65 35
      V\n65 27 V\n66 18 V\n65 12 V\n66 6 V\n65 2 V\n65 0 V\n66 1 V\n65 2
      V\n65 5 V\n66 11 V\n65 18 V\n66 25 V\n65 34 V\n65 44 V\n66 52 V\n65 62
      V\n65 70 V\n66 78 V\n65 84 V\n66 88 V\n65 92 V\n65 93 V\n66 91 V\n65 90
      V\n65 85 V\n66 79 V\n65 72 V\n66 63 V\n65 55 V\n65 45 V\n66 36 V\n65 27
      V\n65 19 V\n66 12 V\n65 7 V\n66 2 V\n65 1 V\n65 0 V\n66 2 V\n65 5
      V\n1.000 UP\nstroke\ngrestore\nend\nshowpage\n%%Trailer\n%%DocumentFonts:
      Helvetica\n>|ps>||||||>

      \;
    </output>

    <\input|GNUplot] >
      set noclip

      set yrange[-30:10]

      plot x*(abs((x-4)/(x+4)))**(1./2.)
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-2.0 EPSF-2.0\n%%Title:
      temp.eps\n%%Creator: gnuplot 3.7 patchlevel 2\n%%CreationDate: Fri Jul
      25 16:51:11 2003\n%%DocumentFonts: (atend)\n%%BoundingBox: 50 50 410
      302\n%%Orientation: Portrait\n%%EndComments\n/gnudict 256 dict
      def\ngnudict begin\n/Color false def\n/Solid false def\n/gnulinewidth
      5.000 def\n/userlinewidth gnulinewidth def\n/vshift -46 def\n/dl {10
      mul} def\n/hpt_ 31.5 def\n/vpt_ 31.5 def\n/hpt hpt_ def\n/vpt vpt_
      def\n/M {moveto} bind def\n/L {lineto} bind def\n/R {rmoveto} bind
      def\n/V {rlineto} bind def\n/vpt2 vpt 2 mul def\n/hpt2 hpt 2 mul
      def\n/Lshow { currentpoint stroke M\n \ 0 vshift R show } def\n/Rshow {
      currentpoint stroke M\n \ dup stringwidth pop neg vshift R show }
      def\n/Cshow { currentpoint stroke M\n \ dup stringwidth pop -2 div
      vshift R show } def\n/UP { dup vpt_ mul /vpt exch def hpt_ mul /hpt
      exch def\n \ /hpt2 hpt 2 mul def /vpt2 vpt 2 mul def } def\n/DL { Color
      {setrgbcolor Solid {pop []} if 0 setdash }\n {pop pop pop Solid {pop
      []} if 0 setdash} ifelse } def\n/BL { stroke userlinewidth 2 mul
      setlinewidth } def\n/AL { stroke userlinewidth 2 div setlinewidth }
      def\n/UL { dup gnulinewidth mul /userlinewidth exch def\n \ \ \ \ \ dup
      1 lt {pop 1} if 10 mul /udl exch def } def\n/PL { stroke userlinewidth
      setlinewidth } def\n/LTb { BL [] 0 0 0 DL } def\n/LTa { AL [1 udl mul 2
      udl mul] 0 setdash 0 0 0 setrgbcolor } def\n/LT0 { PL [] 1 0 0 DL }
      def\n/LT1 { PL [4 dl 2 dl] 0 1 0 DL } def\n/LT2 { PL [2 dl 3 dl] 0 0 1
      DL } def\n/LT3 { PL [1 dl 1.5 dl] 1 0 1 DL } def\n/LT4 { PL [5 dl 2 dl
      1 dl 2 dl] 0 1 1 DL } def\n/LT5 { PL [4 dl 3 dl 1 dl 3 dl] 1 1 0 DL }
      def\n/LT6 { PL [2 dl 2 dl 2 dl 4 dl] 0 0 0 DL } def\n/LT7 { PL [2 dl 2
      dl 2 dl 2 dl 2 dl 4 dl] 1 0.3 0 DL } def\n/LT8 { PL [2 dl 2 dl 2 dl 2
      dl 2 dl 2 dl 2 dl 4 dl] 0.5 0.5 0.5 DL } def\n/Pnt { stroke [] 0
      setdash\n \ \ gsave 1 setlinecap M 0 0 V stroke grestore } def\n/Dia {
      stroke [] 0 setdash 2 copy vpt add M\n \ hpt neg vpt neg V hpt vpt neg
      V\n \ hpt vpt V hpt neg vpt V closepath stroke\n \ Pnt } def\n/Pls {
      stroke [] 0 setdash vpt sub M 0 vpt2 V\n \ currentpoint stroke M\n
      \ hpt neg vpt neg R hpt2 0 V stroke\n \ } def\n/Box { stroke [] 0
      setdash 2 copy exch hpt sub exch vpt add M\n \ 0 vpt2 neg V hpt2 0 V 0
      vpt2 V\n \ hpt2 neg 0 V closepath stroke\n \ Pnt } def\n/Crs { stroke
      [] 0 setdash exch hpt sub exch vpt add M\n \ hpt2 vpt2 neg V
      currentpoint stroke M\n \ hpt2 neg 0 R hpt2 vpt2 V stroke } def\n/TriU
      { stroke [] 0 setdash 2 copy vpt 1.12 mul add M\n \ hpt neg vpt -1.62
      mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath stroke\n
      \ Pnt \ } def\n/Star { 2 copy Pls Crs } def\n/BoxF { stroke [] 0
      setdash exch hpt sub exch vpt add M\n \ 0 vpt2 neg V \ hpt2 0 V \ 0
      vpt2 V\n \ hpt2 neg 0 V \ closepath fill } def\n/TriUF { stroke [] 0
      setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n \ hpt 2 mul 0
      V\n \ hpt neg vpt 1.62 mul V closepath fill } def\n/TriD { stroke [] 0
      setdash 2 copy vpt 1.12 mul sub M\n \ hpt neg vpt 1.62 mul V\n \ hpt 2
      mul 0 V\n \ hpt neg vpt -1.62 mul V closepath stroke\n \ Pnt \ }
      def\n/TriDF { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt
      1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath
      fill} def\n/DiaF { stroke [] 0 setdash vpt add M\n \ hpt neg vpt neg V
      hpt vpt neg V\n \ hpt vpt V hpt neg vpt V closepath fill } def\n/Pent {
      stroke [] 0 setdash 2 copy gsave\n \ translate 0 hpt M 4 {72 rotate 0
      hpt L} repeat\n \ closepath stroke grestore Pnt } def\n/PentF { stroke
      [] 0 setdash gsave\n \ translate 0 hpt M 4 {72 rotate 0 hpt L} repeat\n
      \ closepath fill grestore } def\n/Circle { stroke [] 0 setdash 2 copy\n
      \ hpt 0 360 arc stroke Pnt } def\n/CircleF { stroke [] 0 setdash hpt 0
      360 arc fill } def\n/C0 { BL [] 0 setdash 2 copy moveto vpt 90 450
      \ arc } bind def\n/C1 { BL [] 0 setdash 2 copy \ \ \ \ \ \ \ moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 0 90 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C2 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 90 180 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath }
      bind def\n/C3 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy
      \ vpt 0 180 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360
      arc closepath } bind def\n/C4 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 180 270 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C5 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 90 arc\n
      \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 180 270 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind
      def\n/C6 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 90
      270 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C7 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2
      copy \ vpt 0 270 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
      360 arc closepath } bind def\n/C8 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ 2 copy vpt 270 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C9 { BL
      [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 270 450 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath }
      bind def\n/C10 { BL [] 0 setdash 2 copy 2 copy moveto vpt 270 360 arc
      closepath fill\n \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy vpt 90
      180 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C11 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 0 180 arc closepath fill\n \ \ \ \ \ \ 2 copy
      moveto\n \ \ \ \ \ \ 2 copy \ vpt 270 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C12 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 180 360 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath }
      bind def\n/C13 { BL [] 0 setdash \ 2 copy moveto\n \ \ \ \ \ \ 2 copy
      \ vpt 0 90 arc closepath fill\n \ \ \ \ \ \ 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 180 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C14 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 90 360 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind
      def\n/C15 { BL [] 0 setdash 2 copy vpt 0 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/Rec
      \ \ { newpath 4 2 roll moveto 1 index 0 rlineto 0 exch rlineto\n
      \ \ \ \ \ \ neg 0 rlineto closepath } bind def\n/Square { dup Rec }
      bind def\n/Bsquare { vpt sub exch vpt sub exch vpt2 Square } bind
      def\n/S0 { BL [] 0 setdash 2 copy moveto 0 vpt rlineto BL Bsquare }
      bind def\n/S1 { BL [] 0 setdash 2 copy vpt Square fill Bsquare } bind
      def\n/S2 { BL [] 0 setdash 2 copy exch vpt sub exch vpt Square fill
      Bsquare } bind def\n/S3 { BL [] 0 setdash 2 copy exch vpt sub exch vpt2
      vpt Rec fill Bsquare } bind def\n/S4 { BL [] 0 setdash 2 copy exch vpt
      sub exch vpt sub vpt Square fill Bsquare } bind def\n/S5 { BL [] 0
      setdash 2 copy 2 copy vpt Square fill\n \ \ \ \ \ \ exch vpt sub exch
      vpt sub vpt Square fill Bsquare } bind def\n/S6 { BL [] 0 setdash 2
      copy exch vpt sub exch vpt sub vpt vpt2 Rec fill Bsquare } bind
      def\n/S7 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt vpt2
      Rec fill\n \ \ \ \ \ \ 2 copy vpt Square fill\n \ \ \ \ \ \ Bsquare }
      bind def\n/S8 { BL [] 0 setdash 2 copy vpt sub vpt Square fill Bsquare
      } bind def\n/S9 { BL [] 0 setdash 2 copy vpt sub vpt vpt2 Rec fill
      Bsquare } bind def\n/S10 { BL [] 0 setdash 2 copy vpt sub vpt Square
      fill 2 copy exch vpt sub exch vpt Square fill\n \ \ \ \ \ \ Bsquare }
      bind def\n/S11 { BL [] 0 setdash 2 copy vpt sub vpt Square fill 2 copy
      exch vpt sub exch vpt2 vpt Rec fill\n \ \ \ \ \ \ Bsquare } bind
      def\n/S12 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt2 vpt
      Rec fill Bsquare } bind def\n/S13 { BL [] 0 setdash 2 copy exch vpt sub
      exch vpt sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2 copy vpt Square fill
      Bsquare } bind def\n/S14 { BL [] 0 setdash 2 copy exch vpt sub exch vpt
      sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2 copy exch vpt sub exch vpt Square
      fill Bsquare } bind def\n/S15 { BL [] 0 setdash 2 copy Bsquare fill
      Bsquare } bind def\n/D0 { gsave translate 45 rotate 0 0 S0 stroke
      grestore } bind def\n/D1 { gsave translate 45 rotate 0 0 S1 stroke
      grestore } bind def\n/D2 { gsave translate 45 rotate 0 0 S2 stroke
      grestore } bind def\n/D3 { gsave translate 45 rotate 0 0 S3 stroke
      grestore } bind def\n/D4 { gsave translate 45 rotate 0 0 S4 stroke
      grestore } bind def\n/D5 { gsave translate 45 rotate 0 0 S5 stroke
      grestore } bind def\n/D6 { gsave translate 45 rotate 0 0 S6 stroke
      grestore } bind def\n/D7 { gsave translate 45 rotate 0 0 S7 stroke
      grestore } bind def\n/D8 { gsave translate 45 rotate 0 0 S8 stroke
      grestore } bind def\n/D9 { gsave translate 45 rotate 0 0 S9 stroke
      grestore } bind def\n/D10 { gsave translate 45 rotate 0 0 S10 stroke
      grestore } bind def\n/D11 { gsave translate 45 rotate 0 0 S11 stroke
      grestore } bind def\n/D12 { gsave translate 45 rotate 0 0 S12 stroke
      grestore } bind def\n/D13 { gsave translate 45 rotate 0 0 S13 stroke
      grestore } bind def\n/D14 { gsave translate 45 rotate 0 0 S14 stroke
      grestore } bind def\n/D15 { gsave translate 45 rotate 0 0 S15 stroke
      grestore } bind def\n/DiaE { stroke [] 0 setdash vpt add M\n \ hpt neg
      vpt neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V closepath stroke }
      def\n/BoxE { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V closepath stroke } def\n/TriUE
      { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath stroke }
      def\n/TriDE { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt
      1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath
      stroke } def\n/PentE { stroke [] 0 setdash gsave\n \ translate 0 hpt M
      4 {72 rotate 0 hpt L} repeat\n \ closepath stroke grestore }
      def\n/CircE { stroke [] 0 setdash \n \ hpt 0 360 arc stroke }
      def\n/Opaque { gsave closepath 1 setgray fill grestore 0 setgray
      closepath } def\n/DiaW { stroke [] 0 setdash vpt add M\n \ hpt neg vpt
      neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V Opaque stroke }
      def\n/BoxW { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V Opaque stroke } def\n/TriUW {
      stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V Opaque stroke } def\n/TriDW
      { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt 1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V Opaque stroke } def\n/PentW
      { stroke [] 0 setdash gsave\n \ translate 0 hpt M 4 {72 rotate 0 hpt L}
      repeat\n \ Opaque stroke grestore } def\n/CircW { stroke [] 0 setdash
      \n \ hpt 0 360 arc Opaque stroke } def\n/BoxFill { gsave Rec 1 setgray
      fill grestore } def\n/Symbol-Oblique /Symbol findfont [1 0 .167 1 0 0]
      makefont\ndup length dict begin {1 index /FID eq {pop pop} {def}
      ifelse} forall\ncurrentdict end definefont\n/MFshow {{dup dup 0 get
      findfont exch 1 get scalefont setfont\n \ \ \ \ [ currentpoint ] exch
      dup 2 get 0 exch rmoveto dup dup 5 get exch 4 get\n \ \ \ \ {show}
      {stringwidth pop 0 rmoveto}ifelse dup 3 get\n \ \ \ \ {2 get neg 0 exch
      rmoveto pop} {pop aload pop moveto}ifelse} forall} bind def\n/MFwidth
      {0 exch {dup 3 get{dup dup 0 get findfont exch 1 get scalefont
      setfont\n \ \ \ \ \ 5 get stringwidth pop add}\n \ \ \ {pop} ifelse}
      forall} bind def\n/MLshow { currentpoint stroke M\n \ 0 exch R MFshow }
      bind def\n/MRshow { currentpoint stroke M\n \ exch dup MFwidth neg 3 -1
      roll R MFshow } def\n/MCshow { currentpoint stroke M\n \ exch dup
      MFwidth -2 div 3 -1 roll R MFshow } def\nend\n%%EndProlog\ngnudict
      begin\ngsave\n50 50 translate\n0.050 0.050 scale\n0
      setgray\nnewpath\n(Helvetica) findfont 140 scalefont setfont\n1.000
      UL\nLTb\n490 280 M\n63 0 V\n6409 0 R\n-63 0 V\n stroke\n406 280 M\n[
      [(Helvetica) 140.0 0.0 true true (-30)]\n] -46.7 MRshow\n490 854 M\n63
      0 V\n6409 0 R\n-63 0 V\n stroke\n406 854 M\n[ [(Helvetica) 140.0 0.0
      true true (-25)]\n] -46.7 MRshow\n490 1428 M\n63 0 V\n6409 0 R\n-63 0
      V\n stroke\n406 1428 M\n[ [(Helvetica) 140.0 0.0 true true (-20)]\n]
      -46.7 MRshow\n490 2002 M\n63 0 V\n6409 0 R\n-63 0 V\n stroke\n406 2002
      M\n[ [(Helvetica) 140.0 0.0 true true (-15)]\n] -46.7 MRshow\n490 2576
      M\n63 0 V\n6409 0 R\n-63 0 V\n stroke\n406 2576 M\n[ [(Helvetica) 140.0
      0.0 true true (-10)]\n] -46.7 MRshow\n490 3150 M\n63 0 V\n6409 0 R\n-63
      0 V\n stroke\n406 3150 M\n[ [(Helvetica) 140.0 0.0 true true (-5)]\n]
      -46.7 MRshow\n490 3724 M\n63 0 V\n6409 0 R\n-63 0 V\n stroke\n406 3724
      M\n[ [(Helvetica) 140.0 0.0 true true ( 0)]\n] -46.7 MRshow\n490 4298
      M\n63 0 V\n6409 0 R\n-63 0 V\n stroke\n406 4298 M\n[ [(Helvetica) 140.0
      0.0 true true ( 5)]\n] -46.7 MRshow\n490 4872 M\n63 0 V\n6409 0 R\n-63
      0 V\n stroke\n406 4872 M\n[ [(Helvetica) 140.0 0.0 true true ( 10)]\n]
      -46.7 MRshow\n490 280 M\n0 63 V\n0 4529 R\n0 -63 V\n stroke\n490 140
      M\n[ [(Helvetica) 140.0 0.0 true true (-10)]\n] -46.7 MCshow\n2108 280
      M\n0 63 V\n0 4529 R\n0 -63 V\n stroke\n2108 140 M\n[ [(Helvetica) 140.0
      0.0 true true (-5)]\n] -46.7 MCshow\n3726 280 M\n0 63 V\n0 4529 R\n0
      -63 V\n stroke\n3726 140 M\n[ [(Helvetica) 140.0 0.0 true true ( 0)]\n]
      -46.7 MCshow\n5344 280 M\n0 63 V\n0 4529 R\n0 -63 V\n stroke\n5344 140
      M\n[ [(Helvetica) 140.0 0.0 true true ( 5)]\n] -46.7 MCshow\n6962 280
      M\n0 63 V\n0 4529 R\n0 -63 V\n stroke\n6962 140 M\n[ [(Helvetica) 140.0
      0.0 true true ( 10)]\n] -46.7 MCshow\n1.000 UL\nLTb\n490 280 M\n6472 0
      V\n0 4592 V\n-6472 0 V\n490 280 L\n1.000 UL\nLT0\n6311 4739 M\n[
      [(Helvetica) 140.0 0.0 true true (x*\\(abs\\(\\(x-4\\)/\\(x+4\\)\\)\\)**\\(1./2.\\))]\n]
      -46.7 MRshow\n6395 4739 M\n399 0 V\n490 1970 M\n65 19 V\n66 18 V\n65 18
      V\n65 17 V\n66 17 V\n65 16 V\n66 16 V\n65 15 V\n65 15 V\n66 14 V\n65 12
      V\n65 12 V\n66 11 V\n65 9 V\n66 7 V\n65 6 V\n65 2 V\n66 0 V\n65 -4
      V\n65 -8 V\n66 -15 V\n65 -24 V\n66 -35 V\n65 -51 V\n65 -77 V\n66 -120
      V\n65 -201 V\n65 -393 V\n197 129 R\n65 695 V\n65 365 V\n66 239 V\n65
      175 V\n65 135 V\n66 110 V\n65 92 V\n66 78 V\n65 68 V\n65 60 V\n66 53
      V\n65 47 V\n65 42 V\n66 38 V\n65 35 V\n66 31 V\n65 28 V\n65 26 V\n66 23
      V\n65 21 V\n65 19 V\n66 17 V\n65 15 V\n66 13 V\n65 11 V\n65 10 V\n66 8
      V\n65 6 V\n65 4 V\n66 2 V\n65 1 V\n66 -2 V\n65 -4 V\n65 -7 V\n66 -10
      V\n65 -14 V\n65 -22 V\n66 -39 V\n65 23 V\n66 38 V\n65 31 V\n65 27 V\n66
      26 V\n65 25 V\n65 24 V\n66 23 V\n65 24 V\n66 22 V\n65 23 V\n65 23 V\n66
      22 V\n65 23 V\n65 22 V\n66 23 V\n65 22 V\n66 22 V\n65 23 V\n65 22 V\n66
      22 V\n65 23 V\n65 22 V\n66 22 V\n65 23 V\n66 22 V\n65 22 V\n65 23 V\n66
      22 V\n65 23 V\nstroke\ngrestore\nend\nshowpage\n%%Trailer\n%%DocumentFonts:
      Helvetica\n>|ps>||||||>

      \;
    </output>

    <\input|GNUplot] >
      set pm3d hidden 100

      set style line 100 lt 3

      set nosurface

      set size 0.7,1

      set view 80,180,1,1

      set noborder

      set noxtics ~set noytics ~set noztics

      set parametric

      set samples 36

      set isosamples 20,36

      set ticslevel 0

      set nocolorbox

      set urange [0:pi] ~set vrange [0:2*pi]

      splot sin(u)*cos(v),sin(u)*sin(v),cos(u)
    </input>

    <\output>
      <postscript|<tuple|<raw_data|%!PS-Adobe-2.0 EPSF-2.0\n%%Title:
      temp.eps\n%%Creator: gnuplot 3.8h patchlevel 0\n%%CreationDate: Thu Jul
      10 07:04:11 2003\n%%DocumentFonts: (atend)\n%%BoundingBox: 50 50 301
      302\n%%Orientation: Portrait\n%%EndComments\n/gnudict 256 dict
      def\ngnudict begin\n/Color false def\n/Solid false def\n/gnulinewidth
      5.000 def\n/userlinewidth gnulinewidth def\n/vshift -46 def\n/dl {10.0
      mul} def\n/hpt_ 31.5 def\n/vpt_ 31.5 def\n/hpt hpt_ def\n/vpt vpt_
      def\n/M {moveto} bind def\n/L {lineto} bind def\n/R {rmoveto} bind
      def\n/V {rlineto} bind def\n/N {newpath moveto} bind def\n/f {rlineto
      fill} bind def\n/vpt2 vpt 2 mul def\n/hpt2 hpt 2 mul def\n/Lshow {
      currentpoint stroke M\n \ 0 vshift R show } def\n/Rshow { currentpoint
      stroke M\n \ dup stringwidth pop neg vshift R show } def\n/Cshow {
      currentpoint stroke M\n \ dup stringwidth pop -2 div vshift R show }
      def\n/UP { dup vpt_ mul /vpt exch def hpt_ mul /hpt exch def\n \ /hpt2
      hpt 2 mul def /vpt2 vpt 2 mul def } def\n/DL { Color {setrgbcolor Solid
      {pop []} if 0 setdash }\n {pop pop pop Solid {pop []} if 0 setdash}
      ifelse } def\n/BL { stroke userlinewidth 2 mul setlinewidth } def\n/AL
      { stroke userlinewidth 2 div setlinewidth } def\n/UL { dup gnulinewidth
      mul /userlinewidth exch def\n \ \ \ \ \ dup 1 lt {pop 1} if 10 mul /udl
      exch def } def\n/PL { stroke userlinewidth setlinewidth } def\n/LTb {
      BL [] 0 0 0 DL } def\n/LTa { AL [1 udl mul 2 udl mul] 0 setdash 0 0 0
      setrgbcolor } def\n/LT0 { PL [] 1 0 0 DL } def\n/LT1 { PL [4 dl 2 dl] 0
      1 0 DL } def\n/LT2 { PL [2 dl 3 dl] 0 0 1 DL } def\n/LT3 { PL [1 dl 1.5
      dl] 1 0 1 DL } def\n/LT4 { PL [5 dl 2 dl 1 dl 2 dl] 0 1 1 DL }
      def\n/LT5 { PL [4 dl 3 dl 1 dl 3 dl] 1 1 0 DL } def\n/LT6 { PL [2 dl 2
      dl 2 dl 4 dl] 0 0 0 DL } def\n/LT7 { PL [2 dl 2 dl 2 dl 2 dl 2 dl 4 dl]
      1 0.3 0 DL } def\n/LT8 { PL [2 dl 2 dl 2 dl 2 dl 2 dl 2 dl 2 dl 4 dl]
      0.5 0.5 0.5 DL } def\n/Pnt { stroke [] 0 setdash\n \ \ gsave 1
      setlinecap M 0 0 V stroke grestore } def\n/Dia { stroke [] 0 setdash 2
      copy vpt add M\n \ hpt neg vpt neg V hpt vpt neg V\n \ hpt vpt V hpt
      neg vpt V closepath stroke\n \ Pnt } def\n/Pls { stroke [] 0 setdash
      vpt sub M 0 vpt2 V\n \ currentpoint stroke M\n \ hpt neg vpt neg R hpt2
      0 V stroke\n \ } def\n/Box { stroke [] 0 setdash 2 copy exch hpt sub
      exch vpt add M\n \ 0 vpt2 neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V
      closepath stroke\n \ Pnt } def\n/Crs { stroke [] 0 setdash exch hpt sub
      exch vpt add M\n \ hpt2 vpt2 neg V currentpoint stroke M\n \ hpt2 neg 0
      R hpt2 vpt2 V stroke } def\n/TriU { stroke [] 0 setdash 2 copy vpt 1.12
      mul add M\n \ hpt neg vpt -1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt
      1.62 mul V closepath stroke\n \ Pnt \ } def\n/Star { 2 copy Pls Crs }
      def\n/BoxF { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V \ hpt2 0 V \ 0 vpt2 V\n \ hpt2 neg 0 V \ closepath fill }
      def\n/TriUF { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt
      -1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath fill
      } def\n/TriD { stroke [] 0 setdash 2 copy vpt 1.12 mul sub M\n \ hpt
      neg vpt 1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V
      closepath stroke\n \ Pnt \ } def\n/TriDF { stroke [] 0 setdash vpt 1.12
      mul sub M\n \ hpt neg vpt 1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt
      -1.62 mul V closepath fill} def\n/DiaF { stroke [] 0 setdash vpt add
      M\n \ hpt neg vpt neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V
      closepath fill } def\n/Pent { stroke [] 0 setdash 2 copy gsave\n
      \ translate 0 hpt M 4 {72 rotate 0 hpt L} repeat\n \ closepath stroke
      grestore Pnt } def\n/PentF { stroke [] 0 setdash gsave\n \ translate 0
      hpt M 4 {72 rotate 0 hpt L} repeat\n \ closepath fill grestore }
      def\n/Circle { stroke [] 0 setdash 2 copy\n \ hpt 0 360 arc stroke Pnt
      } def\n/CircleF { stroke [] 0 setdash hpt 0 360 arc fill } def\n/C0 {
      BL [] 0 setdash 2 copy moveto vpt 90 450 \ arc } bind def\n/C1 { BL []
      0 setdash 2 copy \ \ \ \ \ \ \ moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 90
      arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C2 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 90 180 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C3 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 180 arc
      closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath }
      bind def\n/C4 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy
      \ vpt 180 270 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
      360 arc closepath } bind def\n/C5 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 0 90 arc\n \ \ \ \ \ \ 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 180 270 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind def\n/C6 { BL [] 0
      setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 90 270 arc closepath
      fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind
      def\n/C7 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 0 270
      arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath
      } bind def\n/C8 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2 copy vpt
      270 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C9 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ 2
      copy \ vpt 270 450 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
      360 arc closepath } bind def\n/C10 { BL [] 0 setdash 2 copy 2 copy
      moveto vpt 270 360 arc closepath fill\n \ \ \ \ \ \ 2 copy moveto\n
      \ \ \ \ \ \ 2 copy vpt 90 180 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C11 {
      BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 180 arc
      closepath fill\n \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt
      270 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C12 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 180 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C13 {
      BL [] 0 setdash \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 90 arc
      closepath fill\n \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt
      180 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
      closepath } bind def\n/C14 { BL [] 0 setdash 2 copy moveto\n
      \ \ \ \ \ \ 2 copy \ vpt 90 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind def\n/C15 { BL [] 0
      setdash 2 copy vpt 0 360 arc closepath fill\n
      \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/Rec
      \ \ { newpath 4 2 roll moveto 1 index 0 rlineto 0 exch rlineto\n
      \ \ \ \ \ \ neg 0 rlineto closepath } bind def\n/Square { dup Rec }
      bind def\n/Bsquare { vpt sub exch vpt sub exch vpt2 Square } bind
      def\n/S0 { BL [] 0 setdash 2 copy moveto 0 vpt rlineto BL Bsquare }
      bind def\n/S1 { BL [] 0 setdash 2 copy vpt Square fill Bsquare } bind
      def\n/S2 { BL [] 0 setdash 2 copy exch vpt sub exch vpt Square fill
      Bsquare } bind def\n/S3 { BL [] 0 setdash 2 copy exch vpt sub exch vpt2
      vpt Rec fill Bsquare } bind def\n/S4 { BL [] 0 setdash 2 copy exch vpt
      sub exch vpt sub vpt Square fill Bsquare } bind def\n/S5 { BL [] 0
      setdash 2 copy 2 copy vpt Square fill\n \ \ \ \ \ \ exch vpt sub exch
      vpt sub vpt Square fill Bsquare } bind def\n/S6 { BL [] 0 setdash 2
      copy exch vpt sub exch vpt sub vpt vpt2 Rec fill Bsquare } bind
      def\n/S7 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt vpt2
      Rec fill\n \ \ \ \ \ \ 2 copy vpt Square fill\n \ \ \ \ \ \ Bsquare }
      bind def\n/S8 { BL [] 0 setdash 2 copy vpt sub vpt Square fill Bsquare
      } bind def\n/S9 { BL [] 0 setdash 2 copy vpt sub vpt vpt2 Rec fill
      Bsquare } bind def\n/S10 { BL [] 0 setdash 2 copy vpt sub vpt Square
      fill 2 copy exch vpt sub exch vpt Square fill\n \ \ \ \ \ \ Bsquare }
      bind def\n/S11 { BL [] 0 setdash 2 copy vpt sub vpt Square fill 2 copy
      exch vpt sub exch vpt2 vpt Rec fill\n \ \ \ \ \ \ Bsquare } bind
      def\n/S12 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt2 vpt
      Rec fill Bsquare } bind def\n/S13 { BL [] 0 setdash 2 copy exch vpt sub
      exch vpt sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2 copy vpt Square fill
      Bsquare } bind def\n/S14 { BL [] 0 setdash 2 copy exch vpt sub exch vpt
      sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2 copy exch vpt sub exch vpt Square
      fill Bsquare } bind def\n/S15 { BL [] 0 setdash 2 copy Bsquare fill
      Bsquare } bind def\n/D0 { gsave translate 45 rotate 0 0 S0 stroke
      grestore } bind def\n/D1 { gsave translate 45 rotate 0 0 S1 stroke
      grestore } bind def\n/D2 { gsave translate 45 rotate 0 0 S2 stroke
      grestore } bind def\n/D3 { gsave translate 45 rotate 0 0 S3 stroke
      grestore } bind def\n/D4 { gsave translate 45 rotate 0 0 S4 stroke
      grestore } bind def\n/D5 { gsave translate 45 rotate 0 0 S5 stroke
      grestore } bind def\n/D6 { gsave translate 45 rotate 0 0 S6 stroke
      grestore } bind def\n/D7 { gsave translate 45 rotate 0 0 S7 stroke
      grestore } bind def\n/D8 { gsave translate 45 rotate 0 0 S8 stroke
      grestore } bind def\n/D9 { gsave translate 45 rotate 0 0 S9 stroke
      grestore } bind def\n/D10 { gsave translate 45 rotate 0 0 S10 stroke
      grestore } bind def\n/D11 { gsave translate 45 rotate 0 0 S11 stroke
      grestore } bind def\n/D12 { gsave translate 45 rotate 0 0 S12 stroke
      grestore } bind def\n/D13 { gsave translate 45 rotate 0 0 S13 stroke
      grestore } bind def\n/D14 { gsave translate 45 rotate 0 0 S14 stroke
      grestore } bind def\n/D15 { gsave translate 45 rotate 0 0 S15 stroke
      grestore } bind def\n/DiaE { stroke [] 0 setdash vpt add M\n \ hpt neg
      vpt neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V closepath stroke }
      def\n/BoxE { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V closepath stroke } def\n/TriUE
      { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath stroke }
      def\n/TriDE { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt
      1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath
      stroke } def\n/PentE { stroke [] 0 setdash gsave\n \ translate 0 hpt M
      4 {72 rotate 0 hpt L} repeat\n \ closepath stroke grestore }
      def\n/CircE { stroke [] 0 setdash \n \ hpt 0 360 arc stroke }
      def\n/Opaque { gsave closepath 1 setgray fill grestore 0 setgray
      closepath } def\n/DiaW { stroke [] 0 setdash vpt add M\n \ hpt neg vpt
      neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V Opaque stroke }
      def\n/BoxW { stroke [] 0 setdash exch hpt sub exch vpt add M\n \ 0 vpt2
      neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V Opaque stroke } def\n/TriUW {
      stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V Opaque stroke } def\n/TriDW
      { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt 1.62 mul V\n
      \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V Opaque stroke } def\n/PentW
      { stroke [] 0 setdash gsave\n \ translate 0 hpt M 4 {72 rotate 0 hpt L}
      repeat\n \ Opaque stroke grestore } def\n/CircW { stroke [] 0 setdash
      \n \ hpt 0 360 arc Opaque stroke } def\n/BoxFill { gsave Rec 1 setgray
      fill grestore } def\n/Symbol-Oblique /Symbol findfont [1 0 .167 1 0 0]
      makefont\ndup length dict begin {1 index /FID eq {pop pop} {def}
      ifelse} forall\ncurrentdict end definefont pop\n/MFshow {{dup dup 0 get
      findfont exch 1 get scalefont setfont\n \ \ \ \ [ currentpoint ] exch
      dup 2 get 0 exch R dup 5 get 2 ne {dup dup 6\n \ \ \ \ get exch 4 get
      {show} {stringwidth pop 0 R} ifelse }if dup 5 get 0 eq\n \ \ \ \ {dup 3
      get {2 get neg 0 exch R pop} {pop aload pop M} ifelse} {dup 5\n
      \ \ \ \ get 1 eq {dup 2 get exch dup 3 get exch 6 get stringwidth pop
      -2 div\n \ \ \ \ dup 0 R} {dup 6 get stringwidth pop -2 div 0 R 6 get\n
      \ \ \ \ show 2 index {aload pop M neg 3 -1 roll neg R pop pop} {pop pop
      pop\n \ \ \ \ pop aload pop M} ifelse }ifelse }ifelse } forall} bind
      def\n/MFwidth {0 exch {dup 3 get{dup dup 0 get findfont exch 1 get
      scalefont\n \ \ \ \ setfont 6 get stringwidth pop add} {pop} ifelse}
      forall} bind def\n/MLshow { currentpoint stroke M\n \ 0 exch R MFshow }
      bind def\n/MRshow { currentpoint stroke M\n \ exch dup MFwidth neg 3 -1
      roll R MFshow } def\n/MCshow { currentpoint stroke M\n \ exch dup
      MFwidth -2 div 3 -1 roll R MFshow } def\nend\n%%EndProlog\ngnudict
      begin\ngsave\n50 50 translate\n0.050 0.050 scale\n0
      setgray\nnewpath\n(Helvetica) findfont 140 scalefont setfont\n1.000
      UL\nLTb\ngsave %% colour palette begin\n/maxcolors 0
      def\n/maxcolorsLast {maxcolors 1 sub maxcolors div} def\n/pm3dround
      {maxcolors 0 gt {dup maxcolorsLast ge {pop 1} {maxcolors mul floor
      maxcolors div} ifelse} if} def\n/cF7 {sqrt} bind def\t% sqrt(x)\n/cF5
      {dup dup mul mul} bind def\t% x^3\n/cF15 {360 mul sin} bind def\t%
      sin(360x)\nColor true and { % COLOUR vs. GRAY map\n \ /g {stroke
      pm3dround dup cF7 exch dup cF5 exch cF15 setrgbcolor} bind def\n \ /h
      {rlineto rlineto rlineto fill} bind def\n}{\n \ /g {stroke pm3dround
      setgray} bind def\n \ /h {rlineto rlineto rlineto fill} bind def\n}
      ifelse\n%pm3d_map_begin\n.001 g 2404 1335 N 0 0 116 -9 1 4 h\n1.000
      UP\n1.000 UL\nLT2\n2404 1335 M\n117 -5 V\n-116 9 V\n-1 -4 V\n.005 g
      2287 1350 N -1 -4 114 -18 4 7 h\n1.000 UP\n1.000 UL\nLT2\n2287 1350
      M\n117 -15 V\n1 4 V\n-114 18 V\n-4 -7 V\n.013 g 2172 1375 N -4 -7 113
      -29 6 11 h\n1.000 UP\n1.000 UL\nLT2\n2172 1375 M\n115 -25 V\n4 7
      V\n-113 29 V\n-6 -11 V\n.025 g 2060 1410 N -6 -11 110 -38 8 14 h\n1.000
      UP\n1.000 UL\nLT2\n2060 1410 M\n112 -35 V\n6 11 V\n-110 38 V\n-8 -14
      V\n.0407 g 1952 1455 N -8 -14 107 -48 9 17 h\n1.000 UP\n1.000
      UL\nLT2\n1952 1455 M\n108 -45 V\n8 14 V\n-107 48 V\n-9 -17 V\n.0601 g
      1848 1508 N -9 -17 102 -57 11 21 h\n1.000 UP\n1.000 UL\nLT2\n1848 1508
      M\n104 -53 V\n9 17 V\n-102 57 V\n-11 -21 V\n.0831 g 1750 1570 N -11 -21
      96 -65 13 24 h\n1.000 UP\n1.000 UL\nLT2\n1750 1570 M\n98 -62 V\n11 21
      V\n-96 65 V\n-13 -24 V\n.1095 g 1658 1641 N -13 -24 91 -73 14 26
      h\n1.000 UP\n1.000 UL\nLT2\n1658 1641 M\n92 -71 V\n13 24 V\n-91 73
      V\n-14 -26 V\n.139 g 1573 1719 N -14 -26 84 -81 15 29 h\n1.000
      UP\n1.000 UL\nLT2\n1573 1719 M\n85 -78 V\n14 26 V\n-84 81 V\n-15 -29
      V\n.1714 g 1496 1804 N -15 -29 76 -87 16 31 h\n1.000 UP\n1.000
      UL\nLT2\n1496 1804 M\n77 -85 V\n15 29 V\n-76 87 V\n-16 -31 V\n.2064 g
      1427 1896 N -16 -31 68 -94 17 33 h\n1.000 UP\n1.000 UL\nLT2\n1427 1896
      M\n69 -92 V\n16 31 V\n-68 94 V\n-17 -33 V\n.2438 g 1366 1993 N -17 -33
      59 -99 19 35 h\n1.000 UP\n1.000 UL\nLT2\n1366 1993 M\n61 -97 V\n17 33
      V\n-59 99 V\n-19 -35 V\n.2833 g 1315 2094 N -19 -35 50 -103 20 37
      h\n1.000 UP\n1.000 UL\nLT2\n1315 2094 M\n51 -101 V\n19 35 V\n-50 103
      V\n-20 -37 V\n.3245 g 1274 2200 N -20 -37 41 -107 20 38 h\n1.000
      UP\n1.000 UL\nLT2\n1274 2200 M\n41 -106 V\n20 37 V\n-41 107 V\n-20 -38
      V\n.3671 g 1243 2309 N -20 -38 31 -110 20 39 h\n1.000 UP\n1.000
      UL\nLT2\n1243 2309 M\n31 -109 V\n20 38 V\n-31 110 V\n-20 -39 V\n.4108 g
      1222 2420 N -20 -39 21 -112 20 40 h\n1.000 UP\n1.000 UL\nLT2\n1222 2420
      M\n21 -111 V\n20 39 V\n-21 112 V\n-20 -40 V\n.4552 g 1211 2533 N -20
      -40 10 -113 21 40 h\n1.000 UP\n1.000 UL\nLT2\n1211 2533 M\n11 -113
      V\n20 40 V\n-10 113 V\n-21 -40 V\n.5 g 1211 2646 N -21 -40 0 -113 21 40
      h\n1.000 UP\n1.000 UL\nLT2\n1211 2646 M\n0 -113 V\n21 40 V\n0 113
      V\n-21 -40 V\n.5448 g 1222 2759 N -21 -40 -10 -112 20 39 h\n1.000
      UP\n1.000 UL\nLT2\n1222 2759 M\n-11 -113 V\n21 40 V\n10 112 V\n-20 -39
      V\n.5892 g 1243 2870 N -20 -39 -21 -111 20 39 h\n1.000 UP\n1.000
      UL\nLT2\n1243 2870 M\n-21 -111 V\n20 39 V\n21 111 V\n-20 -39 V\n.6329 g
      1274 2979 N -20 -39 -31 -108 20 38 h\n1.000 UP\n1.000 UL\nLT2\n1274
      2979 M\n-31 -109 V\n20 39 V\n31 108 V\n-20 -38 V\n.6755 g 1315 3085 N
      -20 -38 -41 -104 20 36 h\n1.000 UP\n1.000 UL\nLT2\n1315 3085 M\n-41
      -106 V\n20 38 V\n41 104 V\n-20 -36 V\n.7167 g 1366 3186 N -20 -36 -50
      -100 19 35 h\n1.000 UP\n1.000 UL\nLT2\n1366 3186 M\n-51 -101 V\n20 36
      V\n50 100 V\n-19 -35 V\n.7562 g 1427 3283 N -19 -35 -59 -96 17 34
      h\n1.000 UP\n1.000 UL\nLT2\n1427 3283 M\n-61 -97 V\n19 35 V\n59 96
      V\n-17 -34 V\n.7936 g 1496 3375 N -17 -34 -68 -89 16 31 h\n1.000
      UP\n1.000 UL\nLT2\n1496 3375 M\n-69 -92 V\n17 34 V\n68 89 V\n-16 -31
      V\n.8286 g 1573 3460 N -16 -31 -76 -83 15 29 h\n1.000 UP\n1.000
      UL\nLT2\n1573 3460 M\n-77 -85 V\n16 31 V\n76 83 V\n-15 -29 V\n.861 g
      1658 3538 N -15 -29 -84 -75 14 26 h\n1.000 UP\n1.000 UL\nLT2\n1658 3538
      M\n-85 -78 V\n15 29 V\n84 75 V\n-14 -26 V\n.8905 g 1750 3609 N -14 -26
      -91 -68 13 23 h\n1.000 UP\n1.000 UL\nLT2\n1750 3609 M\n-92 -71 V\n14 26
      V\n91 68 V\n-13 -23 V\n.9169 g 1848 3671 N -13 -23 -96 -59 11 20
      h\n1.000 UP\n1.000 UL\nLT2\n1848 3671 M\n-98 -62 V\n13 23 V\n96 59
      V\n-11 -20 V\n.9399 g 1952 3724 N -11 -20 -102 -51 9 18 h\n1.000
      UP\n1.000 UL\nLT2\n1952 3724 M\n-104 -53 V\n11 20 V\n102 51 V\n-9 -18
      V\n.9593 g 2060 3769 N -9 -18 -107 -41 8 14 h\n1.000 UP\n1.000
      UL\nLT2\n2060 3769 M\n-108 -45 V\n9 18 V\n107 41 V\n-8 -14 V\n.975 g
      2172 3804 N -8 -14 -110 -31 6 10 h\n1.000 UP\n1.000 UL\nLT2\n2172 3804
      M\n-112 -35 V\n8 14 V\n110 31 V\n-6 -10 V\n.987 g 2287 3829 N -6 -10
      -113 -22 4 7 h\n1.000 UP\n1.000 UL\nLT2\n2287 3829 M\n-115 -25 V\n6 10
      V\n113 22 V\n-4 -7 V\n.995 g 2404 3844 N -4 -7 -114 -12 1 4 h\n1.000
      UP\n1.000 UL\nLT2\n2404 3844 M\n-117 -15 V\n4 7 V\n114 12 V\n-1 -4
      V\n.999 g 2521 3849 N -1 -4 -116 -1 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n-117 -5 V\n1 4 V\n116 1 V\n.001 g 2405 1339 N 0 0
      110 -12 6 3 h\n1.000 UP\n1.000 UL\nLT2\n2405 1339 M\n116 -9 V\n-110 12
      V\n-6 -3 V\n.005 g 2291 1357 N -6 -3 109 -22 11 7 h\n1.000 UP\n1.000
      UL\nLT2\n2291 1357 M\n114 -18 V\n6 3 V\n-109 22 V\n-11 -7 V\n.013 g
      2178 1386 N -11 -7 108 -32 16 10 h\n1.000 UP\n1.000 UL\nLT2\n2178 1386
      M\n113 -29 V\n11 7 V\n-108 32 V\n-16 -10 V\n.025 g 2068 1424 N -16 -10
      104 -42 22 14 h\n1.000 UP\n1.000 UL\nLT2\n2068 1424 M\n110 -38 V\n16 10
      V\n-104 42 V\n-22 -14 V\n.0407 g 1961 1472 N -22 -14 102 -51 27 17
      h\n1.000 UP\n1.000 UL\nLT2\n1961 1472 M\n107 -48 V\n22 14 V\n-102 51
      V\n-27 -17 V\n.0601 g 1859 1529 N -27 -17 97 -59 32 19 h\n1.000
      UP\n1.000 UL\nLT2\n1859 1529 M\n102 -57 V\n27 17 V\n-97 59 V\n-32 -19
      V\n.0831 g 1763 1594 N -32 -19 92 -68 36 22 h\n1.000 UP\n1.000
      UL\nLT2\n1763 1594 M\n96 -65 V\n32 19 V\n-92 68 V\n-36 -22 V\n.1095 g
      1672 1667 N -36 -22 86 -76 41 25 h\n1.000 UP\n1.000 UL\nLT2\n1672 1667
      M\n91 -73 V\n36 22 V\n-86 76 V\n-41 -25 V\n.139 g 1588 1748 N -41 -25
      79 -84 46 28 h\n1.000 UP\n1.000 UL\nLT2\n1588 1748 M\n84 -81 V\n41 25
      V\n-79 84 V\n-46 -28 V\n.1714 g 1512 1835 N -46 -28 73 -89 49 30
      h\n1.000 UP\n1.000 UL\nLT2\n1512 1835 M\n76 -87 V\n46 28 V\n-73 89
      V\n-49 -30 V\n.2064 g 1444 1929 N -49 -30 65 -96 52 32 h\n1.000
      UP\n1.000 UL\nLT2\n1444 1929 M\n68 -94 V\n49 30 V\n-65 96 V\n-52 -32
      V\n.2438 g 1385 2028 N -52 -32 56 -100 55 33 h\n1.000 UP\n1.000
      UL\nLT2\n1385 2028 M\n59 -99 V\n52 32 V\n-56 100 V\n-55 -33 V\n.2833 g
      1335 2131 N -55 -33 48 -105 57 35 h\n1.000 UP\n1.000 UL\nLT2\n1335 2131
      M\n50 -103 V\n55 33 V\n-48 105 V\n-57 -35 V\n.3245 g 1294 2238 N -57
      -35 39 -109 59 37 h\n1.000 UP\n1.000 UL\nLT2\n1294 2238 M\n41 -107
      V\n57 35 V\n-39 109 V\n-59 -37 V\n.3671 g 1263 2348 N -59 -37 29 -110
      61 37 h\n1.000 UP\n1.000 UL\nLT2\n1263 2348 M\n31 -110 V\n59 37 V\n-29
      110 V\n-61 -37 V\n.4108 g 1242 2460 N -61 -37 20 -113 62 38 h\n1.000
      UP\n1.000 UL\nLT2\n1242 2460 M\n21 -112 V\n61 37 V\n-20 113 V\n-62 -38
      V\n.4552 g 1232 2573 N -62 -38 9 -113 63 38 h\n1.000 UP\n1.000
      UL\nLT2\n1232 2573 M\n10 -113 V\n62 38 V\n-9 113 V\n-63 -38 V\n.5 g
      1232 2686 N -63 -38 0 -113 63 38 h\n1.000 UP\n1.000 UL\nLT2\n1232 2686
      M\n0 -113 V\n63 38 V\n0 113 V\n-63 -38 V\n.5448 g 1242 2798 N -63 -38
      -9 -112 62 38 h\n1.000 UP\n1.000 UL\nLT2\n1242 2798 M\n-10 -112 V\n63
      38 V\n9 112 V\n-62 -38 V\n.5892 g 1263 2909 N -62 -38 -20 -110 61 37
      h\n1.000 UP\n1.000 UL\nLT2\n1263 2909 M\n-21 -111 V\n62 38 V\n20 110
      V\n-61 -37 V\n.6329 g 1294 3017 N -61 -37 -29 -107 59 36 h\n1.000
      UP\n1.000 UL\nLT2\n1294 3017 M\n-31 -108 V\n61 37 V\n29 107 V\n-59 -36
      V\n.6755 g 1335 3121 N -59 -36 -39 -103 57 35 h\n1.000 UP\n1.000
      UL\nLT2\n1335 3121 M\n-41 -104 V\n59 36 V\n39 103 V\n-57 -35 V\n.7167 g
      1385 3221 N -57 -35 -48 -99 55 34 h\n1.000 UP\n1.000 UL\nLT2\n1385 3221
      M\n-50 -100 V\n57 35 V\n48 99 V\n-55 -34 V\n.7562 g 1444 3317 N -55 -34
      -56 -94 52 32 h\n1.000 UP\n1.000 UL\nLT2\n1444 3317 M\n-59 -96 V\n55 34
      V\n56 94 V\n-52 -32 V\n.7936 g 1512 3406 N -52 -32 -65 -87 49 30
      h\n1.000 UP\n1.000 UL\nLT2\n1512 3406 M\n-68 -89 V\n52 32 V\n65 87
      V\n-49 -30 V\n.8286 g 1588 3489 N -49 -30 -73 -80 46 27 h\n1.000
      UP\n1.000 UL\nLT2\n1588 3489 M\n-76 -83 V\n49 30 V\n73 80 V\n-46 -27
      V\n.861 g 1672 3564 N -46 -27 -79 -73 41 25 h\n1.000 UP\n1.000
      UL\nLT2\n1672 3564 M\n-84 -75 V\n46 27 V\n79 73 V\n-41 -25 V\n.8905 g
      1763 3632 N -41 -25 -86 -65 36 22 h\n1.000 UP\n1.000 UL\nLT2\n1763 3632
      M\n-91 -68 V\n41 25 V\n86 65 V\n-36 -22 V\n.9169 g 1859 3691 N -36 -22
      -92 -57 32 20 h\n1.000 UP\n1.000 UL\nLT2\n1859 3691 M\n-96 -59 V\n36 22
      V\n92 57 V\n-32 -20 V\n.9399 g 1961 3742 N -32 -20 -97 -47 27 16
      h\n1.000 UP\n1.000 UL\nLT2\n1961 3742 M\n-102 -51 V\n32 20 V\n97 47
      V\n-27 -16 V\n.9593 g 2068 3783 N -27 -16 -102 -38 22 13 h\n1.000
      UP\n1.000 UL\nLT2\n2068 3783 M\n-107 -41 V\n27 16 V\n102 38 V\n-22 -13
      V\n.975 g 2178 3814 N -22 -13 -104 -28 16 10 h\n1.000 UP\n1.000
      UL\nLT2\n2178 3814 M\n-110 -31 V\n22 13 V\n104 28 V\n-16 -10 V\n.987 g
      2291 3836 N -16 -10 -108 -19 11 7 h\n1.000 UP\n1.000 UL\nLT2\n2291 3836
      M\n-113 -22 V\n16 10 V\n108 19 V\n-11 -7 V\n.995 g 2405 3848 N -11 -7
      -109 -8 6 3 h\n1.000 UP\n1.000 UL\nLT2\n2405 3848 M\n-114 -12 V\n11 7
      V\n109 8 V\n-6 -3 V\n.999 g 2521 3849 N -6 -3 -110 2 0 0 h\n1.000
      UP\n1.000 UL\nLT2\n2521 3849 M\n-116 -1 V\n6 3 V\n110 -2 V\n.001 g 2411
      1342 N 0 0 101 -15 9 3 h\n1.000 UP\n1.000 UL\nLT2\n2411 1342 M\n110 -12
      V\n-101 15 V\n-9 -3 V\n.005 g 2302 1364 N -9 -3 100 -26 18 7 h\n1.000
      UP\n1.000 UL\nLT2\n2302 1364 M\n109 -22 V\n9 3 V\n-100 26 V\n-18 -7
      V\n.013 g 2194 1396 N -18 -7 98 -35 28 10 h\n1.000 UP\n1.000
      UL\nLT2\n2194 1396 M\n108 -32 V\n18 7 V\n-98 35 V\n-28 -10 V\n.025 g
      2090 1438 N -28 -10 96 -44 36 12 h\n1.000 UP\n1.000 UL\nLT2\n2090 1438
      M\n104 -42 V\n28 10 V\n-96 44 V\n-36 -12 V\n.0407 g 1988 1489 N -36 -12
      93 -54 45 15 h\n1.000 UP\n1.000 UL\nLT2\n1988 1489 M\n102 -51 V\n36 12
      V\n-93 54 V\n-45 -15 V\n.0601 g 1891 1548 N -45 -15 89 -63 53 19
      h\n1.000 UP\n1.000 UL\nLT2\n1891 1548 M\n97 -59 V\n45 15 V\n-89 63
      V\n-53 -19 V\n.0831 g 1799 1616 N -53 -19 85 -71 60 22 h\n1.000
      UP\n1.000 UL\nLT2\n1799 1616 M\n92 -68 V\n53 19 V\n-85 71 V\n-60 -22
      V\n.1095 g 1713 1692 N -60 -22 79 -78 67 24 h\n1.000 UP\n1.000
      UL\nLT2\n1713 1692 M\n86 -76 V\n60 22 V\n-79 78 V\n-67 -24 V\n.139 g
      1634 1776 N -67 -24 73 -86 73 26 h\n1.000 UP\n1.000 UL\nLT2\n1634 1776
      M\n79 -84 V\n67 24 V\n-73 86 V\n-73 -26 V\n.1714 g 1561 1865 N -73 -26
      66 -91 80 28 h\n1.000 UP\n1.000 UL\nLT2\n1561 1865 M\n73 -89 V\n73 26
      V\n-66 91 V\n-80 -28 V\n.2064 g 1496 1961 N -80 -28 59 -98 86 30
      h\n1.000 UP\n1.000 UL\nLT2\n1496 1961 M\n65 -96 V\n80 28 V\n-59 98
      V\n-86 -30 V\n.2438 g 1440 2061 N -86 -30 52 -102 90 32 h\n1.000
      UP\n1.000 UL\nLT2\n1440 2061 M\n56 -100 V\n86 30 V\n-52 102 V\n-90 -32
      V\n.2833 g 1392 2166 N -90 -32 44 -106 94 33 h\n1.000 UP\n1.000
      UL\nLT2\n1392 2166 M\n48 -105 V\n90 32 V\n-44 106 V\n-94 -33 V\n.3245 g
      1353 2275 N -94 -33 36 -110 97 34 h\n1.000 UP\n1.000 UL\nLT2\n1353 2275
      M\n39 -109 V\n94 33 V\n-36 110 V\n-97 -34 V\n.3671 g 1324 2385 N -97
      -34 26 -112 100 36 h\n1.000 UP\n1.000 UL\nLT2\n1324 2385 M\n29 -110
      V\n97 34 V\n-26 112 V\n-100 -36 V\n.4108 g 1304 2498 N -100 -36 18 -113
      102 36 h\n1.000 UP\n1.000 UL\nLT2\n1304 2498 M\n20 -113 V\n100 36
      V\n-18 113 V\n-102 -36 V\n.4552 g 1295 2611 N -102 -36 10 -113 101 36
      h\n1.000 UP\n1.000 UL\nLT2\n1295 2611 M\n9 -113 V\n102 36 V\n-10 113
      V\n-101 -36 V\n.5 g 1295 2724 N -101 -36 0 -113 101 36 h\n1.000
      UP\n1.000 UL\nLT2\n1295 2724 M\n0 -113 V\n101 36 V\n0 113 V\n-101 -36
      V\n.5448 g 1304 2836 N -101 -36 -10 -112 102 36 h\n1.000 UP\n1.000
      UL\nLT2\n1304 2836 M\n-9 -112 V\n101 36 V\n10 112 V\n-102 -36 V\n.5892
      g 1324 2946 N -102 -36 -18 -109 100 35 h\n1.000 UP\n1.000 UL\nLT2\n1324
      2946 M\n-20 -110 V\n102 36 V\n18 109 V\n-100 -35 V\n.6329 g 1353 3053 N
      -100 -35 -26 -106 97 34 h\n1.000 UP\n1.000 UL\nLT2\n1353 3053 M\n-29
      -107 V\n100 35 V\n26 106 V\n-97 -34 V\n.6755 g 1392 3156 N -97 -34 -36
      -102 94 33 h\n1.000 UP\n1.000 UL\nLT2\n1392 3156 M\n-39 -103 V\n97 34
      V\n36 102 V\n-94 -33 V\n.7167 g 1440 3255 N -94 -33 -44 -98 90 32
      h\n1.000 UP\n1.000 UL\nLT2\n1440 3255 M\n-48 -99 V\n94 33 V\n44 98
      V\n-90 -32 V\n.7562 g 1496 3349 N -90 -32 -52 -92 86 30 h\n1.000
      UP\n1.000 UL\nLT2\n1496 3349 M\n-56 -94 V\n90 32 V\n52 92 V\n-86 -30
      V\n.7936 g 1561 3436 N -86 -30 -59 -85 80 28 h\n1.000 UP\n1.000
      UL\nLT2\n1561 3436 M\n-65 -87 V\n86 30 V\n59 85 V\n-80 -28 V\n.8286 g
      1634 3516 N -80 -28 -66 -78 73 26 h\n1.000 UP\n1.000 UL\nLT2\n1634 3516
      M\n-73 -80 V\n80 28 V\n66 78 V\n-73 -26 V\n.861 g 1713 3589 N -73 -26
      -73 -71 67 24 h\n1.000 UP\n1.000 UL\nLT2\n1713 3589 M\n-79 -73 V\n73 26
      V\n73 71 V\n-67 -24 V\n.8905 g 1799 3654 N -67 -24 -79 -63 60 22
      h\n1.000 UP\n1.000 UL\nLT2\n1799 3654 M\n-86 -65 V\n67 24 V\n79 63
      V\n-60 -22 V\n.9169 g 1891 3711 N -60 -22 -85 -53 53 18 h\n1.000
      UP\n1.000 UL\nLT2\n1891 3711 M\n-92 -57 V\n60 22 V\n85 53 V\n-53 -18
      V\n.9399 g 1988 3758 N -53 -18 -89 -45 45 16 h\n1.000 UP\n1.000
      UL\nLT2\n1988 3758 M\n-97 -47 V\n53 18 V\n89 45 V\n-45 -16 V\n.9593 g
      2090 3796 N -45 -16 -93 -35 36 13 h\n1.000 UP\n1.000 UL\nLT2\n2090 3796
      M\n-102 -38 V\n45 16 V\n93 35 V\n-36 -13 V\n.975 g 2194 3824 N -36 -13
      -96 -25 28 10 h\n1.000 UP\n1.000 UL\nLT2\n2194 3824 M\n-104 -28 V\n36
      13 V\n96 25 V\n-28 -10 V\n.987 g 2302 3843 N -28 -10 -98 -15 18 6
      h\n1.000 UP\n1.000 UL\nLT2\n2302 3843 M\n-108 -19 V\n28 10 V\n98 15
      V\n-18 -6 V\n.995 g 2411 3851 N -18 -6 -100 -5 9 3 h\n1.000 UP\n1.000
      UL\nLT2\n2411 3851 M\n-109 -8 V\n18 6 V\n100 5 V\n-9 -3 V\n.999 g 2521
      3849 N -9 -3 -101 5 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-110 2
      V\n9 3 V\n101 -5 V\n.001 g 2420 1345 N 0 0 88 -18 13 3 h\n1.000
      UP\n1.000 UL\nLT2\n2420 1345 M\n101 -15 V\n-88 18 V\n-13 -3 V\n.005 g
      2320 1371 N -13 -3 88 -28 25 5 h\n1.000 UP\n1.000 UL\nLT2\n2320 1371
      M\n100 -26 V\n13 3 V\n-88 28 V\n-25 -5 V\n.013 g 2222 1406 N -25 -5 87
      -38 36 8 h\n1.000 UP\n1.000 UL\nLT2\n2222 1406 M\n98 -35 V\n25 5 V\n-87
      38 V\n-36 -8 V\n.025 g 2126 1450 N -36 -8 84 -48 48 12 h\n1.000
      UP\n1.000 UL\nLT2\n2126 1450 M\n96 -44 V\n36 8 V\n-84 48 V\n-48 -12
      V\n.0407 g 2033 1504 N -48 -12 81 -56 60 14 h\n1.000 UP\n1.000
      UL\nLT2\n2033 1504 M\n93 -54 V\n48 12 V\n-81 56 V\n-60 -14 V\n.0601 g
      1944 1567 N -60 -14 78 -65 71 16 h\n1.000 UP\n1.000 UL\nLT2\n1944 1567
      M\n89 -63 V\n60 14 V\n-78 65 V\n-71 -16 V\n.0831 g 1859 1638 N -71 -16
      74 -74 82 19 h\n1.000 UP\n1.000 UL\nLT2\n1859 1638 M\n85 -71 V\n71 16
      V\n-74 74 V\n-82 -19 V\n.1095 g 1780 1716 N -82 -19 70 -80 91 21
      h\n1.000 UP\n1.000 UL\nLT2\n1780 1716 M\n79 -78 V\n82 19 V\n-70 80
      V\n-91 -21 V\n.139 g 1707 1802 N -91 -21 64 -88 100 23 h\n1.000
      UP\n1.000 UL\nLT2\n1707 1802 M\n73 -86 V\n91 21 V\n-64 88 V\n-100 -23
      V\n.1714 g 1641 1893 N -100 -23 58 -94 108 26 h\n1.000 UP\n1.000
      UL\nLT2\n1641 1893 M\n66 -91 V\n100 23 V\n-58 94 V\n-108 -26 V\n.2064 g
      1582 1991 N -108 -26 52 -99 115 27 h\n1.000 UP\n1.000 UL\nLT2\n1582
      1991 M\n59 -98 V\n108 26 V\n-52 99 V\n-115 -27 V\n.2438 g 1530 2093 N
      -115 -27 46 -104 121 29 h\n1.000 UP\n1.000 UL\nLT2\n1530 2093 M\n52
      -102 V\n115 27 V\n-46 104 V\n-121 -29 V\n.2833 g 1486 2199 N -121 -29
      38 -107 127 30 h\n1.000 UP\n1.000 UL\nLT2\n1486 2199 M\n44 -106 V\n121
      29 V\n-38 107 V\n-127 -30 V\n.3245 g 1450 2309 N -127 -30 31 -111 132
      31 h\n1.000 UP\n1.000 UL\nLT2\n1450 2309 M\n36 -110 V\n127 30 V\n-31
      111 V\n-132 -31 V\n.3671 g 1424 2421 N -132 -31 24 -112 134 31 h\n1.000
      UP\n1.000 UL\nLT2\n1424 2421 M\n26 -112 V\n132 31 V\n-24 112 V\n-134
      -31 V\n.4108 g 1406 2534 N -134 -31 16 -114 136 32 h\n1.000 UP\n1.000
      UL\nLT2\n1406 2534 M\n18 -113 V\n134 31 V\n-16 114 V\n-136 -32 V\n.4552
      g 1396 2647 N -136 -32 7 -113 139 32 h\n1.000 UP\n1.000 UL\nLT2\n1396
      2647 M\n10 -113 V\n136 32 V\n-7 113 V\n-139 -32 V\n.5 g 1396 2760 N
      -139 -32 0 -113 139 32 h\n1.000 UP\n1.000 UL\nLT2\n1396 2760 M\n0 -113
      V\n139 32 V\n0 113 V\n-139 -32 V\n.5448 g 1406 2872 N -139 -32 -7 -112
      136 32 h\n1.000 UP\n1.000 UL\nLT2\n1406 2872 M\n-10 -112 V\n139 32 V\n7
      112 V\n-136 -32 V\n.5892 g 1424 2981 N -136 -32 -16 -109 134 32
      h\n1.000 UP\n1.000 UL\nLT2\n1424 2981 M\n-18 -109 V\n136 32 V\n16 109
      V\n-134 -32 V\n.6329 g 1450 3087 N -134 -32 -24 -105 132 31 h\n1.000
      UP\n1.000 UL\nLT2\n1450 3087 M\n-26 -106 V\n134 32 V\n24 105 V\n-132
      -31 V\n.6755 g 1486 3189 N -132 -31 -31 -101 127 30 h\n1.000 UP\n1.000
      UL\nLT2\n1486 3189 M\n-36 -102 V\n132 31 V\n31 101 V\n-127 -30 V\n.7167
      g 1530 3287 N -127 -30 -38 -96 121 28 h\n1.000 UP\n1.000 UL\nLT2\n1530
      3287 M\n-44 -98 V\n127 30 V\n38 96 V\n-121 -28 V\n.7562 g 1582 3379 N
      -121 -28 -46 -91 115 27 h\n1.000 UP\n1.000 UL\nLT2\n1582 3379 M\n-52
      -92 V\n121 28 V\n46 91 V\n-115 -27 V\n.7936 g 1641 3464 N -115 -27 -52
      -83 108 25 h\n1.000 UP\n1.000 UL\nLT2\n1641 3464 M\n-59 -85 V\n115 27
      V\n52 83 V\n-108 -25 V\n.8286 g 1707 3542 N -108 -25 -58 -77 100 24
      h\n1.000 UP\n1.000 UL\nLT2\n1707 3542 M\n-66 -78 V\n108 25 V\n58 77
      V\n-100 -24 V\n.861 g 1780 3613 N -100 -24 -64 -68 91 21 h\n1.000
      UP\n1.000 UL\nLT2\n1780 3613 M\n-73 -71 V\n100 24 V\n64 68 V\n-91 -21
      V\n.8905 g 1859 3676 N -91 -21 -70 -61 82 19 h\n1.000 UP\n1.000
      UL\nLT2\n1859 3676 M\n-79 -63 V\n91 21 V\n70 61 V\n-82 -19 V\n.9169 g
      1944 3729 N -82 -19 -74 -51 71 17 h\n1.000 UP\n1.000 UL\nLT2\n1944 3729
      M\n-85 -53 V\n82 19 V\n74 51 V\n-71 -17 V\n.9399 g 2033 3774 N -71 -17
      -78 -42 60 14 h\n1.000 UP\n1.000 UL\nLT2\n2033 3774 M\n-89 -45 V\n71 17
      V\n78 42 V\n-60 -14 V\n.9593 g 2126 3809 N -60 -14 -81 -32 48 11
      h\n1.000 UP\n1.000 UL\nLT2\n2126 3809 M\n-93 -35 V\n60 14 V\n81 32
      V\n-48 -11 V\n.975 g 2222 3834 N -48 -11 -84 -23 36 9 h\n1.000
      UP\n1.000 UL\nLT2\n2222 3834 M\n-96 -25 V\n48 11 V\n84 23 V\n-36 -9
      V\n.987 g 2320 3849 N -36 -9 -87 -12 25 6 h\n1.000 UP\n1.000
      UL\nLT2\n2320 3849 M\n-98 -15 V\n36 9 V\n87 12 V\n-25 -6 V\n.995 g 2420
      3854 N -25 -6 -88 -2 13 3 h\n1.000 UP\n1.000 UL\nLT2\n2420 3854 M\n-100
      -5 V\n25 6 V\n88 2 V\n-13 -3 V\n.999 g 2521 3849 N -13 -3 -88 8 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-101 5 V\n13 3 V\n88 -8
      V\n.001 g 2433 1348 N 0 0 73 -21 15 3 h\n1.000 UP\n1.000 UL\nLT2\n2433
      1348 M\n88 -18 V\n-73 21 V\n-15 -3 V\n.005 g 2345 1376 N -15 -3 73 -30
      30 5 h\n1.000 UP\n1.000 UL\nLT2\n2345 1376 M\n88 -28 V\n15 3 V\n-73 30
      V\n-30 -5 V\n.013 g 2258 1414 N -30 -5 71 -41 46 8 h\n1.000 UP\n1.000
      UL\nLT2\n2258 1414 M\n87 -38 V\n30 5 V\n-71 41 V\n-46 -8 V\n.025 g 2174
      1462 N -46 -8 70 -49 60 9 h\n1.000 UP\n1.000 UL\nLT2\n2174 1462 M\n84
      -48 V\n46 8 V\n-70 49 V\n-60 -9 V\n.0407 g 2093 1518 N -60 -9 68 -59 73
      12 h\n1.000 UP\n1.000 UL\nLT2\n2093 1518 M\n81 -56 V\n60 9 V\n-68 59
      V\n-73 -12 V\n.0601 g 2015 1583 N -73 -12 64 -67 87 14 h\n1.000
      UP\n1.000 UL\nLT2\n2015 1583 M\n78 -65 V\n73 12 V\n-64 67 V\n-87 -14
      V\n.0831 g 1941 1657 N -87 -14 61 -76 100 16 h\n1.000 UP\n1.000
      UL\nLT2\n1941 1657 M\n74 -74 V\n87 14 V\n-61 76 V\n-100 -16 V\n.1095 g
      1871 1737 N -100 -16 58 -82 112 18 h\n1.000 UP\n1.000 UL\nLT2\n1871
      1737 M\n70 -80 V\n100 16 V\n-58 82 V\n-112 -18 V\n.139 g 1807 1825 N
      -112 -18 53 -90 123 20 h\n1.000 UP\n1.000 UL\nLT2\n1807 1825 M\n64 -88
      V\n112 18 V\n-53 90 V\n-123 -20 V\n.1714 g 1749 1919 N -123 -20 48 -95
      133 21 h\n1.000 UP\n1.000 UL\nLT2\n1749 1919 M\n58 -94 V\n123 20 V\n-48
      95 V\n-133 -21 V\n.2064 g 1697 2018 N -133 -21 43 -101 142 23 h\n1.000
      UP\n1.000 UL\nLT2\n1697 2018 M\n52 -99 V\n133 21 V\n-43 101 V\n-142 -23
      V\n.2438 g 1651 2122 N -142 -23 38 -105 150 24 h\n1.000 UP\n1.000
      UL\nLT2\n1651 2122 M\n46 -104 V\n142 23 V\n-38 105 V\n-150 -24 V\n.2833
      g 1613 2229 N -150 -24 32 -108 156 25 h\n1.000 UP\n1.000 UL\nLT2\n1613
      2229 M\n38 -107 V\n150 24 V\n-32 108 V\n-156 -25 V\n.3245 g 1582 2340 N
      -156 -25 26 -112 161 26 h\n1.000 UP\n1.000 UL\nLT2\n1582 2340 M\n31
      -111 V\n156 25 V\n-26 112 V\n-161 -26 V\n.3671 g 1558 2452 N -161 -26
      19 -113 166 27 h\n1.000 UP\n1.000 UL\nLT2\n1558 2452 M\n24 -112 V\n161
      26 V\n-19 113 V\n-166 -27 V\n.4108 g 1542 2566 N -166 -27 13 -114 169
      27 h\n1.000 UP\n1.000 UL\nLT2\n1542 2566 M\n16 -114 V\n166 27 V\n-13
      114 V\n-169 -27 V\n.4552 g 1535 2679 N -169 -27 7 -114 169 28 h\n1.000
      UP\n1.000 UL\nLT2\n1535 2679 M\n7 -113 V\n169 27 V\n-7 114 V\n-169 -28
      V\n.5 g 1535 2792 N -169 -28 0 -113 169 28 h\n1.000 UP\n1.000
      UL\nLT2\n1535 2792 M\n0 -113 V\n169 28 V\n0 113 V\n-169 -28 V\n.5448 g
      1542 2904 N -169 -28 -7 -111 169 27 h\n1.000 UP\n1.000 UL\nLT2\n1542
      2904 M\n-7 -112 V\n169 28 V\n7 111 V\n-169 -27 V\n.5892 g 1558 3013 N
      -169 -27 -13 -108 166 26 h\n1.000 UP\n1.000 UL\nLT2\n1558 3013 M\n-16
      -109 V\n169 27 V\n13 108 V\n-166 -26 V\n.6329 g 1582 3118 N -166 -26
      -19 -105 161 26 h\n1.000 UP\n1.000 UL\nLT2\n1582 3118 M\n-24 -105
      V\n166 26 V\n19 105 V\n-161 -26 V\n.6755 g 1613 3219 N -161 -26 -26
      -101 156 26 h\n1.000 UP\n1.000 UL\nLT2\n1613 3219 M\n-31 -101 V\n161 26
      V\n26 101 V\n-156 -26 V\n.7167 g 1651 3315 N -156 -26 -32 -95 150 25
      h\n1.000 UP\n1.000 UL\nLT2\n1651 3315 M\n-38 -96 V\n156 26 V\n32 95
      V\n-150 -25 V\n.7562 g 1697 3406 N -150 -25 -38 -89 142 23 h\n1.000
      UP\n1.000 UL\nLT2\n1697 3406 M\n-46 -91 V\n150 25 V\n38 89 V\n-142 -23
      V\n.7936 g 1749 3489 N -142 -23 -43 -82 133 22 h\n1.000 UP\n1.000
      UL\nLT2\n1749 3489 M\n-52 -83 V\n142 23 V\n43 82 V\n-133 -22 V\n.8286 g
      1807 3566 N -133 -22 -48 -75 123 20 h\n1.000 UP\n1.000 UL\nLT2\n1807
      3566 M\n-58 -77 V\n133 22 V\n48 75 V\n-123 -20 V\n.861 g 1871 3634 N
      -123 -20 -53 -67 112 19 h\n1.000 UP\n1.000 UL\nLT2\n1871 3634 M\n-64
      -68 V\n123 20 V\n53 67 V\n-112 -19 V\n.8905 g 1941 3695 N -112 -19 -58
      -58 100 16 h\n1.000 UP\n1.000 UL\nLT2\n1941 3695 M\n-70 -61 V\n112 19
      V\n58 58 V\n-100 -16 V\n.9169 g 2015 3746 N -100 -16 -61 -49 87 14
      h\n1.000 UP\n1.000 UL\nLT2\n2015 3746 M\n-74 -51 V\n100 16 V\n61 49
      V\n-87 -14 V\n.9399 g 2093 3788 N -87 -14 -64 -40 73 12 h\n1.000
      UP\n1.000 UL\nLT2\n2093 3788 M\n-78 -42 V\n87 14 V\n64 40 V\n-73 -12
      V\n.9593 g 2174 3820 N -73 -12 -68 -30 60 10 h\n1.000 UP\n1.000
      UL\nLT2\n2174 3820 M\n-81 -32 V\n73 12 V\n68 30 V\n-60 -10 V\n.975 g
      2258 3843 N -60 -10 -70 -20 46 7 h\n1.000 UP\n1.000 UL\nLT2\n2258 3843
      M\n-84 -23 V\n60 10 V\n70 20 V\n-46 -7 V\n.987 g 2345 3855 N -46 -7 -71
      -10 30 5 h\n1.000 UP\n1.000 UL\nLT2\n2345 3855 M\n-87 -12 V\n46 7 V\n71
      10 V\n-30 -5 V\n.995 g 2433 3857 N -30 -5 -73 0 15 3 h\n1.000 UP\n1.000
      UL\nLT2\n2433 3857 M\n-88 -2 V\n30 5 V\n73 0 V\n-15 -3 V\n.999 g 2521
      3849 N -15 -3 -73 11 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-88 8
      V\n15 3 V\n73 -11 V\n.001 g 2448 1351 N 0 0 56 -23 17 2 h\n1.000
      UP\n1.000 UL\nLT2\n2448 1351 M\n73 -21 V\n-56 23 V\n-17 -2 V\n.005 g
      2375 1381 N -17 -2 55 -32 35 4 h\n1.000 UP\n1.000 UL\nLT2\n2375 1381
      M\n73 -30 V\n17 2 V\n-55 32 V\n-35 -4 V\n.013 g 2304 1422 N -35 -4 54
      -42 52 5 h\n1.000 UP\n1.000 UL\nLT2\n2304 1422 M\n71 -41 V\n35 4 V\n-54
      42 V\n-52 -5 V\n.025 g 2234 1471 N -52 -5 53 -52 69 8 h\n1.000
      UP\n1.000 UL\nLT2\n2234 1471 M\n70 -49 V\n52 5 V\n-53 52 V\n-69 -8
      V\n.0407 g 2166 1530 N -69 -8 52 -61 85 10 h\n1.000 UP\n1.000
      UL\nLT2\n2166 1530 M\n68 -59 V\n69 8 V\n-52 61 V\n-85 -10 V\n.0601 g
      2102 1597 N -85 -10 49 -69 100 12 h\n1.000 UP\n1.000 UL\nLT2\n2102 1597
      M\n64 -67 V\n85 10 V\n-49 69 V\n-100 -12 V\n.0831 g 2041 1673 N -100
      -12 46 -77 115 13 h\n1.000 UP\n1.000 UL\nLT2\n2041 1673 M\n61 -76
      V\n100 12 V\n-46 77 V\n-115 -13 V\n.1095 g 1983 1755 N -115 -13 44 -84
      129 15 h\n1.000 UP\n1.000 UL\nLT2\n1983 1755 M\n58 -82 V\n115 13 V\n-44
      84 V\n-129 -15 V\n.139 g 1930 1845 N -129 -15 40 -91 142 16 h\n1.000
      UP\n1.000 UL\nLT2\n1930 1845 M\n53 -90 V\n129 15 V\n-40 91 V\n-142 -16
      V\n.1714 g 1882 1940 N -142 -16 37 -96 153 17 h\n1.000 UP\n1.000
      UL\nLT2\n1882 1940 M\n48 -95 V\n142 16 V\n-37 96 V\n-153 -17 V\n.2064 g
      1839 2041 N -153 -17 33 -102 163 18 h\n1.000 UP\n1.000 UL\nLT2\n1839
      2041 M\n43 -101 V\n153 17 V\n-33 102 V\n-163 -18 V\n.2438 g 1801 2146 N
      -163 -18 28 -106 173 19 h\n1.000 UP\n1.000 UL\nLT2\n1801 2146 M\n38
      -105 V\n163 18 V\n-28 106 V\n-173 -19 V\n.2833 g 1769 2254 N -173 -19
      24 -110 181 21 h\n1.000 UP\n1.000 UL\nLT2\n1769 2254 M\n32 -108 V\n173
      19 V\n-24 110 V\n-181 -21 V\n.3245 g 1743 2366 N -181 -21 20 -112 187
      21 h\n1.000 UP\n1.000 UL\nLT2\n1743 2366 M\n26 -112 V\n181 21 V\n-20
      112 V\n-187 -21 V\n.3671 g 1724 2479 N -187 -21 15 -113 191 21 h\n1.000
      UP\n1.000 UL\nLT2\n1724 2479 M\n19 -113 V\n187 21 V\n-15 113 V\n-191
      -21 V\n.4108 g 1711 2593 N -191 -21 10 -115 194 22 h\n1.000 UP\n1.000
      UL\nLT2\n1711 2593 M\n13 -114 V\n191 21 V\n-10 115 V\n-194 -22 V\n.4552
      g 1704 2707 N -194 -22 5 -114 196 22 h\n1.000 UP\n1.000 UL\nLT2\n1704
      2707 M\n7 -114 V\n194 22 V\n-5 114 V\n-196 -22 V\n.5 g 1704 2820 N -196
      -22 0 -113 196 22 h\n1.000 UP\n1.000 UL\nLT2\n1704 2820 M\n0 -113
      V\n196 22 V\n0 113 V\n-196 -22 V\n.5448 g 1711 2931 N -196 -22 -5 -111
      194 22 h\n1.000 UP\n1.000 UL\nLT2\n1711 2931 M\n-7 -111 V\n196 22 V\n5
      111 V\n-194 -22 V\n.5892 g 1724 3039 N -194 -22 -10 -108 191 22
      h\n1.000 UP\n1.000 UL\nLT2\n1724 3039 M\n-13 -108 V\n194 22 V\n10 108
      V\n-191 -22 V\n.6329 g 1743 3144 N -191 -22 -15 -104 187 21 h\n1.000
      UP\n1.000 UL\nLT2\n1743 3144 M\n-19 -105 V\n191 22 V\n15 104 V\n-187
      -21 V\n.6755 g 1769 3245 N -187 -21 -20 -100 181 20 h\n1.000 UP\n1.000
      UL\nLT2\n1769 3245 M\n-26 -101 V\n187 21 V\n20 100 V\n-181 -20 V\n.7167
      g 1801 3340 N -181 -20 -24 -94 173 19 h\n1.000 UP\n1.000 UL\nLT2\n1801
      3340 M\n-32 -95 V\n181 20 V\n24 94 V\n-173 -19 V\n.7562 g 1839 3429 N
      -173 -19 -28 -88 163 18 h\n1.000 UP\n1.000 UL\nLT2\n1839 3429 M\n-38
      -89 V\n173 19 V\n28 88 V\n-163 -18 V\n.7936 g 1882 3511 N -163 -18 -33
      -81 153 17 h\n1.000 UP\n1.000 UL\nLT2\n1882 3511 M\n-43 -82 V\n163 18
      V\n33 81 V\n-153 -17 V\n.8286 g 1930 3586 N -153 -17 -37 -74 142 16
      h\n1.000 UP\n1.000 UL\nLT2\n1930 3586 M\n-48 -75 V\n153 17 V\n37 74
      V\n-142 -16 V\n.861 g 1983 3653 N -142 -16 -40 -65 129 14 h\n1.000
      UP\n1.000 UL\nLT2\n1983 3653 M\n-53 -67 V\n142 16 V\n40 65 V\n-129 -14
      V\n.8905 g 2041 3711 N -129 -14 -44 -57 115 13 h\n1.000 UP\n1.000
      UL\nLT2\n2041 3711 M\n-58 -58 V\n129 14 V\n44 57 V\n-115 -13 V\n.9169 g
      2102 3760 N -115 -13 -46 -47 100 11 h\n1.000 UP\n1.000 UL\nLT2\n2102
      3760 M\n-61 -49 V\n115 13 V\n46 47 V\n-100 -11 V\n.9399 g 2166 3800 N
      -100 -11 -49 -38 85 9 h\n1.000 UP\n1.000 UL\nLT2\n2166 3800 M\n-64 -40
      V\n100 11 V\n49 38 V\n-85 -9 V\n.9593 g 2234 3830 N -85 -9 -52 -29 69 8
      h\n1.000 UP\n1.000 UL\nLT2\n2234 3830 M\n-68 -30 V\n85 9 V\n52 29
      V\n-69 -8 V\n.975 g 2304 3850 N -69 -8 -53 -18 52 6 h\n1.000 UP\n1.000
      UL\nLT2\n2304 3850 M\n-70 -20 V\n69 8 V\n53 18 V\n-52 -6 V\n.987 g 2375
      3860 N -52 -6 -54 -8 35 4 h\n1.000 UP\n1.000 UL\nLT2\n2375 3860 M\n-71
      -10 V\n52 6 V\n54 8 V\n-35 -4 V\n.995 g 2448 3860 N -35 -4 -55 2 17 2
      h\n1.000 UP\n1.000 UL\nLT2\n2448 3860 M\n-73 0 V\n35 4 V\n55 -2 V\n-17
      -2 V\n.999 g 2521 3849 N -17 -2 -56 13 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n-73 11 V\n17 2 V\n56 -13 V\n.001 g 2465 1353 N 0
      0 36 -24 20 1 h\n1.000 UP\n1.000 UL\nLT2\n2465 1353 M\n56 -23 V\n-36 24
      V\n-20 -1 V\n.005 g 2410 1385 N -20 -1 36 -34 39 3 h\n1.000 UP\n1.000
      UL\nLT2\n2410 1385 M\n55 -32 V\n20 1 V\n-36 34 V\n-39 -3 V\n.013 g 2356
      1427 N -39 -3 36 -44 57 5 h\n1.000 UP\n1.000 UL\nLT2\n2356 1427 M\n54
      -42 V\n39 3 V\n-36 44 V\n-57 -5 V\n.025 g 2303 1479 N -57 -5 34 -53 76
      6 h\n1.000 UP\n1.000 UL\nLT2\n2303 1479 M\n53 -52 V\n57 5 V\n-34 53
      V\n-76 -6 V\n.0407 g 2251 1540 N -76 -6 34 -61 94 6 h\n1.000 UP\n1.000
      UL\nLT2\n2251 1540 M\n52 -61 V\n76 6 V\n-34 61 V\n-94 -6 V\n.0601 g
      2202 1609 N -94 -6 32 -71 111 8 h\n1.000 UP\n1.000 UL\nLT2\n2202 1609
      M\n49 -69 V\n94 6 V\n-32 71 V\n-111 -8 V\n.0831 g 2156 1686 N -111 -8
      30 -78 127 9 h\n1.000 UP\n1.000 UL\nLT2\n2156 1686 M\n46 -77 V\n111 8
      V\n-30 78 V\n-127 -9 V\n.1095 g 2112 1770 N -127 -9 29 -85 142 10
      h\n1.000 UP\n1.000 UL\nLT2\n2112 1770 M\n44 -84 V\n127 9 V\n-29 85
      V\n-142 -10 V\n.139 g 2072 1861 N -142 -10 26 -92 156 11 h\n1.000
      UP\n1.000 UL\nLT2\n2072 1861 M\n40 -91 V\n142 10 V\n-26 92 V\n-156 -11
      V\n.1714 g 2035 1957 N -156 -11 24 -98 169 13 h\n1.000 UP\n1.000
      UL\nLT2\n2035 1957 M\n37 -96 V\n156 11 V\n-24 98 V\n-169 -13 V\n.2064 g
      2002 2059 N -169 -13 21 -102 181 13 h\n1.000 UP\n1.000 UL\nLT2\n2002
      2059 M\n33 -102 V\n169 13 V\n-21 102 V\n-181 -13 V\n.2438 g 1974 2165 N
      -181 -13 19 -107 190 14 h\n1.000 UP\n1.000 UL\nLT2\n1974 2165 M\n28
      -106 V\n181 13 V\n-19 107 V\n-190 -14 V\n.2833 g 1950 2275 N -190 -14
      16 -110 198 14 h\n1.000 UP\n1.000 UL\nLT2\n1950 2275 M\n24 -110 V\n190
      14 V\n-16 110 V\n-198 -14 V\n.3245 g 1930 2387 N -198 -14 12 -113 206
      15 h\n1.000 UP\n1.000 UL\nLT2\n1930 2387 M\n20 -112 V\n198 14 V\n-12
      113 V\n-206 -15 V\n.3671 g 1915 2500 N -206 -15 10 -114 211 16 h\n1.000
      UP\n1.000 UL\nLT2\n1915 2500 M\n15 -113 V\n206 15 V\n-10 114 V\n-211
      -16 V\n.4108 g 1905 2615 N -211 -16 6 -114 215 15 h\n1.000 UP\n1.000
      UL\nLT2\n1905 2615 M\n10 -115 V\n211 16 V\n-6 114 V\n-215 -15 V\n.4552
      g 1900 2729 N -215 -15 4 -114 216 15 h\n1.000 UP\n1.000 UL\nLT2\n1900
      2729 M\n5 -114 V\n215 15 V\n-4 114 V\n-216 -15 V\n.5 g 1900 2842 N -216
      -15 0 -113 216 15 h\n1.000 UP\n1.000 UL\nLT2\n1900 2842 M\n0 -113
      V\n216 15 V\n0 113 V\n-216 -15 V\n.5448 g 1905 2953 N -216 -15 -4 -111
      215 15 h\n1.000 UP\n1.000 UL\nLT2\n1905 2953 M\n-5 -111 V\n216 15 V\n4
      111 V\n-215 -15 V\n.5892 g 1915 3061 N -215 -15 -6 -108 211 15 h\n1.000
      UP\n1.000 UL\nLT2\n1915 3061 M\n-10 -108 V\n215 15 V\n6 108 V\n-211 -15
      V\n.6329 g 1930 3165 N -211 -15 -10 -104 206 15 h\n1.000 UP\n1.000
      UL\nLT2\n1930 3165 M\n-15 -104 V\n211 15 V\n10 104 V\n-206 -15 V\n.6755
      g 1950 3265 N -206 -15 -12 -99 198 14 h\n1.000 UP\n1.000 UL\nLT2\n1950
      3265 M\n-20 -100 V\n206 15 V\n12 99 V\n-198 -14 V\n.7167 g 1974 3359 N
      -198 -14 -16 -94 190 14 h\n1.000 UP\n1.000 UL\nLT2\n1974 3359 M\n-24
      -94 V\n198 14 V\n16 94 V\n-190 -14 V\n.7562 g 2002 3447 N -190 -14 -19
      -87 181 13 h\n1.000 UP\n1.000 UL\nLT2\n2002 3447 M\n-28 -88 V\n190 14
      V\n19 87 V\n-181 -13 V\n.7936 g 2035 3528 N -181 -13 -21 -80 169 12
      h\n1.000 UP\n1.000 UL\nLT2\n2035 3528 M\n-33 -81 V\n181 13 V\n21 80
      V\n-169 -12 V\n.8286 g 2072 3602 N -169 -12 -24 -73 156 11 h\n1.000
      UP\n1.000 UL\nLT2\n2072 3602 M\n-37 -74 V\n169 12 V\n24 73 V\n-156 -11
      V\n.861 g 2112 3667 N -156 -11 -26 -64 142 10 h\n1.000 UP\n1.000
      UL\nLT2\n2112 3667 M\n-40 -65 V\n156 11 V\n26 64 V\n-142 -10 V\n.8905 g
      2156 3724 N -142 -10 -29 -56 127 9 h\n1.000 UP\n1.000 UL\nLT2\n2156
      3724 M\n-44 -57 V\n142 10 V\n29 56 V\n-127 -9 V\n.9169 g 2202 3771 N
      -127 -9 -30 -46 111 8 h\n1.000 UP\n1.000 UL\nLT2\n2202 3771 M\n-46 -47
      V\n127 9 V\n30 46 V\n-111 -8 V\n.9399 g 2251 3809 N -111 -8 -32 -37 94
      7 h\n1.000 UP\n1.000 UL\nLT2\n2251 3809 M\n-49 -38 V\n111 8 V\n32 37
      V\n-94 -7 V\n.9593 g 2303 3838 N -94 -7 -34 -27 76 5 h\n1.000 UP\n1.000
      UL\nLT2\n2303 3838 M\n-52 -29 V\n94 7 V\n34 27 V\n-76 -5 V\n.975 g 2356
      3856 N -76 -5 -34 -17 57 4 h\n1.000 UP\n1.000 UL\nLT2\n2356 3856 M\n-53
      -18 V\n76 5 V\n34 17 V\n-57 -4 V\n.987 g 2410 3864 N -57 -4 -36 -7 39 3
      h\n1.000 UP\n1.000 UL\nLT2\n2410 3864 M\n-54 -8 V\n57 4 V\n36 7 V\n-39
      -3 V\n.995 g 2465 3862 N -39 -3 -36 4 20 1 h\n1.000 UP\n1.000
      UL\nLT2\n2465 3862 M\n-55 2 V\n39 3 V\n36 -4 V\n-20 -1 V\n.999 g 2521
      3849 N -20 -1 -36 14 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-56
      13 V\n20 1 V\n36 -14 V\n.001 g 2485 1354 N 0 0 16 -25 20 1 h\n1.000
      UP\n1.000 UL\nLT2\n2485 1354 M\n36 -24 V\n-16 25 V\n-20 -1 V\n.005 g
      2449 1388 N -20 -1 15 -35 41 2 h\n1.000 UP\n1.000 UL\nLT2\n2449 1388
      M\n36 -34 V\n20 1 V\n-15 35 V\n-41 -2 V\n.013 g 2413 1432 N -41 -2 16
      -44 61 2 h\n1.000 UP\n1.000 UL\nLT2\n2413 1432 M\n36 -44 V\n41 2 V\n-16
      44 V\n-61 -2 V\n.025 g 2379 1485 N -61 -2 15 -54 80 3 h\n1.000
      UP\n1.000 UL\nLT2\n2379 1485 M\n34 -53 V\n61 2 V\n-15 54 V\n-80 -3
      V\n.0407 g 2345 1546 N -80 -3 14 -62 100 4 h\n1.000 UP\n1.000
      UL\nLT2\n2345 1546 M\n34 -61 V\n80 3 V\n-14 62 V\n-100 -4 V\n.0601 g
      2313 1617 N -100 -4 14 -71 118 4 h\n1.000 UP\n1.000 UL\nLT2\n2313 1617
      M\n32 -71 V\n100 4 V\n-14 71 V\n-118 -4 V\n.0831 g 2283 1695 N -118 -4
      13 -79 135 5 h\n1.000 UP\n1.000 UL\nLT2\n2283 1695 M\n30 -78 V\n118 4
      V\n-13 79 V\n-135 -5 V\n.1095 g 2254 1780 N -135 -5 13 -86 151 6
      h\n1.000 UP\n1.000 UL\nLT2\n2254 1780 M\n29 -85 V\n135 5 V\n-13 86
      V\n-151 -6 V\n.139 g 2228 1872 N -151 -6 11 -92 166 6 h\n1.000
      UP\n1.000 UL\nLT2\n2228 1872 M\n26 -92 V\n151 6 V\n-11 92 V\n-166 -6
      V\n.1714 g 2204 1970 N -166 -6 10 -99 180 7 h\n1.000 UP\n1.000
      UL\nLT2\n2204 1970 M\n24 -98 V\n166 6 V\n-10 99 V\n-180 -7 V\n.2064 g
      2183 2072 N -180 -7 10 -103 191 8 h\n1.000 UP\n1.000 UL\nLT2\n2183 2072
      M\n21 -102 V\n180 7 V\n-10 103 V\n-191 -8 V\n.2438 g 2164 2179 N -191
      -8 8 -107 202 8 h\n1.000 UP\n1.000 UL\nLT2\n2164 2179 M\n19 -107 V\n191
      8 V\n-8 107 V\n-202 -8 V\n.2833 g 2148 2289 N -202 -8 7 -110 211 8
      h\n1.000 UP\n1.000 UL\nLT2\n2148 2289 M\n16 -110 V\n202 8 V\n-7 110
      V\n-211 -8 V\n.3245 g 2136 2402 N -211 -8 5 -113 218 8 h\n1.000
      UP\n1.000 UL\nLT2\n2136 2402 M\n12 -113 V\n211 8 V\n-5 113 V\n-218 -8
      V\n.3671 g 2126 2516 N -218 -8 4 -114 224 8 h\n1.000 UP\n1.000
      UL\nLT2\n2126 2516 M\n10 -114 V\n218 8 V\n-4 114 V\n-224 -8 V\n.4108 g
      2120 2630 N -224 -8 3 -115 227 9 h\n1.000 UP\n1.000 UL\nLT2\n2120 2630
      M\n6 -114 V\n224 8 V\n-3 115 V\n-227 -9 V\n.4552 g 2116 2744 N -227 -9
      2 -114 229 9 h\n1.000 UP\n1.000 UL\nLT2\n2116 2744 M\n4 -114 V\n227 9
      V\n-2 114 V\n-229 -9 V\n.5 g 2116 2857 N -229 -9 0 -113 229 9 h\n1.000
      UP\n1.000 UL\nLT2\n2116 2857 M\n0 -113 V\n229 9 V\n0 113 V\n-229 -9
      V\n.5448 g 2120 2968 N -229 -9 -2 -111 227 9 h\n1.000 UP\n1.000
      UL\nLT2\n2120 2968 M\n-4 -111 V\n229 9 V\n2 111 V\n-227 -9 V\n.5892 g
      2126 3076 N -227 -9 -3 -108 224 9 h\n1.000 UP\n1.000 UL\nLT2\n2126 3076
      M\n-6 -108 V\n227 9 V\n3 108 V\n-224 -9 V\n.6329 g 2136 3180 N -224 -9
      -4 -103 218 8 h\n1.000 UP\n1.000 UL\nLT2\n2136 3180 M\n-10 -104 V\n224
      9 V\n4 103 V\n-218 -8 V\n.6755 g 2148 3279 N -218 -8 -5 -99 211 8
      h\n1.000 UP\n1.000 UL\nLT2\n2148 3279 M\n-12 -99 V\n218 8 V\n5 99
      V\n-211 -8 V\n.7167 g 2164 3373 N -211 -8 -7 -94 202 8 h\n1.000
      UP\n1.000 UL\nLT2\n2164 3373 M\n-16 -94 V\n211 8 V\n7 94 V\n-202 -8
      V\n.7562 g 2183 3460 N -202 -8 -8 -86 191 7 h\n1.000 UP\n1.000
      UL\nLT2\n2183 3460 M\n-19 -87 V\n202 8 V\n8 86 V\n-191 -7 V\n.7936 g
      2204 3540 N -191 -7 -10 -80 180 7 h\n1.000 UP\n1.000 UL\nLT2\n2204 3540
      M\n-21 -80 V\n191 7 V\n10 80 V\n-180 -7 V\n.8286 g 2228 3613 N -180 -7
      -10 -72 166 6 h\n1.000 UP\n1.000 UL\nLT2\n2228 3613 M\n-24 -73 V\n180 7
      V\n10 72 V\n-166 -6 V\n.861 g 2254 3677 N -166 -6 -11 -64 151 6
      h\n1.000 UP\n1.000 UL\nLT2\n2254 3677 M\n-26 -64 V\n166 6 V\n11 64
      V\n-151 -6 V\n.8905 g 2283 3733 N -151 -6 -13 -55 135 5 h\n1.000
      UP\n1.000 UL\nLT2\n2283 3733 M\n-29 -56 V\n151 6 V\n13 55 V\n-135 -5
      V\n.9169 g 2313 3779 N -135 -5 -13 -46 118 5 h\n1.000 UP\n1.000
      UL\nLT2\n2313 3779 M\n-30 -46 V\n135 5 V\n13 46 V\n-118 -5 V\n.9399 g
      2345 3816 N -118 -5 -14 -36 100 4 h\n1.000 UP\n1.000 UL\nLT2\n2345 3816
      M\n-32 -37 V\n118 5 V\n14 36 V\n-100 -4 V\n.9593 g 2379 3843 N -100 -4
      -14 -26 80 3 h\n1.000 UP\n1.000 UL\nLT2\n2379 3843 M\n-34 -27 V\n100 4
      V\n14 26 V\n-80 -3 V\n.975 g 2413 3860 N -80 -3 -15 -16 61 2 h\n1.000
      UP\n1.000 UL\nLT2\n2413 3860 M\n-34 -17 V\n80 3 V\n15 16 V\n-61 -2
      V\n.987 g 2449 3867 N -61 -2 -16 -6 41 1 h\n1.000 UP\n1.000
      UL\nLT2\n2449 3867 M\n-36 -7 V\n61 2 V\n16 6 V\n-41 -1 V\n.995 g 2485
      3863 N -41 -1 -15 4 20 1 h\n1.000 UP\n1.000 UL\nLT2\n2485 3863 M\n-36 4
      V\n41 1 V\n15 -4 V\n-20 -1 V\n.999 g 2521 3849 N -20 -1 -16 15 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-36 14 V\n20 1 V\n16 -15
      V\n.001 g 2505 1355 N 0 0 -5 -25 21 0 h\n1.000 UP\n1.000 UL\nLT2\n2505
      1355 M\n16 -25 V\n5 25 V\n-21 0 V\n.005 g 2490 1390 N -21 0 -6 -35 42 0
      h\n1.000 UP\n1.000 UL\nLT2\n2490 1390 M\n15 -35 V\n21 0 V\n6 35 V\n-42
      0 V\n.013 g 2474 1434 N -42 0 -5 -44 63 0 h\n1.000 UP\n1.000
      UL\nLT2\n2474 1434 M\n16 -44 V\n42 0 V\n5 44 V\n-63 0 V\n.025 g 2459
      1488 N -63 0 -5 -54 83 0 h\n1.000 UP\n1.000 UL\nLT2\n2459 1488 M\n15
      -54 V\n63 0 V\n5 54 V\n-83 0 V\n.0407 g 2445 1550 N -83 0 -5 -63 102 1
      h\n1.000 UP\n1.000 UL\nLT2\n2445 1550 M\n14 -62 V\n83 0 V\n5 63 V\n-102
      -1 V\n.0601 g 2431 1621 N -102 -1 -4 -71 120 1 h\n1.000 UP\n1.000
      UL\nLT2\n2431 1621 M\n14 -71 V\n102 1 V\n4 71 V\n-120 -1 V\n.0831 g
      2418 1700 N -120 -1 -5 -79 138 1 h\n1.000 UP\n1.000 UL\nLT2\n2418 1700
      M\n13 -79 V\n120 1 V\n5 79 V\n-138 -1 V\n.1095 g 2405 1786 N -138 -1 -4
      -86 155 1 h\n1.000 UP\n1.000 UL\nLT2\n2405 1786 M\n13 -86 V\n138 1 V\n4
      86 V\n-155 -1 V\n.139 g 2394 1878 N -155 -1 -4 -93 170 2 h\n1.000
      UP\n1.000 UL\nLT2\n2394 1878 M\n11 -92 V\n155 1 V\n4 93 V\n-170 -2
      V\n.1714 g 2384 1977 N -170 -2 -3 -98 183 1 h\n1.000 UP\n1.000
      UL\nLT2\n2384 1977 M\n10 -99 V\n170 2 V\n3 98 V\n-183 -1 V\n.2064 g
      2374 2080 N -183 -1 -3 -103 196 1 h\n1.000 UP\n1.000 UL\nLT2\n2374 2080
      M\n10 -103 V\n183 1 V\n3 103 V\n-196 -1 V\n.2438 g 2366 2187 N -196 -1
      -3 -107 207 1 h\n1.000 UP\n1.000 UL\nLT2\n2366 2187 M\n8 -107 V\n196 1
      V\n3 107 V\n-207 -1 V\n.2833 g 2359 2297 N -207 -1 -2 -111 216 2
      h\n1.000 UP\n1.000 UL\nLT2\n2359 2297 M\n7 -110 V\n207 1 V\n2 111
      V\n-216 -2 V\n.3245 g 2354 2410 N -216 -2 -2 -113 223 2 h\n1.000
      UP\n1.000 UL\nLT2\n2354 2410 M\n5 -113 V\n216 2 V\n2 113 V\n-223 -2
      V\n.3671 g 2350 2524 N -223 -2 -2 -114 229 2 h\n1.000 UP\n1.000
      UL\nLT2\n2350 2524 M\n4 -114 V\n223 2 V\n2 114 V\n-229 -2 V\n.4108 g
      2347 2639 N -229 -2 0 -115 232 2 h\n1.000 UP\n1.000 UL\nLT2\n2347 2639
      M\n3 -115 V\n229 2 V\n0 115 V\n-232 -2 V\n.4552 g 2345 2753 N -232 -2
      -1 -114 235 2 h\n1.000 UP\n1.000 UL\nLT2\n2345 2753 M\n2 -114 V\n232 2
      V\n1 114 V\n-235 -2 V\n.5 g 2345 2866 N -235 -2 0 -113 235 2 h\n1.000
      UP\n1.000 UL\nLT2\n2345 2866 M\n0 -113 V\n235 2 V\n0 113 V\n-235 -2
      V\n.5448 g 2347 2977 N -235 -2 1 -111 232 2 h\n1.000 UP\n1.000
      UL\nLT2\n2347 2977 M\n-2 -111 V\n235 2 V\n-1 111 V\n-232 -2 V\n.5892 g
      2350 3085 N -232 -2 0 -108 229 2 h\n1.000 UP\n1.000 UL\nLT2\n2350 3085
      M\n-3 -108 V\n232 2 V\n0 108 V\n-229 -2 V\n.6329 g 2354 3188 N -229 -2
      2 -103 223 2 h\n1.000 UP\n1.000 UL\nLT2\n2354 3188 M\n-4 -103 V\n229 2
      V\n-2 103 V\n-223 -2 V\n.6755 g 2359 3287 N -223 -2 2 -99 216 2
      h\n1.000 UP\n1.000 UL\nLT2\n2359 3287 M\n-5 -99 V\n223 2 V\n-2 99
      V\n-216 -2 V\n.7167 g 2366 3381 N -216 -2 2 -93 207 1 h\n1.000
      UP\n1.000 UL\nLT2\n2366 3381 M\n-7 -94 V\n216 2 V\n-2 93 V\n-207 -1
      V\n.7562 g 2374 3467 N -207 -1 3 -87 196 2 h\n1.000 UP\n1.000
      UL\nLT2\n2374 3467 M\n-8 -86 V\n207 1 V\n-3 87 V\n-196 -2 V\n.7936 g
      2384 3547 N -196 -2 3 -80 183 2 h\n1.000 UP\n1.000 UL\nLT2\n2384 3547
      M\n-10 -80 V\n196 2 V\n-3 80 V\n-183 -2 V\n.8286 g 2394 3619 N -183 -2
      3 -72 170 2 h\n1.000 UP\n1.000 UL\nLT2\n2394 3619 M\n-10 -72 V\n183 2
      V\n-3 72 V\n-170 -2 V\n.861 g 2405 3683 N -170 -2 4 -63 155 1 h\n1.000
      UP\n1.000 UL\nLT2\n2405 3683 M\n-11 -64 V\n170 2 V\n-4 63 V\n-155 -1
      V\n.8905 g 2418 3738 N -155 -1 4 -55 138 1 h\n1.000 UP\n1.000
      UL\nLT2\n2418 3738 M\n-13 -55 V\n155 1 V\n-4 55 V\n-138 -1 V\n.9169 g
      2431 3784 N -138 -1 5 -46 120 1 h\n1.000 UP\n1.000 UL\nLT2\n2431 3784
      M\n-13 -46 V\n138 1 V\n-5 46 V\n-120 -1 V\n.9399 g 2445 3820 N -120 -1
      4 -36 102 1 h\n1.000 UP\n1.000 UL\nLT2\n2445 3820 M\n-14 -36 V\n120 1
      V\n-4 36 V\n-102 -1 V\n.9593 g 2459 3846 N -102 -1 5 -26 83 1 h\n1.000
      UP\n1.000 UL\nLT2\n2459 3846 M\n-14 -26 V\n102 1 V\n-5 26 V\n-83 -1
      V\n.975 g 2474 3862 N -83 -1 5 -16 63 1 h\n1.000 UP\n1.000
      UL\nLT2\n2474 3862 M\n-15 -16 V\n83 1 V\n-5 16 V\n-63 -1 V\n.987 g 2490
      3868 N -63 -1 5 -6 42 1 h\n1.000 UP\n1.000 UL\nLT2\n2490 3868 M\n-16 -6
      V\n63 1 V\n-5 6 V\n-42 -1 V\n.995 g 2505 3864 N -42 -1 6 5 21 0
      h\n1.000 UP\n1.000 UL\nLT2\n2505 3864 M\n-15 4 V\n42 1 V\n-6 -5 V\n-21
      0 V\n.999 g 2521 3849 N -21 0 5 15 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521
      3849 M\n-16 15 V\n21 0 V\n-5 -15 V\n.001 g 2526 1355 N 0 0 -26 -24 21
      -1 h\n1.000 UP\n1.000 UL\nLT2\n2526 1355 M\n-5 -25 V\n26 24 V\n-21 1
      V\n.005 g 2532 1390 N -21 1 -26 -35 41 -1 h\n1.000 UP\n1.000
      UL\nLT2\n2532 1390 M\n-6 -35 V\n21 -1 V\n26 35 V\n-41 1 V\n.013 g 2537
      1434 N -41 1 -26 -44 62 -1 h\n1.000 UP\n1.000 UL\nLT2\n2537 1434 M\n-5
      -44 V\n41 -1 V\n26 44 V\n-62 1 V\n.025 g 2542 1488 N -62 1 -25 -53 82
      -2 h\n1.000 UP\n1.000 UL\nLT2\n2542 1488 M\n-5 -54 V\n62 -1 V\n25 53
      V\n-82 2 V\n.0407 g 2547 1551 N -82 2 -24 -63 101 -2 h\n1.000 UP\n1.000
      UL\nLT2\n2547 1551 M\n-5 -63 V\n82 -2 V\n24 63 V\n-101 2 V\n.0601 g
      2551 1622 N -101 2 -23 -71 120 -2 h\n1.000 UP\n1.000 UL\nLT2\n2551 1622
      M\n-4 -71 V\n101 -2 V\n23 71 V\n-120 2 V\n.0831 g 2556 1701 N -120 2
      -22 -78 137 -3 h\n1.000 UP\n1.000 UL\nLT2\n2556 1701 M\n-5 -79 V\n120
      -2 V\n22 78 V\n-137 3 V\n.1095 g 2560 1787 N -137 3 -20 -86 153 -3
      h\n1.000 UP\n1.000 UL\nLT2\n2560 1787 M\n-4 -86 V\n137 -3 V\n20 86
      V\n-153 3 V\n.139 g 2564 1880 N -153 3 -19 -92 168 -4 h\n1.000
      UP\n1.000 UL\nLT2\n2564 1880 M\n-4 -93 V\n153 -3 V\n19 92 V\n-168 4
      V\n.1714 g 2567 1978 N -168 4 -17 -98 182 -4 h\n1.000 UP\n1.000
      UL\nLT2\n2567 1978 M\n-3 -98 V\n168 -4 V\n17 98 V\n-182 4 V\n.2064 g
      2570 2081 N -182 4 -16 -103 195 -4 h\n1.000 UP\n1.000 UL\nLT2\n2570
      2081 M\n-3 -103 V\n182 -4 V\n16 103 V\n-195 4 V\n.2438 g 2573 2188 N
      -195 4 -13 -107 205 -4 h\n1.000 UP\n1.000 UL\nLT2\n2573 2188 M\n-3 -107
      V\n195 -4 V\n13 107 V\n-205 4 V\n.2833 g 2575 2299 N -205 4 -11 -110
      214 -5 h\n1.000 UP\n1.000 UL\nLT2\n2575 2299 M\n-2 -111 V\n205 -4 V\n11
      110 V\n-214 5 V\n.3245 g 2577 2412 N -214 5 -10 -113 222 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2577 2412 M\n-2 -113 V\n214 -5 V\n10 113 V\n-222 5
      V\n.3671 g 2579 2526 N -222 5 -7 -114 227 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2579 2526 M\n-2 -114 V\n222 -5 V\n7 114 V\n-227 5 V\n.4108 g
      2579 2641 N -227 5 -4 -114 231 -6 h\n1.000 UP\n1.000 UL\nLT2\n2579 2641
      M\n0 -115 V\n227 -5 V\n4 114 V\n-231 6 V\n.4552 g 2580 2755 N -231 6 -3
      -115 233 -5 h\n1.000 UP\n1.000 UL\nLT2\n2580 2755 M\n-1 -114 V\n231 -6
      V\n3 115 V\n-233 5 V\n.5 g 2580 2868 N -233 5 0 -113 233 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2580 2868 M\n0 -113 V\n233 -5 V\n0 113 V\n-233 5
      V\n.5448 g 2579 2979 N -233 5 3 -111 231 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2579 2979 M\n1 -111 V\n233 -5 V\n-3 111 V\n-231 5 V\n.5892 g
      2579 3087 N -231 5 4 -107 227 -6 h\n1.000 UP\n1.000 UL\nLT2\n2579 3087
      M\n0 -108 V\n231 -5 V\n-4 107 V\n-227 6 V\n.6329 g 2577 3190 N -227 6 7
      -104 222 -5 h\n1.000 UP\n1.000 UL\nLT2\n2577 3190 M\n2 -103 V\n227 -6
      V\n-7 104 V\n-222 5 V\n.6755 g 2575 3289 N -222 5 10 -99 214 -5
      h\n1.000 UP\n1.000 UL\nLT2\n2575 3289 M\n2 -99 V\n222 -5 V\n-10 99
      V\n-214 5 V\n.7167 g 2573 3382 N -214 5 11 -93 205 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2573 3382 M\n2 -93 V\n214 -5 V\n-11 93 V\n-205 5
      V\n.7562 g 2570 3469 N -205 5 13 -87 195 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2570 3469 M\n3 -87 V\n205 -5 V\n-13 87 V\n-195 5 V\n.7936 g
      2567 3549 N -195 5 16 -80 182 -5 h\n1.000 UP\n1.000 UL\nLT2\n2567 3549
      M\n3 -80 V\n195 -5 V\n-16 80 V\n-182 5 V\n.8286 g 2564 3621 N -182 5 17
      -73 168 -4 h\n1.000 UP\n1.000 UL\nLT2\n2564 3621 M\n3 -72 V\n182 -5
      V\n-17 73 V\n-168 4 V\n.861 g 2560 3684 N -168 4 19 -64 153 -3 h\n1.000
      UP\n1.000 UL\nLT2\n2560 3684 M\n4 -63 V\n168 -4 V\n-19 64 V\n-153 3
      V\n.8905 g 2556 3739 N -153 3 20 -55 137 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2556 3739 M\n4 -55 V\n153 -3 V\n-20 55 V\n-137 3 V\n.9169 g
      2551 3785 N -137 3 22 -46 120 -3 h\n1.000 UP\n1.000 UL\nLT2\n2551 3785
      M\n5 -46 V\n137 -3 V\n-22 46 V\n-120 3 V\n.9399 g 2547 3821 N -120 3 23
      -36 101 -3 h\n1.000 UP\n1.000 UL\nLT2\n2547 3821 M\n4 -36 V\n120 -3
      V\n-23 36 V\n-101 3 V\n.9593 g 2542 3847 N -101 3 24 -27 82 -2 h\n1.000
      UP\n1.000 UL\nLT2\n2542 3847 M\n5 -26 V\n101 -3 V\n-24 27 V\n-82 2
      V\n.975 g 2537 3863 N -82 2 25 -16 62 -2 h\n1.000 UP\n1.000
      UL\nLT2\n2537 3863 M\n5 -16 V\n82 -2 V\n-25 16 V\n-62 2 V\n.987 g 2532
      3869 N -62 2 26 -7 41 -1 h\n1.000 UP\n1.000 UL\nLT2\n2532 3869 M\n5 -6
      V\n62 -2 V\n-26 7 V\n-41 1 V\n.995 g 2526 3864 N -41 1 26 5 21 -1
      h\n1.000 UP\n1.000 UL\nLT2\n2526 3864 M\n6 5 V\n41 -1 V\n-26 -5 V\n-21
      1 V\n.999 g 2521 3849 N -21 1 26 14 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n5 15 V\n21 -1 V\n-26 -14 V\n.001 g 2547 1354 N 0
      0 -46 -23 20 -1 h\n1.000 UP\n1.000 UL\nLT2\n2547 1354 M\n-26 -24 V\n46
      23 V\n-20 1 V\n.005 g 2573 1389 N -20 1 -46 -34 40 -2 h\n1.000
      UP\n1.000 UL\nLT2\n2573 1389 M\n-26 -35 V\n20 -1 V\n46 34 V\n-40 2
      V\n.013 g 2599 1433 N -40 2 -45 -43 59 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2599 1433 M\n-26 -44 V\n40 -2 V\n45 43 V\n-59 3 V\n.025 g 2624
      1486 N -59 3 -44 -52 78 -4 h\n1.000 UP\n1.000 UL\nLT2\n2624 1486 M\n-25
      -53 V\n59 -3 V\n44 52 V\n-78 4 V\n.0407 g 2648 1549 N -78 4 -43 -61 97
      -6 h\n1.000 UP\n1.000 UL\nLT2\n2648 1549 M\n-24 -63 V\n78 -4 V\n43 61
      V\n-97 6 V\n.0601 g 2671 1620 N -97 6 -41 -70 115 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2671 1620 M\n-23 -71 V\n97 -6 V\n41 70 V\n-115 7 V\n.0831 g
      2693 1698 N -115 7 -38 -78 131 -7 h\n1.000 UP\n1.000 UL\nLT2\n2693 1698
      M\n-22 -78 V\n115 -7 V\n38 78 V\n-131 7 V\n.1095 g 2713 1784 N -131 7
      -36 -85 147 -8 h\n1.000 UP\n1.000 UL\nLT2\n2713 1784 M\n-20 -86 V\n131
      -7 V\n36 85 V\n-147 8 V\n.139 g 2732 1876 N -147 8 -34 -91 162 -9
      h\n1.000 UP\n1.000 UL\nLT2\n2732 1876 M\n-19 -92 V\n147 -8 V\n34 91
      V\n-162 9 V\n.1714 g 2749 1974 N -162 9 -30 -97 175 -10 h\n1.000
      UP\n1.000 UL\nLT2\n2749 1974 M\n-17 -98 V\n162 -9 V\n30 97 V\n-175 10
      V\n.2064 g 2765 2077 N -175 10 -27 -102 186 -11 h\n1.000 UP\n1.000
      UL\nLT2\n2765 2077 M\n-16 -103 V\n175 -10 V\n27 102 V\n-186 11 V\n.2438
      g 2778 2184 N -186 11 -24 -107 197 -11 h\n1.000 UP\n1.000 UL\nLT2\n2778
      2184 M\n-13 -107 V\n186 -11 V\n24 107 V\n-197 11 V\n.2833 g 2789 2294 N
      -197 11 -20 -110 206 -11 h\n1.000 UP\n1.000 UL\nLT2\n2789 2294 M\n-11
      -110 V\n197 -11 V\n20 110 V\n-206 11 V\n.3245 g 2799 2407 N -206 11 -16
      -112 212 -12 h\n1.000 UP\n1.000 UL\nLT2\n2799 2407 M\n-10 -113 V\n206
      -11 V\n16 112 V\n-212 12 V\n.3671 g 2806 2521 N -212 12 -13 -114 218
      -12 h\n1.000 UP\n1.000 UL\nLT2\n2806 2521 M\n-7 -114 V\n212 -12 V\n13
      114 V\n-218 12 V\n.4108 g 2810 2635 N -218 12 -8 -114 222 -12 h\n1.000
      UP\n1.000 UL\nLT2\n2810 2635 M\n-4 -114 V\n218 -12 V\n8 114 V\n-222 12
      V\n.4552 g 2813 2750 N -222 12 -4 -114 223 -13 h\n1.000 UP\n1.000
      UL\nLT2\n2813 2750 M\n-3 -115 V\n222 -12 V\n4 114 V\n-223 13 V\n.5 g
      2813 2863 N -223 13 0 -113 223 -13 h\n1.000 UP\n1.000 UL\nLT2\n2813
      2863 M\n0 -113 V\n223 -13 V\n0 113 V\n-223 13 V\n.5448 g 2810 2974 N
      -223 13 4 -111 222 -13 h\n1.000 UP\n1.000 UL\nLT2\n2810 2974 M\n3 -111
      V\n223 -13 V\n-4 111 V\n-222 13 V\n.5892 g 2806 3081 N -222 13 8 -108
      218 -12 h\n1.000 UP\n1.000 UL\nLT2\n2806 3081 M\n4 -107 V\n222 -13
      V\n-8 108 V\n-218 12 V\n.6329 g 2799 3185 N -218 12 13 -104 212 -12
      h\n1.000 UP\n1.000 UL\nLT2\n2799 3185 M\n7 -104 V\n218 -12 V\n-13 104
      V\n-212 12 V\n.6755 g 2789 3284 N -212 12 16 -100 206 -11 h\n1.000
      UP\n1.000 UL\nLT2\n2789 3284 M\n10 -99 V\n212 -12 V\n-16 100 V\n-206 11
      V\n.7167 g 2778 3377 N -206 11 20 -94 197 -10 h\n1.000 UP\n1.000
      UL\nLT2\n2778 3377 M\n11 -93 V\n206 -11 V\n-20 94 V\n-197 10 V\n.7562 g
      2765 3464 N -197 10 24 -87 186 -10 h\n1.000 UP\n1.000 UL\nLT2\n2765
      3464 M\n13 -87 V\n197 -10 V\n-24 87 V\n-186 10 V\n.7936 g 2749 3544 N
      -186 10 27 -81 175 -9 h\n1.000 UP\n1.000 UL\nLT2\n2749 3544 M\n16 -80
      V\n186 -10 V\n-27 81 V\n-175 9 V\n.8286 g 2732 3617 N -175 9 30 -73 162
      -9 h\n1.000 UP\n1.000 UL\nLT2\n2732 3617 M\n17 -73 V\n175 -9 V\n-30 73
      V\n-162 9 V\n.861 g 2713 3681 N -162 9 34 -65 147 -8 h\n1.000 UP\n1.000
      UL\nLT2\n2713 3681 M\n19 -64 V\n162 -9 V\n-34 65 V\n-147 8 V\n.8905 g
      2693 3736 N -147 8 36 -56 131 -7 h\n1.000 UP\n1.000 UL\nLT2\n2693 3736
      M\n20 -55 V\n147 -8 V\n-36 56 V\n-131 7 V\n.9169 g 2671 3782 N -131 7
      38 -47 115 -6 h\n1.000 UP\n1.000 UL\nLT2\n2671 3782 M\n22 -46 V\n131 -7
      V\n-38 47 V\n-115 6 V\n.9399 g 2648 3818 N -115 6 41 -37 97 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2648 3818 M\n23 -36 V\n115 -6 V\n-41 37 V\n-97 5
      V\n.9593 g 2624 3845 N -97 5 43 -28 78 -4 h\n1.000 UP\n1.000
      UL\nLT2\n2624 3845 M\n24 -27 V\n97 -5 V\n-43 28 V\n-78 4 V\n.975 g 2599
      3861 N -78 4 44 -17 59 -3 h\n1.000 UP\n1.000 UL\nLT2\n2599 3861 M\n25
      -16 V\n78 -4 V\n-44 17 V\n-59 3 V\n.987 g 2573 3868 N -59 3 45 -7 40 -3
      h\n1.000 UP\n1.000 UL\nLT2\n2573 3868 M\n26 -7 V\n59 -3 V\n-45 7 V\n-40
      3 V\n.995 g 2547 3863 N -40 3 46 3 20 -1 h\n1.000 UP\n1.000
      UL\nLT2\n2547 3863 M\n26 5 V\n40 -3 V\n-46 -3 V\n-20 1 V\n.999 g 2521
      3849 N -20 1 46 13 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n26 14
      V\n20 -1 V\n-46 -13 V\n.001 g 2567 1353 N 0 0 -65 -22 19 -1 h\n1.000
      UP\n1.000 UL\nLT2\n2567 1353 M\n-46 -23 V\n65 22 V\n-19 1 V\n.005 g
      2613 1387 N -19 1 -64 -31 37 -4 h\n1.000 UP\n1.000 UL\nLT2\n2613 1387
      M\n-46 -34 V\n19 -1 V\n64 31 V\n-37 4 V\n.013 g 2658 1430 N -37 4 -63
      -42 55 -5 h\n1.000 UP\n1.000 UL\nLT2\n2658 1430 M\n-45 -43 V\n37 -4
      V\n63 42 V\n-55 5 V\n.025 g 2702 1482 N -55 5 -62 -51 73 -6 h\n1.000
      UP\n1.000 UL\nLT2\n2702 1482 M\n-44 -52 V\n55 -5 V\n62 51 V\n-73 6
      V\n.0407 g 2745 1543 N -73 6 -60 -59 90 -8 h\n1.000 UP\n1.000
      UL\nLT2\n2745 1543 M\n-43 -61 V\n73 -6 V\n60 59 V\n-90 8 V\n.0601 g
      2786 1613 N -90 8 -57 -68 106 -10 h\n1.000 UP\n1.000 UL\nLT2\n2786 1613
      M\n-41 -70 V\n90 -8 V\n57 68 V\n-106 10 V\n.0831 g 2824 1691 N -106 10
      -54 -77 122 -11 h\n1.000 UP\n1.000 UL\nLT2\n2824 1691 M\n-38 -78 V\n106
      -10 V\n54 77 V\n-122 11 V\n.1095 g 2860 1776 N -122 11 -50 -83 136 -13
      h\n1.000 UP\n1.000 UL\nLT2\n2860 1776 M\n-36 -85 V\n122 -11 V\n50 83
      V\n-136 13 V\n.139 g 2894 1867 N -136 13 -47 -90 149 -14 h\n1.000
      UP\n1.000 UL\nLT2\n2894 1867 M\n-34 -91 V\n136 -13 V\n47 90 V\n-149 14
      V\n.1714 g 2924 1964 N -149 14 -43 -96 162 -15 h\n1.000 UP\n1.000
      UL\nLT2\n2924 1964 M\n-30 -97 V\n149 -14 V\n43 96 V\n-162 15 V\n.2064 g
      2951 2066 N -162 15 -38 -102 173 -15 h\n1.000 UP\n1.000 UL\nLT2\n2951
      2066 M\n-27 -102 V\n162 -15 V\n38 102 V\n-173 15 V\n.2438 g 2975 2173 N
      -173 15 -33 -105 182 -17 h\n1.000 UP\n1.000 UL\nLT2\n2975 2173 M\n-24
      -107 V\n173 -15 V\n33 105 V\n-182 17 V\n.2833 g 2995 2283 N -182 17 -28
      -109 190 -18 h\n1.000 UP\n1.000 UL\nLT2\n2995 2283 M\n-20 -110 V\n182
      -17 V\n28 109 V\n-190 18 V\n.3245 g 3011 2395 N -190 18 -23 -112 197
      -18 h\n1.000 UP\n1.000 UL\nLT2\n3011 2395 M\n-16 -112 V\n190 -18 V\n23
      112 V\n-197 18 V\n.3671 g 3024 2509 N -197 18 -18 -113 202 -19 h\n1.000
      UP\n1.000 UL\nLT2\n3024 2509 M\n-13 -114 V\n197 -18 V\n18 113 V\n-202
      19 V\n.4108 g 3032 2623 N -202 19 -11 -114 205 -19 h\n1.000 UP\n1.000
      UL\nLT2\n3032 2623 M\n-8 -114 V\n202 -19 V\n11 114 V\n-205 19 V\n.4552
      g 3036 2737 N -205 19 -6 -115 207 -18 h\n1.000 UP\n1.000 UL\nLT2\n3036
      2737 M\n-4 -114 V\n205 -19 V\n6 115 V\n-207 18 V\n.5 g 3036 2850 N -207
      18 0 -113 207 -18 h\n1.000 UP\n1.000 UL\nLT2\n3036 2850 M\n0 -113
      V\n207 -18 V\n0 113 V\n-207 18 V\n.5448 g 3032 2961 N -207 18 6 -111
      205 -18 h\n1.000 UP\n1.000 UL\nLT2\n3032 2961 M\n4 -111 V\n207 -18
      V\n-6 111 V\n-205 18 V\n.5892 g 3024 3069 N -205 18 11 -108 202 -18
      h\n1.000 UP\n1.000 UL\nLT2\n3024 3069 M\n8 -108 V\n205 -18 V\n-11 108
      V\n-202 18 V\n.6329 g 3011 3173 N -202 18 18 -104 197 -18 h\n1.000
      UP\n1.000 UL\nLT2\n3011 3173 M\n13 -104 V\n202 -18 V\n-18 104 V\n-197
      18 V\n.6755 g 2995 3273 N -197 18 23 -100 190 -18 h\n1.000 UP\n1.000
      UL\nLT2\n2995 3273 M\n16 -100 V\n197 -18 V\n-23 100 V\n-190 18 V\n.7167
      g 2975 3367 N -190 18 28 -95 182 -17 h\n1.000 UP\n1.000 UL\nLT2\n2975
      3367 M\n20 -94 V\n190 -18 V\n-28 95 V\n-182 17 V\n.7562 g 2951 3454 N
      -182 17 33 -88 173 -16 h\n1.000 UP\n1.000 UL\nLT2\n2951 3454 M\n24 -87
      V\n182 -17 V\n-33 88 V\n-173 16 V\n.7936 g 2924 3535 N -173 16 38 -82
      162 -15 h\n1.000 UP\n1.000 UL\nLT2\n2924 3535 M\n27 -81 V\n173 -16
      V\n-38 82 V\n-162 15 V\n.8286 g 2894 3608 N -162 15 43 -74 149 -14
      h\n1.000 UP\n1.000 UL\nLT2\n2894 3608 M\n30 -73 V\n162 -15 V\n-43 74
      V\n-149 14 V\n.861 g 2860 3673 N -149 14 47 -66 136 -13 h\n1.000
      UP\n1.000 UL\nLT2\n2860 3673 M\n34 -65 V\n149 -14 V\n-47 66 V\n-136 13
      V\n.8905 g 2824 3729 N -136 13 50 -58 122 -11 h\n1.000 UP\n1.000
      UL\nLT2\n2824 3729 M\n36 -56 V\n136 -13 V\n-50 58 V\n-122 11 V\n.9169 g
      2786 3776 N -122 11 54 -48 106 -10 h\n1.000 UP\n1.000 UL\nLT2\n2786
      3776 M\n38 -47 V\n122 -11 V\n-54 48 V\n-106 10 V\n.9399 g 2745 3813 N
      -106 10 57 -39 90 -8 h\n1.000 UP\n1.000 UL\nLT2\n2745 3813 M\n41 -37
      V\n106 -10 V\n-57 39 V\n-90 8 V\n.9593 g 2702 3841 N -90 8 60 -29 73 -7
      h\n1.000 UP\n1.000 UL\nLT2\n2702 3841 M\n43 -28 V\n90 -8 V\n-60 29
      V\n-73 7 V\n.975 g 2658 3858 N -73 7 62 -19 55 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2658 3858 M\n44 -17 V\n73 -7 V\n-62 19 V\n-55 5 V\n.987 g 2613
      3865 N -55 5 63 -9 37 -3 h\n1.000 UP\n1.000 UL\nLT2\n2613 3865 M\n45 -7
      V\n55 -5 V\n-63 9 V\n-37 3 V\n.995 g 2567 3862 N -37 3 64 1 19 -1
      h\n1.000 UP\n1.000 UL\nLT2\n2567 3862 M\n46 3 V\n37 -3 V\n-64 -1 V\n-19
      1 V\n.999 g 2521 3849 N -19 1 65 12 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n46 13 V\n19 -1 V\n-65 -12 V\n.001 g 2586 1352 N 0
      0 -81 -19 16 -3 h\n1.000 UP\n1.000 UL\nLT2\n2586 1352 M\n-65 -22 V\n81
      19 V\n-16 3 V\n.005 g 2650 1383 N -16 3 -81 -30 33 -4 h\n1.000
      UP\n1.000 UL\nLT2\n2650 1383 M\n-64 -31 V\n16 -3 V\n81 30 V\n-33 4
      V\n.013 g 2713 1425 N -33 4 -79 -39 49 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2713 1425 M\n-63 -42 V\n33 -4 V\n79 39 V\n-49 7 V\n.025 g 2775
      1476 N -49 7 -78 -49 65 -9 h\n1.000 UP\n1.000 UL\nLT2\n2775 1476 M\n-62
      -51 V\n49 -7 V\n78 49 V\n-65 9 V\n.0407 g 2835 1535 N -65 9 -74 -57 79
      -11 h\n1.000 UP\n1.000 UL\nLT2\n2835 1535 M\n-60 -59 V\n65 -9 V\n74 57
      V\n-79 11 V\n.0601 g 2892 1603 N -79 11 -72 -67 94 -12 h\n1.000
      UP\n1.000 UL\nLT2\n2892 1603 M\n-57 -68 V\n79 -11 V\n72 67 V\n-94 12
      V\n.0831 g 2946 1680 N -94 12 -68 -74 108 -15 h\n1.000 UP\n1.000
      UL\nLT2\n2946 1680 M\n-54 -77 V\n94 -12 V\n68 74 V\n-108 15 V\n.1095 g
      2996 1763 N -108 15 -63 -82 121 -16 h\n1.000 UP\n1.000 UL\nLT2\n2996
      1763 M\n-50 -83 V\n108 -15 V\n63 82 V\n-121 16 V\n.139 g 3043 1853 N
      -121 16 -59 -88 133 -18 h\n1.000 UP\n1.000 UL\nLT2\n3043 1853 M\n-47
      -90 V\n121 -16 V\n59 88 V\n-133 18 V\n.1714 g 3086 1949 N -133 18 -54
      -95 144 -19 h\n1.000 UP\n1.000 UL\nLT2\n3086 1949 M\n-43 -96 V\n133 -18
      V\n54 95 V\n-144 19 V\n.2064 g 3124 2051 N -144 19 -48 -100 154 -21
      h\n1.000 UP\n1.000 UL\nLT2\n3124 2051 M\n-38 -102 V\n144 -19 V\n48 100
      V\n-154 21 V\n.2438 g 3157 2156 N -154 21 -41 -104 162 -22 h\n1.000
      UP\n1.000 UL\nLT2\n3157 2156 M\n-33 -105 V\n154 -21 V\n41 104 V\n-162
      22 V\n.2833 g 3185 2265 N -162 22 -36 -108 170 -23 h\n1.000 UP\n1.000
      UL\nLT2\n3185 2265 M\n-28 -109 V\n162 -22 V\n36 108 V\n-170 23 V\n.3245
      g 3208 2377 N -170 23 -28 -111 175 -24 h\n1.000 UP\n1.000 UL\nLT2\n3208
      2377 M\n-23 -112 V\n170 -23 V\n28 111 V\n-175 24 V\n.3671 g 3226 2490 N
      -175 24 -22 -113 179 -24 h\n1.000 UP\n1.000 UL\nLT2\n3226 2490 M\n-18
      -113 V\n175 -24 V\n22 113 V\n-179 24 V\n.4108 g 3237 2604 N -179 24 -14
      -114 182 -24 h\n1.000 UP\n1.000 UL\nLT2\n3237 2604 M\n-11 -114 V\n179
      -24 V\n14 114 V\n-182 24 V\n.4552 g 3243 2719 N -182 24 -8 -114 184 -25
      h\n1.000 UP\n1.000 UL\nLT2\n3243 2719 M\n-6 -115 V\n182 -24 V\n8 114
      V\n-184 25 V\n.5 g 3243 2832 N -184 25 0 -113 184 -25 h\n1.000
      UP\n1.000 UL\nLT2\n3243 2832 M\n0 -113 V\n184 -25 V\n0 113 V\n-184 25
      V\n.5448 g 3237 2943 N -184 25 8 -111 182 -25 h\n1.000 UP\n1.000
      UL\nLT2\n3237 2943 M\n6 -111 V\n184 -25 V\n-8 111 V\n-182 25 V\n.5892 g
      3226 3051 N -182 25 14 -109 179 -24 h\n1.000 UP\n1.000 UL\nLT2\n3226
      3051 M\n11 -108 V\n182 -25 V\n-14 109 V\n-179 24 V\n.6329 g 3208 3155 N
      -179 24 22 -105 175 -23 h\n1.000 UP\n1.000 UL\nLT2\n3208 3155 M\n18
      -104 V\n179 -24 V\n-22 105 V\n-175 23 V\n.6755 g 3185 3255 N -175 23 28
      -100 170 -23 h\n1.000 UP\n1.000 UL\nLT2\n3185 3255 M\n23 -100 V\n175
      -23 V\n-28 100 V\n-170 23 V\n.7167 g 3157 3350 N -170 23 36 -96 162 -22
      h\n1.000 UP\n1.000 UL\nLT2\n3157 3350 M\n28 -95 V\n170 -23 V\n-36 96
      V\n-162 22 V\n.7562 g 3124 3438 N -162 22 41 -90 154 -20 h\n1.000
      UP\n1.000 UL\nLT2\n3124 3438 M\n33 -88 V\n162 -22 V\n-41 90 V\n-154 20
      V\n.7936 g 3086 3520 N -154 20 48 -83 144 -19 h\n1.000 UP\n1.000
      UL\nLT2\n3086 3520 M\n38 -82 V\n154 -20 V\n-48 83 V\n-144 19 V\n.8286 g
      3043 3594 N -144 19 54 -75 133 -18 h\n1.000 UP\n1.000 UL\nLT2\n3043
      3594 M\n43 -74 V\n144 -19 V\n-54 75 V\n-133 18 V\n.861 g 2996 3660 N
      -133 18 59 -68 121 -16 h\n1.000 UP\n1.000 UL\nLT2\n2996 3660 M\n47 -66
      V\n133 -18 V\n-59 68 V\n-121 16 V\n.8905 g 2946 3718 N -121 16 63 -59
      108 -15 h\n1.000 UP\n1.000 UL\nLT2\n2946 3718 M\n50 -58 V\n121 -16
      V\n-63 59 V\n-108 15 V\n.9169 g 2892 3766 N -108 15 68 -50 94 -13
      h\n1.000 UP\n1.000 UL\nLT2\n2892 3766 M\n54 -48 V\n108 -15 V\n-68 50
      V\n-94 13 V\n.9399 g 2835 3805 N -94 13 72 -41 79 -11 h\n1.000
      UP\n1.000 UL\nLT2\n2835 3805 M\n57 -39 V\n94 -13 V\n-72 41 V\n-79 11
      V\n.9593 g 2775 3834 N -79 11 74 -31 65 -9 h\n1.000 UP\n1.000
      UL\nLT2\n2775 3834 M\n60 -29 V\n79 -11 V\n-74 31 V\n-65 9 V\n.975 g
      2713 3853 N -65 9 78 -21 49 -7 h\n1.000 UP\n1.000 UL\nLT2\n2713 3853
      M\n62 -19 V\n65 -9 V\n-78 21 V\n-49 7 V\n.987 g 2650 3862 N -49 7 79
      -12 33 -4 h\n1.000 UP\n1.000 UL\nLT2\n2650 3862 M\n63 -9 V\n49 -7
      V\n-79 12 V\n-33 4 V\n.995 g 2586 3861 N -33 4 81 0 16 -3 h\n1.000
      UP\n1.000 UL\nLT2\n2586 3861 M\n64 1 V\n33 -4 V\n-81 0 V\n-16 3 V\n.999
      g 2521 3849 N -16 3 81 9 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849
      M\n65 12 V\n16 -3 V\n-81 -9 V\n.001 g 2602 1349 N 0 0 -95 -17 14 -2
      h\n1.000 UP\n1.000 UL\nLT2\n2602 1349 M\n-81 -19 V\n95 17 V\n-14 2
      V\n.005 g 2683 1379 N -14 2 -95 -27 28 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2683 1379 M\n-81 -30 V\n14 -2 V\n95 27 V\n-28 5 V\n.013 g 2762
      1418 N -28 5 -92 -36 41 -8 h\n1.000 UP\n1.000 UL\nLT2\n2762 1418 M\n-79
      -39 V\n28 -5 V\n92 36 V\n-41 8 V\n.025 g 2840 1467 N -41 8 -91 -46 54
      -11 h\n1.000 UP\n1.000 UL\nLT2\n2840 1467 M\n-78 -49 V\n41 -8 V\n91 46
      V\n-54 11 V\n.0407 g 2914 1524 N -54 11 -87 -55 67 -13 h\n1.000
      UP\n1.000 UL\nLT2\n2914 1524 M\n-74 -57 V\n54 -11 V\n87 55 V\n-67 13
      V\n.0601 g 2986 1591 N -67 13 -84 -64 79 -16 h\n1.000 UP\n1.000
      UL\nLT2\n2986 1591 M\n-72 -67 V\n67 -13 V\n84 64 V\n-79 16 V\n.0831 g
      3054 1665 N -79 16 -80 -72 91 -18 h\n1.000 UP\n1.000 UL\nLT2\n3054 1665
      M\n-68 -74 V\n79 -16 V\n80 72 V\n-91 18 V\n.1095 g 3117 1747 N -91 18
      -74 -80 102 -20 h\n1.000 UP\n1.000 UL\nLT2\n3117 1747 M\n-63 -82 V\n91
      -18 V\n74 80 V\n-102 20 V\n.139 g 3176 1835 N -102 20 -69 -87 112 -21
      h\n1.000 UP\n1.000 UL\nLT2\n3176 1835 M\n-59 -88 V\n102 -20 V\n69 87
      V\n-112 21 V\n.1714 g 3230 1930 N -112 21 -63 -92 121 -24 h\n1.000
      UP\n1.000 UL\nLT2\n3230 1930 M\n-54 -95 V\n112 -21 V\n63 92 V\n-121 24
      V\n.2064 g 3278 2030 N -121 24 -56 -99 129 -25 h\n1.000 UP\n1.000
      UL\nLT2\n3278 2030 M\n-48 -100 V\n121 -24 V\n56 99 V\n-129 25 V\n.2438
      g 3319 2134 N -129 25 -48 -103 136 -26 h\n1.000 UP\n1.000 UL\nLT2\n3319
      2134 M\n-41 -104 V\n129 -25 V\n48 103 V\n-136 26 V\n.2833 g 3355 2242 N
      -136 26 -42 -107 142 -27 h\n1.000 UP\n1.000 UL\nLT2\n3355 2242 M\n-36
      -108 V\n136 -26 V\n42 107 V\n-142 27 V\n.3245 g 3383 2353 N -142 27 -33
      -110 147 -28 h\n1.000 UP\n1.000 UL\nLT2\n3383 2353 M\n-28 -111 V\n142
      -27 V\n33 110 V\n-147 28 V\n.3671 g 3405 2466 N -147 28 -26 -112 151
      -29 h\n1.000 UP\n1.000 UL\nLT2\n3405 2466 M\n-22 -113 V\n147 -28 V\n26
      112 V\n-151 29 V\n.4108 g 3419 2580 N -151 29 -17 -113 154 -30 h\n1.000
      UP\n1.000 UL\nLT2\n3419 2580 M\n-14 -114 V\n151 -29 V\n17 113 V\n-154
      30 V\n.4552 g 3427 2694 N -154 30 -8 -114 154 -30 h\n1.000 UP\n1.000
      UL\nLT2\n3427 2694 M\n-8 -114 V\n154 -30 V\n8 114 V\n-154 30 V\n.5 g
      3427 2807 N -154 30 0 -113 154 -30 h\n1.000 UP\n1.000 UL\nLT2\n3427
      2807 M\n0 -113 V\n154 -30 V\n0 113 V\n-154 30 V\n.5448 g 3419 2918 N
      -154 30 8 -111 154 -30 h\n1.000 UP\n1.000 UL\nLT2\n3419 2918 M\n8 -111
      V\n154 -30 V\n-8 111 V\n-154 30 V\n.5892 g 3405 3027 N -154 30 17 -109
      151 -30 h\n1.000 UP\n1.000 UL\nLT2\n3405 3027 M\n14 -109 V\n154 -30
      V\n-17 109 V\n-151 30 V\n.6329 g 3383 3132 N -151 30 26 -106 147 -29
      h\n1.000 UP\n1.000 UL\nLT2\n3383 3132 M\n22 -105 V\n151 -30 V\n-26 106
      V\n-147 29 V\n.6755 g 3355 3232 N -147 29 33 -102 142 -27 h\n1.000
      UP\n1.000 UL\nLT2\n3355 3232 M\n28 -100 V\n147 -29 V\n-33 102 V\n-142
      27 V\n.7167 g 3319 3328 N -142 27 42 -97 136 -26 h\n1.000 UP\n1.000
      UL\nLT2\n3319 3328 M\n36 -96 V\n142 -27 V\n-42 97 V\n-136 26 V\n.7562 g
      3278 3418 N -136 26 48 -91 129 -25 h\n1.000 UP\n1.000 UL\nLT2\n3278
      3418 M\n41 -90 V\n136 -26 V\n-48 91 V\n-129 25 V\n.7936 g 3230 3501 N
      -129 25 56 -84 121 -24 h\n1.000 UP\n1.000 UL\nLT2\n3230 3501 M\n48 -83
      V\n129 -25 V\n-56 84 V\n-121 24 V\n.8286 g 3176 3576 N -121 24 63 -77
      112 -22 h\n1.000 UP\n1.000 UL\nLT2\n3176 3576 M\n54 -75 V\n121 -24
      V\n-63 77 V\n-112 22 V\n.861 g 3117 3644 N -112 22 69 -70 102 -20
      h\n1.000 UP\n1.000 UL\nLT2\n3117 3644 M\n59 -68 V\n112 -22 V\n-69 70
      V\n-102 20 V\n.8905 g 3054 3703 N -102 20 74 -61 91 -18 h\n1.000
      UP\n1.000 UL\nLT2\n3054 3703 M\n63 -59 V\n102 -20 V\n-74 61 V\n-91 18
      V\n.9169 g 2986 3753 N -91 18 80 -53 79 -15 h\n1.000 UP\n1.000
      UL\nLT2\n2986 3753 M\n68 -50 V\n91 -18 V\n-80 53 V\n-79 15 V\n.9399 g
      2914 3794 N -79 15 84 -43 67 -13 h\n1.000 UP\n1.000 UL\nLT2\n2914 3794
      M\n72 -41 V\n79 -15 V\n-84 43 V\n-67 13 V\n.9593 g 2840 3825 N -67 13
      87 -34 54 -10 h\n1.000 UP\n1.000 UL\nLT2\n2840 3825 M\n74 -31 V\n67 -13
      V\n-87 34 V\n-54 10 V\n.975 g 2762 3846 N -54 10 91 -23 41 -8 h\n1.000
      UP\n1.000 UL\nLT2\n2762 3846 M\n78 -21 V\n54 -10 V\n-91 23 V\n-41 8
      V\n.987 g 2683 3858 N -41 8 92 -14 28 -6 h\n1.000 UP\n1.000
      UL\nLT2\n2683 3858 M\n79 -12 V\n41 -8 V\n-92 14 V\n-28 6 V\n.995 g 2602
      3858 N -28 6 95 -4 14 -2 h\n1.000 UP\n1.000 UL\nLT2\n2602 3858 M\n81 0
      V\n28 -6 V\n-95 4 V\n-14 2 V\n.999 g 2521 3849 N -14 2 95 7 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n81 9 V\n14 -2 V\n-95 -7
      V\n.001 g 2616 1347 N 0 0 -106 -14 11 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2616 1347 M\n-95 -17 V\n106 14 V\n-11 3 V\n.005 g 2711 1374 N
      -11 3 -105 -23 21 -7 h\n1.000 UP\n1.000 UL\nLT2\n2711 1374 M\n-95 -27
      V\n11 -3 V\n105 23 V\n-21 7 V\n.013 g 2803 1410 N -21 7 -103 -34 32 -9
      h\n1.000 UP\n1.000 UL\nLT2\n2803 1410 M\n-92 -36 V\n21 -7 V\n103 34
      V\n-32 9 V\n.025 g 2894 1456 N -32 9 -101 -43 42 -12 h\n1.000 UP\n1.000
      UL\nLT2\n2894 1456 M\n-91 -46 V\n32 -9 V\n101 43 V\n-42 12 V\n.0407 g
      2981 1511 N -42 12 -98 -53 53 -14 h\n1.000 UP\n1.000 UL\nLT2\n2981 1511
      M\n-87 -55 V\n42 -12 V\n98 53 V\n-53 14 V\n.0601 g 3065 1575 N -53 14
      -93 -61 62 -17 h\n1.000 UP\n1.000 UL\nLT2\n3065 1575 M\n-84 -64 V\n53
      -14 V\n93 61 V\n-62 17 V\n.0831 g 3145 1647 N -62 17 -89 -69 71 -20
      h\n1.000 UP\n1.000 UL\nLT2\n3145 1647 M\n-80 -72 V\n62 -17 V\n89 69
      V\n-71 20 V\n.1095 g 3219 1727 N -71 20 -83 -77 80 -23 h\n1.000
      UP\n1.000 UL\nLT2\n3219 1727 M\n-74 -80 V\n71 -20 V\n83 77 V\n-80 23
      V\n.139 g 3288 1814 N -80 23 -76 -85 87 -25 h\n1.000 UP\n1.000
      UL\nLT2\n3288 1814 M\n-69 -87 V\n80 -23 V\n76 85 V\n-87 25 V\n.1714 g
      3351 1906 N -87 25 -70 -91 94 -26 h\n1.000 UP\n1.000 UL\nLT2\n3351 1906
      M\n-63 -92 V\n87 -25 V\n70 91 V\n-94 26 V\n.2064 g 3407 2005 N -94 26
      -62 -96 100 -29 h\n1.000 UP\n1.000 UL\nLT2\n3407 2005 M\n-56 -99 V\n94
      -26 V\n62 96 V\n-100 29 V\n.2438 g 3455 2108 N -100 29 -55 -102 107 -30
      h\n1.000 UP\n1.000 UL\nLT2\n3455 2108 M\n-48 -103 V\n100 -29 V\n55 102
      V\n-107 30 V\n.2833 g 3497 2215 N -107 30 -46 -105 111 -32 h\n1.000
      UP\n1.000 UL\nLT2\n3497 2215 M\n-42 -107 V\n107 -30 V\n46 105 V\n-111
      32 V\n.3245 g 3530 2325 N -111 32 -37 -109 115 -33 h\n1.000 UP\n1.000
      UL\nLT2\n3530 2325 M\n-33 -110 V\n111 -32 V\n37 109 V\n-115 33 V\n.3671
      g 3556 2437 N -115 33 -28 -111 117 -34 h\n1.000 UP\n1.000 UL\nLT2\n3556
      2437 M\n-26 -112 V\n115 -33 V\n28 111 V\n-117 34 V\n.4108 g 3573 2550 N
      -117 34 -19 -113 119 -34 h\n1.000 UP\n1.000 UL\nLT2\n3573 2550 M\n-17
      -113 V\n117 -34 V\n19 113 V\n-119 34 V\n.4552 g 3581 2664 N -119 34 -10
      -113 121 -35 h\n1.000 UP\n1.000 UL\nLT2\n3581 2664 M\n-8 -114 V\n119
      -34 V\n10 113 V\n-121 35 V\n.5 g 3581 2777 N -121 35 0 -113 121 -35
      h\n1.000 UP\n1.000 UL\nLT2\n3581 2777 M\n0 -113 V\n121 -35 V\n0 113
      V\n-121 35 V\n.5448 g 3573 2888 N -121 35 10 -112 119 -34 h\n1.000
      UP\n1.000 UL\nLT2\n3573 2888 M\n8 -111 V\n121 -35 V\n-10 112 V\n-119 34
      V\n.5892 g 3556 2997 N -119 34 19 -110 117 -33 h\n1.000 UP\n1.000
      UL\nLT2\n3556 2997 M\n17 -109 V\n119 -34 V\n-19 110 V\n-117 33 V\n.6329
      g 3530 3103 N -117 33 28 -107 115 -32 h\n1.000 UP\n1.000 UL\nLT2\n3530
      3103 M\n26 -106 V\n117 -33 V\n-28 107 V\n-115 32 V\n.6755 g 3497 3205 N
      -115 32 37 -102 111 -32 h\n1.000 UP\n1.000 UL\nLT2\n3497 3205 M\n33
      -102 V\n115 -32 V\n-37 102 V\n-111 32 V\n.7167 g 3455 3302 N -111 32 46
      -98 107 -31 h\n1.000 UP\n1.000 UL\nLT2\n3455 3302 M\n42 -97 V\n111 -32
      V\n-46 98 V\n-107 31 V\n.7562 g 3407 3393 N -107 31 55 -93 100 -29
      h\n1.000 UP\n1.000 UL\nLT2\n3407 3393 M\n48 -91 V\n107 -31 V\n-55 93
      V\n-100 29 V\n.7936 g 3351 3477 N -100 29 62 -86 94 -27 h\n1.000
      UP\n1.000 UL\nLT2\n3351 3477 M\n56 -84 V\n100 -29 V\n-62 86 V\n-94 27
      V\n.8286 g 3288 3554 N -94 27 70 -80 87 -24 h\n1.000 UP\n1.000
      UL\nLT2\n3288 3554 M\n63 -77 V\n94 -27 V\n-70 80 V\n-87 24 V\n.861 g
      3219 3624 N -87 24 76 -72 80 -22 h\n1.000 UP\n1.000 UL\nLT2\n3219 3624
      M\n69 -70 V\n87 -24 V\n-76 72 V\n-80 22 V\n.8905 g 3145 3685 N -80 22
      83 -63 71 -20 h\n1.000 UP\n1.000 UL\nLT2\n3145 3685 M\n74 -61 V\n80 -22
      V\n-83 63 V\n-71 20 V\n.9169 g 3065 3738 N -71 20 89 -55 62 -18
      h\n1.000 UP\n1.000 UL\nLT2\n3065 3738 M\n80 -53 V\n71 -20 V\n-89 55
      V\n-62 18 V\n.9399 g 2981 3781 N -62 18 93 -46 53 -15 h\n1.000
      UP\n1.000 UL\nLT2\n2981 3781 M\n84 -43 V\n62 -18 V\n-93 46 V\n-53 15
      V\n.9593 g 2894 3815 N -53 15 98 -37 42 -12 h\n1.000 UP\n1.000
      UL\nLT2\n2894 3815 M\n87 -34 V\n53 -15 V\n-98 37 V\n-42 12 V\n.975 g
      2803 3838 N -42 12 101 -26 32 -9 h\n1.000 UP\n1.000 UL\nLT2\n2803 3838
      M\n91 -23 V\n42 -12 V\n-101 26 V\n-32 9 V\n.987 g 2711 3852 N -32 9 103
      -17 21 -6 h\n1.000 UP\n1.000 UL\nLT2\n2711 3852 M\n92 -14 V\n32 -9
      V\n-103 17 V\n-21 6 V\n.995 g 2616 3856 N -21 6 105 -7 11 -3 h\n1.000
      UP\n1.000 UL\nLT2\n2616 3856 M\n95 -4 V\n21 -6 V\n-105 7 V\n-11 3
      V\n.999 g 2521 3849 N -11 3 106 4 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521
      3849 M\n95 7 V\n11 -3 V\n-106 -4 V\n.001 g 2627 1344 N 0 0 -113 -10 7
      -4 h\n1.000 UP\n1.000 UL\nLT2\n2627 1344 M\n-106 -14 V\n113 10 V\n-7 4
      V\n.005 g 2732 1367 N -7 4 -113 -21 15 -6 h\n1.000 UP\n1.000
      UL\nLT2\n2732 1367 M\n-105 -23 V\n7 -4 V\n113 21 V\n-15 6 V\n.013 g
      2835 1401 N -15 6 -110 -30 22 -10 h\n1.000 UP\n1.000 UL\nLT2\n2835 1401
      M\n-103 -34 V\n15 -6 V\n110 30 V\n-22 10 V\n.025 g 2936 1444 N -22 10
      -108 -40 29 -13 h\n1.000 UP\n1.000 UL\nLT2\n2936 1444 M\n-101 -43 V\n22
      -10 V\n108 40 V\n-29 13 V\n.0407 g 3034 1497 N -29 13 -105 -49 36 -17
      h\n1.000 UP\n1.000 UL\nLT2\n3034 1497 M\n-98 -53 V\n29 -13 V\n105 49
      V\n-36 17 V\n.0601 g 3127 1558 N -36 17 -100 -59 43 -19 h\n1.000
      UP\n1.000 UL\nLT2\n3127 1558 M\n-93 -61 V\n36 -17 V\n100 59 V\n-43 19
      V\n.0831 g 3216 1627 N -43 19 -94 -66 48 -22 h\n1.000 UP\n1.000
      UL\nLT2\n3216 1627 M\n-89 -69 V\n43 -19 V\n94 66 V\n-48 22 V\n.1095 g
      3299 1704 N -48 22 -89 -75 54 -24 h\n1.000 UP\n1.000 UL\nLT2\n3299 1704
      M\n-83 -77 V\n48 -22 V\n89 75 V\n-54 24 V\n.139 g 3375 1789 N -54 24
      -82 -82 60 -27 h\n1.000 UP\n1.000 UL\nLT2\n3375 1789 M\n-76 -85 V\n54
      -24 V\n82 82 V\n-60 27 V\n.1714 g 3445 1880 N -60 27 -75 -88 65 -30
      h\n1.000 UP\n1.000 UL\nLT2\n3445 1880 M\n-70 -91 V\n60 -27 V\n75 88
      V\n-65 30 V\n.2064 g 3507 1976 N -65 30 -66 -95 69 -31 h\n1.000
      UP\n1.000 UL\nLT2\n3507 1976 M\n-62 -96 V\n65 -30 V\n66 95 V\n-69 31
      V\n.2438 g 3562 2078 N -69 31 -58 -100 72 -33 h\n1.000 UP\n1.000
      UL\nLT2\n3562 2078 M\n-55 -102 V\n69 -31 V\n58 100 V\n-72 33 V\n.2833 g
      3608 2183 N -72 33 -50 -104 76 -34 h\n1.000 UP\n1.000 UL\nLT2\n3608
      2183 M\n-46 -105 V\n72 -33 V\n50 104 V\n-76 34 V\n.3245 g 3645 2292 N
      -76 34 -39 -108 78 -35 h\n1.000 UP\n1.000 UL\nLT2\n3645 2292 M\n-37
      -109 V\n76 -34 V\n39 108 V\n-78 35 V\n.3671 g 3673 2403 N -78 35 -31
      -110 81 -36 h\n1.000 UP\n1.000 UL\nLT2\n3673 2403 M\n-28 -111 V\n78 -35
      V\n31 110 V\n-81 36 V\n.4108 g 3692 2516 N -81 36 -20 -112 82 -37
      h\n1.000 UP\n1.000 UL\nLT2\n3692 2516 M\n-19 -113 V\n81 -36 V\n20 112
      V\n-82 37 V\n.4552 g 3702 2629 N -82 37 -10 -113 82 -37 h\n1.000
      UP\n1.000 UL\nLT2\n3702 2629 M\n-10 -113 V\n82 -37 V\n10 113 V\n-82 37
      V\n.5 g 3702 2742 N -82 37 0 -113 82 -37 h\n1.000 UP\n1.000
      UL\nLT2\n3702 2742 M\n0 -113 V\n82 -37 V\n0 113 V\n-82 37 V\n.5448 g
      3692 2854 N -82 37 10 -112 82 -37 h\n1.000 UP\n1.000 UL\nLT2\n3692 2854
      M\n10 -112 V\n82 -37 V\n-10 112 V\n-82 37 V\n.5892 g 3673 2964 N -82 37
      20 -111 81 -36 h\n1.000 UP\n1.000 UL\nLT2\n3673 2964 M\n19 -110 V\n82
      -37 V\n-20 111 V\n-81 36 V\n.6329 g 3645 3071 N -81 36 31 -107 78 -36
      h\n1.000 UP\n1.000 UL\nLT2\n3645 3071 M\n28 -107 V\n81 -36 V\n-31 107
      V\n-78 36 V\n.6755 g 3608 3173 N -78 36 39 -104 76 -34 h\n1.000
      UP\n1.000 UL\nLT2\n3608 3173 M\n37 -102 V\n78 -36 V\n-39 104 V\n-76 34
      V\n.7167 g 3562 3271 N -76 34 50 -100 72 -32 h\n1.000 UP\n1.000
      UL\nLT2\n3562 3271 M\n46 -98 V\n76 -34 V\n-50 100 V\n-72 32 V\n.7562 g
      3507 3364 N -72 32 58 -94 69 -31 h\n1.000 UP\n1.000 UL\nLT2\n3507 3364
      M\n55 -93 V\n72 -32 V\n-58 94 V\n-69 31 V\n.7936 g 3445 3450 N -69 31
      66 -88 65 -29 h\n1.000 UP\n1.000 UL\nLT2\n3445 3450 M\n62 -86 V\n69 -31
      V\n-66 88 V\n-65 29 V\n.8286 g 3375 3530 N -65 29 75 -82 60 -27
      h\n1.000 UP\n1.000 UL\nLT2\n3375 3530 M\n70 -80 V\n65 -29 V\n-75 82
      V\n-60 27 V\n.861 g 3299 3602 N -60 27 82 -74 54 -25 h\n1.000 UP\n1.000
      UL\nLT2\n3299 3602 M\n76 -72 V\n60 -27 V\n-82 74 V\n-54 25 V\n.8905 g
      3216 3665 N -54 25 89 -66 48 -22 h\n1.000 UP\n1.000 UL\nLT2\n3216 3665
      M\n83 -63 V\n54 -25 V\n-89 66 V\n-48 22 V\n.9169 g 3127 3720 N -48 22
      94 -58 43 -19 h\n1.000 UP\n1.000 UL\nLT2\n3127 3720 M\n89 -55 V\n48 -22
      V\n-94 58 V\n-43 19 V\n.9399 g 3034 3766 N -43 19 100 -49 36 -16
      h\n1.000 UP\n1.000 UL\nLT2\n3034 3766 M\n93 -46 V\n43 -19 V\n-100 49
      V\n-36 16 V\n.9593 g 2936 3803 N -36 16 105 -40 29 -13 h\n1.000
      UP\n1.000 UL\nLT2\n2936 3803 M\n98 -37 V\n36 -16 V\n-105 40 V\n-29 13
      V\n.975 g 2835 3829 N -29 13 108 -29 22 -10 h\n1.000 UP\n1.000
      UL\nLT2\n2835 3829 M\n101 -26 V\n29 -13 V\n-108 29 V\n-22 10 V\n.987 g
      2732 3846 N -22 10 110 -20 15 -7 h\n1.000 UP\n1.000 UL\nLT2\n2732 3846
      M\n103 -17 V\n22 -10 V\n-110 20 V\n-15 7 V\n.995 g 2627 3853 N -15 7
      113 -10 7 -4 h\n1.000 UP\n1.000 UL\nLT2\n2627 3853 M\n105 -7 V\n15 -7
      V\n-113 10 V\n-7 4 V\n.999 g 2521 3849 N -7 4 113 0 0 0 h\n1.000
      UP\n1.000 UL\nLT2\n2521 3849 M\n106 4 V\n7 -4 V\n-113 0 V\n.001 g 2634
      1340 N 0 0 -117 -7 4 -3 h\n1.000 UP\n1.000 UL\nLT2\n2634 1340 M\n-113
      -10 V\n117 7 V\n-4 3 V\n.005 g 2747 1361 N -4 3 -116 -17 7 -7 h\n1.000
      UP\n1.000 UL\nLT2\n2747 1361 M\n-113 -21 V\n4 -3 V\n116 17 V\n-7 7
      V\n.013 g 2857 1391 N -7 7 -115 -27 12 -10 h\n1.000 UP\n1.000
      UL\nLT2\n2857 1391 M\n-110 -30 V\n7 -7 V\n115 27 V\n-12 10 V\n.025 g
      2965 1431 N -12 10 -111 -36 15 -14 h\n1.000 UP\n1.000 UL\nLT2\n2965
      1431 M\n-108 -40 V\n12 -10 V\n111 36 V\n-15 14 V\n.0407 g 3070 1480 N
      -15 14 -108 -46 18 -17 h\n1.000 UP\n1.000 UL\nLT2\n3070 1480 M\n-105
      -49 V\n15 -14 V\n108 46 V\n-18 17 V\n.0601 g 3170 1539 N -18 17 -103
      -55 21 -21 h\n1.000 UP\n1.000 UL\nLT2\n3170 1539 M\n-100 -59 V\n18 -17
      V\n103 55 V\n-21 21 V\n.0831 g 3264 1605 N -21 21 -98 -64 25 -23
      h\n1.000 UP\n1.000 UL\nLT2\n3264 1605 M\n-94 -66 V\n21 -21 V\n98 64
      V\n-25 23 V\n.1095 g 3353 1680 N -25 23 -92 -72 28 -26 h\n1.000
      UP\n1.000 UL\nLT2\n3353 1680 M\n-89 -75 V\n25 -23 V\n92 72 V\n-28 26
      V\n.139 g 3435 1762 N -28 26 -84 -79 30 -29 h\n1.000 UP\n1.000
      UL\nLT2\n3435 1762 M\n-82 -82 V\n28 -26 V\n84 79 V\n-30 29 V\n.1714 g
      3510 1850 N -30 29 -77 -87 32 -30 h\n1.000 UP\n1.000 UL\nLT2\n3510 1850
      M\n-75 -88 V\n30 -29 V\n77 87 V\n-32 30 V\n.2064 g 3576 1945 N -32 30
      -69 -92 35 -33 h\n1.000 UP\n1.000 UL\nLT2\n3576 1945 M\n-66 -95 V\n32
      -30 V\n69 92 V\n-35 33 V\n.2438 g 3634 2045 N -35 33 -60 -98 37 -35
      h\n1.000 UP\n1.000 UL\nLT2\n3634 2045 M\n-58 -100 V\n35 -33 V\n60 98
      V\n-37 35 V\n.2833 g 3684 2149 N -37 35 -51 -103 38 -36 h\n1.000
      UP\n1.000 UL\nLT2\n3684 2149 M\n-50 -104 V\n37 -35 V\n51 103 V\n-38 36
      V\n.3245 g 3723 2257 N -38 36 -41 -106 40 -38 h\n1.000 UP\n1.000
      UL\nLT2\n3723 2257 M\n-39 -108 V\n38 -36 V\n41 106 V\n-40 38 V\n.3671 g
      3754 2367 N -40 38 -32 -110 41 -38 h\n1.000 UP\n1.000 UL\nLT2\n3754
      2367 M\n-31 -110 V\n40 -38 V\n32 110 V\n-41 38 V\n.4108 g 3774 2479 N
      -41 38 -20 -111 41 -39 h\n1.000 UP\n1.000 UL\nLT2\n3774 2479 M\n-20
      -112 V\n41 -38 V\n20 111 V\n-41 39 V\n.4552 g 3784 2592 N -41 39 -11
      -113 42 -39 h\n1.000 UP\n1.000 UL\nLT2\n3784 2592 M\n-10 -113 V\n41 -39
      V\n11 113 V\n-42 39 V\n.5 g 3784 2705 N -42 39 0 -113 42 -39 h\n1.000
      UP\n1.000 UL\nLT2\n3784 2705 M\n0 -113 V\n42 -39 V\n0 113 V\n-42 39
      V\n.5448 g 3774 2817 N -42 39 11 -112 41 -39 h\n1.000 UP\n1.000
      UL\nLT2\n3774 2817 M\n10 -112 V\n42 -39 V\n-11 112 V\n-41 39 V\n.5892 g
      3754 2928 N -41 39 20 -111 41 -39 h\n1.000 UP\n1.000 UL\nLT2\n3754 2928
      M\n20 -111 V\n41 -39 V\n-20 111 V\n-41 39 V\n.6329 g 3723 3035 N -41 39
      32 -109 40 -37 h\n1.000 UP\n1.000 UL\nLT2\n3723 3035 M\n31 -107 V\n41
      -39 V\n-32 109 V\n-40 37 V\n.6755 g 3684 3139 N -40 37 41 -105 38 -36
      h\n1.000 UP\n1.000 UL\nLT2\n3684 3139 M\n39 -104 V\n40 -37 V\n-41 105
      V\n-38 36 V\n.7167 g 3634 3239 N -38 36 51 -101 37 -35 h\n1.000
      UP\n1.000 UL\nLT2\n3634 3239 M\n50 -100 V\n38 -36 V\n-51 101 V\n-37 35
      V\n.7562 g 3576 3333 N -37 35 60 -96 35 -33 h\n1.000 UP\n1.000
      UL\nLT2\n3576 3333 M\n58 -94 V\n37 -35 V\n-60 96 V\n-35 33 V\n.7936 g
      3510 3421 N -35 33 69 -90 32 -31 h\n1.000 UP\n1.000 UL\nLT2\n3510 3421
      M\n66 -88 V\n35 -33 V\n-69 90 V\n-32 31 V\n.8286 g 3435 3503 N -32 31
      77 -84 30 -29 h\n1.000 UP\n1.000 UL\nLT2\n3435 3503 M\n75 -82 V\n32 -31
      V\n-77 84 V\n-30 29 V\n.861 g 3353 3577 N -30 29 84 -77 28 -26 h\n1.000
      UP\n1.000 UL\nLT2\n3353 3577 M\n82 -74 V\n30 -29 V\n-84 77 V\n-28 26
      V\n.8905 g 3264 3643 N -28 26 92 -69 25 -23 h\n1.000 UP\n1.000
      UL\nLT2\n3264 3643 M\n89 -66 V\n28 -26 V\n-92 69 V\n-25 23 V\n.9169 g
      3170 3701 N -25 23 98 -61 21 -20 h\n1.000 UP\n1.000 UL\nLT2\n3170 3701
      M\n94 -58 V\n25 -23 V\n-98 61 V\n-21 20 V\n.9399 g 3070 3750 N -21 20
      103 -52 18 -17 h\n1.000 UP\n1.000 UL\nLT2\n3070 3750 M\n100 -49 V\n21
      -20 V\n-103 52 V\n-18 17 V\n.9593 g 2965 3790 N -18 17 108 -43 15 -14
      h\n1.000 UP\n1.000 UL\nLT2\n2965 3790 M\n105 -40 V\n18 -17 V\n-108 43
      V\n-15 14 V\n.975 g 2857 3819 N -15 14 111 -33 12 -10 h\n1.000
      UP\n1.000 UL\nLT2\n2857 3819 M\n108 -29 V\n15 -14 V\n-111 33 V\n-12 10
      V\n.987 g 2747 3839 N -12 10 115 -23 7 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2747 3839 M\n110 -20 V\n12 -10 V\n-115 23 V\n-7 7 V\n.995 g
      2634 3849 N -7 7 116 -14 4 -3 h\n1.000 UP\n1.000 UL\nLT2\n2634 3849
      M\n113 -10 V\n7 -7 V\n-116 14 V\n-4 3 V\n.999 g 2521 3849 N -4 3 117 -3
      0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n113 0 V\n4 -3 V\n-117 3
      V\n.001 g 2638 1337 N 0 0 -117 -3 0 -4 h\n1.000 UP\n1.000 UL\nLT2\n2638
      1337 M\n-117 -7 V\n117 3 V\n0 4 V\n.005 g 2754 1354 N 0 4 -116 -14 0 -7
      h\n1.000 UP\n1.000 UL\nLT2\n2754 1354 M\n-116 -17 V\n0 -4 V\n116 14
      V\n0 7 V\n.013 g 2869 1381 N 0 7 -115 -23 0 -11 h\n1.000 UP\n1.000
      UL\nLT2\n2869 1381 M\n-115 -27 V\n0 -7 V\n115 23 V\n0 11 V\n.025 g 2980
      1417 N 0 11 -111 -33 0 -14 h\n1.000 UP\n1.000 UL\nLT2\n2980 1417
      M\n-111 -36 V\n0 -11 V\n111 33 V\n0 14 V\n.0407 g 3088 1463 N 0 14 -108
      -43 0 -17 h\n1.000 UP\n1.000 UL\nLT2\n3088 1463 M\n-108 -46 V\n0 -14
      V\n108 43 V\n0 17 V\n.0601 g 3191 1518 N 0 17 -103 -52 0 -20 h\n1.000
      UP\n1.000 UL\nLT2\n3191 1518 M\n-103 -55 V\n0 -17 V\n103 52 V\n0 20
      V\n.0831 g 3289 1582 N 0 20 -98 -61 0 -23 h\n1.000 UP\n1.000
      UL\nLT2\n3289 1582 M\n-98 -64 V\n0 -20 V\n98 61 V\n0 23 V\n.1095 g 3381
      1654 N 0 23 -92 -69 0 -26 h\n1.000 UP\n1.000 UL\nLT2\n3381 1654 M\n-92
      -72 V\n0 -23 V\n92 69 V\n0 26 V\n.139 g 3465 1733 N 0 26 -84 -77 0 -28
      h\n1.000 UP\n1.000 UL\nLT2\n3465 1733 M\n-84 -79 V\n0 -26 V\n84 77 V\n0
      28 V\n.1714 g 3542 1820 N 0 28 -77 -84 0 -31 h\n1.000 UP\n1.000
      UL\nLT2\n3542 1820 M\n-77 -87 V\n0 -28 V\n77 84 V\n0 31 V\n.2064 g 3611
      1912 N 0 31 -69 -90 0 -33 h\n1.000 UP\n1.000 UL\nLT2\n3611 1912 M\n-69
      -92 V\n0 -31 V\n69 90 V\n0 33 V\n.2438 g 3671 2010 N 0 33 -60 -96 0 -35
      h\n1.000 UP\n1.000 UL\nLT2\n3671 2010 M\n-60 -98 V\n0 -33 V\n60 96 V\n0
      35 V\n.2833 g 3722 2113 N 0 35 -51 -101 0 -37 h\n1.000 UP\n1.000
      UL\nLT2\n3722 2113 M\n-51 -103 V\n0 -35 V\n51 101 V\n0 37 V\n.3245 g
      3763 2219 N 0 37 -41 -105 0 -38 h\n1.000 UP\n1.000 UL\nLT2\n3763 2219
      M\n-41 -106 V\n0 -37 V\n41 105 V\n0 38 V\n.3671 g 3795 2329 N 0 38 -32
      -109 0 -39 h\n1.000 UP\n1.000 UL\nLT2\n3795 2329 M\n-32 -110 V\n0 -38
      V\n32 109 V\n0 39 V\n.4108 g 3815 2440 N 0 39 -20 -111 0 -39 h\n1.000
      UP\n1.000 UL\nLT2\n3815 2440 M\n-20 -111 V\n0 -39 V\n20 111 V\n0 39
      V\n.4552 g 3826 2553 N 0 39 -11 -112 0 -40 h\n1.000 UP\n1.000
      UL\nLT2\n3826 2553 M\n-11 -113 V\n0 -39 V\n11 112 V\n0 40 V\n.5 g 3826
      2666 N 0 40 0 -113 0 -40 h\n1.000 UP\n1.000 UL\nLT2\n3826 2666 M\n0
      -113 V\n0 -40 V\n0 113 V\n0 40 V\n.5448 g 3815 2778 N 0 40 11 -113 0
      -39 h\n1.000 UP\n1.000 UL\nLT2\n3815 2778 M\n11 -112 V\n0 -40 V\n-11
      113 V\n0 39 V\n.5892 g 3795 2889 N 0 39 20 -111 0 -39 h\n1.000
      UP\n1.000 UL\nLT2\n3795 2889 M\n20 -111 V\n0 -39 V\n-20 111 V\n0 39
      V\n.6329 g 3763 2998 N 0 39 32 -110 0 -38 h\n1.000 UP\n1.000
      UL\nLT2\n3763 2998 M\n32 -109 V\n0 -39 V\n-32 110 V\n0 38 V\n.6755 g
      3722 3103 N 0 38 41 -106 0 -37 h\n1.000 UP\n1.000 UL\nLT2\n3722 3103
      M\n41 -105 V\n0 -38 V\n-41 106 V\n0 37 V\n.7167 g 3671 3204 N 0 37 51
      -103 0 -35 h\n1.000 UP\n1.000 UL\nLT2\n3671 3204 M\n51 -101 V\n0 -37
      V\n-51 103 V\n0 35 V\n.7562 g 3611 3300 N 0 35 60 -98 0 -33 h\n1.000
      UP\n1.000 UL\nLT2\n3611 3300 M\n60 -96 V\n0 -35 V\n-60 98 V\n0 33
      V\n.7936 g 3542 3390 N 0 33 69 -92 0 -31 h\n1.000 UP\n1.000
      UL\nLT2\n3542 3390 M\n69 -90 V\n0 -33 V\n-69 92 V\n0 31 V\n.8286 g 3465
      3474 N 0 31 77 -87 0 -28 h\n1.000 UP\n1.000 UL\nLT2\n3465 3474 M\n77
      -84 V\n0 -31 V\n-77 87 V\n0 28 V\n.861 g 3381 3551 N 0 28 84 -79 0 -26
      h\n1.000 UP\n1.000 UL\nLT2\n3381 3551 M\n84 -77 V\n0 -28 V\n-84 79 V\n0
      26 V\n.8905 g 3289 3620 N 0 26 92 -72 0 -23 h\n1.000 UP\n1.000
      UL\nLT2\n3289 3620 M\n92 -69 V\n0 -26 V\n-92 72 V\n0 23 V\n.9169 g 3191
      3681 N 0 23 98 -64 0 -20 h\n1.000 UP\n1.000 UL\nLT2\n3191 3681 M\n98
      -61 V\n0 -23 V\n-98 64 V\n0 20 V\n.9399 g 3088 3733 N 0 20 103 -55 0
      -17 h\n1.000 UP\n1.000 UL\nLT2\n3088 3733 M\n103 -52 V\n0 -20 V\n-103
      55 V\n0 17 V\n.9593 g 2980 3776 N 0 17 108 -46 0 -14 h\n1.000 UP\n1.000
      UL\nLT2\n2980 3776 M\n108 -43 V\n0 -17 V\n-108 46 V\n0 14 V\n.975 g
      2869 3809 N 0 14 111 -36 0 -11 h\n1.000 UP\n1.000 UL\nLT2\n2869 3809
      M\n111 -33 V\n0 -14 V\n-111 36 V\n0 11 V\n.987 g 2754 3832 N 0 11 115
      -27 0 -7 h\n1.000 UP\n1.000 UL\nLT2\n2754 3832 M\n115 -23 V\n0 -11
      V\n-115 27 V\n0 7 V\n.995 g 2638 3846 N 0 7 116 -17 0 -4 h\n1.000
      UP\n1.000 UL\nLT2\n2638 3846 M\n116 -14 V\n0 -7 V\n-116 17 V\n0 4
      V\n.999 g 2521 3849 N 0 4 117 -7 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521
      3849 M\n117 -3 V\n0 -4 V\n-117 7 V\n.001 g 2638 1333 N 0 0 -113 0 -4 -3
      h\n1.000 UP\n1.000 UL\nLT2\n2638 1333 M\n-117 -3 V\n113 0 V\n4 3
      V\n.005 g 2754 1347 N 4 3 -113 -10 -7 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2754 1347 M\n-116 -14 V\n-4 -3 V\n113 10 V\n7 7 V\n.013 g 2869
      1370 N 7 7 -110 -20 -12 -10 h\n1.000 UP\n1.000 UL\nLT2\n2869 1370
      M\n-115 -23 V\n-7 -7 V\n110 20 V\n12 10 V\n.025 g 2980 1403 N 12 10
      -108 -29 -15 -14 h\n1.000 UP\n1.000 UL\nLT2\n2980 1403 M\n-111 -33
      V\n-12 -10 V\n108 29 V\n15 14 V\n.0407 g 3088 1446 N 15 14 -105 -40 -18
      -17 h\n1.000 UP\n1.000 UL\nLT2\n3088 1446 M\n-108 -43 V\n-15 -14 V\n105
      40 V\n18 17 V\n.0601 g 3191 1498 N 18 17 -100 -49 -21 -20 h\n1.000
      UP\n1.000 UL\nLT2\n3191 1498 M\n-103 -52 V\n-18 -17 V\n100 49 V\n21 20
      V\n.0831 g 3289 1559 N 21 20 -94 -58 -25 -23 h\n1.000 UP\n1.000
      UL\nLT2\n3289 1559 M\n-98 -61 V\n-21 -20 V\n94 58 V\n25 23 V\n.1095 g
      3381 1628 N 25 23 -89 -66 -28 -26 h\n1.000 UP\n1.000 UL\nLT2\n3381 1628
      M\n-92 -69 V\n-25 -23 V\n89 66 V\n28 26 V\n.139 g 3465 1705 N 28 26 -82
      -74 -30 -29 h\n1.000 UP\n1.000 UL\nLT2\n3465 1705 M\n-84 -77 V\n-28 -26
      V\n82 74 V\n30 29 V\n.1714 g 3542 1789 N 30 29 -75 -82 -32 -31 h\n1.000
      UP\n1.000 UL\nLT2\n3542 1789 M\n-77 -84 V\n-30 -29 V\n75 82 V\n32 31
      V\n.2064 g 3611 1879 N 32 31 -66 -88 -35 -33 h\n1.000 UP\n1.000
      UL\nLT2\n3611 1879 M\n-69 -90 V\n-32 -31 V\n66 88 V\n35 33 V\n.2438 g
      3671 1975 N 35 33 -58 -94 -37 -35 h\n1.000 UP\n1.000 UL\nLT2\n3671 1975
      M\n-60 -96 V\n-35 -33 V\n58 94 V\n37 35 V\n.2833 g 3722 2076 N 37 35
      -50 -100 -38 -36 h\n1.000 UP\n1.000 UL\nLT2\n3722 2076 M\n-51 -101
      V\n-37 -35 V\n50 100 V\n38 36 V\n.3245 g 3763 2181 N 38 36 -39 -104 -40
      -37 h\n1.000 UP\n1.000 UL\nLT2\n3763 2181 M\n-41 -105 V\n-38 -36 V\n39
      104 V\n40 37 V\n.3671 g 3795 2290 N 40 37 -31 -107 -41 -39 h\n1.000
      UP\n1.000 UL\nLT2\n3795 2290 M\n-32 -109 V\n-40 -37 V\n31 107 V\n41 39
      V\n.4108 g 3815 2401 N 41 39 -20 -111 -41 -39 h\n1.000 UP\n1.000
      UL\nLT2\n3815 2401 M\n-20 -111 V\n-41 -39 V\n20 111 V\n41 39 V\n.4552 g
      3826 2513 N 41 39 -10 -112 -42 -39 h\n1.000 UP\n1.000 UL\nLT2\n3826
      2513 M\n-11 -112 V\n-41 -39 V\n10 112 V\n42 39 V\n.5 g 3826 2626 N 42
      39 0 -113 -42 -39 h\n1.000 UP\n1.000 UL\nLT2\n3826 2626 M\n0 -113
      V\n-42 -39 V\n0 113 V\n42 39 V\n.5448 g 3815 2739 N 42 39 10 -113 -41
      -39 h\n1.000 UP\n1.000 UL\nLT2\n3815 2739 M\n11 -113 V\n-42 -39 V\n-10
      113 V\n41 39 V\n.5892 g 3795 2850 N 41 39 20 -112 -41 -38 h\n1.000
      UP\n1.000 UL\nLT2\n3795 2850 M\n20 -111 V\n-41 -39 V\n-20 112 V\n41 38
      V\n.6329 g 3763 2960 N 41 38 31 -110 -40 -38 h\n1.000 UP\n1.000
      UL\nLT2\n3763 2960 M\n32 -110 V\n-41 -38 V\n-31 110 V\n40 38 V\n.6755 g
      3722 3066 N 40 38 39 -108 -38 -36 h\n1.000 UP\n1.000 UL\nLT2\n3722 3066
      M\n41 -106 V\n-40 -38 V\n-39 108 V\n38 36 V\n.7167 g 3671 3169 N 38 36
      50 -104 -37 -35 h\n1.000 UP\n1.000 UL\nLT2\n3671 3169 M\n51 -103 V\n-38
      -36 V\n-50 104 V\n37 35 V\n.7562 g 3611 3267 N 37 35 58 -100 -35 -33
      h\n1.000 UP\n1.000 UL\nLT2\n3611 3267 M\n60 -98 V\n-37 -35 V\n-58 100
      V\n35 33 V\n.7936 g 3542 3359 N 35 33 66 -95 -32 -30 h\n1.000 UP\n1.000
      UL\nLT2\n3542 3359 M\n69 -92 V\n-35 -33 V\n-66 95 V\n32 30 V\n.8286 g
      3465 3446 N 32 30 75 -88 -30 -29 h\n1.000 UP\n1.000 UL\nLT2\n3465 3446
      M\n77 -87 V\n-32 -30 V\n-75 88 V\n30 29 V\n.861 g 3381 3525 N 30 29 82
      -82 -28 -26 h\n1.000 UP\n1.000 UL\nLT2\n3381 3525 M\n84 -79 V\n-30 -29
      V\n-82 82 V\n28 26 V\n.8905 g 3289 3597 N 28 26 89 -75 -25 -23 h\n1.000
      UP\n1.000 UL\nLT2\n3289 3597 M\n92 -72 V\n-28 -26 V\n-89 75 V\n25 23
      V\n.9169 g 3191 3661 N 25 23 94 -66 -21 -21 h\n1.000 UP\n1.000
      UL\nLT2\n3191 3661 M\n98 -64 V\n-25 -23 V\n-94 66 V\n21 21 V\n.9399 g
      3088 3716 N 21 21 100 -59 -18 -17 h\n1.000 UP\n1.000 UL\nLT2\n3088 3716
      M\n103 -55 V\n-21 -21 V\n-100 59 V\n18 17 V\n.9593 g 2980 3762 N 18 17
      105 -49 -15 -14 h\n1.000 UP\n1.000 UL\nLT2\n2980 3762 M\n108 -46 V\n-18
      -17 V\n-105 49 V\n15 14 V\n.975 g 2869 3798 N 15 14 108 -40 -12 -10
      h\n1.000 UP\n1.000 UL\nLT2\n2869 3798 M\n111 -36 V\n-15 -14 V\n-108 40
      V\n12 10 V\n.987 g 2754 3825 N 12 10 110 -30 -7 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2754 3825 M\n115 -27 V\n-12 -10 V\n-110 30 V\n7 7 V\n.995 g
      2638 3842 N 7 7 113 -21 -4 -3 h\n1.000 UP\n1.000 UL\nLT2\n2638 3842
      M\n116 -17 V\n-7 -7 V\n-113 21 V\n4 3 V\n.999 g 2521 3849 N 4 3 113 -10
      0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n117 -7 V\n-4 -3 V\n-113 10
      V\n.001 g 2634 1330 N 0 0 -106 4 -7 -4 h\n1.000 UP\n1.000 UL\nLT2\n2634
      1330 M\n-113 0 V\n106 -4 V\n7 4 V\n.005 g 2747 1340 N 7 4 -105 -7 -15
      -7 h\n1.000 UP\n1.000 UL\nLT2\n2747 1340 M\n-113 -10 V\n-7 -4 V\n105 7
      V\n15 7 V\n.013 g 2857 1360 N 15 7 -103 -17 -22 -10 h\n1.000 UP\n1.000
      UL\nLT2\n2857 1360 M\n-110 -20 V\n-15 -7 V\n103 17 V\n22 10 V\n.025 g
      2965 1389 N 22 10 -101 -26 -29 -13 h\n1.000 UP\n1.000 UL\nLT2\n2965
      1389 M\n-108 -29 V\n-22 -10 V\n101 26 V\n29 13 V\n.0407 g 3070 1429 N
      29 13 -98 -37 -36 -16 h\n1.000 UP\n1.000 UL\nLT2\n3070 1429 M\n-105 -40
      V\n-29 -13 V\n98 37 V\n36 16 V\n.0601 g 3170 1478 N 36 16 -93 -46 -43
      -19 h\n1.000 UP\n1.000 UL\nLT2\n3170 1478 M\n-100 -49 V\n-36 -16 V\n93
      46 V\n43 19 V\n.0831 g 3264 1536 N 43 19 -89 -55 -48 -22 h\n1.000
      UP\n1.000 UL\nLT2\n3264 1536 M\n-94 -58 V\n-43 -19 V\n89 55 V\n48 22
      V\n.1095 g 3353 1602 N 48 22 -83 -63 -54 -25 h\n1.000 UP\n1.000
      UL\nLT2\n3353 1602 M\n-89 -66 V\n-48 -22 V\n83 63 V\n54 25 V\n.139 g
      3435 1676 N 54 25 -76 -72 -60 -27 h\n1.000 UP\n1.000 UL\nLT2\n3435 1676
      M\n-82 -74 V\n-54 -25 V\n76 72 V\n60 27 V\n.1714 g 3510 1758 N 60 27
      -70 -80 -65 -29 h\n1.000 UP\n1.000 UL\nLT2\n3510 1758 M\n-75 -82 V\n-60
      -27 V\n70 80 V\n65 29 V\n.2064 g 3576 1846 N 65 29 -62 -86 -69 -31
      h\n1.000 UP\n1.000 UL\nLT2\n3576 1846 M\n-66 -88 V\n-65 -29 V\n62 86
      V\n69 31 V\n.2438 g 3634 1940 N 69 31 -55 -93 -72 -32 h\n1.000
      UP\n1.000 UL\nLT2\n3634 1940 M\n-58 -94 V\n-69 -31 V\n55 93 V\n72 32
      V\n.2833 g 3684 2040 N 72 32 -46 -98 -76 -34 h\n1.000 UP\n1.000
      UL\nLT2\n3684 2040 M\n-50 -100 V\n-72 -32 V\n46 98 V\n76 34 V\n.3245 g
      3723 2144 N 76 34 -37 -102 -78 -36 h\n1.000 UP\n1.000 UL\nLT2\n3723
      2144 M\n-39 -104 V\n-76 -34 V\n37 102 V\n78 36 V\n.3671 g 3754 2251 N
      78 36 -28 -107 -81 -36 h\n1.000 UP\n1.000 UL\nLT2\n3754 2251 M\n-31
      -107 V\n-78 -36 V\n28 107 V\n81 36 V\n.4108 g 3774 2362 N 81 36 -19
      -110 -82 -37 h\n1.000 UP\n1.000 UL\nLT2\n3774 2362 M\n-20 -111 V\n-81
      -36 V\n19 110 V\n82 37 V\n.4552 g 3784 2474 N 82 37 -10 -112 -82 -37
      h\n1.000 UP\n1.000 UL\nLT2\n3784 2474 M\n-10 -112 V\n-82 -37 V\n10 112
      V\n82 37 V\n.5 g 3784 2587 N 82 37 0 -113 -82 -37 h\n1.000 UP\n1.000
      UL\nLT2\n3784 2587 M\n0 -113 V\n-82 -37 V\n0 113 V\n82 37 V\n.5448 g
      3774 2700 N 82 37 10 -113 -82 -37 h\n1.000 UP\n1.000 UL\nLT2\n3774 2700
      M\n10 -113 V\n-82 -37 V\n-10 113 V\n82 37 V\n.5892 g 3754 2812 N 82 37
      19 -113 -81 -36 h\n1.000 UP\n1.000 UL\nLT2\n3754 2812 M\n20 -112 V\n-82
      -37 V\n-19 113 V\n81 36 V\n.6329 g 3723 2922 N 81 36 28 -111 -78 -35
      h\n1.000 UP\n1.000 UL\nLT2\n3723 2922 M\n31 -110 V\n-81 -36 V\n-28 111
      V\n78 35 V\n.6755 g 3684 3030 N 78 35 37 -109 -76 -34 h\n1.000
      UP\n1.000 UL\nLT2\n3684 3030 M\n39 -108 V\n-78 -35 V\n-37 109 V\n76 34
      V\n.7167 g 3634 3134 N 76 34 46 -105 -72 -33 h\n1.000 UP\n1.000
      UL\nLT2\n3634 3134 M\n50 -104 V\n-76 -34 V\n-46 105 V\n72 33 V\n.7562 g
      3576 3234 N 72 33 55 -102 -69 -31 h\n1.000 UP\n1.000 UL\nLT2\n3576 3234
      M\n58 -100 V\n-72 -33 V\n-55 102 V\n69 31 V\n.7936 g 3510 3329 N 69 31
      62 -96 -65 -30 h\n1.000 UP\n1.000 UL\nLT2\n3510 3329 M\n66 -95 V\n-69
      -31 V\n-62 96 V\n65 30 V\n.8286 g 3435 3417 N 65 30 70 -91 -60 -27
      h\n1.000 UP\n1.000 UL\nLT2\n3435 3417 M\n75 -88 V\n-65 -30 V\n-70 91
      V\n60 27 V\n.861 g 3353 3499 N 60 27 76 -85 -54 -24 h\n1.000 UP\n1.000
      UL\nLT2\n3353 3499 M\n82 -82 V\n-60 -27 V\n-76 85 V\n54 24 V\n.8905 g
      3264 3574 N 54 24 83 -77 -48 -22 h\n1.000 UP\n1.000 UL\nLT2\n3264 3574
      M\n89 -75 V\n-54 -24 V\n-83 77 V\n48 22 V\n.9169 g 3170 3640 N 48 22 89
      -69 -43 -19 h\n1.000 UP\n1.000 UL\nLT2\n3170 3640 M\n94 -66 V\n-48 -22
      V\n-89 69 V\n43 19 V\n.9399 g 3070 3699 N 43 19 93 -61 -36 -17 h\n1.000
      UP\n1.000 UL\nLT2\n3070 3699 M\n100 -59 V\n-43 -19 V\n-93 61 V\n36 17
      V\n.9593 g 2965 3748 N 36 17 98 -53 -29 -13 h\n1.000 UP\n1.000
      UL\nLT2\n2965 3748 M\n105 -49 V\n-36 -17 V\n-98 53 V\n29 13 V\n.975 g
      2857 3788 N 29 13 101 -43 -22 -10 h\n1.000 UP\n1.000 UL\nLT2\n2857 3788
      M\n108 -40 V\n-29 -13 V\n-101 43 V\n22 10 V\n.987 g 2747 3818 N 22 10
      103 -34 -15 -6 h\n1.000 UP\n1.000 UL\nLT2\n2747 3818 M\n110 -30 V\n-22
      -10 V\n-103 34 V\n15 6 V\n.995 g 2634 3839 N 15 6 105 -23 -7 -4
      h\n1.000 UP\n1.000 UL\nLT2\n2634 3839 M\n113 -21 V\n-15 -6 V\n-105 23
      V\n7 4 V\n.999 g 2521 3849 N 7 4 106 -14 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n113 -10 V\n-7 -4 V\n-106 14 V\n.001 g 2627 1326 N
      0 0 -95 7 -11 -3 h\n1.000 UP\n1.000 UL\nLT2\n2627 1326 M\n-106 4 V\n95
      -7 V\n11 3 V\n.005 g 2732 1333 N 11 3 -95 -4 -21 -6 h\n1.000 UP\n1.000
      UL\nLT2\n2732 1333 M\n-105 -7 V\n-11 -3 V\n95 4 V\n21 6 V\n.013 g 2835
      1350 N 21 6 -92 -14 -32 -9 h\n1.000 UP\n1.000 UL\nLT2\n2835 1350
      M\n-103 -17 V\n-21 -6 V\n92 14 V\n32 9 V\n.025 g 2936 1376 N 32 9 -91
      -23 -42 -12 h\n1.000 UP\n1.000 UL\nLT2\n2936 1376 M\n-101 -26 V\n-32 -9
      V\n91 23 V\n42 12 V\n.0407 g 3034 1413 N 42 12 -87 -34 -53 -15 h\n1.000
      UP\n1.000 UL\nLT2\n3034 1413 M\n-98 -37 V\n-42 -12 V\n87 34 V\n53 15
      V\n.0601 g 3127 1459 N 53 15 -84 -43 -62 -18 h\n1.000 UP\n1.000
      UL\nLT2\n3127 1459 M\n-93 -46 V\n-53 -15 V\n84 43 V\n62 18 V\n.0831 g
      3216 1514 N 62 18 -80 -53 -71 -20 h\n1.000 UP\n1.000 UL\nLT2\n3216 1514
      M\n-89 -55 V\n-62 -18 V\n80 53 V\n71 20 V\n.1095 g 3299 1577 N 71 20
      -74 -61 -80 -22 h\n1.000 UP\n1.000 UL\nLT2\n3299 1577 M\n-83 -63 V\n-71
      -20 V\n74 61 V\n80 22 V\n.139 g 3375 1649 N 80 22 -69 -70 -87 -24
      h\n1.000 UP\n1.000 UL\nLT2\n3375 1649 M\n-76 -72 V\n-80 -22 V\n69 70
      V\n87 24 V\n.1714 g 3445 1729 N 87 24 -63 -77 -94 -27 h\n1.000
      UP\n1.000 UL\nLT2\n3445 1729 M\n-70 -80 V\n-87 -24 V\n63 77 V\n94 27
      V\n.2064 g 3507 1815 N 94 27 -56 -84 -100 -29 h\n1.000 UP\n1.000
      UL\nLT2\n3507 1815 M\n-62 -86 V\n-94 -27 V\n56 84 V\n100 29 V\n.2438 g
      3562 1908 N 100 29 -48 -91 -107 -31 h\n1.000 UP\n1.000 UL\nLT2\n3562
      1908 M\n-55 -93 V\n-100 -29 V\n48 91 V\n107 31 V\n.2833 g 3608 2006 N
      107 31 -42 -97 -111 -32 h\n1.000 UP\n1.000 UL\nLT2\n3608 2006 M\n-46
      -98 V\n-107 -31 V\n42 97 V\n111 32 V\n.3245 g 3645 2108 N 111 32 -33
      -102 -115 -32 h\n1.000 UP\n1.000 UL\nLT2\n3645 2108 M\n-37 -102 V\n-111
      -32 V\n33 102 V\n115 32 V\n.3671 g 3673 2215 N 115 32 -26 -106 -117 -33
      h\n1.000 UP\n1.000 UL\nLT2\n3673 2215 M\n-28 -107 V\n-115 -32 V\n26 106
      V\n117 33 V\n.4108 g 3692 2325 N 117 33 -17 -109 -119 -34 h\n1.000
      UP\n1.000 UL\nLT2\n3692 2325 M\n-19 -110 V\n-117 -33 V\n17 109 V\n119
      34 V\n.4552 g 3702 2437 N 119 34 -8 -111 -121 -35 h\n1.000 UP\n1.000
      UL\nLT2\n3702 2437 M\n-10 -112 V\n-119 -34 V\n8 111 V\n121 35 V\n.5 g
      3702 2550 N 121 35 0 -113 -121 -35 h\n1.000 UP\n1.000 UL\nLT2\n3702
      2550 M\n0 -113 V\n-121 -35 V\n0 113 V\n121 35 V\n.5448 g 3692 2663 N
      121 35 8 -114 -119 -34 h\n1.000 UP\n1.000 UL\nLT2\n3692 2663 M\n10 -113
      V\n-121 -35 V\n-8 114 V\n119 34 V\n.5892 g 3673 2776 N 119 34 17 -113
      -117 -34 h\n1.000 UP\n1.000 UL\nLT2\n3673 2776 M\n19 -113 V\n-119 -34
      V\n-17 113 V\n117 34 V\n.6329 g 3645 2887 N 117 34 26 -112 -115 -33
      h\n1.000 UP\n1.000 UL\nLT2\n3645 2887 M\n28 -111 V\n-117 -34 V\n-26 112
      V\n115 33 V\n.6755 g 3608 2996 N 115 33 33 -110 -111 -32 h\n1.000
      UP\n1.000 UL\nLT2\n3608 2996 M\n37 -109 V\n-115 -33 V\n-33 110 V\n111
      32 V\n.7167 g 3562 3101 N 111 32 42 -107 -107 -30 h\n1.000 UP\n1.000
      UL\nLT2\n3562 3101 M\n46 -105 V\n-111 -32 V\n-42 107 V\n107 30 V\n.7562
      g 3507 3203 N 107 30 48 -103 -100 -29 h\n1.000 UP\n1.000 UL\nLT2\n3507
      3203 M\n55 -102 V\n-107 -30 V\n-48 103 V\n100 29 V\n.7936 g 3445 3299 N
      100 29 56 -99 -94 -26 h\n1.000 UP\n1.000 UL\nLT2\n3445 3299 M\n62 -96
      V\n-100 -29 V\n-56 99 V\n94 26 V\n.8286 g 3375 3390 N 94 26 63 -92 -87
      -25 h\n1.000 UP\n1.000 UL\nLT2\n3375 3390 M\n70 -91 V\n-94 -26 V\n-63
      92 V\n87 25 V\n.861 g 3299 3475 N 87 25 69 -87 -80 -23 h\n1.000
      UP\n1.000 UL\nLT2\n3299 3475 M\n76 -85 V\n-87 -25 V\n-69 87 V\n80 23
      V\n.8905 g 3216 3552 N 80 23 74 -80 -71 -20 h\n1.000 UP\n1.000
      UL\nLT2\n3216 3552 M\n83 -77 V\n-80 -23 V\n-74 80 V\n71 20 V\n.9169 g
      3127 3621 N 71 20 80 -72 -62 -17 h\n1.000 UP\n1.000 UL\nLT2\n3127 3621
      M\n89 -69 V\n-71 -20 V\n-80 72 V\n62 17 V\n.9399 g 3034 3682 N 62 17 84
      -64 -53 -14 h\n1.000 UP\n1.000 UL\nLT2\n3034 3682 M\n93 -61 V\n-62 -17
      V\n-84 64 V\n53 14 V\n.9593 g 2936 3735 N 53 14 87 -55 -42 -12 h\n1.000
      UP\n1.000 UL\nLT2\n2936 3735 M\n98 -53 V\n-53 -14 V\n-87 55 V\n42 12
      V\n.975 g 2835 3778 N 42 12 91 -46 -32 -9 h\n1.000 UP\n1.000
      UL\nLT2\n2835 3778 M\n101 -43 V\n-42 -12 V\n-91 46 V\n32 9 V\n.987 g
      2732 3812 N 32 9 92 -36 -21 -7 h\n1.000 UP\n1.000 UL\nLT2\n2732 3812
      M\n103 -34 V\n-32 -9 V\n-92 36 V\n21 7 V\n.995 g 2627 3835 N 21 7 95
      -27 -11 -3 h\n1.000 UP\n1.000 UL\nLT2\n2627 3835 M\n105 -23 V\n-21 -7
      V\n-95 27 V\n11 3 V\n.999 g 2521 3849 N 11 3 95 -17 0 0 h\n1.000
      UP\n1.000 UL\nLT2\n2521 3849 M\n106 -14 V\n-11 -3 V\n-95 17 V\n.001 g
      2616 1323 N 0 0 -81 9 -14 -2 h\n1.000 UP\n1.000 UL\nLT2\n2616 1323
      M\n-95 7 V\n81 -9 V\n14 2 V\n.005 g 2711 1327 N 14 2 -81 0 -28 -6
      h\n1.000 UP\n1.000 UL\nLT2\n2711 1327 M\n-95 -4 V\n-14 -2 V\n81 0 V\n28
      6 V\n.013 g 2803 1341 N 28 6 -79 -12 -41 -8 h\n1.000 UP\n1.000
      UL\nLT2\n2803 1341 M\n-92 -14 V\n-28 -6 V\n79 12 V\n41 8 V\n.025 g 2894
      1364 N 41 8 -78 -21 -54 -10 h\n1.000 UP\n1.000 UL\nLT2\n2894 1364
      M\n-91 -23 V\n-41 -8 V\n78 21 V\n54 10 V\n.0407 g 2981 1398 N 54 10 -74
      -31 -67 -13 h\n1.000 UP\n1.000 UL\nLT2\n2981 1398 M\n-87 -34 V\n-54 -10
      V\n74 31 V\n67 13 V\n.0601 g 3065 1441 N 67 13 -72 -41 -79 -15 h\n1.000
      UP\n1.000 UL\nLT2\n3065 1441 M\n-84 -43 V\n-67 -13 V\n72 41 V\n79 15
      V\n.0831 g 3145 1494 N 79 15 -68 -50 -91 -18 h\n1.000 UP\n1.000
      UL\nLT2\n3145 1494 M\n-80 -53 V\n-79 -15 V\n68 50 V\n91 18 V\n.1095 g
      3219 1555 N 91 18 -63 -59 -102 -20 h\n1.000 UP\n1.000 UL\nLT2\n3219
      1555 M\n-74 -61 V\n-91 -18 V\n63 59 V\n102 20 V\n.139 g 3288 1625 N 102
      20 -59 -68 -112 -22 h\n1.000 UP\n1.000 UL\nLT2\n3288 1625 M\n-69 -70
      V\n-102 -20 V\n59 68 V\n112 22 V\n.1714 g 3351 1702 N 112 22 -54 -75
      -121 -24 h\n1.000 UP\n1.000 UL\nLT2\n3351 1702 M\n-63 -77 V\n-112 -22
      V\n54 75 V\n121 24 V\n.2064 g 3407 1786 N 121 24 -48 -83 -129 -25
      h\n1.000 UP\n1.000 UL\nLT2\n3407 1786 M\n-56 -84 V\n-121 -24 V\n48 83
      V\n129 25 V\n.2438 g 3455 1877 N 129 25 -41 -90 -136 -26 h\n1.000
      UP\n1.000 UL\nLT2\n3455 1877 M\n-48 -91 V\n-129 -25 V\n41 90 V\n136 26
      V\n.2833 g 3497 1974 N 136 26 -36 -96 -142 -27 h\n1.000 UP\n1.000
      UL\nLT2\n3497 1974 M\n-42 -97 V\n-136 -26 V\n36 96 V\n142 27 V\n.3245 g
      3530 2076 N 142 27 -28 -100 -147 -29 h\n1.000 UP\n1.000 UL\nLT2\n3530
      2076 M\n-33 -102 V\n-142 -27 V\n28 100 V\n147 29 V\n.3671 g 3556 2182 N
      147 29 -22 -105 -151 -30 h\n1.000 UP\n1.000 UL\nLT2\n3556 2182 M\n-26
      -106 V\n-147 -29 V\n22 105 V\n151 30 V\n.4108 g 3573 2291 N 151 30 -14
      -109 -154 -30 h\n1.000 UP\n1.000 UL\nLT2\n3573 2291 M\n-17 -109 V\n-151
      -30 V\n14 109 V\n154 30 V\n.4552 g 3581 2402 N 154 30 -8 -111 -154 -30
      h\n1.000 UP\n1.000 UL\nLT2\n3581 2402 M\n-8 -111 V\n-154 -30 V\n8 111
      V\n154 30 V\n.5 g 3581 2515 N 154 30 0 -113 -154 -30 h\n1.000 UP\n1.000
      UL\nLT2\n3581 2515 M\n0 -113 V\n-154 -30 V\n0 113 V\n154 30 V\n.5448 g
      3573 2629 N 154 30 8 -114 -154 -30 h\n1.000 UP\n1.000 UL\nLT2\n3573
      2629 M\n8 -114 V\n-154 -30 V\n-8 114 V\n154 30 V\n.5892 g 3556 2742 N
      154 30 14 -114 -151 -29 h\n1.000 UP\n1.000 UL\nLT2\n3556 2742 M\n17
      -113 V\n-154 -30 V\n-14 114 V\n151 29 V\n.6329 g 3530 2854 N 151 29 22
      -113 -147 -28 h\n1.000 UP\n1.000 UL\nLT2\n3530 2854 M\n26 -112 V\n-151
      -29 V\n-22 113 V\n147 28 V\n.6755 g 3497 2964 N 147 28 28 -111 -142 -27
      h\n1.000 UP\n1.000 UL\nLT2\n3497 2964 M\n33 -110 V\n-147 -28 V\n-28 111
      V\n142 27 V\n.7167 g 3455 3071 N 142 27 36 -108 -136 -26 h\n1.000
      UP\n1.000 UL\nLT2\n3455 3071 M\n42 -107 V\n-142 -27 V\n-36 108 V\n136
      26 V\n.7562 g 3407 3174 N 136 26 41 -104 -129 -25 h\n1.000 UP\n1.000
      UL\nLT2\n3407 3174 M\n48 -103 V\n-136 -26 V\n-41 104 V\n129 25 V\n.7936
      g 3351 3273 N 129 25 48 -100 -121 -24 h\n1.000 UP\n1.000 UL\nLT2\n3351
      3273 M\n56 -99 V\n-129 -25 V\n-48 100 V\n121 24 V\n.8286 g 3288 3365 N
      121 24 54 -95 -112 -21 h\n1.000 UP\n1.000 UL\nLT2\n3288 3365 M\n63 -92
      V\n-121 -24 V\n-54 95 V\n112 21 V\n.861 g 3219 3452 N 112 21 59 -88
      -102 -20 h\n1.000 UP\n1.000 UL\nLT2\n3219 3452 M\n69 -87 V\n-112 -21
      V\n-59 88 V\n102 20 V\n.8905 g 3145 3532 N 102 20 63 -82 -91 -18
      h\n1.000 UP\n1.000 UL\nLT2\n3145 3532 M\n74 -80 V\n-102 -20 V\n-63 82
      V\n91 18 V\n.9169 g 3065 3604 N 91 18 68 -74 -79 -16 h\n1.000 UP\n1.000
      UL\nLT2\n3065 3604 M\n80 -72 V\n-91 -18 V\n-68 74 V\n79 16 V\n.9399 g
      2981 3668 N 79 16 72 -67 -67 -13 h\n1.000 UP\n1.000 UL\nLT2\n2981 3668
      M\n84 -64 V\n-79 -16 V\n-72 67 V\n67 13 V\n.9593 g 2894 3723 N 67 13 74
      -57 -54 -11 h\n1.000 UP\n1.000 UL\nLT2\n2894 3723 M\n87 -55 V\n-67 -13
      V\n-74 57 V\n54 11 V\n.975 g 2803 3769 N 54 11 78 -49 -41 -8 h\n1.000
      UP\n1.000 UL\nLT2\n2803 3769 M\n91 -46 V\n-54 -11 V\n-78 49 V\n41 8
      V\n.987 g 2711 3805 N 41 8 79 -39 -28 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2711 3805 M\n92 -36 V\n-41 -8 V\n-79 39 V\n28 5 V\n.995 g 2616
      3832 N 28 5 81 -30 -14 -2 h\n1.000 UP\n1.000 UL\nLT2\n2616 3832 M\n95
      -27 V\n-28 -5 V\n-81 30 V\n14 2 V\n.999 g 2521 3849 N 14 2 81 -19 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n95 -17 V\n-14 -2 V\n-81 19
      V\n.001 g 2602 1321 N 0 0 -65 12 -16 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2602 1321 M\n-81 9 V\n65 -12 V\n16 3 V\n.005 g 2683 1321 N 16
      3 -64 1 -33 -4 h\n1.000 UP\n1.000 UL\nLT2\n2683 1321 M\n-81 0 V\n-16 -3
      V\n64 -1 V\n33 4 V\n.013 g 2762 1333 N 33 4 -63 -9 -49 -7 h\n1.000
      UP\n1.000 UL\nLT2\n2762 1333 M\n-79 -12 V\n-33 -4 V\n63 9 V\n49 7
      V\n.025 g 2840 1354 N 49 7 -62 -19 -65 -9 h\n1.000 UP\n1.000
      UL\nLT2\n2840 1354 M\n-78 -21 V\n-49 -7 V\n62 19 V\n65 9 V\n.0407 g
      2914 1385 N 65 9 -60 -29 -79 -11 h\n1.000 UP\n1.000 UL\nLT2\n2914 1385
      M\n-74 -31 V\n-65 -9 V\n60 29 V\n79 11 V\n.0601 g 2986 1426 N 79 11 -57
      -39 -94 -13 h\n1.000 UP\n1.000 UL\nLT2\n2986 1426 M\n-72 -41 V\n-79 -11
      V\n57 39 V\n94 13 V\n.0831 g 3054 1476 N 94 13 -54 -48 -108 -15
      h\n1.000 UP\n1.000 UL\nLT2\n3054 1476 M\n-68 -50 V\n-94 -13 V\n54 48
      V\n108 15 V\n.1095 g 3117 1535 N 108 15 -50 -58 -121 -16 h\n1.000
      UP\n1.000 UL\nLT2\n3117 1535 M\n-63 -59 V\n-108 -15 V\n50 58 V\n121 16
      V\n.139 g 3176 1603 N 121 16 -47 -66 -133 -18 h\n1.000 UP\n1.000
      UL\nLT2\n3176 1603 M\n-59 -68 V\n-121 -16 V\n47 66 V\n133 18 V\n.1714 g
      3230 1678 N 133 18 -43 -74 -144 -19 h\n1.000 UP\n1.000 UL\nLT2\n3230
      1678 M\n-54 -75 V\n-133 -18 V\n43 74 V\n144 19 V\n.2064 g 3278 1761 N
      144 19 -38 -82 -154 -20 h\n1.000 UP\n1.000 UL\nLT2\n3278 1761 M\n-48
      -83 V\n-144 -19 V\n38 82 V\n154 20 V\n.2438 g 3319 1851 N 154 20 -33
      -88 -162 -22 h\n1.000 UP\n1.000 UL\nLT2\n3319 1851 M\n-41 -90 V\n-154
      -20 V\n33 88 V\n162 22 V\n.2833 g 3355 1947 N 162 22 -28 -95 -170 -23
      h\n1.000 UP\n1.000 UL\nLT2\n3355 1947 M\n-36 -96 V\n-162 -22 V\n28 95
      V\n170 23 V\n.3245 g 3383 2047 N 170 23 -23 -100 -175 -23 h\n1.000
      UP\n1.000 UL\nLT2\n3383 2047 M\n-28 -100 V\n-170 -23 V\n23 100 V\n175
      23 V\n.3671 g 3405 2152 N 175 23 -18 -104 -179 -24 h\n1.000 UP\n1.000
      UL\nLT2\n3405 2152 M\n-22 -105 V\n-175 -23 V\n18 104 V\n179 24 V\n.4108
      g 3419 2261 N 179 24 -11 -108 -182 -25 h\n1.000 UP\n1.000 UL\nLT2\n3419
      2261 M\n-14 -109 V\n-179 -24 V\n11 108 V\n182 25 V\n.4552 g 3427 2372 N
      182 25 -6 -111 -184 -25 h\n1.000 UP\n1.000 UL\nLT2\n3427 2372 M\n-8
      -111 V\n-182 -25 V\n6 111 V\n184 25 V\n.5 g 3427 2485 N 184 25 0 -113
      -184 -25 h\n1.000 UP\n1.000 UL\nLT2\n3427 2485 M\n0 -113 V\n-184 -25
      V\n0 113 V\n184 25 V\n.5448 g 3419 2599 N 184 25 6 -115 -182 -24
      h\n1.000 UP\n1.000 UL\nLT2\n3419 2599 M\n8 -114 V\n-184 -25 V\n-6 115
      V\n182 24 V\n.5892 g 3405 2713 N 182 24 11 -114 -179 -24 h\n1.000
      UP\n1.000 UL\nLT2\n3405 2713 M\n14 -114 V\n-182 -24 V\n-11 114 V\n179
      24 V\n.6329 g 3383 2826 N 179 24 18 -113 -175 -24 h\n1.000 UP\n1.000
      UL\nLT2\n3383 2826 M\n22 -113 V\n-179 -24 V\n-18 113 V\n175 24 V\n.6755
      g 3355 2937 N 175 24 23 -112 -170 -23 h\n1.000 UP\n1.000 UL\nLT2\n3355
      2937 M\n28 -111 V\n-175 -24 V\n-23 112 V\n170 23 V\n.7167 g 3319 3045 N
      170 23 28 -109 -162 -22 h\n1.000 UP\n1.000 UL\nLT2\n3319 3045 M\n36
      -108 V\n-170 -23 V\n-28 109 V\n162 22 V\n.7562 g 3278 3149 N 162 22 33
      -105 -154 -21 h\n1.000 UP\n1.000 UL\nLT2\n3278 3149 M\n41 -104 V\n-162
      -22 V\n-33 105 V\n154 21 V\n.7936 g 3230 3249 N 154 21 38 -102 -144 -19
      h\n1.000 UP\n1.000 UL\nLT2\n3230 3249 M\n48 -100 V\n-154 -21 V\n-38 102
      V\n144 19 V\n.8286 g 3176 3344 N 144 19 43 -96 -133 -18 h\n1.000
      UP\n1.000 UL\nLT2\n3176 3344 M\n54 -95 V\n-144 -19 V\n-43 96 V\n133 18
      V\n.861 g 3117 3432 N 133 18 47 -90 -121 -16 h\n1.000 UP\n1.000
      UL\nLT2\n3117 3432 M\n59 -88 V\n-133 -18 V\n-47 90 V\n121 16 V\n.8905 g
      3054 3514 N 121 16 50 -83 -108 -15 h\n1.000 UP\n1.000 UL\nLT2\n3054
      3514 M\n63 -82 V\n-121 -16 V\n-50 83 V\n108 15 V\n.9169 g 2986 3588 N
      108 15 54 -77 -94 -12 h\n1.000 UP\n1.000 UL\nLT2\n2986 3588 M\n68 -74
      V\n-108 -15 V\n-54 77 V\n94 12 V\n.9399 g 2914 3655 N 94 12 57 -68 -79
      -11 h\n1.000 UP\n1.000 UL\nLT2\n2914 3655 M\n72 -67 V\n-94 -12 V\n-57
      68 V\n79 11 V\n.9593 g 2840 3712 N 79 11 60 -59 -65 -9 h\n1.000
      UP\n1.000 UL\nLT2\n2840 3712 M\n74 -57 V\n-79 -11 V\n-60 59 V\n65 9
      V\n.975 g 2762 3761 N 65 9 62 -51 -49 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2762 3761 M\n78 -49 V\n-65 -9 V\n-62 51 V\n49 7 V\n.987 g 2683
      3800 N 49 7 63 -42 -33 -4 h\n1.000 UP\n1.000 UL\nLT2\n2683 3800 M\n79
      -39 V\n-49 -7 V\n-63 42 V\n33 4 V\n.995 g 2602 3830 N 33 4 64 -31 -16
      -3 h\n1.000 UP\n1.000 UL\nLT2\n2602 3830 M\n81 -30 V\n-33 -4 V\n-64 31
      V\n16 3 V\n.999 g 2521 3849 N 16 3 65 -22 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n81 -19 V\n-16 -3 V\n-65 22 V\n.001 g 2586 1318 N
      0 0 -46 13 -19 -1 h\n1.000 UP\n1.000 UL\nLT2\n2586 1318 M\n-65 12 V\n46
      -13 V\n19 1 V\n.005 g 2650 1317 N 19 1 -46 3 -37 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2650 1317 M\n-64 1 V\n-19 -1 V\n46 -3 V\n37 3 V\n.013 g 2713
      1326 N 37 3 -45 -7 -55 -5 h\n1.000 UP\n1.000 UL\nLT2\n2713 1326 M\n-63
      -9 V\n-37 -3 V\n45 7 V\n55 5 V\n.025 g 2775 1345 N 55 5 -44 -17 -73 -7
      h\n1.000 UP\n1.000 UL\nLT2\n2775 1345 M\n-62 -19 V\n-55 -5 V\n44 17
      V\n73 7 V\n.0407 g 2835 1374 N 73 7 -43 -28 -90 -8 h\n1.000 UP\n1.000
      UL\nLT2\n2835 1374 M\n-60 -29 V\n-73 -7 V\n43 28 V\n90 8 V\n.0601 g
      2892 1413 N 90 8 -41 -37 -106 -10 h\n1.000 UP\n1.000 UL\nLT2\n2892 1413
      M\n-57 -39 V\n-90 -8 V\n41 37 V\n106 10 V\n.0831 g 2946 1461 N 106 10
      -38 -47 -122 -11 h\n1.000 UP\n1.000 UL\nLT2\n2946 1461 M\n-54 -48
      V\n-106 -10 V\n38 47 V\n122 11 V\n.1095 g 2996 1519 N 122 11 -36 -56
      -136 -13 h\n1.000 UP\n1.000 UL\nLT2\n2996 1519 M\n-50 -58 V\n-122 -11
      V\n36 56 V\n136 13 V\n.139 g 3043 1585 N 136 13 -34 -65 -149 -14
      h\n1.000 UP\n1.000 UL\nLT2\n3043 1585 M\n-47 -66 V\n-136 -13 V\n34 65
      V\n149 14 V\n.1714 g 3086 1659 N 149 14 -30 -73 -162 -15 h\n1.000
      UP\n1.000 UL\nLT2\n3086 1659 M\n-43 -74 V\n-149 -14 V\n30 73 V\n162 15
      V\n.2064 g 3124 1741 N 162 15 -27 -81 -173 -16 h\n1.000 UP\n1.000
      UL\nLT2\n3124 1741 M\n-38 -82 V\n-162 -15 V\n27 81 V\n173 16 V\n.2438 g
      3157 1829 N 173 16 -24 -87 -182 -17 h\n1.000 UP\n1.000 UL\nLT2\n3157
      1829 M\n-33 -88 V\n-173 -16 V\n24 87 V\n182 17 V\n.2833 g 3185 1924 N
      182 17 -20 -94 -190 -18 h\n1.000 UP\n1.000 UL\nLT2\n3185 1924 M\n-28
      -95 V\n-182 -17 V\n20 94 V\n190 18 V\n.3245 g 3208 2024 N 190 18 -16
      -100 -197 -18 h\n1.000 UP\n1.000 UL\nLT2\n3208 2024 M\n-23 -100 V\n-190
      -18 V\n16 100 V\n197 18 V\n.3671 g 3226 2128 N 197 18 -13 -104 -202 -18
      h\n1.000 UP\n1.000 UL\nLT2\n3226 2128 M\n-18 -104 V\n-197 -18 V\n13 104
      V\n202 18 V\n.4108 g 3237 2236 N 202 18 -8 -108 -205 -18 h\n1.000
      UP\n1.000 UL\nLT2\n3237 2236 M\n-11 -108 V\n-202 -18 V\n8 108 V\n205 18
      V\n.4552 g 3243 2347 N 205 18 -4 -111 -207 -18 h\n1.000 UP\n1.000
      UL\nLT2\n3243 2347 M\n-6 -111 V\n-205 -18 V\n4 111 V\n207 18 V\n.5 g
      3243 2460 N 207 18 0 -113 -207 -18 h\n1.000 UP\n1.000 UL\nLT2\n3243
      2460 M\n0 -113 V\n-207 -18 V\n0 113 V\n207 18 V\n.5448 g 3237 2575 N
      207 18 4 -114 -205 -19 h\n1.000 UP\n1.000 UL\nLT2\n3237 2575 M\n6 -115
      V\n-207 -18 V\n-4 114 V\n205 19 V\n.5892 g 3226 2689 N 205 19 8 -114
      -202 -19 h\n1.000 UP\n1.000 UL\nLT2\n3226 2689 M\n11 -114 V\n-205 -19
      V\n-8 114 V\n202 19 V\n.6329 g 3208 2802 N 202 19 13 -114 -197 -18
      h\n1.000 UP\n1.000 UL\nLT2\n3208 2802 M\n18 -113 V\n-202 -19 V\n-13 114
      V\n197 18 V\n.6755 g 3185 2914 N 197 18 16 -112 -190 -18 h\n1.000
      UP\n1.000 UL\nLT2\n3185 2914 M\n23 -112 V\n-197 -18 V\n-16 112 V\n190
      18 V\n.7167 g 3157 3023 N 190 18 20 -110 -182 -17 h\n1.000 UP\n1.000
      UL\nLT2\n3157 3023 M\n28 -109 V\n-190 -18 V\n-20 110 V\n182 17 V\n.7562
      g 3124 3128 N 182 17 24 -107 -173 -15 h\n1.000 UP\n1.000 UL\nLT2\n3124
      3128 M\n33 -105 V\n-182 -17 V\n-24 107 V\n173 15 V\n.7936 g 3086 3230 N
      173 15 27 -102 -162 -15 h\n1.000 UP\n1.000 UL\nLT2\n3086 3230 M\n38
      -102 V\n-173 -15 V\n-27 102 V\n162 15 V\n.8286 g 3043 3326 N 162 15 30
      -97 -149 -14 h\n1.000 UP\n1.000 UL\nLT2\n3043 3326 M\n43 -96 V\n-162
      -15 V\n-30 97 V\n149 14 V\n.861 g 2996 3416 N 149 14 34 -91 -136 -13
      h\n1.000 UP\n1.000 UL\nLT2\n2996 3416 M\n47 -90 V\n-149 -14 V\n-34 91
      V\n136 13 V\n.8905 g 2946 3499 N 136 13 36 -85 -122 -11 h\n1.000
      UP\n1.000 UL\nLT2\n2946 3499 M\n50 -83 V\n-136 -13 V\n-36 85 V\n122 11
      V\n.9169 g 2892 3576 N 122 11 38 -78 -106 -10 h\n1.000 UP\n1.000
      UL\nLT2\n2892 3576 M\n54 -77 V\n-122 -11 V\n-38 78 V\n106 10 V\n.9399 g
      2835 3644 N 106 10 41 -70 -90 -8 h\n1.000 UP\n1.000 UL\nLT2\n2835 3644
      M\n57 -68 V\n-106 -10 V\n-41 70 V\n90 8 V\n.9593 g 2775 3703 N 90 8 43
      -61 -73 -6 h\n1.000 UP\n1.000 UL\nLT2\n2775 3703 M\n60 -59 V\n-90 -8
      V\n-43 61 V\n73 6 V\n.975 g 2713 3754 N 73 6 44 -52 -55 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2713 3754 M\n62 -51 V\n-73 -6 V\n-44 52 V\n55 5
      V\n.987 g 2650 3796 N 55 5 45 -43 -37 -4 h\n1.000 UP\n1.000
      UL\nLT2\n2650 3796 M\n63 -42 V\n-55 -5 V\n-45 43 V\n37 4 V\n.995 g 2586
      3827 N 37 4 46 -34 -19 -1 h\n1.000 UP\n1.000 UL\nLT2\n2586 3827 M\n64
      -31 V\n-37 -4 V\n-46 34 V\n19 1 V\n.999 g 2521 3849 N 19 1 46 -23 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n65 -22 V\n-19 -1 V\n-46 23
      V\n.001 g 2567 1317 N 0 0 -26 14 -20 -1 h\n1.000 UP\n1.000
      UL\nLT2\n2567 1317 M\n-46 13 V\n26 -14 V\n20 1 V\n.005 g 2613 1314 N 20
      1 -26 5 -40 -3 h\n1.000 UP\n1.000 UL\nLT2\n2613 1314 M\n-46 3 V\n-20 -1
      V\n26 -5 V\n40 3 V\n.013 g 2658 1321 N 40 3 -26 -7 -59 -3 h\n1.000
      UP\n1.000 UL\nLT2\n2658 1321 M\n-45 -7 V\n-40 -3 V\n26 7 V\n59 3
      V\n.025 g 2702 1338 N 59 3 -25 -16 -78 -4 h\n1.000 UP\n1.000
      UL\nLT2\n2702 1338 M\n-44 -17 V\n-59 -3 V\n25 16 V\n78 4 V\n.0407 g
      2745 1366 N 78 4 -24 -27 -97 -5 h\n1.000 UP\n1.000 UL\nLT2\n2745 1366
      M\n-43 -28 V\n-78 -4 V\n24 27 V\n97 5 V\n.0601 g 2786 1403 N 97 5 -23
      -36 -115 -6 h\n1.000 UP\n1.000 UL\nLT2\n2786 1403 M\n-41 -37 V\n-97 -5
      V\n23 36 V\n115 6 V\n.0831 g 2824 1450 N 115 6 -22 -46 -131 -7 h\n1.000
      UP\n1.000 UL\nLT2\n2824 1450 M\n-38 -47 V\n-115 -6 V\n22 46 V\n131 7
      V\n.1095 g 2860 1506 N 131 7 -20 -55 -147 -8 h\n1.000 UP\n1.000
      UL\nLT2\n2860 1506 M\n-36 -56 V\n-131 -7 V\n20 55 V\n147 8 V\n.139 g
      2894 1571 N 147 8 -19 -64 -162 -9 h\n1.000 UP\n1.000 UL\nLT2\n2894 1571
      M\n-34 -65 V\n-147 -8 V\n19 64 V\n162 9 V\n.1714 g 2924 1644 N 162 9
      -17 -73 -175 -9 h\n1.000 UP\n1.000 UL\nLT2\n2924 1644 M\n-30 -73
      V\n-162 -9 V\n17 73 V\n175 9 V\n.2064 g 2951 1725 N 175 9 -16 -80 -186
      -10 h\n1.000 UP\n1.000 UL\nLT2\n2951 1725 M\n-27 -81 V\n-175 -9 V\n16
      80 V\n186 10 V\n.2438 g 2975 1812 N 186 10 -13 -87 -197 -10 h\n1.000
      UP\n1.000 UL\nLT2\n2975 1812 M\n-24 -87 V\n-186 -10 V\n13 87 V\n197 10
      V\n.2833 g 2995 1906 N 197 10 -11 -93 -206 -11 h\n1.000 UP\n1.000
      UL\nLT2\n2995 1906 M\n-20 -94 V\n-197 -10 V\n11 93 V\n206 11 V\n.3245 g
      3011 2006 N 206 11 -10 -99 -212 -12 h\n1.000 UP\n1.000 UL\nLT2\n3011
      2006 M\n-16 -100 V\n-206 -11 V\n10 99 V\n212 12 V\n.3671 g 3024 2110 N
      212 12 -7 -104 -218 -12 h\n1.000 UP\n1.000 UL\nLT2\n3024 2110 M\n-13
      -104 V\n-212 -12 V\n7 104 V\n218 12 V\n.4108 g 3032 2218 N 218 12 -4
      -107 -222 -13 h\n1.000 UP\n1.000 UL\nLT2\n3032 2218 M\n-8 -108 V\n-218
      -12 V\n4 107 V\n222 13 V\n.4552 g 3036 2329 N 222 13 -3 -111 -223 -13
      h\n1.000 UP\n1.000 UL\nLT2\n3036 2329 M\n-4 -111 V\n-222 -13 V\n3 111
      V\n223 13 V\n.5 g 3036 2442 N 223 13 0 -113 -223 -13 h\n1.000 UP\n1.000
      UL\nLT2\n3036 2442 M\n0 -113 V\n-223 -13 V\n0 113 V\n223 13 V\n.5448 g
      3032 2556 N 223 13 3 -115 -222 -12 h\n1.000 UP\n1.000 UL\nLT2\n3032
      2556 M\n4 -114 V\n-223 -13 V\n-3 115 V\n222 12 V\n.5892 g 3024 2670 N
      222 12 4 -114 -218 -12 h\n1.000 UP\n1.000 UL\nLT2\n3024 2670 M\n8 -114
      V\n-222 -12 V\n-4 114 V\n218 12 V\n.6329 g 3011 2784 N 218 12 7 -114
      -212 -12 h\n1.000 UP\n1.000 UL\nLT2\n3011 2784 M\n13 -114 V\n-218 -12
      V\n-7 114 V\n212 12 V\n.6755 g 2995 2896 N 212 12 10 -113 -206 -11
      h\n1.000 UP\n1.000 UL\nLT2\n2995 2896 M\n16 -112 V\n-212 -12 V\n-10 113
      V\n206 11 V\n.7167 g 2975 3006 N 206 11 11 -110 -197 -11 h\n1.000
      UP\n1.000 UL\nLT2\n2975 3006 M\n20 -110 V\n-206 -11 V\n-11 110 V\n197
      11 V\n.7562 g 2951 3113 N 197 11 13 -107 -186 -11 h\n1.000 UP\n1.000
      UL\nLT2\n2951 3113 M\n24 -107 V\n-197 -11 V\n-13 107 V\n186 11 V\n.7936
      g 2924 3215 N 186 11 16 -103 -175 -10 h\n1.000 UP\n1.000 UL\nLT2\n2924
      3215 M\n27 -102 V\n-186 -11 V\n-16 103 V\n175 10 V\n.8286 g 2894 3312 N
      175 10 17 -98 -162 -9 h\n1.000 UP\n1.000 UL\nLT2\n2894 3312 M\n30 -97
      V\n-175 -10 V\n-17 98 V\n162 9 V\n.861 g 2860 3403 N 162 9 19 -92 -147
      -8 h\n1.000 UP\n1.000 UL\nLT2\n2860 3403 M\n34 -91 V\n-162 -9 V\n-19 92
      V\n147 8 V\n.8905 g 2824 3488 N 147 8 20 -86 -131 -7 h\n1.000 UP\n1.000
      UL\nLT2\n2824 3488 M\n36 -85 V\n-147 -8 V\n-20 86 V\n131 7 V\n.9169 g
      2786 3566 N 131 7 22 -78 -115 -7 h\n1.000 UP\n1.000 UL\nLT2\n2786 3566
      M\n38 -78 V\n-131 -7 V\n-22 78 V\n115 7 V\n.9399 g 2745 3636 N 115 7 23
      -71 -97 -6 h\n1.000 UP\n1.000 UL\nLT2\n2745 3636 M\n41 -70 V\n-115 -7
      V\n-23 71 V\n97 6 V\n.9593 g 2702 3697 N 97 6 24 -63 -78 -4 h\n1.000
      UP\n1.000 UL\nLT2\n2702 3697 M\n43 -61 V\n-97 -6 V\n-24 63 V\n78 4
      V\n.975 g 2658 3749 N 78 4 25 -53 -59 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2658 3749 M\n44 -52 V\n-78 -4 V\n-25 53 V\n59 3 V\n.987 g 2613
      3792 N 59 3 26 -44 -40 -2 h\n1.000 UP\n1.000 UL\nLT2\n2613 3792 M\n45
      -43 V\n-59 -3 V\n-26 44 V\n40 2 V\n.995 g 2567 3826 N 40 2 26 -35 -20
      -1 h\n1.000 UP\n1.000 UL\nLT2\n2567 3826 M\n46 -34 V\n-40 -2 V\n-26 35
      V\n20 1 V\n.999 g 2521 3849 N 20 1 26 -24 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n46 -23 V\n-20 -1 V\n-26 24 V\n.001 g 2547 1316 N
      0 0 -5 15 -21 -1 h\n1.000 UP\n1.000 UL\nLT2\n2547 1316 M\n-26 14 V\n5
      -15 V\n21 1 V\n.005 g 2573 1311 N 21 1 -6 5 -41 -1 h\n1.000 UP\n1.000
      UL\nLT2\n2573 1311 M\n-26 5 V\n-21 -1 V\n6 -5 V\n41 1 V\n.013 g 2599
      1318 N 41 1 -5 -6 -62 -2 h\n1.000 UP\n1.000 UL\nLT2\n2599 1318 M\n-26
      -7 V\n-41 -1 V\n5 6 V\n62 2 V\n.025 g 2624 1334 N 62 2 -5 -16 -82 -2
      h\n1.000 UP\n1.000 UL\nLT2\n2624 1334 M\n-25 -16 V\n-62 -2 V\n5 16
      V\n82 2 V\n.0407 g 2648 1361 N 82 2 -5 -26 -101 -3 h\n1.000 UP\n1.000
      UL\nLT2\n2648 1361 M\n-24 -27 V\n-82 -2 V\n5 26 V\n101 3 V\n.0601 g
      2671 1397 N 101 3 -4 -36 -120 -3 h\n1.000 UP\n1.000 UL\nLT2\n2671 1397
      M\n-23 -36 V\n-101 -3 V\n4 36 V\n120 3 V\n.0831 g 2693 1443 N 120 3 -5
      -46 -137 -3 h\n1.000 UP\n1.000 UL\nLT2\n2693 1443 M\n-22 -46 V\n-120 -3
      V\n5 46 V\n137 3 V\n.1095 g 2713 1498 N 137 3 -4 -55 -153 -3 h\n1.000
      UP\n1.000 UL\nLT2\n2713 1498 M\n-20 -55 V\n-137 -3 V\n4 55 V\n153 3
      V\n.139 g 2732 1562 N 153 3 -4 -63 -168 -4 h\n1.000 UP\n1.000
      UL\nLT2\n2732 1562 M\n-19 -64 V\n-153 -3 V\n4 63 V\n168 4 V\n.1714 g
      2749 1635 N 168 4 -3 -72 -182 -5 h\n1.000 UP\n1.000 UL\nLT2\n2749 1635
      M\n-17 -73 V\n-168 -4 V\n3 72 V\n182 5 V\n.2064 g 2765 1715 N 182 5 -3
      -80 -195 -5 h\n1.000 UP\n1.000 UL\nLT2\n2765 1715 M\n-16 -80 V\n-182 -5
      V\n3 80 V\n195 5 V\n.2438 g 2778 1802 N 195 5 -3 -87 -205 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2778 1802 M\n-13 -87 V\n-195 -5 V\n3 87 V\n205 5
      V\n.2833 g 2789 1895 N 205 5 -2 -93 -214 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2789 1895 M\n-11 -93 V\n-205 -5 V\n2 93 V\n214 5 V\n.3245 g
      2799 1994 N 214 5 -2 -99 -222 -5 h\n1.000 UP\n1.000 UL\nLT2\n2799 1994
      M\n-10 -99 V\n-214 -5 V\n2 99 V\n222 5 V\n.3671 g 2806 2098 N 222 5 -2
      -103 -227 -6 h\n1.000 UP\n1.000 UL\nLT2\n2806 2098 M\n-7 -104 V\n-222
      -5 V\n2 103 V\n227 6 V\n.4108 g 2810 2205 N 227 6 0 -108 -231 -5
      h\n1.000 UP\n1.000 UL\nLT2\n2810 2205 M\n-4 -107 V\n-227 -6 V\n0 108
      V\n231 5 V\n.4552 g 2813 2316 N 231 5 -1 -111 -233 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2813 2316 M\n-3 -111 V\n-231 -5 V\n1 111 V\n233 5
      V\n.5 g 2813 2429 N 233 5 0 -113 -233 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2813 2429 M\n0 -113 V\n-233 -5 V\n0 113 V\n233 5 V\n.5448 g
      2810 2544 N 233 5 1 -114 -231 -6 h\n1.000 UP\n1.000 UL\nLT2\n2810 2544
      M\n3 -115 V\n-233 -5 V\n-1 114 V\n231 6 V\n.5892 g 2806 2658 N 231 6 0
      -115 -227 -5 h\n1.000 UP\n1.000 UL\nLT2\n2806 2658 M\n4 -114 V\n-231 -6
      V\n0 115 V\n227 5 V\n.6329 g 2799 2772 N 227 5 2 -114 -222 -5 h\n1.000
      UP\n1.000 UL\nLT2\n2799 2772 M\n7 -114 V\n-227 -5 V\n-2 114 V\n222 5
      V\n.6755 g 2789 2885 N 222 5 2 -113 -214 -5 h\n1.000 UP\n1.000
      UL\nLT2\n2789 2885 M\n10 -113 V\n-222 -5 V\n-2 113 V\n214 5 V\n.7167 g
      2778 2995 N 214 5 2 -111 -205 -4 h\n1.000 UP\n1.000 UL\nLT2\n2778 2995
      M\n11 -110 V\n-214 -5 V\n-2 111 V\n205 4 V\n.7562 g 2765 3102 N 205 4 3
      -107 -195 -4 h\n1.000 UP\n1.000 UL\nLT2\n2765 3102 M\n13 -107 V\n-205
      -4 V\n-3 107 V\n195 4 V\n.7936 g 2749 3205 N 195 4 3 -103 -182 -4
      h\n1.000 UP\n1.000 UL\nLT2\n2749 3205 M\n16 -103 V\n-195 -4 V\n-3 103
      V\n182 4 V\n.8286 g 2732 3303 N 182 4 3 -98 -168 -4 h\n1.000 UP\n1.000
      UL\nLT2\n2732 3303 M\n17 -98 V\n-182 -4 V\n-3 98 V\n168 4 V\n.861 g
      2713 3395 N 168 4 4 -93 -153 -3 h\n1.000 UP\n1.000 UL\nLT2\n2713 3395
      M\n19 -92 V\n-168 -4 V\n-4 93 V\n153 3 V\n.8905 g 2693 3481 N 153 3 4
      -86 -137 -3 h\n1.000 UP\n1.000 UL\nLT2\n2693 3481 M\n20 -86 V\n-153 -3
      V\n-4 86 V\n137 3 V\n.9169 g 2671 3559 N 137 3 5 -79 -120 -2 h\n1.000
      UP\n1.000 UL\nLT2\n2671 3559 M\n22 -78 V\n-137 -3 V\n-5 79 V\n120 2
      V\n.9399 g 2648 3630 N 120 2 4 -71 -101 -2 h\n1.000 UP\n1.000
      UL\nLT2\n2648 3630 M\n23 -71 V\n-120 -2 V\n-4 71 V\n101 2 V\n.9593 g
      2624 3693 N 101 2 5 -63 -82 -2 h\n1.000 UP\n1.000 UL\nLT2\n2624 3693
      M\n24 -63 V\n-101 -2 V\n-5 63 V\n82 2 V\n.975 g 2599 3746 N 82 2 5 -54
      -62 -1 h\n1.000 UP\n1.000 UL\nLT2\n2599 3746 M\n25 -53 V\n-82 -2 V\n-5
      54 V\n62 1 V\n.987 g 2573 3790 N 62 1 5 -44 -41 -1 h\n1.000 UP\n1.000
      UL\nLT2\n2573 3790 M\n26 -44 V\n-62 -1 V\n-5 44 V\n41 1 V\n.995 g 2547
      3825 N 41 1 6 -35 -21 -1 h\n1.000 UP\n1.000 UL\nLT2\n2547 3825 M\n26
      -35 V\n-41 -1 V\n-6 35 V\n21 1 V\n.999 g 2521 3849 N 21 1 5 -25 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n26 -24 V\n-21 -1 V\n-5 25
      V\n.001 g 2526 1315 N 0 0 16 15 -21 0 h\n1.000 UP\n1.000 UL\nLT2\n2526
      1315 M\n-5 15 V\n-16 -15 V\n21 0 V\n.005 g 2532 1310 N 21 0 15 4 -42 1
      h\n1.000 UP\n1.000 UL\nLT2\n2532 1310 M\n-6 5 V\n-21 0 V\n-15 -4 V\n42
      -1 V\n.013 g 2537 1316 N 42 -1 16 -6 -63 1 h\n1.000 UP\n1.000
      UL\nLT2\n2537 1316 M\n-5 -6 V\n-42 1 V\n-16 6 V\n63 -1 V\n.025 g 2542
      1332 N 63 -1 15 -16 -83 1 h\n1.000 UP\n1.000 UL\nLT2\n2542 1332 M\n-5
      -16 V\n-63 1 V\n-15 16 V\n83 -1 V\n.0407 g 2547 1358 N 83 -1 14 -26
      -102 1 h\n1.000 UP\n1.000 UL\nLT2\n2547 1358 M\n-5 -26 V\n-83 1 V\n-14
      26 V\n102 -1 V\n.0601 g 2551 1394 N 102 -1 14 -36 -120 1 h\n1.000
      UP\n1.000 UL\nLT2\n2551 1394 M\n-4 -36 V\n-102 1 V\n-14 36 V\n120 -1
      V\n.0831 g 2556 1440 N 120 -1 13 -46 -138 1 h\n1.000 UP\n1.000
      UL\nLT2\n2556 1440 M\n-5 -46 V\n-120 1 V\n-13 46 V\n138 -1 V\n.1095 g
      2560 1495 N 138 -1 13 -55 -155 1 h\n1.000 UP\n1.000 UL\nLT2\n2560 1495
      M\n-4 -55 V\n-138 1 V\n-13 55 V\n155 -1 V\n.139 g 2564 1558 N 155 -1 11
      -64 -170 2 h\n1.000 UP\n1.000 UL\nLT2\n2564 1558 M\n-4 -63 V\n-155 1
      V\n-11 64 V\n170 -2 V\n.1714 g 2567 1630 N 170 -2 10 -72 -183 2
      h\n1.000 UP\n1.000 UL\nLT2\n2567 1630 M\n-3 -72 V\n-170 2 V\n-10 72
      V\n183 -2 V\n.2064 g 2570 1710 N 183 -2 10 -80 -196 2 h\n1.000
      UP\n1.000 UL\nLT2\n2570 1710 M\n-3 -80 V\n-183 2 V\n-10 80 V\n196 -2
      V\n.2438 g 2573 1797 N 196 -2 8 -86 -207 1 h\n1.000 UP\n1.000
      UL\nLT2\n2573 1797 M\n-3 -87 V\n-196 2 V\n-8 86 V\n207 -1 V\n.2833 g
      2575 1890 N 207 -1 7 -94 -216 2 h\n1.000 UP\n1.000 UL\nLT2\n2575 1890
      M\n-2 -93 V\n-207 1 V\n-7 94 V\n216 -2 V\n.3245 g 2577 1989 N 216 -2 5
      -99 -223 2 h\n1.000 UP\n1.000 UL\nLT2\n2577 1989 M\n-2 -99 V\n-216 2
      V\n-5 99 V\n223 -2 V\n.3671 g 2579 2092 N 223 -2 4 -103 -229 2 h\n1.000
      UP\n1.000 UL\nLT2\n2579 2092 M\n-2 -103 V\n-223 2 V\n-4 103 V\n229 -2
      V\n.4108 g 2579 2200 N 229 -2 3 -108 -232 2 h\n1.000 UP\n1.000
      UL\nLT2\n2579 2200 M\n0 -108 V\n-229 2 V\n-3 108 V\n232 -2 V\n.4552 g
      2580 2311 N 232 -2 2 -111 -235 2 h\n1.000 UP\n1.000 UL\nLT2\n2580 2311
      M\n-1 -111 V\n-232 2 V\n-2 111 V\n235 -2 V\n.5 g 2580 2424 N 235 -2 0
      -113 -235 2 h\n1.000 UP\n1.000 UL\nLT2\n2580 2424 M\n0 -113 V\n-235 2
      V\n0 113 V\n235 -2 V\n.5448 g 2579 2538 N 235 -2 -2 -114 -232 2
      h\n1.000 UP\n1.000 UL\nLT2\n2579 2538 M\n1 -114 V\n-235 2 V\n2 114
      V\n232 -2 V\n.5892 g 2579 2653 N 232 -2 -3 -115 -229 2 h\n1.000
      UP\n1.000 UL\nLT2\n2579 2653 M\n0 -115 V\n-232 2 V\n3 115 V\n229 -2
      V\n.6329 g 2577 2767 N 229 -2 -4 -114 -223 2 h\n1.000 UP\n1.000
      UL\nLT2\n2577 2767 M\n2 -114 V\n-229 2 V\n4 114 V\n223 -2 V\n.6755 g
      2575 2880 N 223 -2 -5 -113 -216 2 h\n1.000 UP\n1.000 UL\nLT2\n2575 2880
      M\n2 -113 V\n-223 2 V\n5 113 V\n216 -2 V\n.7167 g 2573 2991 N 216 -2 -7
      -110 -207 1 h\n1.000 UP\n1.000 UL\nLT2\n2573 2991 M\n2 -111 V\n-216 2
      V\n7 110 V\n207 -1 V\n.7562 g 2570 3098 N 207 -1 -8 -107 -196 1
      h\n1.000 UP\n1.000 UL\nLT2\n2570 3098 M\n3 -107 V\n-207 1 V\n8 107
      V\n196 -1 V\n.7936 g 2567 3201 N 196 -1 -10 -103 -183 1 h\n1.000
      UP\n1.000 UL\nLT2\n2567 3201 M\n3 -103 V\n-196 1 V\n10 103 V\n183 -1
      V\n.8286 g 2564 3299 N 183 -1 -10 -99 -170 2 h\n1.000 UP\n1.000
      UL\nLT2\n2564 3299 M\n3 -98 V\n-183 1 V\n10 99 V\n170 -2 V\n.861 g 2560
      3392 N 170 -2 -11 -92 -155 1 h\n1.000 UP\n1.000 UL\nLT2\n2560 3392 M\n4
      -93 V\n-170 2 V\n11 92 V\n155 -1 V\n.8905 g 2556 3478 N 155 -1 -13 -86
      -138 1 h\n1.000 UP\n1.000 UL\nLT2\n2556 3478 M\n4 -86 V\n-155 1 V\n13
      86 V\n138 -1 V\n.9169 g 2551 3557 N 138 -1 -13 -79 -120 1 h\n1.000
      UP\n1.000 UL\nLT2\n2551 3557 M\n5 -79 V\n-138 1 V\n13 79 V\n120 -1
      V\n.9399 g 2547 3628 N 120 -1 -14 -71 -102 1 h\n1.000 UP\n1.000
      UL\nLT2\n2547 3628 M\n4 -71 V\n-120 1 V\n14 71 V\n102 -1 V\n.9593 g
      2542 3691 N 102 -1 -14 -62 -83 0 h\n1.000 UP\n1.000 UL\nLT2\n2542 3691
      M\n5 -63 V\n-102 1 V\n14 62 V\n83 0 V\n.975 g 2537 3745 N 83 0 -15 -54
      -63 0 h\n1.000 UP\n1.000 UL\nLT2\n2537 3745 M\n5 -54 V\n-83 0 V\n15 54
      V\n63 0 V\n.987 g 2532 3789 N 63 0 -16 -44 -42 0 h\n1.000 UP\n1.000
      UL\nLT2\n2532 3789 M\n5 -44 V\n-63 0 V\n16 44 V\n42 0 V\n.995 g 2526
      3824 N 42 0 -15 -35 -21 0 h\n1.000 UP\n1.000 UL\nLT2\n2526 3824 M\n6
      -35 V\n-42 0 V\n15 35 V\n21 0 V\n.999 g 2521 3849 N 21 0 -16 -25 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n5 -25 V\n-21 0 V\n16 25
      V\n.001 g 2505 1315 N 0 0 36 14 -20 1 h\n1.000 UP\n1.000 UL\nLT2\n2505
      1315 M\n16 15 V\n-36 -14 V\n20 -1 V\n.005 g 2490 1311 N 20 -1 36 4 -41
      1 h\n1.000 UP\n1.000 UL\nLT2\n2490 1311 M\n15 4 V\n-20 1 V\n-36 -4
      V\n41 -1 V\n.013 g 2474 1317 N 41 -1 36 -7 -61 2 h\n1.000 UP\n1.000
      UL\nLT2\n2474 1317 M\n16 -6 V\n-41 1 V\n-36 7 V\n61 -2 V\n.025 g 2459
      1333 N 61 -2 34 -17 -80 3 h\n1.000 UP\n1.000 UL\nLT2\n2459 1333 M\n15
      -16 V\n-61 2 V\n-34 17 V\n80 -3 V\n.0407 g 2445 1359 N 80 -3 34 -27
      -100 4 h\n1.000 UP\n1.000 UL\nLT2\n2445 1359 M\n14 -26 V\n-80 3 V\n-34
      27 V\n100 -4 V\n.0601 g 2431 1395 N 100 -4 32 -37 -118 5 h\n1.000
      UP\n1.000 UL\nLT2\n2431 1395 M\n14 -36 V\n-100 4 V\n-32 37 V\n118 -5
      V\n.0831 g 2418 1441 N 118 -5 30 -46 -135 5 h\n1.000 UP\n1.000
      UL\nLT2\n2418 1441 M\n13 -46 V\n-118 5 V\n-30 46 V\n135 -5 V\n.1095 g
      2405 1496 N 135 -5 29 -56 -151 6 h\n1.000 UP\n1.000 UL\nLT2\n2405 1496
      M\n13 -55 V\n-135 5 V\n-29 56 V\n151 -6 V\n.139 g 2394 1560 N 151 -6 26
      -64 -166 6 h\n1.000 UP\n1.000 UL\nLT2\n2394 1560 M\n11 -64 V\n-151 6
      V\n-26 64 V\n166 -6 V\n.1714 g 2384 1632 N 166 -6 24 -73 -180 7
      h\n1.000 UP\n1.000 UL\nLT2\n2384 1632 M\n10 -72 V\n-166 6 V\n-24 73
      V\n180 -7 V\n.2064 g 2374 1712 N 180 -7 21 -80 -191 7 h\n1.000
      UP\n1.000 UL\nLT2\n2374 1712 M\n10 -80 V\n-180 7 V\n-21 80 V\n191 -7
      V\n.2438 g 2366 1798 N 191 -7 19 -87 -202 8 h\n1.000 UP\n1.000
      UL\nLT2\n2366 1798 M\n8 -86 V\n-191 7 V\n-19 87 V\n202 -8 V\n.2833 g
      2359 1892 N 202 -8 16 -94 -211 8 h\n1.000 UP\n1.000 UL\nLT2\n2359 1892
      M\n7 -94 V\n-202 8 V\n-16 94 V\n211 -8 V\n.3245 g 2354 1991 N 211 -8 12
      -99 -218 8 h\n1.000 UP\n1.000 UL\nLT2\n2354 1991 M\n5 -99 V\n-211 8
      V\n-12 99 V\n218 -8 V\n.3671 g 2350 2094 N 218 -8 10 -104 -224 9
      h\n1.000 UP\n1.000 UL\nLT2\n2350 2094 M\n4 -103 V\n-218 8 V\n-10 104
      V\n224 -9 V\n.4108 g 2347 2202 N 224 -9 6 -108 -227 9 h\n1.000
      UP\n1.000 UL\nLT2\n2347 2202 M\n3 -108 V\n-224 9 V\n-6 108 V\n227 -9
      V\n.4552 g 2345 2313 N 227 -9 4 -111 -229 9 h\n1.000 UP\n1.000
      UL\nLT2\n2345 2313 M\n2 -111 V\n-227 9 V\n-4 111 V\n229 -9 V\n.5 g 2345
      2426 N 229 -9 0 -113 -229 9 h\n1.000 UP\n1.000 UL\nLT2\n2345 2426 M\n0
      -113 V\n-229 9 V\n0 113 V\n229 -9 V\n.5448 g 2347 2540 N 229 -9 -4 -114
      -227 9 h\n1.000 UP\n1.000 UL\nLT2\n2347 2540 M\n-2 -114 V\n-229 9 V\n4
      114 V\n227 -9 V\n.5892 g 2350 2655 N 227 -9 -6 -114 -224 8 h\n1.000
      UP\n1.000 UL\nLT2\n2350 2655 M\n-3 -115 V\n-227 9 V\n6 114 V\n224 -8
      V\n.6329 g 2354 2769 N 224 -8 -10 -114 -218 8 h\n1.000 UP\n1.000
      UL\nLT2\n2354 2769 M\n-4 -114 V\n-224 8 V\n10 114 V\n218 -8 V\n.6755 g
      2359 2882 N 218 -8 -12 -113 -211 8 h\n1.000 UP\n1.000 UL\nLT2\n2359
      2882 M\n-5 -113 V\n-218 8 V\n12 113 V\n211 -8 V\n.7167 g 2366 2992 N
      211 -8 -16 -110 -202 8 h\n1.000 UP\n1.000 UL\nLT2\n2366 2992 M\n-7 -110
      V\n-211 8 V\n16 110 V\n202 -8 V\n.7562 g 2374 3099 N 202 -8 -19 -107
      -191 8 h\n1.000 UP\n1.000 UL\nLT2\n2374 3099 M\n-8 -107 V\n-202 8 V\n19
      107 V\n191 -8 V\n.7936 g 2384 3202 N 191 -8 -21 -102 -180 7 h\n1.000
      UP\n1.000 UL\nLT2\n2384 3202 M\n-10 -103 V\n-191 8 V\n21 102 V\n180 -7
      V\n.8286 g 2394 3301 N 180 -7 -24 -98 -166 6 h\n1.000 UP\n1.000
      UL\nLT2\n2394 3301 M\n-10 -99 V\n-180 7 V\n24 98 V\n166 -6 V\n.861 g
      2405 3393 N 166 -6 -26 -92 -151 6 h\n1.000 UP\n1.000 UL\nLT2\n2405 3393
      M\n-11 -92 V\n-166 6 V\n26 92 V\n151 -6 V\n.8905 g 2418 3479 N 151 -6
      -29 -85 -135 5 h\n1.000 UP\n1.000 UL\nLT2\n2418 3479 M\n-13 -86 V\n-151
      6 V\n29 85 V\n135 -5 V\n.9169 g 2431 3558 N 135 -5 -30 -78 -118 4
      h\n1.000 UP\n1.000 UL\nLT2\n2431 3558 M\n-13 -79 V\n-135 5 V\n30 78
      V\n118 -4 V\n.9399 g 2445 3629 N 118 -4 -32 -71 -100 4 h\n1.000
      UP\n1.000 UL\nLT2\n2445 3629 M\n-14 -71 V\n-118 4 V\n32 71 V\n100 -4
      V\n.9593 g 2459 3691 N 100 -4 -34 -61 -80 3 h\n1.000 UP\n1.000
      UL\nLT2\n2459 3691 M\n-14 -62 V\n-100 4 V\n34 61 V\n80 -3 V\n.975 g
      2474 3745 N 80 -3 -34 -53 -61 2 h\n1.000 UP\n1.000 UL\nLT2\n2474 3745
      M\n-15 -54 V\n-80 3 V\n34 53 V\n61 -2 V\n.987 g 2490 3789 N 61 -2 -36
      -44 -41 2 h\n1.000 UP\n1.000 UL\nLT2\n2490 3789 M\n-16 -44 V\n-61 2
      V\n36 44 V\n41 -2 V\n.995 g 2505 3824 N 41 -2 -36 -34 -20 1 h\n1.000
      UP\n1.000 UL\nLT2\n2505 3824 M\n-15 -35 V\n-41 2 V\n36 34 V\n20 -1
      V\n.999 g 2521 3849 N 20 -1 -36 -24 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n-16 -25 V\n-20 1 V\n36 24 V\n.001 g 2485 1316 N 0
      0 56 13 -20 1 h\n1.000 UP\n1.000 UL\nLT2\n2485 1316 M\n36 14 V\n-56 -13
      V\n20 -1 V\n.005 g 2449 1312 N 20 -1 55 2 -39 3 h\n1.000 UP\n1.000
      UL\nLT2\n2449 1312 M\n36 4 V\n-20 1 V\n-55 -2 V\n39 -3 V\n.013 g 2413
      1319 N 39 -3 54 -8 -57 4 h\n1.000 UP\n1.000 UL\nLT2\n2413 1319 M\n36 -7
      V\n-39 3 V\n-54 8 V\n57 -4 V\n.025 g 2379 1336 N 57 -4 53 -18 -76 5
      h\n1.000 UP\n1.000 UL\nLT2\n2379 1336 M\n34 -17 V\n-57 4 V\n-53 18
      V\n76 -5 V\n.0407 g 2345 1363 N 76 -5 52 -29 -94 7 h\n1.000 UP\n1.000
      UL\nLT2\n2345 1363 M\n34 -27 V\n-76 5 V\n-52 29 V\n94 -7 V\n.0601 g
      2313 1400 N 94 -7 49 -38 -111 8 h\n1.000 UP\n1.000 UL\nLT2\n2313 1400
      M\n32 -37 V\n-94 7 V\n-49 38 V\n111 -8 V\n.0831 g 2283 1446 N 111 -8 46
      -47 -127 9 h\n1.000 UP\n1.000 UL\nLT2\n2283 1446 M\n30 -46 V\n-111 8
      V\n-46 47 V\n127 -9 V\n.1095 g 2254 1502 N 127 -9 44 -57 -142 10
      h\n1.000 UP\n1.000 UL\nLT2\n2254 1502 M\n29 -56 V\n-127 9 V\n-44 57
      V\n142 -10 V\n.139 g 2228 1566 N 142 -10 40 -65 -156 11 h\n1.000
      UP\n1.000 UL\nLT2\n2228 1566 M\n26 -64 V\n-142 10 V\n-40 65 V\n156 -11
      V\n.1714 g 2204 1639 N 156 -11 37 -74 -169 12 h\n1.000 UP\n1.000
      UL\nLT2\n2204 1639 M\n24 -73 V\n-156 11 V\n-37 74 V\n169 -12 V\n.2064 g
      2183 1719 N 169 -12 33 -81 -181 13 h\n1.000 UP\n1.000 UL\nLT2\n2183
      1719 M\n21 -80 V\n-169 12 V\n-33 81 V\n181 -13 V\n.2438 g 2164 1806 N
      181 -13 28 -88 -190 14 h\n1.000 UP\n1.000 UL\nLT2\n2164 1806 M\n19 -87
      V\n-181 13 V\n-28 88 V\n190 -14 V\n.2833 g 2148 1900 N 190 -14 24 -94
      -198 14 h\n1.000 UP\n1.000 UL\nLT2\n2148 1900 M\n16 -94 V\n-190 14
      V\n-24 94 V\n198 -14 V\n.3245 g 2136 1999 N 198 -14 20 -100 -206 15
      h\n1.000 UP\n1.000 UL\nLT2\n2136 1999 M\n12 -99 V\n-198 14 V\n-20 100
      V\n206 -15 V\n.3671 g 2126 2103 N 206 -15 15 -104 -211 15 h\n1.000
      UP\n1.000 UL\nLT2\n2126 2103 M\n10 -104 V\n-206 15 V\n-15 104 V\n211
      -15 V\n.4108 g 2120 2211 N 211 -15 10 -108 -215 15 h\n1.000 UP\n1.000
      UL\nLT2\n2120 2211 M\n6 -108 V\n-211 15 V\n-10 108 V\n215 -15 V\n.4552
      g 2116 2322 N 215 -15 5 -111 -216 15 h\n1.000 UP\n1.000 UL\nLT2\n2116
      2322 M\n4 -111 V\n-215 15 V\n-5 111 V\n216 -15 V\n.5 g 2116 2435 N 216
      -15 0 -113 -216 15 h\n1.000 UP\n1.000 UL\nLT2\n2116 2435 M\n0 -113
      V\n-216 15 V\n0 113 V\n216 -15 V\n.5448 g 2120 2549 N 216 -15 -5 -114
      -215 15 h\n1.000 UP\n1.000 UL\nLT2\n2120 2549 M\n-4 -114 V\n-216 15
      V\n5 114 V\n215 -15 V\n.5892 g 2126 2663 N 215 -15 -10 -115 -211 16
      h\n1.000 UP\n1.000 UL\nLT2\n2126 2663 M\n-6 -114 V\n-215 15 V\n10 115
      V\n211 -16 V\n.6329 g 2136 2777 N 211 -16 -15 -113 -206 15 h\n1.000
      UP\n1.000 UL\nLT2\n2136 2777 M\n-10 -114 V\n-211 16 V\n15 113 V\n206
      -15 V\n.6755 g 2148 2890 N 206 -15 -20 -112 -198 14 h\n1.000 UP\n1.000
      UL\nLT2\n2148 2890 M\n-12 -113 V\n-206 15 V\n20 112 V\n198 -14 V\n.7167
      g 2164 3000 N 198 -14 -24 -110 -190 14 h\n1.000 UP\n1.000 UL\nLT2\n2164
      3000 M\n-16 -110 V\n-198 14 V\n24 110 V\n190 -14 V\n.7562 g 2183 3107 N
      190 -14 -28 -106 -181 13 h\n1.000 UP\n1.000 UL\nLT2\n2183 3107 M\n-19
      -107 V\n-190 14 V\n28 106 V\n181 -13 V\n.7936 g 2204 3209 N 181 -13 -33
      -102 -169 13 h\n1.000 UP\n1.000 UL\nLT2\n2204 3209 M\n-21 -102 V\n-181
      13 V\n33 102 V\n169 -13 V\n.8286 g 2228 3307 N 169 -13 -37 -96 -156 11
      h\n1.000 UP\n1.000 UL\nLT2\n2228 3307 M\n-24 -98 V\n-169 13 V\n37 96
      V\n156 -11 V\n.861 g 2254 3399 N 156 -11 -40 -91 -142 10 h\n1.000
      UP\n1.000 UL\nLT2\n2254 3399 M\n-26 -92 V\n-156 11 V\n40 91 V\n142 -10
      V\n.8905 g 2283 3484 N 142 -10 -44 -84 -127 9 h\n1.000 UP\n1.000
      UL\nLT2\n2283 3484 M\n-29 -85 V\n-142 10 V\n44 84 V\n127 -9 V\n.9169 g
      2313 3562 N 127 -9 -46 -77 -111 8 h\n1.000 UP\n1.000 UL\nLT2\n2313 3562
      M\n-30 -78 V\n-127 9 V\n46 77 V\n111 -8 V\n.9399 g 2345 3633 N 111 -8
      -49 -69 -94 6 h\n1.000 UP\n1.000 UL\nLT2\n2345 3633 M\n-32 -71 V\n-111
      8 V\n49 69 V\n94 -6 V\n.9593 g 2379 3694 N 94 -6 -52 -61 -76 6 h\n1.000
      UP\n1.000 UL\nLT2\n2379 3694 M\n-34 -61 V\n-94 6 V\n52 61 V\n76 -6
      V\n.975 g 2413 3747 N 76 -6 -53 -52 -57 5 h\n1.000 UP\n1.000
      UL\nLT2\n2413 3747 M\n-34 -53 V\n-76 6 V\n53 52 V\n57 -5 V\n.987 g 2449
      3791 N 57 -5 -54 -42 -39 3 h\n1.000 UP\n1.000 UL\nLT2\n2449 3791 M\n-36
      -44 V\n-57 5 V\n54 42 V\n39 -3 V\n.995 g 2485 3825 N 39 -3 -55 -32 -20
      1 h\n1.000 UP\n1.000 UL\nLT2\n2485 3825 M\n-36 -34 V\n-39 3 V\n55 32
      V\n20 -1 V\n.999 g 2521 3849 N 20 -1 -56 -23 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n-36 -24 V\n-20 1 V\n56 23 V\n.001 g 2465 1317 N 0
      0 73 11 -17 2 h\n1.000 UP\n1.000 UL\nLT2\n2465 1317 M\n56 13 V\n-73 -11
      V\n17 -2 V\n.005 g 2410 1315 N 17 -2 73 0 -35 4 h\n1.000 UP\n1.000
      UL\nLT2\n2410 1315 M\n55 2 V\n-17 2 V\n-73 0 V\n35 -4 V\n.013 g 2356
      1323 N 35 -4 71 -10 -52 6 h\n1.000 UP\n1.000 UL\nLT2\n2356 1323 M\n54
      -8 V\n-35 4 V\n-71 10 V\n52 -6 V\n.025 g 2303 1341 N 52 -6 70 -20 -69 8
      h\n1.000 UP\n1.000 UL\nLT2\n2303 1341 M\n53 -18 V\n-52 6 V\n-70 20
      V\n69 -8 V\n.0407 g 2251 1370 N 69 -8 68 -30 -85 9 h\n1.000 UP\n1.000
      UL\nLT2\n2251 1370 M\n52 -29 V\n-69 8 V\n-68 30 V\n85 -9 V\n.0601 g
      2202 1408 N 85 -9 64 -40 -100 11 h\n1.000 UP\n1.000 UL\nLT2\n2202 1408
      M\n49 -38 V\n-85 9 V\n-64 40 V\n100 -11 V\n.0831 g 2156 1455 N 100 -11
      61 -49 -115 13 h\n1.000 UP\n1.000 UL\nLT2\n2156 1455 M\n46 -47 V\n-100
      11 V\n-61 49 V\n115 -13 V\n.1095 g 2112 1512 N 115 -13 58 -58 -129 14
      h\n1.000 UP\n1.000 UL\nLT2\n2112 1512 M\n44 -57 V\n-115 13 V\n-58 58
      V\n129 -14 V\n.139 g 2072 1577 N 129 -14 53 -67 -142 16 h\n1.000
      UP\n1.000 UL\nLT2\n2072 1577 M\n40 -65 V\n-129 14 V\n-53 67 V\n142 -16
      V\n.1714 g 2035 1651 N 142 -16 48 -75 -153 17 h\n1.000 UP\n1.000
      UL\nLT2\n2035 1651 M\n37 -74 V\n-142 16 V\n-48 75 V\n153 -17 V\n.2064 g
      2002 1732 N 153 -17 43 -82 -163 18 h\n1.000 UP\n1.000 UL\nLT2\n2002
      1732 M\n33 -81 V\n-153 17 V\n-43 82 V\n163 -18 V\n.2438 g 1974 1820 N
      163 -18 38 -89 -173 19 h\n1.000 UP\n1.000 UL\nLT2\n1974 1820 M\n28 -88
      V\n-163 18 V\n-38 89 V\n173 -19 V\n.2833 g 1950 1914 N 173 -19 32 -95
      -181 20 h\n1.000 UP\n1.000 UL\nLT2\n1950 1914 M\n24 -94 V\n-173 19
      V\n-32 95 V\n181 -20 V\n.3245 g 1930 2014 N 181 -20 26 -101 -187 21
      h\n1.000 UP\n1.000 UL\nLT2\n1930 2014 M\n20 -100 V\n-181 20 V\n-26 101
      V\n187 -21 V\n.3671 g 1915 2118 N 187 -21 19 -105 -191 22 h\n1.000
      UP\n1.000 UL\nLT2\n1915 2118 M\n15 -104 V\n-187 21 V\n-19 105 V\n191
      -22 V\n.4108 g 1905 2226 N 191 -22 13 -108 -194 22 h\n1.000 UP\n1.000
      UL\nLT2\n1905 2226 M\n10 -108 V\n-191 22 V\n-13 108 V\n194 -22 V\n.4552
      g 1900 2337 N 194 -22 7 -111 -196 22 h\n1.000 UP\n1.000 UL\nLT2\n1900
      2337 M\n5 -111 V\n-194 22 V\n-7 111 V\n196 -22 V\n.5 g 1900 2450 N 196
      -22 0 -113 -196 22 h\n1.000 UP\n1.000 UL\nLT2\n1900 2450 M\n0 -113
      V\n-196 22 V\n0 113 V\n196 -22 V\n.5448 g 1905 2564 N 196 -22 -7 -114
      -194 22 h\n1.000 UP\n1.000 UL\nLT2\n1905 2564 M\n-5 -114 V\n-196 22
      V\n7 114 V\n194 -22 V\n.5892 g 1915 2679 N 194 -22 -13 -114 -191 21
      h\n1.000 UP\n1.000 UL\nLT2\n1915 2679 M\n-10 -115 V\n-194 22 V\n13 114
      V\n191 -21 V\n.6329 g 1930 2792 N 191 -21 -19 -113 -187 21 h\n1.000
      UP\n1.000 UL\nLT2\n1930 2792 M\n-15 -113 V\n-191 21 V\n19 113 V\n187
      -21 V\n.6755 g 1950 2904 N 187 -21 -26 -112 -181 21 h\n1.000 UP\n1.000
      UL\nLT2\n1950 2904 M\n-20 -112 V\n-187 21 V\n26 112 V\n181 -21 V\n.7167
      g 1974 3014 N 181 -21 -32 -108 -173 19 h\n1.000 UP\n1.000 UL\nLT2\n1974
      3014 M\n-24 -110 V\n-181 21 V\n32 108 V\n173 -19 V\n.7562 g 2002 3120 N
      173 -19 -38 -105 -163 18 h\n1.000 UP\n1.000 UL\nLT2\n2002 3120 M\n-28
      -106 V\n-173 19 V\n38 105 V\n163 -18 V\n.7936 g 2035 3222 N 163 -18 -43
      -101 -153 17 h\n1.000 UP\n1.000 UL\nLT2\n2035 3222 M\n-33 -102 V\n-163
      18 V\n43 101 V\n153 -17 V\n.8286 g 2072 3318 N 153 -17 -48 -95 -142 16
      h\n1.000 UP\n1.000 UL\nLT2\n2072 3318 M\n-37 -96 V\n-153 17 V\n48 95
      V\n142 -16 V\n.861 g 2112 3409 N 142 -16 -53 -90 -129 15 h\n1.000
      UP\n1.000 UL\nLT2\n2112 3409 M\n-40 -91 V\n-142 16 V\n53 90 V\n129 -15
      V\n.8905 g 2156 3493 N 129 -15 -58 -82 -115 13 h\n1.000 UP\n1.000
      UL\nLT2\n2156 3493 M\n-44 -84 V\n-129 15 V\n58 82 V\n115 -13 V\n.9169 g
      2202 3570 N 115 -13 -61 -76 -100 12 h\n1.000 UP\n1.000 UL\nLT2\n2202
      3570 M\n-46 -77 V\n-115 13 V\n61 76 V\n100 -12 V\n.9399 g 2251 3639 N
      100 -12 -64 -67 -85 10 h\n1.000 UP\n1.000 UL\nLT2\n2251 3639 M\n-49 -69
      V\n-100 12 V\n64 67 V\n85 -10 V\n.9593 g 2303 3700 N 85 -10 -68 -59 -69
      8 h\n1.000 UP\n1.000 UL\nLT2\n2303 3700 M\n-52 -61 V\n-85 10 V\n68 59
      V\n69 -8 V\n.975 g 2356 3752 N 69 -8 -70 -49 -52 5 h\n1.000 UP\n1.000
      UL\nLT2\n2356 3752 M\n-53 -52 V\n-69 8 V\n70 49 V\n52 -5 V\n.987 g 2410
      3794 N 52 -5 -71 -41 -35 4 h\n1.000 UP\n1.000 UL\nLT2\n2410 3794 M\n-54
      -42 V\n-52 5 V\n71 41 V\n35 -4 V\n.995 g 2465 3826 N 35 -4 -73 -30 -17
      2 h\n1.000 UP\n1.000 UL\nLT2\n2465 3826 M\n-55 -32 V\n-35 4 V\n73 30
      V\n17 -2 V\n.999 g 2521 3849 N 17 -2 -73 -21 0 0 h\n1.000 UP\n1.000
      UL\nLT2\n2521 3849 M\n-56 -23 V\n-17 2 V\n73 21 V\n.001 g 2448 1319 N 0
      0 88 8 -15 3 h\n1.000 UP\n1.000 UL\nLT2\n2448 1319 M\n73 11 V\n-88 -8
      V\n15 -3 V\n.005 g 2375 1319 N 15 -3 88 -2 -30 5 h\n1.000 UP\n1.000
      UL\nLT2\n2375 1319 M\n73 0 V\n-15 3 V\n-88 2 V\n30 -5 V\n.013 g 2304
      1329 N 30 -5 87 -12 -46 7 h\n1.000 UP\n1.000 UL\nLT2\n2304 1329 M\n71
      -10 V\n-30 5 V\n-87 12 V\n46 -7 V\n.025 g 2234 1349 N 46 -7 84 -23 -60
      10 h\n1.000 UP\n1.000 UL\nLT2\n2234 1349 M\n70 -20 V\n-46 7 V\n-84 23
      V\n60 -10 V\n.0407 g 2166 1379 N 60 -10 81 -32 -73 12 h\n1.000
      UP\n1.000 UL\nLT2\n2166 1379 M\n68 -30 V\n-60 10 V\n-81 32 V\n73 -12
      V\n.0601 g 2102 1419 N 73 -12 78 -42 -87 14 h\n1.000 UP\n1.000
      UL\nLT2\n2102 1419 M\n64 -40 V\n-73 12 V\n-78 42 V\n87 -14 V\n.0831 g
      2041 1468 N 87 -14 74 -51 -100 16 h\n1.000 UP\n1.000 UL\nLT2\n2041 1468
      M\n61 -49 V\n-87 14 V\n-74 51 V\n100 -16 V\n.1095 g 1983 1526 N 100 -16
      70 -61 -112 19 h\n1.000 UP\n1.000 UL\nLT2\n1983 1526 M\n58 -58 V\n-100
      16 V\n-70 61 V\n112 -19 V\n.139 g 1930 1593 N 112 -19 64 -68 -123 20
      h\n1.000 UP\n1.000 UL\nLT2\n1930 1593 M\n53 -67 V\n-112 19 V\n-64 68
      V\n123 -20 V\n.1714 g 1882 1668 N 123 -20 58 -77 -133 22 h\n1.000
      UP\n1.000 UL\nLT2\n1882 1668 M\n48 -75 V\n-123 20 V\n-58 77 V\n133 -22
      V\n.2064 g 1839 1750 N 133 -22 52 -83 -142 23 h\n1.000 UP\n1.000
      UL\nLT2\n1839 1750 M\n43 -82 V\n-133 22 V\n-52 83 V\n142 -23 V\n.2438 g
      1801 1839 N 142 -23 46 -91 -150 25 h\n1.000 UP\n1.000 UL\nLT2\n1801
      1839 M\n38 -89 V\n-142 23 V\n-46 91 V\n150 -25 V\n.2833 g 1769 1934 N
      150 -25 38 -96 -156 26 h\n1.000 UP\n1.000 UL\nLT2\n1769 1934 M\n32 -95
      V\n-150 25 V\n-38 96 V\n156 -26 V\n.3245 g 1743 2035 N 156 -26 31 -101
      -161 26 h\n1.000 UP\n1.000 UL\nLT2\n1743 2035 M\n26 -101 V\n-156 26
      V\n-31 101 V\n161 -26 V\n.3671 g 1724 2140 N 161 -26 24 -105 -166 26
      h\n1.000 UP\n1.000 UL\nLT2\n1724 2140 M\n19 -105 V\n-161 26 V\n-24 105
      V\n166 -26 V\n.4108 g 1711 2248 N 166 -26 16 -109 -169 27 h\n1.000
      UP\n1.000 UL\nLT2\n1711 2248 M\n13 -108 V\n-166 26 V\n-16 109 V\n169
      -27 V\n.4552 g 1704 2359 N 169 -27 7 -112 -169 28 h\n1.000 UP\n1.000
      UL\nLT2\n1704 2359 M\n7 -111 V\n-169 27 V\n-7 112 V\n169 -28 V\n.5 g
      1704 2472 N 169 -28 0 -113 -169 28 h\n1.000 UP\n1.000 UL\nLT2\n1704
      2472 M\n0 -113 V\n-169 28 V\n0 113 V\n169 -28 V\n.5448 g 1711 2586 N
      169 -28 -7 -113 -169 27 h\n1.000 UP\n1.000 UL\nLT2\n1711 2586 M\n-7
      -114 V\n-169 28 V\n7 113 V\n169 -27 V\n.5892 g 1724 2700 N 169 -27 -16
      -114 -166 27 h\n1.000 UP\n1.000 UL\nLT2\n1724 2700 M\n-13 -114 V\n-169
      27 V\n16 114 V\n166 -27 V\n.6329 g 1743 2813 N 166 -27 -24 -112 -161 26
      h\n1.000 UP\n1.000 UL\nLT2\n1743 2813 M\n-19 -113 V\n-166 27 V\n24 112
      V\n161 -26 V\n.6755 g 1769 2925 N 161 -26 -31 -111 -156 25 h\n1.000
      UP\n1.000 UL\nLT2\n1769 2925 M\n-26 -112 V\n-161 26 V\n31 111 V\n156
      -25 V\n.7167 g 1801 3033 N 156 -25 -38 -107 -150 24 h\n1.000 UP\n1.000
      UL\nLT2\n1801 3033 M\n-32 -108 V\n-156 25 V\n38 107 V\n150 -24 V\n.7562
      g 1839 3138 N 150 -24 -46 -104 -142 23 h\n1.000 UP\n1.000 UL\nLT2\n1839
      3138 M\n-38 -105 V\n-150 24 V\n46 104 V\n142 -23 V\n.7936 g 1882 3239 N
      142 -23 -52 -99 -133 21 h\n1.000 UP\n1.000 UL\nLT2\n1882 3239 M\n-43
      -101 V\n-142 23 V\n52 99 V\n133 -21 V\n.8286 g 1930 3334 N 133 -21 -58
      -94 -123 20 h\n1.000 UP\n1.000 UL\nLT2\n1930 3334 M\n-48 -95 V\n-133 21
      V\n58 94 V\n123 -20 V\n.861 g 1983 3424 N 123 -20 -64 -88 -112 18
      h\n1.000 UP\n1.000 UL\nLT2\n1983 3424 M\n-53 -90 V\n-123 20 V\n64 88
      V\n112 -18 V\n.8905 g 2041 3506 N 112 -18 -70 -80 -100 16 h\n1.000
      UP\n1.000 UL\nLT2\n2041 3506 M\n-58 -82 V\n-112 18 V\n70 80 V\n100 -16
      V\n.9169 g 2102 3582 N 100 -16 -74 -74 -87 14 h\n1.000 UP\n1.000
      UL\nLT2\n2102 3582 M\n-61 -76 V\n-100 16 V\n74 74 V\n87 -14 V\n.9399 g
      2166 3649 N 87 -14 -78 -65 -73 12 h\n1.000 UP\n1.000 UL\nLT2\n2166 3649
      M\n-64 -67 V\n-87 14 V\n78 65 V\n73 -12 V\n.9593 g 2234 3708 N 73 -12
      -81 -56 -60 9 h\n1.000 UP\n1.000 UL\nLT2\n2234 3708 M\n-68 -59 V\n-73
      12 V\n81 56 V\n60 -9 V\n.975 g 2304 3757 N 60 -9 -84 -48 -46 8 h\n1.000
      UP\n1.000 UL\nLT2\n2304 3757 M\n-70 -49 V\n-60 9 V\n84 48 V\n46 -8
      V\n.987 g 2375 3798 N 46 -8 -87 -38 -30 5 h\n1.000 UP\n1.000
      UL\nLT2\n2375 3798 M\n-71 -41 V\n-46 8 V\n87 38 V\n30 -5 V\n.995 g 2448
      3828 N 30 -5 -88 -28 -15 3 h\n1.000 UP\n1.000 UL\nLT2\n2448 3828 M\n-73
      -30 V\n-30 5 V\n88 28 V\n15 -3 V\n.999 g 2521 3849 N 15 -3 -88 -18 0 0
      h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-73 -21 V\n-15 3 V\n88 18
      V\n.001 g 2433 1322 N 0 0 101 5 -13 3 h\n1.000 UP\n1.000 UL\nLT2\n2433
      1322 M\n88 8 V\n-101 -5 V\n13 -3 V\n.005 g 2345 1324 N 13 -3 100 -5 -25
      6 h\n1.000 UP\n1.000 UL\nLT2\n2345 1324 M\n88 -2 V\n-13 3 V\n-100 5
      V\n25 -6 V\n.013 g 2258 1336 N 25 -6 98 -15 -36 9 h\n1.000 UP\n1.000
      UL\nLT2\n2258 1336 M\n87 -12 V\n-25 6 V\n-98 15 V\n36 -9 V\n.025 g 2174
      1359 N 36 -9 96 -25 -48 11 h\n1.000 UP\n1.000 UL\nLT2\n2174 1359 M\n84
      -23 V\n-36 9 V\n-96 25 V\n48 -11 V\n.0407 g 2093 1391 N 48 -11 93 -35
      -60 14 h\n1.000 UP\n1.000 UL\nLT2\n2093 1391 M\n81 -32 V\n-48 11 V\n-93
      35 V\n60 -14 V\n.0601 g 2015 1433 N 60 -14 89 -45 -71 17 h\n1.000
      UP\n1.000 UL\nLT2\n2015 1433 M\n78 -42 V\n-60 14 V\n-89 45 V\n71 -17
      V\n.0831 g 1941 1484 N 71 -17 85 -53 -82 19 h\n1.000 UP\n1.000
      UL\nLT2\n1941 1484 M\n74 -51 V\n-71 17 V\n-85 53 V\n82 -19 V\n.1095 g
      1871 1545 N 82 -19 79 -63 -91 21 h\n1.000 UP\n1.000 UL\nLT2\n1871 1545
      M\n70 -61 V\n-82 19 V\n-79 63 V\n91 -21 V\n.139 g 1807 1613 N 91 -21 73
      -71 -100 24 h\n1.000 UP\n1.000 UL\nLT2\n1807 1613 M\n64 -68 V\n-91 21
      V\n-73 71 V\n100 -24 V\n.1714 g 1749 1690 N 100 -24 66 -78 -108 25
      h\n1.000 UP\n1.000 UL\nLT2\n1749 1690 M\n58 -77 V\n-100 24 V\n-66 78
      V\n108 -25 V\n.2064 g 1697 1773 N 108 -25 59 -85 -115 27 h\n1.000
      UP\n1.000 UL\nLT2\n1697 1773 M\n52 -83 V\n-108 25 V\n-59 85 V\n115 -27
      V\n.2438 g 1651 1864 N 115 -27 52 -92 -121 28 h\n1.000 UP\n1.000
      UL\nLT2\n1651 1864 M\n46 -91 V\n-115 27 V\n-52 92 V\n121 -28 V\n.2833 g
      1613 1960 N 121 -28 44 -98 -127 30 h\n1.000 UP\n1.000 UL\nLT2\n1613
      1960 M\n38 -96 V\n-121 28 V\n-44 98 V\n127 -30 V\n.3245 g 1582 2061 N
      127 -30 36 -102 -132 31 h\n1.000 UP\n1.000 UL\nLT2\n1582 2061 M\n31
      -101 V\n-127 30 V\n-36 102 V\n132 -31 V\n.3671 g 1558 2166 N 132 -31 26
      -106 -134 32 h\n1.000 UP\n1.000 UL\nLT2\n1558 2166 M\n24 -105 V\n-132
      31 V\n-26 106 V\n134 -32 V\n.4108 g 1542 2275 N 134 -32 18 -109 -136 32
      h\n1.000 UP\n1.000 UL\nLT2\n1542 2275 M\n16 -109 V\n-134 32 V\n-18 109
      V\n136 -32 V\n.4552 g 1535 2387 N 136 -32 10 -112 -139 32 h\n1.000
      UP\n1.000 UL\nLT2\n1535 2387 M\n7 -112 V\n-136 32 V\n-10 112 V\n139 -32
      V\n.5 g 1535 2500 N 139 -32 0 -113 -139 32 h\n1.000 UP\n1.000
      UL\nLT2\n1535 2500 M\n0 -113 V\n-139 32 V\n0 113 V\n139 -32 V\n.5448 g
      1542 2613 N 139 -32 -10 -113 -136 32 h\n1.000 UP\n1.000 UL\nLT2\n1542
      2613 M\n-7 -113 V\n-139 32 V\n10 113 V\n136 -32 V\n.5892 g 1558 2727 N
      136 -32 -18 -113 -134 31 h\n1.000 UP\n1.000 UL\nLT2\n1558 2727 M\n-16
      -114 V\n-136 32 V\n18 113 V\n134 -31 V\n.6329 g 1582 2839 N 134 -31 -26
      -112 -132 31 h\n1.000 UP\n1.000 UL\nLT2\n1582 2839 M\n-24 -112 V\n-134
      31 V\n26 112 V\n132 -31 V\n.6755 g 1613 2950 N 132 -31 -36 -110 -127 30
      h\n1.000 UP\n1.000 UL\nLT2\n1613 2950 M\n-31 -111 V\n-132 31 V\n36 110
      V\n127 -30 V\n.7167 g 1651 3057 N 127 -30 -44 -106 -121 29 h\n1.000
      UP\n1.000 UL\nLT2\n1651 3057 M\n-38 -107 V\n-127 30 V\n44 106 V\n121
      -29 V\n.7562 g 1697 3161 N 121 -29 -52 -102 -115 27 h\n1.000 UP\n1.000
      UL\nLT2\n1697 3161 M\n-46 -104 V\n-121 29 V\n52 102 V\n115 -27 V\n.7936
      g 1749 3260 N 115 -27 -59 -98 -108 26 h\n1.000 UP\n1.000 UL\nLT2\n1749
      3260 M\n-52 -99 V\n-115 27 V\n59 98 V\n108 -26 V\n.8286 g 1807 3354 N
      108 -26 -66 -91 -100 23 h\n1.000 UP\n1.000 UL\nLT2\n1807 3354 M\n-58
      -94 V\n-108 26 V\n66 91 V\n100 -23 V\n.861 g 1871 3442 N 100 -23 -73
      -86 -91 21 h\n1.000 UP\n1.000 UL\nLT2\n1871 3442 M\n-64 -88 V\n-100 23
      V\n73 86 V\n91 -21 V\n.8905 g 1941 3522 N 91 -21 -79 -78 -82 19
      h\n1.000 UP\n1.000 UL\nLT2\n1941 3522 M\n-70 -80 V\n-91 21 V\n79 78
      V\n82 -19 V\n.9169 g 2015 3596 N 82 -19 -85 -71 -71 16 h\n1.000
      UP\n1.000 UL\nLT2\n2015 3596 M\n-74 -74 V\n-82 19 V\n85 71 V\n71 -16
      V\n.9399 g 2093 3661 N 71 -16 -89 -63 -60 14 h\n1.000 UP\n1.000
      UL\nLT2\n2093 3661 M\n-78 -65 V\n-71 16 V\n89 63 V\n60 -14 V\n.9593 g
      2174 3717 N 60 -14 -93 -54 -48 12 h\n1.000 UP\n1.000 UL\nLT2\n2174 3717
      M\n-81 -56 V\n-60 14 V\n93 54 V\n48 -12 V\n.975 g 2258 3765 N 48 -12
      -96 -44 -36 8 h\n1.000 UP\n1.000 UL\nLT2\n2258 3765 M\n-84 -48 V\n-48
      12 V\n96 44 V\n36 -8 V\n.987 g 2345 3803 N 36 -8 -98 -35 -25 5 h\n1.000
      UP\n1.000 UL\nLT2\n2345 3803 M\n-87 -38 V\n-36 8 V\n98 35 V\n25 -5
      V\n.995 g 2433 3831 N 25 -5 -100 -26 -13 3 h\n1.000 UP\n1.000
      UL\nLT2\n2433 3831 M\n-88 -28 V\n-25 5 V\n100 26 V\n13 -3 V\n.999 g
      2521 3849 N 13 -3 -101 -15 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849
      M\n-88 -18 V\n-13 3 V\n101 15 V\n.001 g 2420 1325 N 0 0 110 2 -9 3
      h\n1.000 UP\n1.000 UL\nLT2\n2420 1325 M\n101 5 V\n-110 -2 V\n9 -3
      V\n.005 g 2320 1330 N 9 -3 109 -8 -18 6 h\n1.000 UP\n1.000
      UL\nLT2\n2320 1330 M\n100 -5 V\n-9 3 V\n-109 8 V\n18 -6 V\n.013 g 2222
      1345 N 18 -6 108 -19 -28 10 h\n1.000 UP\n1.000 UL\nLT2\n2222 1345 M\n98
      -15 V\n-18 6 V\n-108 19 V\n28 -10 V\n.025 g 2126 1370 N 28 -10 104 -28
      -36 13 h\n1.000 UP\n1.000 UL\nLT2\n2126 1370 M\n96 -25 V\n-28 10
      V\n-104 28 V\n36 -13 V\n.0407 g 2033 1405 N 36 -13 102 -38 -45 16
      h\n1.000 UP\n1.000 UL\nLT2\n2033 1405 M\n93 -35 V\n-36 13 V\n-102 38
      V\n45 -16 V\n.0601 g 1944 1450 N 45 -16 97 -47 -53 18 h\n1.000
      UP\n1.000 UL\nLT2\n1944 1450 M\n89 -45 V\n-45 16 V\n-97 47 V\n53 -18
      V\n.0831 g 1859 1503 N 53 -18 92 -57 -60 22 h\n1.000 UP\n1.000
      UL\nLT2\n1859 1503 M\n85 -53 V\n-53 18 V\n-92 57 V\n60 -22 V\n.1095 g
      1780 1566 N 60 -22 86 -65 -67 24 h\n1.000 UP\n1.000 UL\nLT2\n1780 1566
      M\n79 -63 V\n-60 22 V\n-86 65 V\n67 -24 V\n.139 g 1707 1637 N 67 -24 79
      -73 -73 26 h\n1.000 UP\n1.000 UL\nLT2\n1707 1637 M\n73 -71 V\n-67 24
      V\n-79 73 V\n73 -26 V\n.1714 g 1641 1715 N 73 -26 73 -80 -80 28
      h\n1.000 UP\n1.000 UL\nLT2\n1641 1715 M\n66 -78 V\n-73 26 V\n-73 80
      V\n80 -28 V\n.2064 g 1582 1800 N 80 -28 65 -87 -86 30 h\n1.000
      UP\n1.000 UL\nLT2\n1582 1800 M\n59 -85 V\n-80 28 V\n-65 87 V\n86 -30
      V\n.2438 g 1530 1892 N 86 -30 56 -94 -90 32 h\n1.000 UP\n1.000
      UL\nLT2\n1530 1892 M\n52 -92 V\n-86 30 V\n-56 94 V\n90 -32 V\n.2833 g
      1486 1990 N 90 -32 48 -99 -94 33 h\n1.000 UP\n1.000 UL\nLT2\n1486 1990
      M\n44 -98 V\n-90 32 V\n-48 99 V\n94 -33 V\n.3245 g 1450 2092 N 94 -33
      39 -103 -97 34 h\n1.000 UP\n1.000 UL\nLT2\n1450 2092 M\n36 -102 V\n-94
      33 V\n-39 103 V\n97 -34 V\n.3671 g 1424 2198 N 97 -34 29 -107 -100 35
      h\n1.000 UP\n1.000 UL\nLT2\n1424 2198 M\n26 -106 V\n-97 34 V\n-29 107
      V\n100 -35 V\n.4108 g 1406 2307 N 100 -35 20 -110 -102 36 h\n1.000
      UP\n1.000 UL\nLT2\n1406 2307 M\n18 -109 V\n-100 35 V\n-20 110 V\n102
      -36 V\n.4552 g 1396 2419 N 102 -36 9 -112 -101 36 h\n1.000 UP\n1.000
      UL\nLT2\n1396 2419 M\n10 -112 V\n-102 36 V\n-9 112 V\n101 -36 V\n.5 g
      1396 2532 N 101 -36 0 -113 -101 36 h\n1.000 UP\n1.000 UL\nLT2\n1396
      2532 M\n0 -113 V\n-101 36 V\n0 113 V\n101 -36 V\n.5448 g 1406 2645 N
      101 -36 -9 -113 -102 36 h\n1.000 UP\n1.000 UL\nLT2\n1406 2645 M\n-10
      -113 V\n-101 36 V\n9 113 V\n102 -36 V\n.5892 g 1424 2758 N 102 -36 -20
      -113 -100 36 h\n1.000 UP\n1.000 UL\nLT2\n1424 2758 M\n-18 -113 V\n-102
      36 V\n20 113 V\n100 -36 V\n.6329 g 1450 2870 N 100 -36 -29 -110 -97 34
      h\n1.000 UP\n1.000 UL\nLT2\n1450 2870 M\n-26 -112 V\n-100 36 V\n29 110
      V\n97 -34 V\n.6755 g 1486 2980 N 97 -34 -39 -109 -94 33 h\n1.000
      UP\n1.000 UL\nLT2\n1486 2980 M\n-36 -110 V\n-97 34 V\n39 109 V\n94 -33
      V\n.7167 g 1530 3086 N 94 -33 -48 -105 -90 32 h\n1.000 UP\n1.000
      UL\nLT2\n1530 3086 M\n-44 -106 V\n-94 33 V\n48 105 V\n90 -32 V\n.7562 g
      1582 3188 N 90 -32 -56 -100 -86 30 h\n1.000 UP\n1.000 UL\nLT2\n1582
      3188 M\n-52 -102 V\n-90 32 V\n56 100 V\n86 -30 V\n.7936 g 1641 3286 N
      86 -30 -65 -96 -80 28 h\n1.000 UP\n1.000 UL\nLT2\n1641 3286 M\n-59 -98
      V\n-86 30 V\n65 96 V\n80 -28 V\n.8286 g 1707 3377 N 80 -28 -73 -89 -73
      26 h\n1.000 UP\n1.000 UL\nLT2\n1707 3377 M\n-66 -91 V\n-80 28 V\n73 89
      V\n73 -26 V\n.861 g 1780 3463 N 73 -26 -79 -84 -67 24 h\n1.000
      UP\n1.000 UL\nLT2\n1780 3463 M\n-73 -86 V\n-73 26 V\n79 84 V\n67 -24
      V\n.8905 g 1859 3541 N 67 -24 -86 -76 -60 22 h\n1.000 UP\n1.000
      UL\nLT2\n1859 3541 M\n-79 -78 V\n-67 24 V\n86 76 V\n60 -22 V\n.9169 g
      1944 3612 N 60 -22 -92 -68 -53 19 h\n1.000 UP\n1.000 UL\nLT2\n1944 3612
      M\n-85 -71 V\n-60 22 V\n92 68 V\n53 -19 V\n.9399 g 2033 3675 N 53 -19
      -97 -59 -45 15 h\n1.000 UP\n1.000 UL\nLT2\n2033 3675 M\n-89 -63 V\n-53
      19 V\n97 59 V\n45 -15 V\n.9593 g 2126 3729 N 45 -15 -102 -51 -36 12
      h\n1.000 UP\n1.000 UL\nLT2\n2126 3729 M\n-93 -54 V\n-45 15 V\n102 51
      V\n36 -12 V\n.975 g 2222 3773 N 36 -12 -104 -42 -28 10 h\n1.000
      UP\n1.000 UL\nLT2\n2222 3773 M\n-96 -44 V\n-36 12 V\n104 42 V\n28 -10
      V\n.987 g 2320 3808 N 28 -10 -108 -32 -18 7 h\n1.000 UP\n1.000
      UL\nLT2\n2320 3808 M\n-98 -35 V\n-28 10 V\n108 32 V\n18 -7 V\n.995 g
      2420 3834 N 18 -7 -109 -22 -9 3 h\n1.000 UP\n1.000 UL\nLT2\n2420 3834
      M\n-100 -26 V\n-18 7 V\n109 22 V\n9 -3 V\n.999 g 2521 3849 N 9 -3 -110
      -12 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-101 -15 V\n-9 3
      V\n110 12 V\n.001 g 2411 1328 N 0 0 116 -1 -6 3 h\n1.000 UP\n1.000
      UL\nLT2\n2411 1328 M\n110 2 V\n-116 1 V\n6 -3 V\n.005 g 2302 1336 N 6
      -3 114 -12 -11 7 h\n1.000 UP\n1.000 UL\nLT2\n2302 1336 M\n109 -8 V\n-6
      3 V\n-114 12 V\n11 -7 V\n.013 g 2194 1355 N 11 -7 113 -22 -16 10
      h\n1.000 UP\n1.000 UL\nLT2\n2194 1355 M\n108 -19 V\n-11 7 V\n-113 22
      V\n16 -10 V\n.025 g 2090 1383 N 16 -10 110 -31 -22 13 h\n1.000
      UP\n1.000 UL\nLT2\n2090 1383 M\n104 -28 V\n-16 10 V\n-110 31 V\n22 -13
      V\n.0407 g 1988 1421 N 22 -13 107 -41 -27 16 h\n1.000 UP\n1.000
      UL\nLT2\n1988 1421 M\n102 -38 V\n-22 13 V\n-107 41 V\n27 -16 V\n.0601 g
      1891 1468 N 27 -16 102 -51 -32 20 h\n1.000 UP\n1.000 UL\nLT2\n1891 1468
      M\n97 -47 V\n-27 16 V\n-102 51 V\n32 -20 V\n.0831 g 1799 1525 N 32 -20
      96 -59 -36 22 h\n1.000 UP\n1.000 UL\nLT2\n1799 1525 M\n92 -57 V\n-32 20
      V\n-96 59 V\n36 -22 V\n.1095 g 1713 1590 N 36 -22 91 -68 -41 25
      h\n1.000 UP\n1.000 UL\nLT2\n1713 1590 M\n86 -65 V\n-36 22 V\n-91 68
      V\n41 -25 V\n.139 g 1634 1663 N 41 -25 84 -75 -46 27 h\n1.000 UP\n1.000
      UL\nLT2\n1634 1663 M\n79 -73 V\n-41 25 V\n-84 75 V\n46 -27 V\n.1714 g
      1561 1743 N 46 -27 76 -83 -49 30 h\n1.000 UP\n1.000 UL\nLT2\n1561 1743
      M\n73 -80 V\n-46 27 V\n-76 83 V\n49 -30 V\n.2064 g 1496 1830 N 49 -30
      68 -89 -52 32 h\n1.000 UP\n1.000 UL\nLT2\n1496 1830 M\n65 -87 V\n-49 30
      V\n-68 89 V\n52 -32 V\n.2438 g 1440 1924 N 52 -32 59 -96 -55 34
      h\n1.000 UP\n1.000 UL\nLT2\n1440 1924 M\n56 -94 V\n-52 32 V\n-59 96
      V\n55 -34 V\n.2833 g 1392 2023 N 55 -34 50 -100 -57 35 h\n1.000
      UP\n1.000 UL\nLT2\n1392 2023 M\n48 -99 V\n-55 34 V\n-50 100 V\n57 -35
      V\n.3245 g 1353 2126 N 57 -35 41 -104 -59 36 h\n1.000 UP\n1.000
      UL\nLT2\n1353 2126 M\n39 -103 V\n-57 35 V\n-41 104 V\n59 -36 V\n.3671 g
      1324 2233 N 59 -36 31 -108 -61 37 h\n1.000 UP\n1.000 UL\nLT2\n1324 2233
      M\n29 -107 V\n-59 36 V\n-31 108 V\n61 -37 V\n.4108 g 1304 2343 N 61 -37
      21 -111 -62 38 h\n1.000 UP\n1.000 UL\nLT2\n1304 2343 M\n20 -110 V\n-61
      37 V\n-21 111 V\n62 -38 V\n.4552 g 1295 2455 N 62 -38 10 -112 -63 38
      h\n1.000 UP\n1.000 UL\nLT2\n1295 2455 M\n9 -112 V\n-62 38 V\n-10 112
      V\n63 -38 V\n.5 g 1295 2568 N 63 -38 0 -113 -63 38 h\n1.000 UP\n1.000
      UL\nLT2\n1295 2568 M\n0 -113 V\n-63 38 V\n0 113 V\n63 -38 V\n.5448 g
      1304 2681 N 63 -38 -10 -113 -62 38 h\n1.000 UP\n1.000 UL\nLT2\n1304
      2681 M\n-9 -113 V\n-63 38 V\n10 113 V\n62 -38 V\n.5892 g 1324 2794 N 62
      -38 -21 -112 -61 37 h\n1.000 UP\n1.000 UL\nLT2\n1324 2794 M\n-20 -113
      V\n-62 38 V\n21 112 V\n61 -37 V\n.6329 g 1353 2904 N 61 -37 -31 -110
      -59 37 h\n1.000 UP\n1.000 UL\nLT2\n1353 2904 M\n-29 -110 V\n-61 37
      V\n31 110 V\n59 -37 V\n.6755 g 1392 3013 N 59 -37 -41 -107 -57 35
      h\n1.000 UP\n1.000 UL\nLT2\n1392 3013 M\n-39 -109 V\n-59 37 V\n41 107
      V\n57 -35 V\n.7167 g 1440 3118 N 57 -35 -50 -103 -55 33 h\n1.000
      UP\n1.000 UL\nLT2\n1440 3118 M\n-48 -105 V\n-57 35 V\n50 103 V\n55 -33
      V\n.7562 g 1496 3218 N 55 -33 -59 -99 -52 32 h\n1.000 UP\n1.000
      UL\nLT2\n1496 3218 M\n-56 -100 V\n-55 33 V\n59 99 V\n52 -32 V\n.7936 g
      1561 3314 N 52 -32 -68 -94 -49 30 h\n1.000 UP\n1.000 UL\nLT2\n1561 3314
      M\n-65 -96 V\n-52 32 V\n68 94 V\n49 -30 V\n.8286 g 1634 3403 N 49 -30
      -76 -87 -46 28 h\n1.000 UP\n1.000 UL\nLT2\n1634 3403 M\n-73 -89 V\n-49
      30 V\n76 87 V\n46 -28 V\n.861 g 1713 3487 N 46 -28 -84 -81 -41 25
      h\n1.000 UP\n1.000 UL\nLT2\n1713 3487 M\n-79 -84 V\n-46 28 V\n84 81
      V\n41 -25 V\n.8905 g 1799 3563 N 41 -25 -91 -73 -36 22 h\n1.000
      UP\n1.000 UL\nLT2\n1799 3563 M\n-86 -76 V\n-41 25 V\n91 73 V\n36 -22
      V\n.9169 g 1891 3631 N 36 -22 -96 -65 -32 19 h\n1.000 UP\n1.000
      UL\nLT2\n1891 3631 M\n-92 -68 V\n-36 22 V\n96 65 V\n32 -19 V\n.9399 g
      1988 3690 N 32 -19 -102 -57 -27 17 h\n1.000 UP\n1.000 UL\nLT2\n1988
      3690 M\n-97 -59 V\n-32 19 V\n102 57 V\n27 -17 V\n.9593 g 2090 3741 N 27
      -17 -107 -48 -22 14 h\n1.000 UP\n1.000 UL\nLT2\n2090 3741 M\n-102 -51
      V\n-27 17 V\n107 48 V\n22 -14 V\n.975 g 2194 3783 N 22 -14 -110 -38 -16
      10 h\n1.000 UP\n1.000 UL\nLT2\n2194 3783 M\n-104 -42 V\n-22 14 V\n110
      38 V\n16 -10 V\n.987 g 2302 3815 N 16 -10 -113 -29 -11 7 h\n1.000
      UP\n1.000 UL\nLT2\n2302 3815 M\n-108 -32 V\n-16 10 V\n113 29 V\n11 -7
      V\n.995 g 2411 3837 N 11 -7 -114 -18 -6 3 h\n1.000 UP\n1.000
      UL\nLT2\n2411 3837 M\n-109 -22 V\n-11 7 V\n114 18 V\n6 -3 V\n.999 g
      2521 3849 N 6 -3 -116 -9 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849
      M\n-110 -12 V\n-6 3 V\n116 9 V\n.001 g 2405 1331 N 0 0 117 -5 -1 4
      h\n1.000 UP\n1.000 UL\nLT2\n2405 1331 M\n116 -1 V\n-117 5 V\n1 -4
      V\n.005 g 2291 1343 N 1 -4 117 -15 -4 7 h\n1.000 UP\n1.000
      UL\nLT2\n2291 1343 M\n114 -12 V\n-1 4 V\n-117 15 V\n4 -7 V\n.013 g 2178
      1365 N 4 -7 115 -25 -6 10 h\n1.000 UP\n1.000 UL\nLT2\n2178 1365 M\n113
      -22 V\n-4 7 V\n-115 25 V\n6 -10 V\n.025 g 2068 1396 N 6 -10 112 -35 -8
      14 h\n1.000 UP\n1.000 UL\nLT2\n2068 1396 M\n110 -31 V\n-6 10 V\n-112 35
      V\n8 -14 V\n.0407 g 1961 1437 N 8 -14 108 -45 -9 18 h\n1.000 UP\n1.000
      UL\nLT2\n1961 1437 M\n107 -41 V\n-8 14 V\n-108 45 V\n9 -18 V\n.0601 g
      1859 1488 N 9 -18 104 -53 -11 20 h\n1.000 UP\n1.000 UL\nLT2\n1859 1488
      M\n102 -51 V\n-9 18 V\n-104 53 V\n11 -20 V\n.0831 g 1763 1547 N 11 -20
      98 -62 -13 23 h\n1.000 UP\n1.000 UL\nLT2\n1763 1547 M\n96 -59 V\n-11 20
      V\n-98 62 V\n13 -23 V\n.1095 g 1672 1615 N 13 -23 92 -71 -14 26
      h\n1.000 UP\n1.000 UL\nLT2\n1672 1615 M\n91 -68 V\n-13 23 V\n-92 71
      V\n14 -26 V\n.139 g 1588 1690 N 14 -26 85 -78 -15 29 h\n1.000 UP\n1.000
      UL\nLT2\n1588 1690 M\n84 -75 V\n-14 26 V\n-85 78 V\n15 -29 V\n.1714 g
      1512 1773 N 15 -29 77 -85 -16 31 h\n1.000 UP\n1.000 UL\nLT2\n1512 1773
      M\n76 -83 V\n-15 29 V\n-77 85 V\n16 -31 V\n.2064 g 1444 1862 N 16 -31
      69 -92 -17 34 h\n1.000 UP\n1.000 UL\nLT2\n1444 1862 M\n68 -89 V\n-16 31
      V\n-69 92 V\n17 -34 V\n.2438 g 1385 1958 N 17 -34 61 -97 -19 35
      h\n1.000 UP\n1.000 UL\nLT2\n1385 1958 M\n59 -96 V\n-17 34 V\n-61 97
      V\n19 -35 V\n.2833 g 1335 2058 N 19 -35 51 -101 -20 36 h\n1.000
      UP\n1.000 UL\nLT2\n1335 2058 M\n50 -100 V\n-19 35 V\n-51 101 V\n20 -36
      V\n.3245 g 1294 2162 N 20 -36 41 -106 -20 38 h\n1.000 UP\n1.000
      UL\nLT2\n1294 2162 M\n41 -104 V\n-20 36 V\n-41 106 V\n20 -38 V\n.3671 g
      1263 2270 N 20 -38 31 -109 -20 39 h\n1.000 UP\n1.000 UL\nLT2\n1263 2270
      M\n31 -108 V\n-20 38 V\n-31 109 V\n20 -39 V\n.4108 g 1242 2381 N 20 -39
      21 -111 -20 39 h\n1.000 UP\n1.000 UL\nLT2\n1242 2381 M\n21 -111 V\n-20
      39 V\n-21 111 V\n20 -39 V\n.4552 g 1232 2493 N 20 -39 11 -113 -21 40
      h\n1.000 UP\n1.000 UL\nLT2\n1232 2493 M\n10 -112 V\n-20 39 V\n-11 113
      V\n21 -40 V\n.5 g 1232 2606 N 21 -40 0 -113 -21 40 h\n1.000 UP\n1.000
      UL\nLT2\n1232 2606 M\n0 -113 V\n-21 40 V\n0 113 V\n21 -40 V\n.5448 g
      1242 2719 N 21 -40 -11 -113 -20 40 h\n1.000 UP\n1.000 UL\nLT2\n1242
      2719 M\n-10 -113 V\n-21 40 V\n11 113 V\n20 -40 V\n.5892 g 1263 2831 N
      20 -40 -21 -111 -20 39 h\n1.000 UP\n1.000 UL\nLT2\n1263 2831 M\n-21
      -112 V\n-20 40 V\n21 111 V\n20 -39 V\n.6329 g 1294 2941 N 20 -39 -31
      -109 -20 38 h\n1.000 UP\n1.000 UL\nLT2\n1294 2941 M\n-31 -110 V\n-20 39
      V\n31 109 V\n20 -38 V\n.6755 g 1335 3048 N 20 -38 -41 -106 -20 37
      h\n1.000 UP\n1.000 UL\nLT2\n1335 3048 M\n-41 -107 V\n-20 38 V\n41 106
      V\n20 -37 V\n.7167 g 1385 3151 N 20 -37 -51 -101 -19 35 h\n1.000
      UP\n1.000 UL\nLT2\n1385 3151 M\n-50 -103 V\n-20 37 V\n51 101 V\n19 -35
      V\n.7562 g 1444 3250 N 19 -35 -61 -97 -17 33 h\n1.000 UP\n1.000
      UL\nLT2\n1444 3250 M\n-59 -99 V\n-19 35 V\n61 97 V\n17 -33 V\n.7936 g
      1512 3344 N 17 -33 -69 -92 -16 31 h\n1.000 UP\n1.000 UL\nLT2\n1512 3344
      M\n-68 -94 V\n-17 33 V\n69 92 V\n16 -31 V\n.8286 g 1588 3431 N 16 -31
      -77 -85 -15 29 h\n1.000 UP\n1.000 UL\nLT2\n1588 3431 M\n-76 -87 V\n-16
      31 V\n77 85 V\n15 -29 V\n.861 g 1672 3512 N 15 -29 -85 -78 -14 26
      h\n1.000 UP\n1.000 UL\nLT2\n1672 3512 M\n-84 -81 V\n-15 29 V\n85 78
      V\n14 -26 V\n.8905 g 1763 3585 N 14 -26 -92 -71 -13 24 h\n1.000
      UP\n1.000 UL\nLT2\n1763 3585 M\n-91 -73 V\n-14 26 V\n92 71 V\n13 -24
      V\n.9169 g 1859 3650 N 13 -24 -98 -62 -11 21 h\n1.000 UP\n1.000
      UL\nLT2\n1859 3650 M\n-96 -65 V\n-13 24 V\n98 62 V\n11 -21 V\n.9399 g
      1961 3707 N 11 -21 -104 -53 -9 17 h\n1.000 UP\n1.000 UL\nLT2\n1961 3707
      M\n-102 -57 V\n-11 21 V\n104 53 V\n9 -17 V\n.9593 g 2068 3755 N 9 -17
      -108 -45 -8 14 h\n1.000 UP\n1.000 UL\nLT2\n2068 3755 M\n-107 -48 V\n-9
      17 V\n108 45 V\n8 -14 V\n.975 g 2178 3793 N 8 -14 -112 -35 -6 11
      h\n1.000 UP\n1.000 UL\nLT2\n2178 3793 M\n-110 -38 V\n-8 14 V\n112 35
      V\n6 -11 V\n.987 g 2291 3822 N 6 -11 -115 -25 -4 7 h\n1.000 UP\n1.000
      UL\nLT2\n2291 3822 M\n-113 -29 V\n-6 11 V\n115 25 V\n4 -7 V\n.995 g
      2405 3840 N 4 -7 -117 -15 -1 4 h\n1.000 UP\n1.000 UL\nLT2\n2405 3840
      M\n-114 -18 V\n-4 7 V\n117 15 V\n1 -4 V\n.999 g 2521 3849 N 1 -4 -117
      -5 0 0 h\n1.000 UP\n1.000 UL\nLT2\n2521 3849 M\n-116 -9 V\n-1 4 V\n117
      5 V\n%pm3d_map_end\ngrestore % colour palette end\n1.000 UP\n1.000
      UP\nstroke\ngrestore\nend\nshowpage\n%%Trailer\n%%DocumentFonts:
      Helvetica\n>|ps>||||||>

      \;
    </output>

    <\input|GNUplot] >
      \;
    </input>
  </session>

  Gnuplot supports many fancy features for users to make high-class graphs.
  If the number of commands is more than one, then you may separate the
  commands by <verbatim|~> or a newline. We recall that a newline is started
  using <key|S-return>.

  <apply|tmdoc-copyright|2003|Chu-Ching Huang>

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
    <associate|toc-12|<tuple|8.4|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|toc-13|<tuple|8.5|?>>
    <associate|idx-3|<tuple|3|?>>
    <associate|gly-4|<tuple|4|?>>
    <associate|toc-14|<tuple|8.6|?>>
    <associate|idx-4|<tuple|7|?>>
    <associate|gly-5|<tuple|5|?>>
    <associate|toc-15|<tuple|8.7|?>>
    <associate|idx-5|<tuple|8|?>>
    <associate|toc-16|<tuple|8.8|?>>
    <associate|gly-6|<tuple|6|?>>
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
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Gnuplot>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
