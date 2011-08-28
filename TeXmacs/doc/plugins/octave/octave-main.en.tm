<TeXmacs|1.0.1.20>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\expand|tmdoc-title>
    Using Octave sessions inside <TeXmacs>
  </expand>

  GNU <name|Octave> is a free clone of <name|Matlab>, which can be downloaded
  from

  <\verbatim>
    \ \ \ \ http://octave.sf.net
  </verbatim>

  An <name|Octave> session is started using <apply|menu|Insert|Session|Octave>.
  Below, it is shown how to do linear algebra operations with <name|Octave>,
  such as matrix multiplication, inversion and diagonalization. Notice that
  you need to use the <verbatim|tmdisp> command (at the moment) in order to
  display the output in mathematical form.

  <\session|octave|default>
    <\output>
      GNU Octave, version 2.1.40 (i386-redhat-linux-gnu).

      Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002 John W. Eaton.

      This is free software; see the source code for copying conditions.

      There is ABSOLUTELY NO WARRANTY; not even for MERCHANTIBILITY or

      FITNESS FOR A PARTICULAR PURPOSE.

      \;

      Report bugs to \<less\>bug-octave@bevo.che.wisc.edu\<gtr\>.

      \;
    </output>

    <\input|octave\<gtr\> >
      A=[1 0 0 0;2 2 0 0;-1 0 2 0;0 -1 2 2]
    </input>

    <\output>
      A =

      \;

      \ \ \ 1 \ \ 0 \ \ 0 \ \ 0

      \ \ \ 2 \ \ 2 \ \ 0 \ \ 0

      \ \ -1 \ \ 0 \ \ 2 \ \ 0

      \ \ \ 0 \ -1 \ \ 2 \ \ 2

      \;

      \;
    </output>

    <\input|octave\<gtr\> >
      tmdisp(A^2)
    </input>

    <\output>
      <with|mode|math|formula style|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|0>>>|<row|<cell|<with|mode|math|6>>|<cell|<with|mode|math|4>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|0>>>|<row|<cell|<with|mode|math|-3>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|4>>|<cell|<with|mode|math|0>>>|<row|<cell|<with|mode|math|-4>>|<cell|<with|mode|math|-4>>|<cell|<with|mode|math|8>>|<cell|<with|mode|math|4>>>>>>>

      \;
    </output>

    <\input|octave\<gtr\> >
      tmdisp(A.^2)
    </input>

    <\output>
      <with|mode|math|formula style|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|0>>>|<row|<cell|<with|mode|math|4>>|<cell|<with|mode|math|4>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|0>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|0>>|<cell|<with|mode|math|4>>|<cell|<with|mode|math|0>>>|<row|<cell|<with|mode|math|0>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|4>>|<cell|<with|mode|math|4>>>>>>>

      \;
    </output>

    <\input|octave\<gtr\> >
      [u,v]=eig(A)
    </input>

    <\output>
      u =

      \;

      \ \ \ 0.00000 \ \ 0.00000 \ \ 0.00000 \ \ 0.21320

      \ \ \ 0.00000 \ \ 0.00000 \ \ 0.00000 \ -0.42640

      \ \ \ 0.00000 \ \ 0.00000 \ \ 0.00000 \ \ 0.21320

      \ \ \ 1.00000 \ \ 1.00000 \ -1.00000 \ -0.85280

      \;

      v =

      \;

      \ \ 2 \ 0 \ 0 \ 0

      \ \ 0 \ 2 \ 0 \ 0

      \ \ 0 \ 0 \ 2 \ 0

      \ \ 0 \ 0 \ 0 \ 1

      \;

      \;
    </output>

    <\input|octave\<gtr\> >
      Q=[1 0 0 0;-2 2 1 0;1 1 0 0;-4 0 0 -1]
    </input>

    <\output>
      Q =

      \;

      \ \ \ 1 \ \ 0 \ \ 0 \ \ 0

      \ \ -2 \ \ 2 \ \ 1 \ \ 0

      \ \ \ 1 \ \ 1 \ \ 0 \ \ 0

      \ \ -4 \ \ 0 \ \ 0 \ -1

      \;

      \;
    </output>

    <\input|octave\<gtr\> >
      P=inv(Q)
    </input>

    <\output>
      P =

      \;

      \ \ \ 1 \ -0 \ \ 0 \ \ 0

      \ \ -1 \ \ 0 \ \ 1 \ \ 0

      \ \ \ 4 \ \ 1 \ -2 \ \ 0

      \ \ -4 \ \ 0 \ \ 0 \ -1

      \;

      \;
    </output>

    <\input|octave\<gtr\> >
      P*A*Q
    </input>

    <\output>
      ans =

      \;

      \ \ 1 \ 0 \ 0 \ 0

      \ \ 0 \ 2 \ 0 \ 0

      \ \ 0 \ 0 \ 2 \ 0

      \ \ 0 \ 0 \ 1 \ 2

      \;

      \;
    </output>

    <\input|octave\<gtr\> >
      \;
    </input>
  </session>

  The second part shows the graph capacity of Octave, 2D and 3D graphs. 2D
  graphs can be automatically embedded into the worksheet but 3D graphs are
  not yet.

  <\session|octave|default>
    <\input|octave\<gtr\> >
      x=linspace(-10,10,1000);
    </input>

    <\input|octave\<gtr\> >
      y=x+sin(x);
    </input>

    <\input|octave\<gtr\> >
      plot(x,y,";Function y=x+sin(x);");
    </input>

    <\output>
      \;

      \;

      <postscript|<tuple|<raw_data|%!PS-Adobe-2.0 EPSF-2.0\n%%Title:
      /tmp/tmplot.eps\n%%Creator: gnuplot 3.7 patchlevel 2\n%%CreationDate:
      Wed Jul 23 17:35:03 2003\n%%DocumentFonts: (atend)\n%%BoundingBox: 50
      50 230 176\n%%Orientation: Portrait\n%%EndComments\n/gnudict 256 dict
      def\ngnudict begin\n/Color true def\n/Solid false def\n/gnulinewidth
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
      UL\nLTb\n490 280 M\n63 0 V\n2809 0 R\n-63 0 V\n stroke\n406 280 M\n[
      [(Helvetica) 140.0 0.0 true true (-10)]\n] -46.7 MRshow\n490 487 M\n63
      0 V\n2809 0 R\n-63 0 V\n stroke\n406 487 M\n[ [(Helvetica) 140.0 0.0
      true true (-8)]\n] -46.7 MRshow\n490 694 M\n63 0 V\n2809 0 R\n-63 0 V\n
      stroke\n406 694 M\n[ [(Helvetica) 140.0 0.0 true true (-6)]\n] -46.7
      MRshow\n490 902 M\n63 0 V\n2809 0 R\n-63 0 V\n stroke\n406 902 M\n[
      [(Helvetica) 140.0 0.0 true true (-4)]\n] -46.7 MRshow\n490 1109 M\n63
      0 V\n2809 0 R\n-63 0 V\n stroke\n406 1109 M\n[ [(Helvetica) 140.0 0.0
      true true (-2)]\n] -46.7 MRshow\n490 1316 M\n63 0 V\n2809 0 R\n-63 0
      V\n stroke\n406 1316 M\n[ [(Helvetica) 140.0 0.0 true true ( 0)]\n]
      -46.7 MRshow\n490 1523 M\n63 0 V\n2809 0 R\n-63 0 V\n stroke\n406 1523
      M\n[ [(Helvetica) 140.0 0.0 true true ( 2)]\n] -46.7 MRshow\n490 1730
      M\n63 0 V\n2809 0 R\n-63 0 V\n stroke\n406 1730 M\n[ [(Helvetica) 140.0
      0.0 true true ( 4)]\n] -46.7 MRshow\n490 1938 M\n63 0 V\n2809 0 R\n-63
      0 V\n stroke\n406 1938 M\n[ [(Helvetica) 140.0 0.0 true true ( 6)]\n]
      -46.7 MRshow\n490 2145 M\n63 0 V\n2809 0 R\n-63 0 V\n stroke\n406 2145
      M\n[ [(Helvetica) 140.0 0.0 true true ( 8)]\n] -46.7 MRshow\n490 2352
      M\n63 0 V\n2809 0 R\n-63 0 V\n stroke\n406 2352 M\n[ [(Helvetica) 140.0
      0.0 true true ( 10)]\n] -46.7 MRshow\n490 280 M\n0 63 V\n0 2009 R\n0
      -63 V\n stroke\n490 140 M\n[ [(Helvetica) 140.0 0.0 true true (-10)]\n]
      -46.7 MCshow\n1208 280 M\n0 63 V\n0 2009 R\n0 -63 V\n stroke\n1208 140
      M\n[ [(Helvetica) 140.0 0.0 true true (-5)]\n] -46.7 MCshow\n1926 280
      M\n0 63 V\n0 2009 R\n0 -63 V\n stroke\n1926 140 M\n[ [(Helvetica) 140.0
      0.0 true true ( 0)]\n] -46.7 MCshow\n2644 280 M\n0 63 V\n0 2009 R\n0
      -63 V\n stroke\n2644 140 M\n[ [(Helvetica) 140.0 0.0 true true ( 5)]\n]
      -46.7 MCshow\n3362 280 M\n0 63 V\n0 2009 R\n0 -63 V\n stroke\n3362 140
      M\n[ [(Helvetica) 140.0 0.0 true true ( 10)]\n] -46.7 MCshow\n1.000
      UL\nLTb\n490 280 M\n2872 0 V\n0 2072 V\n-2872 0 V\n490 280 L\n1.000
      UL\nLT0\n2711 2219 M\n[ [(Helvetica) 140.0 0.0 true true (Function
      y=x+sin\\(x\\))]\n] -46.7 MRshow\n2795 2219 M\n399 0 V\n490 336 M\n3 1
      V\n3 0 V\n3 0 V\n2 1 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0
      V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 1 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n3 0
      V\n2 0 V\n3 1 V\n3 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n2 1 V\n3 0
      V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n2 1 V\n3 0 V\n3 1 V\n3 1
      V\n3 0 V\n3 1 V\n3 1 V\n3 1 V\n2 1 V\n3 0 V\n3 1 V\n3 1 V\n3 1 V\n3 1
      V\n3 1 V\n3 1 V\n2 2 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 2 V\n3 1 V\n3 2
      V\n2 1 V\n3 1 V\n3 2 V\n3 2 V\n3 1 V\n3 2 V\n3 1 V\n3 2 V\n2 2 V\n3 2
      V\n3 2 V\n3 1 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n2 2 V\n3 3 V\n3 2 V\n3 2
      V\n3 2 V\n3 2 V\n3 3 V\n3 2 V\n2 3 V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n3 2
      V\n3 3 V\n3 3 V\n2 2 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3
      V\n2 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 4 V\n3 3 V\n2 3 V\n3 4
      V\n3 3 V\n3 3 V\n3 4 V\n3 3 V\n3 4 V\n3 3 V\n2 4 V\n3 4 V\n3 3 V\n3 4
      V\n3 4 V\n3 3 V\n3 4 V\n3 4 V\n2 4 V\n3 3 V\n3 4 V\n3 4 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4
      V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 5 V\n3 4
      V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 5 V\n2 4 V\n3 4 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n3 5 V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4
      V\n2 4 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n3 4 V\n3 4 V\n3 3 V\n2 4 V\n3 4
      V\n3 3 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n3 3 V\n2 4 V\n3 3 V\n3 3 V\n3 4
      V\n3 3 V\n3 3 V\n3 4 V\n3 3 V\n2 3 V\n3 3 V\n3 3 V\n3 3 V\n3 4 V\n3 3
      V\n3 3 V\n3 2 V\n2 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 2 V\n3 3 V\n3 3
      V\n2 2 V\n3 3 V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n3 2 V\n3 2 V\n2 3 V\n3 2
      V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n2 2 V\n3 2 V\n3 2 V\n3 2
      V\n3 1 V\n3 2 V\n3 2 V\n3 2 V\n2 1 V\n3 2 V\n3 1 V\n3 2 V\n3 1 V\n3 2
      V\n3 1 V\n3 1 V\n2 2 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 2
      V\n2 1 V\n3 0 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 0 V\n2 1 V\n3 1
      V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n2 1 V\n3 0 V\n3 1 V\n3 0
      V\n3 1 V\n3 0 V\n3 0 V\n3 1 V\n2 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n2 1 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0
      V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n2 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 1 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n3 0 V\n3 1
      V\n2 0 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n2 1 V\n3 0
      V\n3 1 V\n3 0 V\n3 1 V\n3 1 V\n3 0 V\n3 1 V\n2 1 V\n3 1 V\n3 1 V\n3 1
      V\n3 0 V\n3 1 V\n3 1 V\n3 1 V\n2 1 V\n3 2 V\ncurrentpoint stroke M\n3 1
      V\n3 1 V\n3 1 V\n3 1 V\n3 2 V\n3 1 V\n2 1 V\n3 2 V\n3 1 V\n3 2 V\n3 1
      V\n3 2 V\n3 1 V\n3 2 V\n2 2 V\n3 1 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2
      V\n3 2 V\n2 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 3 V\n2 2
      V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n3 3 V\n2 2 V\n3 3 V\n3 3
      V\n3 3 V\n3 2 V\n3 3 V\n3 3 V\n3 3 V\n2 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3
      V\n3 3 V\n3 4 V\n3 3 V\n2 3 V\n3 3 V\n3 4 V\n3 3 V\n3 4 V\n3 3 V\n3 3
      V\n3 4 V\n2 3 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n3 3 V\n3 4 V\n3 4 V\n2 4
      V\n3 3 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n2 4 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 5
      V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 5 V\n3 4
      V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 5 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 4
      V\n3 4 V\n3 4 V\n3 4 V\n3 5 V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 3 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n3 3 V\n2 4 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n3 3 V\n3 4
      V\n3 4 V\n2 3 V\n3 4 V\n3 3 V\n3 3 V\n3 4 V\n3 3 V\n3 4 V\n3 3 V\n2 3
      V\n3 3 V\n3 4 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n2 3 V\n3 3 V\n3 3
      V\n3 3 V\n3 2 V\n3 3 V\n3 3 V\n3 3 V\n2 2 V\n3 3 V\n3 3 V\n3 2 V\n3 3
      V\n3 2 V\n3 3 V\n3 2 V\n2 2 V\n3 3 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2
      V\n3 2 V\n2 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 1 V\n2 2
      V\n3 2 V\n3 1 V\n3 2 V\n3 1 V\n3 2 V\n3 1 V\n3 2 V\n2 1 V\n3 1 V\n3 2
      V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 2 V\n2 1 V\n3 1 V\n3 1 V\n3 1 V\n3 0
      V\n3 1 V\n3 1 V\n3 1 V\n2 1 V\n3 1 V\n3 0 V\n3 1 V\n3 1 V\n3 0 V\n3 1
      V\n3 0 V\n2 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n2 0
      V\n3 1 V\n3 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 1
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 1 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n2 0 V\n3 1 V\n3 0 V\n3 0 V\n3 1
      V\n3 0 V\n3 1 V\n3 0 V\n2 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n3 1
      V\n3 1 V\n2 1 V\n3 0 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 0 V\n2 1
      V\n3 2 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n2 2 V\n3 1 V\n3 1
      V\n3 2 V\n3 1 V\n3 2 V\n3 1 V\n3 2 V\n2 1 V\n3 2 V\n3 2 V\n3 2 V\n3 1
      V\n3 2 V\n3 2 V\n3 2 V\n2 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 2
      V\n3 2 V\n2 3 V\n3 2 V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n2 2
      V\n3 3 V\n3 3 V\n3 2 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n2 3 V\n3 2 V\n3 3
      V\n3 3 V\n3 4 V\n3 3 V\n3 3 V\n3 3 V\n2 3 V\n3 3 V\n3 4 V\n3 3 V\n3 3
      V\n3 4 V\n3 3 V\n3 3 V\n2 4 V\n3 3 V\n3 4 V\n3 3 V\n3 4 V\n3 4 V\n3 3
      V\n3 4 V\n2 4 V\n3 3 V\n3 4 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n3 4 V\n2 4
      V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 4
      V\ncurrentpoint stroke M\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 4
      V\n3 4 V\n3 5 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 5 V\n3 4
      V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n2 5 V\n3 4 V\n3 4 V\n3 4 V\n3 4
      V\n3 4 V\n3 4 V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4
      V\n3 4 V\n2 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 4 V\n3 3 V\n2 4
      V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n3 4 V\n3 3 V\n3 4 V\n2 4 V\n3 3 V\n3 4
      V\n3 3 V\n3 4 V\n3 3 V\n3 3 V\n3 4 V\n2 3 V\n3 3 V\n3 4 V\n3 3 V\n3 3
      V\n3 3 V\n3 3 V\n3 3 V\n2 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3 V\n3 3
      V\n3 3 V\n2 2 V\n3 3 V\n3 3 V\n3 2 V\n3 3 V\n3 2 V\n3 3 V\n3 2 V\n2 3
      V\n3 2 V\n3 3 V\n3 2 V\n3 2 V\n3 2 V\n3 2 V\n3 3 V\n2 2 V\n3 2 V\n3 2
      V\n3 2 V\n3 2 V\n3 1 V\n3 2 V\n3 2 V\n2 2 V\n3 2 V\n3 1 V\n3 2 V\n3 1
      V\n3 2 V\n3 2 V\n3 1 V\n2 1 V\n3 2 V\n3 1 V\n3 2 V\n3 1 V\n3 1 V\n3 1
      V\n3 1 V\n2 2 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 1 V\n3 0 V\n2 1
      V\n3 1 V\n3 1 V\n3 1 V\n3 0 V\n3 1 V\n3 1 V\n3 0 V\n2 1 V\n3 0 V\n3 1
      V\n3 0 V\n3 1 V\n3 0 V\n3 1 V\n3 0 V\n2 1 V\n3 0 V\n3 0 V\n3 1 V\n3 0
      V\n3 0 V\n3 0 V\n3 1 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0
      V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n3 0 V\n2 1 V\n3 0 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n3 0 V\n3 0 V\n2 0 V\n3 0 V\n3 0 V\n3 1 V\n3 0 V\n3 0 V\n3 0
      V\n3 0 V\n2 1 V\n3 0 V\n3 0 V\n3 1 V\nstroke\ngrestore\nend\nshowpage\n>|ps>||||||>

      \;
    </output>

    <\input|octave\<gtr\> >
      x0=[2;5;10];
    </input>

    <\input|octave\<gtr\> >
      t = linspace (0,10,800);
    </input>

    <\input|octave\<gtr\> >
      function dx = butter (x ,t) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ dx(1)
      = -10.0*(x(1)-x(2)); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ dx(2)
      = 28.0*x(1)-x(2)-x(1)*x(3); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ dx(3)
      = 8.0/3.0*( x(1)*x(2) -x(3) ); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ end;
    </input>

    <\input|octave\<gtr\> >
      y=lsode("butter",x0,t);
    </input>

    <\input|octave\<gtr\> >
      gset parametric;
    </input>

    <\input|octave\<gtr\> >
      gset set term postscript enhanced color eps;
    </input>

    <\input|octave\<gtr\> >
      gset xtics 10;gset ytics 10; gset ztics 10;
    </input>

    <\input|octave\<gtr\> >
      gset out "/tmp/butterfly.eps";
    </input>

    <\input|octave\<gtr\> >
      gsplot y title "Butterfly Effect"
    </input>

    <\input|octave\<gtr\> >
      \;
    </input>
  </session>

  In order to embed the 3D graph, we first save it as butterfly.eps in /tmp
  directory. Then we can embed this EPS file into the worksheet using
  <apply|menu|Insert|Image>.

  <expand|big-figure|<postscript|<tuple|<raw_data|%!PS-Adobe-2.0
  EPSF-2.0\n%%Title: /tmp/butterfly.eps\n%%Creator: gnuplot 3.7 patchlevel
  3\n%%CreationDate: Wed Jul \ 9 21:26:06 2003\n%%DocumentFonts:
  (atend)\n%%BoundingBox: 50 50 230 176\n%%Orientation:
  Portrait\n%%EndComments\n/gnudict 256 dict def\ngnudict begin\n/Color true
  def\n/Solid false def\n/gnulinewidth 5.000 def\n/userlinewidth gnulinewidth
  def\n/vshift -46 def\n/dl {10 mul} def\n/hpt_ 31.5 def\n/vpt_ 31.5
  def\n/hpt hpt_ def\n/vpt vpt_ def\n/M {moveto} bind def\n/L {lineto} bind
  def\n/R {rmoveto} bind def\n/V {rlineto} bind def\n/vpt2 vpt 2 mul
  def\n/hpt2 hpt 2 mul def\n/Lshow { currentpoint stroke M\n \ 0 vshift R
  show } def\n/Rshow { currentpoint stroke M\n \ dup stringwidth pop neg
  vshift R show } def\n/Cshow { currentpoint stroke M\n \ dup stringwidth pop
  -2 div vshift R show } def\n/UP { dup vpt_ mul /vpt exch def hpt_ mul /hpt
  exch def\n \ /hpt2 hpt 2 mul def /vpt2 vpt 2 mul def } def\n/DL { Color
  {setrgbcolor Solid {pop []} if 0 setdash }\n {pop pop pop Solid {pop []} if
  0 setdash} ifelse } def\n/BL { stroke userlinewidth 2 mul setlinewidth }
  def\n/AL { stroke userlinewidth 2 div setlinewidth } def\n/UL { dup
  gnulinewidth mul /userlinewidth exch def\n \ \ \ \ \ dup 1 lt {pop 1} if 10
  mul /udl exch def } def\n/PL { stroke userlinewidth setlinewidth }
  def\n/LTb { BL [] 0 0 0 DL } def\n/LTa { AL [1 udl mul 2 udl mul] 0 setdash
  0 0 0 setrgbcolor } def\n/LT0 { PL [] 1 0 0 DL } def\n/LT1 { PL [4 dl 2 dl]
  0 1 0 DL } def\n/LT2 { PL [2 dl 3 dl] 0 0 1 DL } def\n/LT3 { PL [1 dl 1.5
  dl] 1 0 1 DL } def\n/LT4 { PL [5 dl 2 dl 1 dl 2 dl] 0 1 1 DL } def\n/LT5 {
  PL [4 dl 3 dl 1 dl 3 dl] 1 1 0 DL } def\n/LT6 { PL [2 dl 2 dl 2 dl 4 dl] 0
  0 0 DL } def\n/LT7 { PL [2 dl 2 dl 2 dl 2 dl 2 dl 4 dl] 1 0.3 0 DL }
  def\n/LT8 { PL [2 dl 2 dl 2 dl 2 dl 2 dl 2 dl 2 dl 4 dl] 0.5 0.5 0.5 DL }
  def\n/Pnt { stroke [] 0 setdash\n \ \ gsave 1 setlinecap M 0 0 V stroke
  grestore } def\n/Dia { stroke [] 0 setdash 2 copy vpt add M\n \ hpt neg vpt
  neg V hpt vpt neg V\n \ hpt vpt V hpt neg vpt V closepath stroke\n \ Pnt }
  def\n/Pls { stroke [] 0 setdash vpt sub M 0 vpt2 V\n \ currentpoint stroke
  M\n \ hpt neg vpt neg R hpt2 0 V stroke\n \ } def\n/Box { stroke [] 0
  setdash 2 copy exch hpt sub exch vpt add M\n \ 0 vpt2 neg V hpt2 0 V 0 vpt2
  V\n \ hpt2 neg 0 V closepath stroke\n \ Pnt } def\n/Crs { stroke [] 0
  setdash exch hpt sub exch vpt add M\n \ hpt2 vpt2 neg V currentpoint stroke
  M\n \ hpt2 neg 0 R hpt2 vpt2 V stroke } def\n/TriU { stroke [] 0 setdash 2
  copy vpt 1.12 mul add M\n \ hpt neg vpt -1.62 mul V\n \ hpt 2 mul 0 V\n
  \ hpt neg vpt 1.62 mul V closepath stroke\n \ Pnt \ } def\n/Star { 2 copy
  Pls Crs } def\n/BoxF { stroke [] 0 setdash exch hpt sub exch vpt add M\n
  \ 0 vpt2 neg V \ hpt2 0 V \ 0 vpt2 V\n \ hpt2 neg 0 V \ closepath fill }
  def\n/TriUF { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg vpt -1.62
  mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath fill }
  def\n/TriD { stroke [] 0 setdash 2 copy vpt 1.12 mul sub M\n \ hpt neg vpt
  1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath stroke\n
  \ Pnt \ } def\n/TriDF { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg
  vpt 1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath
  fill} def\n/DiaF { stroke [] 0 setdash vpt add M\n \ hpt neg vpt neg V hpt
  vpt neg V\n \ hpt vpt V hpt neg vpt V closepath fill } def\n/Pent { stroke
  [] 0 setdash 2 copy gsave\n \ translate 0 hpt M 4 {72 rotate 0 hpt L}
  repeat\n \ closepath stroke grestore Pnt } def\n/PentF { stroke [] 0
  setdash gsave\n \ translate 0 hpt M 4 {72 rotate 0 hpt L} repeat\n
  \ closepath fill grestore } def\n/Circle { stroke [] 0 setdash 2 copy\n
  \ hpt 0 360 arc stroke Pnt } def\n/CircleF { stroke [] 0 setdash hpt 0 360
  arc fill } def\n/C0 { BL [] 0 setdash 2 copy moveto vpt 90 450 \ arc } bind
  def\n/C1 { BL [] 0 setdash 2 copy \ \ \ \ \ \ \ moveto\n \ \ \ \ \ \ 2 copy
  \ vpt 0 90 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
  closepath } bind def\n/C2 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2
  copy \ vpt 90 180 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
  360 arc closepath } bind def\n/C3 { BL [] 0 setdash 2 copy moveto\n
  \ \ \ \ \ \ 2 copy \ vpt 0 180 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C4 { BL []
  0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 180 270 arc closepath
  fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C5
  { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 90 arc\n
  \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 180 270 arc closepath
  fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind def\n/C6 { BL [] 0
  setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 90 270 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C7 { BL [] 0
  setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 0 270 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C8 { BL [] 0
  setdash 2 copy moveto\n \ \ \ \ \ 2 copy vpt 270 360 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C9 { BL [] 0
  setdash 2 copy moveto\n \ \ \ \ \ 2 copy \ vpt 270 450 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C10 { BL []
  0 setdash 2 copy 2 copy moveto vpt 270 360 arc closepath fill\n
  \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy vpt 90 180 arc closepath
  fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C11
  { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 0 180 arc
  closepath fill\n \ \ \ \ \ \ 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 270
  360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc
  closepath } bind def\n/C12 { BL [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2
  copy \ vpt 180 360 arc closepath fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0
  360 arc closepath } bind def\n/C13 { BL [] 0 setdash \ 2 copy moveto\n
  \ \ \ \ \ \ 2 copy \ vpt 0 90 arc closepath fill\n \ \ \ \ \ \ 2 copy
  moveto\n \ \ \ \ \ \ 2 copy \ vpt 180 360 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/C14 { BL
  [] 0 setdash 2 copy moveto\n \ \ \ \ \ \ 2 copy \ vpt 90 360 arc closepath
  fill\n \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc } bind def\n/C15 { BL [] 0
  setdash 2 copy vpt 0 360 arc closepath fill\n
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ vpt 0 360 arc closepath } bind def\n/Rec \ \ {
  newpath 4 2 roll moveto 1 index 0 rlineto 0 exch rlineto\n \ \ \ \ \ \ neg
  0 rlineto closepath } bind def\n/Square { dup Rec } bind def\n/Bsquare {
  vpt sub exch vpt sub exch vpt2 Square } bind def\n/S0 { BL [] 0 setdash 2
  copy moveto 0 vpt rlineto BL Bsquare } bind def\n/S1 { BL [] 0 setdash 2
  copy vpt Square fill Bsquare } bind def\n/S2 { BL [] 0 setdash 2 copy exch
  vpt sub exch vpt Square fill Bsquare } bind def\n/S3 { BL [] 0 setdash 2
  copy exch vpt sub exch vpt2 vpt Rec fill Bsquare } bind def\n/S4 { BL [] 0
  setdash 2 copy exch vpt sub exch vpt sub vpt Square fill Bsquare } bind
  def\n/S5 { BL [] 0 setdash 2 copy 2 copy vpt Square fill\n \ \ \ \ \ \ exch
  vpt sub exch vpt sub vpt Square fill Bsquare } bind def\n/S6 { BL [] 0
  setdash 2 copy exch vpt sub exch vpt sub vpt vpt2 Rec fill Bsquare } bind
  def\n/S7 { BL [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt vpt2 Rec
  fill\n \ \ \ \ \ \ 2 copy vpt Square fill\n \ \ \ \ \ \ Bsquare } bind
  def\n/S8 { BL [] 0 setdash 2 copy vpt sub vpt Square fill Bsquare } bind
  def\n/S9 { BL [] 0 setdash 2 copy vpt sub vpt vpt2 Rec fill Bsquare } bind
  def\n/S10 { BL [] 0 setdash 2 copy vpt sub vpt Square fill 2 copy exch vpt
  sub exch vpt Square fill\n \ \ \ \ \ \ Bsquare } bind def\n/S11 { BL [] 0
  setdash 2 copy vpt sub vpt Square fill 2 copy exch vpt sub exch vpt2 vpt
  Rec fill\n \ \ \ \ \ \ Bsquare } bind def\n/S12 { BL [] 0 setdash 2 copy
  exch vpt sub exch vpt sub vpt2 vpt Rec fill Bsquare } bind def\n/S13 { BL
  [] 0 setdash 2 copy exch vpt sub exch vpt sub vpt2 vpt Rec fill\n
  \ \ \ \ \ \ 2 copy vpt Square fill Bsquare } bind def\n/S14 { BL [] 0
  setdash 2 copy exch vpt sub exch vpt sub vpt2 vpt Rec fill\n \ \ \ \ \ \ 2
  copy exch vpt sub exch vpt Square fill Bsquare } bind def\n/S15 { BL [] 0
  setdash 2 copy Bsquare fill Bsquare } bind def\n/D0 { gsave translate 45
  rotate 0 0 S0 stroke grestore } bind def\n/D1 { gsave translate 45 rotate 0
  0 S1 stroke grestore } bind def\n/D2 { gsave translate 45 rotate 0 0 S2
  stroke grestore } bind def\n/D3 { gsave translate 45 rotate 0 0 S3 stroke
  grestore } bind def\n/D4 { gsave translate 45 rotate 0 0 S4 stroke grestore
  } bind def\n/D5 { gsave translate 45 rotate 0 0 S5 stroke grestore } bind
  def\n/D6 { gsave translate 45 rotate 0 0 S6 stroke grestore } bind def\n/D7
  { gsave translate 45 rotate 0 0 S7 stroke grestore } bind def\n/D8 { gsave
  translate 45 rotate 0 0 S8 stroke grestore } bind def\n/D9 { gsave
  translate 45 rotate 0 0 S9 stroke grestore } bind def\n/D10 { gsave
  translate 45 rotate 0 0 S10 stroke grestore } bind def\n/D11 { gsave
  translate 45 rotate 0 0 S11 stroke grestore } bind def\n/D12 { gsave
  translate 45 rotate 0 0 S12 stroke grestore } bind def\n/D13 { gsave
  translate 45 rotate 0 0 S13 stroke grestore } bind def\n/D14 { gsave
  translate 45 rotate 0 0 S14 stroke grestore } bind def\n/D15 { gsave
  translate 45 rotate 0 0 S15 stroke grestore } bind def\n/DiaE { stroke [] 0
  setdash vpt add M\n \ hpt neg vpt neg V hpt vpt neg V\n \ hpt vpt V hpt neg
  vpt V closepath stroke } def\n/BoxE { stroke [] 0 setdash exch hpt sub exch
  vpt add M\n \ 0 vpt2 neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V closepath
  stroke } def\n/TriUE { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg
  vpt -1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V closepath
  stroke } def\n/TriDE { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg
  vpt 1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V closepath
  stroke } def\n/PentE { stroke [] 0 setdash gsave\n \ translate 0 hpt M 4
  {72 rotate 0 hpt L} repeat\n \ closepath stroke grestore } def\n/CircE {
  stroke [] 0 setdash \n \ hpt 0 360 arc stroke } def\n/Opaque { gsave
  closepath 1 setgray fill grestore 0 setgray closepath } def\n/DiaW { stroke
  [] 0 setdash vpt add M\n \ hpt neg vpt neg V hpt vpt neg V\n \ hpt vpt V
  hpt neg vpt V Opaque stroke } def\n/BoxW { stroke [] 0 setdash exch hpt sub
  exch vpt add M\n \ 0 vpt2 neg V hpt2 0 V 0 vpt2 V\n \ hpt2 neg 0 V Opaque
  stroke } def\n/TriUW { stroke [] 0 setdash vpt 1.12 mul add M\n \ hpt neg
  vpt -1.62 mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt 1.62 mul V Opaque stroke
  } def\n/TriDW { stroke [] 0 setdash vpt 1.12 mul sub M\n \ hpt neg vpt 1.62
  mul V\n \ hpt 2 mul 0 V\n \ hpt neg vpt -1.62 mul V Opaque stroke }
  def\n/PentW { stroke [] 0 setdash gsave\n \ translate 0 hpt M 4 {72 rotate
  0 hpt L} repeat\n \ Opaque stroke grestore } def\n/CircW { stroke [] 0
  setdash \n \ hpt 0 360 arc Opaque stroke } def\n/BoxFill { gsave Rec 1
  setgray fill grestore } def\n/Symbol-Oblique /Symbol findfont [1 0 .167 1 0
  0] makefont\ndup length dict begin {1 index /FID eq {pop pop} {def} ifelse}
  forall\ncurrentdict end definefont pop\n/MFshow {{dup dup 0 get findfont
  exch 1 get scalefont setfont\n \ \ \ \ [ currentpoint ] exch dup 2 get 0
  exch rmoveto dup dup 5 get exch 4 get\n \ \ \ \ {show} {stringwidth pop 0
  rmoveto}ifelse dup 3 get\n \ \ \ \ {2 get neg 0 exch rmoveto pop} {pop
  aload pop moveto}ifelse} forall} bind def\n/MFwidth {0 exch {dup 3 get{dup
  dup 0 get findfont exch 1 get scalefont setfont\n \ \ \ \ \ 5 get
  stringwidth pop add}\n \ \ \ {pop} ifelse} forall} bind def\n/MLshow {
  currentpoint stroke M\n \ 0 exch R MFshow } bind def\n/MRshow {
  currentpoint stroke M\n \ exch dup MFwidth neg 3 -1 roll R MFshow }
  def\n/MCshow { currentpoint stroke M\n \ exch dup MFwidth -2 div 3 -1 roll
  R MFshow } def\nend\n%%EndProlog\ngnudict begin\ngsave\n50 50
  translate\n0.050 0.050 scale\n0 setgray\nnewpath\n(Helvetica) findfont 140
  scalefont setfont\n1.000 UL\nLTb\n1.000 UL\nLT0\n2802 2036 M\n[
  [(Helvetica) 140.0 0.0 true true (Butterfly Effect)]\n] -46.7 MRshow\n2886
  2036 M\n399 0 V\n1989 1344 M\n24 3 V\n27 5 V\n29 8 V\n31 10 V\n34 13 V\n36
  16 V\n39 20 V\n40 22 V\n41 27 V\n40 29 V\n39 33 V\n34 34 V\n27 35 V\n19 35
  V\n6 30 V\n-6 26 V\n-19 18 V\n-33 9 V\n-44 1 V\n-52 -8 V\n-58 -14 V\n-59
  -19 V\n-59 -21 V\n-55 -22 V\n-51 -22 V\n-46 -21 V\n-41 -18 V\n-35 -17
  V\n-30 -15 V\n-26 -13 V\n-23 -12 V\n-19 -11 V\n-16 -10 V\n-13 -10 V\n-12 -9
  V\n-11 -8 V\n-9 -9 V\n-8 -8 V\n-8 -9 V\n-7 -8 V\n-7 -8 V\n-7 -7 V\n-8 -8
  V\n-7 -8 V\n-8 -7 V\n-9 -7 V\n-9 -7 V\n-10 -7 V\n-11 -6 V\n-12 -6 V\n-13 -6
  V\n-14 -5 V\n-16 -4 V\n-17 -4 V\n-18 -3 V\n-20 -3 V\n-21 0 V\n-23 0 V\n-23
  2 V\n-25 4 V\n-25 5 V\n-26 9 V\n-25 11 V\n-24 13 V\n-22 17 V\n-19 20 V\n-15
  22 V\n-10 25 V\n-5 26 V\n2 27 V\n8 26 V\n15 25 V\n21 22 V\n26 19 V\n30 14
  V\n32 10 V\n34 6 V\n34 1 V\n33 -3 V\n31 -6 V\n29 -9 V\n27 -11 V\n23 -12
  V\n21 -13 V\n18 -13 V\n16 -14 V\n14 -14 V\n11 -13 V\n9 -13 V\n7 -13 V\n6
  -13 V\n4 -12 V\n4 -11 V\n2 -12 V\n0 -10 V\n0 -11 V\n0 -10 V\n-2 -9 V\n-3
  -10 V\n-3 -8 V\n-4 -9 V\n-5 -8 V\n-6 -8 V\n-7 -8 V\n-7 -8 V\n-9 -7 V\n-9 -7
  V\n-11 -6 V\n-12 -6 V\n-13 -6 V\n-15 -5 V\n-17 -5 V\n-18 -4 V\n-19 -4
  V\n-22 -2 V\n-23 -2 V\n-26 0 V\n-27 2 V\n-28 3 V\n-30 6 V\n-31 9 V\n-30 12
  V\n-30 16 V\n-28 19 V\n-24 24 V\n-20 27 V\n-14 30 V\n-6 33 V\n2 34 V\n10 33
  V\n19 32 V\n27 28 V\n34 23 V\n38 17 V\n41 11 V\n42 5 V\n42 0 V\n40 -5 V\n37
  -9 V\n34 -12 V\n31 -13 V\n27 -15 V\n23 -16 V\n21 -16 V\n17 -15 V\n15 -16
  V\n12 -14 V\n11 -15 V\n8 -14 V\n7 -13 V\n6 -12 V\n5 -12 V\n4 -12 V\n2 -10
  V\n3 -11 V\n2 -10 V\n1 -9 V\n1 -9 V\n0 -9 V\n1 -8 V\n0 -8 V\n-1 -8 V\n0 -7
  V\n-1 -7 V\n-1 -7 V\n-1 -7 V\n-1 -6 V\n-1 -7 V\n-2 -6 V\n-2 -6 V\n-2 -6
  V\n-2 -5 V\n-3 -6 V\n-3 -5 V\n-4 -5 V\n-4 -6 V\n-4 -5 V\n-5 -5 V\n-6 -5
  V\n-7 -4 V\n-7 -5 V\n-8 -5 V\n-10 -5 V\n-10 -5 V\n-12 -4 V\n-14 -5 V\n-15
  -4 V\n-17 -4 V\n-20 -4 V\n-21 -4 V\n-25 -3 V\n-27 -2 V\n-31 -1 V\n-33 1
  V\n-37 2 V\n-40 5 V\n-42 9 V\n-45 13 V\n-45 18 V\n-44 24 V\n-42 31 V\n-36
  38 V\n-27 45 V\n-16 51 V\n-2 55 V\n15 56 V\n29 53 V\n45 47 V\n55 37 V\n64
  26 V\n66 14 V\n67 4 V\n63 -6 V\n59 -12 V\n53 -17 V\n46 -20 V\n41 -21 V\n35
  -22 V\n29 -21 V\n26 -19 V\n21 -19 V\n18 -17 V\n16 -17 V\n14 -14 V\n11 -14
  V\n11 -12 V\n10 -11 V\n9 -11 V\n8 -9 V\n9 -8 V\n8 -7 V\n9 -7 V\n9 -6 V\n10
  -4 V\n10 -4 V\n11 -3 V\n12 -2 V\n12 -2 V\n14 0 V\n15 1 V\n16 2 V\n16 3
  V\n18 5 V\n19 5 V\n19 7 V\n20 9 V\n21 10 V\n20 11 V\n20 12 V\n19 14 V\n18
  14 V\n15 15 V\n13 14 V\n9 14 V\n5 14 V\n1 11 V\n-5 10 V\n-8 7 V\n-14 5
  V\n-17 2 V\n-21 -1 V\n-23 -4 V\n-25 -5 V\n-26 -8 V\n-26 -8 V\n-26 -10
  V\n-24 -9 V\n-23 -11 V\n-20 -10 V\n-19 -10 V\n-17 -9 V\n-15 -10 V\n-12 -9
  V\n-11 -8 V\n-8 -9 V\n-7 -8 V\n-6 -7 V\n-4 -8 V\n-2 -7 V\n-1 -6 V\n0 -6
  V\n1 -6 V\n2 -6 V\n3 -5 V\n5 -5 V\n5 -4 V\n7 -4 V\n7 -4 V\n9 -2 V\n10 -2
  V\n11 -2 V\n12 0 V\n14 0 V\n15 2 V\n16 2 V\n17 4 V\n19 5 V\n20 6 V\n21 7
  V\n22 10 V\n22 10 V\n23 13 V\n23 13 V\n22 16 V\n21 16 V\n18 17 V\n16 17
  V\n12 17 V\n7 15 V\n2 14 V\n-4 12 V\n-9 9 V\n-15 6 V\n-19 3 V\n-24 -1
  V\n-26 -4 V\n-29 -6 V\n-30 -9 V\n-30 -10 V\n-29 -11 V\n-28 -11 V\n-25 -12
  V\n-24 -11 V\n-21 -11 V\n-19 -11 V\n-16 -10 V\n-14 -10 V\n-12 -9 V\n-10 -9
  V\n-8 -8 V\n-6 -8 V\n-5 -8 V\n-3 -8 V\n-2 -7 V\n-1 -7 V\n0 -7 V\n1 -6 V\n3
  -6 V\n3 -6 V\n4 -5 V\n5 -5 V\n6 -4 V\n7 -4 V\n8 -4 V\n10 -3 V\n10 -2 V\n12
  -1 V\n13 -1 V\n14 1 V\n15 1 V\n18 2 V\n18 4 V\n20 5 V\n22 7 V\n23 8 V\n25
  10 V\n25 12 V\n27 13 V\n26 16 V\n26 17 V\n25 19 V\n23 20 V\n20 20 V\n16 20
  V\n10 20 V\n4 18 V\n-3 15 V\n-9 11 V\n-17 8 V\n-23 3 V\n-28 -1 V\n-32 -4
  V\n-34 -8 V\n-35 -11 V\n-36 -12 V\n-34 -13 V\n-32 -13 V\n-30 -14 V\n-27 -13
  V\n-24 -12 V\n-21 -12 V\n-19 -11 V\n-16 -11 V\n-13 -10 V\n-11 -9 V\n-9 -9
  V\n-8 -9 V\n-6 -8 V\n-4 -9 V\n-4 -8 V\n-2 -7 V\n-1 -8 V\n0 -7 V\n0 -7 V\n1
  -7 V\n2 -6 V\n3 -6 V\ncurrentpoint stroke M\n3 -6 V\n5 -6 V\n4 -5 V\n6 -5
  V\n6 -4 V\n8 -4 V\n8 -3 V\n9 -3 V\n11 -2 V\n11 -2 V\n13 -1 V\n14 1 V\n16 1
  V\n18 2 V\n19 3 V\n22 5 V\n23 7 V\n25 8 V\n28 10 V\n29 13 V\n30 14 V\n32 18
  V\n33 19 V\n32 22 V\n31 24 V\n29 26 V\n24 27 V\n18 26 V\n11 25 V\n3 22
  V\n-7 18 V\n-16 13 V\n-26 7 V\n-33 0 V\n-39 -4 V\n-43 -10 V\n-46 -13 V\n-45
  -15 V\n-44 -17 V\n-41 -17 V\n-37 -16 V\n-34 -16 V\n-30 -15 V\n-26 -14
  V\n-23 -12 V\n-19 -12 V\n-17 -11 V\n-14 -10 V\n-11 -10 V\n-10 -9 V\n-9 -9
  V\n-6 -9 V\n-6 -9 V\n-5 -8 V\n-4 -9 V\n-4 -8 V\n-3 -8 V\n-2 -8 V\n-3 -7
  V\n-2 -8 V\n-2 -7 V\n-2 -8 V\n-2 -7 V\n-2 -7 V\n-3 -7 V\n-2 -7 V\n-3 -6
  V\n-3 -7 V\n-3 -6 V\n-3 -6 V\n-4 -6 V\n-4 -6 V\n-5 -6 V\n-6 -6 V\n-6 -6
  V\n-7 -5 V\n-7 -6 V\n-9 -5 V\n-9 -5 V\n-11 -6 V\n-13 -5 V\n-13 -5 V\n-16 -4
  V\n-17 -5 V\n-20 -4 V\n-21 -3 V\n-25 -3 V\n-27 -2 V\n-29 -1 V\n-33 1 V\n-35
  3 V\n-38 5 V\n-40 9 V\n-42 13 V\n-41 18 V\n-41 24 V\n-37 30 V\n-32 36
  V\n-23 42 V\n-13 47 V\n1 51 V\n14 50 V\n28 47 V\n41 42 V\n51 33 V\n57 23
  V\n61 14 V\n60 3 V\n59 -4 V\n54 -11 V\n49 -15 V\n44 -19 V\n38 -19 V\n33 -20
  V\n28 -20 V\n24 -19 V\n20 -18 V\n18 -18 V\n15 -16 V\n13 -14 V\n11 -14 V\n10
  -13 V\n8 -12 V\n9 -11 V\n7 -10 V\n8 -9 V\n7 -8 V\n8 -7 V\n8 -7 V\n8 -6 V\n9
  -5 V\n9 -5 V\n11 -3 V\n11 -3 V\n12 -2 V\n13 -1 V\n15 0 V\n16 1 V\n17 2
  V\n18 4 V\n20 4 V\n20 7 V\n23 8 V\n23 9 V\n24 12 V\n24 12 V\n24 15 V\n24 16
  V\n22 17 V\n20 18 V\n17 19 V\n12 18 V\n8 17 V\n2 15 V\n-4 13 V\n-10 9
  V\n-16 6 V\n-21 3 V\n-26 -2 V\n-29 -4 V\n-31 -7 V\n-32 -10 V\n-32 -10
  V\n-31 -12 V\n-29 -12 V\n-27 -13 V\n-24 -12 V\n-22 -11 V\n-20 -11 V\n-17
  -11 V\n-14 -10 V\n-12 -9 V\n-10 -9 V\n-9 -9 V\n-6 -8 V\n-5 -9 V\n-4 -7
  V\n-2 -8 V\n-2 -7 V\n0 -7 V\n1 -7 V\n2 -6 V\n2 -6 V\n4 -6 V\n4 -5 V\n6 -5
  V\n6 -5 V\n7 -4 V\n8 -3 V\n9 -3 V\n10 -3 V\n12 -1 V\n13 -1 V\n14 0 V\n16 1
  V\n17 2 V\n19 4 V\n20 5 V\n22 6 V\n24 8 V\n26 10 V\n27 12 V\n27 14 V\n29 16
  V\n29 18 V\n27 19 V\n26 22 V\n24 22 V\n19 23 V\n13 22 V\n7 20 V\n0 18 V\n-8
  14 V\n-15 10 V\n-24 5 V\n-29 0 V\n-34 -5 V\n-37 -8 V\n-39 -11 V\n-39 -13
  V\n-37 -14 V\n-36 -14 V\n-33 -15 V\n-30 -14 V\n-26 -13 V\n-23 -13 V\n-21
  -12 V\n-17 -11 V\n-15 -10 V\n-12 -10 V\n-11 -10 V\n-8 -9 V\n-7 -8 V\n-6 -9
  V\n-4 -8 V\n-3 -8 V\n-2 -8 V\n-2 -8 V\n-1 -7 V\n0 -8 V\n0 -7 V\n1 -7 V\n2
  -6 V\n2 -6 V\n2 -7 V\n3 -5 V\n3 -6 V\n4 -5 V\n5 -5 V\n5 -5 V\n6 -4 V\n7 -4
  V\n7 -3 V\n8 -3 V\n10 -2 V\n10 -2 V\n12 -1 V\n13 0 V\n15 1 V\n16 1 V\n18 3
  V\n21 4 V\n22 5 V\n25 7 V\n27 9 V\n30 12 V\n32 13 V\n34 17 V\n36 19 V\n37
  22 V\n37 25 V\n37 28 V\n33 30 V\n29 31 V\n23 32 V\n14 30 V\n4 26 V\n-8 22
  V\n-20 15 V\n-31 8 V\n-40 0 V\n-47 -7 V\n-52 -13 V\n-54 -17 V\n-52 -19
  V\n-50 -20 V\n-47 -19 V\n-42 -19 V\n-37 -18 V\n-33 -15 V\n-28 -15 V\n-24
  -13 V\n-21 -11 V\n-18 -11 V\n-15 -11 V\n-13 -9 V\n-11 -10 V\n-9 -9 V\n-8 -8
  V\n-7 -9 V\n-7 -9 V\n-6 -8 V\n-5 -8 V\n-6 -8 V\n-6 -8 V\n-5 -8 V\n-6 -8
  V\n-6 -7 V\n-7 -8 V\n-7 -7 V\n-8 -7 V\n-9 -7 V\n-9 -7 V\n-11 -6 V\n-11 -7
  V\n-13 -6 V\n-15 -5 V\n-15 -5 V\n-17 -5 V\n-19 -3 V\n-21 -3 V\n-22 -2
  V\n-24 -1 V\n-26 0 V\n-28 3 V\n-29 5 V\n-30 7 V\n-31 10 V\n-30 14 V\n-29 17
  V\n-26 21 V\n-22 25 V\n-18 28 V\n-10 31 V\n-3 34 V\n5 33 V\n15 33 V\n22 30
  V\n30 26 V\n35 20 V\n40 15 V\n41 8 V\n42 3 V\n41 -3 V\n39 -6 V\n35 -11
  V\n33 -12 V\n29 -14 V\n25 -15 V\n22 -16 V\n19 -16 V\n17 -15 V\n13 -15 V\n12
  -15 V\n9 -14 V\n8 -13 V\n7 -13 V\n5 -13 V\n4 -11 V\n3 -11 V\n3 -11 V\n2 -10
  V\n1 -10 V\n1 -9 V\n1 -9 V\n0 -9 V\n0 -8 V\n0 -8 V\n-1 -7 V\n0 -8 V\n-1 -7
  V\n-1 -7 V\n-2 -6 V\n-2 -7 V\n-1 -6 V\n-3 -6 V\n-2 -6 V\n-3 -6 V\n-3 -5
  V\n-4 -6 V\n-4 -5 V\n-4 -6 V\n-5 -5 V\n-6 -5 V\n-7 -5 V\n-7 -5 V\n-9 -5
  V\n-9 -5 V\n-11 -5 V\n-12 -5 V\n-14 -5 V\n-15 -4 V\n-17 -4 V\n-20 -4 V\n-22
  -4 V\n-24 -3 V\n-28 -2 V\n-30 -1 V\n-33 1 V\n-37 3 V\ncurrentpoint stroke
  M\n-39 5 V\n-42 9 V\n1.000 UL\nLTb\n3024 948 M\n2128 464 L\n575 743 M\n2128
  464 L\n575 743 M\n896 484 V\n3024 948 M\n1471 1227 L\n575 743 M\n0 968 V\n0
  -968 R\n55 29 V\n stroke\n501 676 M\n[ [(Helvetica) 140.0 0.0 true true
  (-20)]\n] -46.7 MCshow\n1471 1227 M\n-56 -30 V\n963 673 M\n55 29 V\n
  stroke\n889 606 M\n[ [(Helvetica) 140.0 0.0 true true (-10)]\n] -46.7
  MCshow\n1860 1157 M\n-56 -30 V\n1351 603 M\n55 29 V\n stroke\n1277 536 M\n[
  [(Helvetica) 140.0 0.0 true true ( 0)]\n] -46.7 MCshow\n2248 1087 M\n-56
  -30 V\n1739 533 M\n55 29 V\n stroke\n1665 466 M\n[ [(Helvetica) 140.0 0.0
  true true ( 10)]\n] -46.7 MCshow\n2636 1018 M\n-56 -30 V\n2128 464 M\n55 29
  V\n stroke\n2054 397 M\n[ [(Helvetica) 140.0 0.0 true true ( 20)]\n] -46.7
  MCshow\n3024 948 M\n-56 -30 V\n2128 464 M\n-63 11 V\n stroke\n2210 439 M\n[
  [(Helvetica) 140.0 0.0 true true (-20)]\n] -46.7 MLshow\n575 743 M\n62 -12
  V\n2352 585 M\n-63 11 V\n stroke\n2434 560 M\n[ [(Helvetica) 140.0 0.0 true
  true (-10)]\n] -46.7 MLshow\n799 864 M\n62 -12 V\n2576 706 M\n-63 11 V\n
  stroke\n2658 681 M\n[ [(Helvetica) 140.0 0.0 true true ( 0)]\n] -46.7
  MLshow\n1023 985 M\n62 -12 V\n2800 827 M\n-63 11 V\n stroke\n2882 802 M\n[
  [(Helvetica) 140.0 0.0 true true ( 10)]\n] -46.7 MLshow\n1247 1106 M\n62
  -12 V\n3024 948 M\n-63 11 V\n stroke\n3106 923 M\n[ [(Helvetica) 140.0 0.0
  true true ( 20)]\n] -46.7 MLshow\n1471 1227 M\n62 -12 V\n575 1066 M\n63 0
  V\n stroke\n449 1066 M\n[ [(Helvetica) 140.0 0.0 true true ( 0)]\n] -46.7
  MRshow\n575 1195 M\n63 0 V\n stroke\n449 1195 M\n[ [(Helvetica) 140.0 0.0
  true true ( 10)]\n] -46.7 MRshow\n575 1324 M\n63 0 V\n stroke\n449 1324
  M\n[ [(Helvetica) 140.0 0.0 true true ( 20)]\n] -46.7 MRshow\n575 1453
  M\n63 0 V\n stroke\n449 1453 M\n[ [(Helvetica) 140.0 0.0 true true (
  30)]\n] -46.7 MRshow\n575 1582 M\n63 0 V\n stroke\n449 1582 M\n[
  [(Helvetica) 140.0 0.0 true true ( 40)]\n] -46.7 MRshow\n575 1711 M\n63 0
  V\n stroke\n449 1711 M\n[ [(Helvetica) 140.0 0.0 true true ( 50)]\n] -46.7
  MRshow\nstroke\ngrestore\nend\nshowpage\n>|eps>||||||>|Embedded 3D graph
  from Octave.>

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
    <associate|toc-12|<tuple|8.4|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|toc-13|<tuple|8.5|?>>
    <associate|idx-3|<tuple|2|?>>
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
    <\associate|figure>
      <tuple|normal||<pageref|gly-1>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Octave>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Octave>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
