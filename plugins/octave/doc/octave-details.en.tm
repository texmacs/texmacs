<TeXmacs|1.0.1.13>

<style|<tuple|generic|octave>>

<\body>
  <\expand|make-title>
    <title|GNU Octave/TeXmacs Interface>

    <author|Michael Graffam (mgraffam@mathlab.sunysb.edu)>
  </expand>

  Octave and TeXmacs communicate via a synchronous pipe interface. This
  allows the Octave console to be presented in a typeset region of a TeXmacs
  document. To display typeset materials in this TeXmacs region, Octave code
  can print escape sequences to transfer Scheme, LaTeX, or Postscript
  materials for display.\ 

  This Octave interface is implmeneted a a set of m-files for interacting
  with TeXmacs via Scheme. As of v1.0.1 TeXmacs includes the required
  interface code and should automagically detect the presense of Octave.
  These m-files generate TeXmacs content from Octave and perform related
  functions:

  <with|color|blue|tmdisp.m> \ Displays an Octave variable "pretty-printed"
  via the TeXmacs interface. It supports scalar, matrix, structure, list and
  string types. You must use tmdisp, instead of disp, to get \ TeXmacs
  formatted variable output.

  <with|color|blue|mesh.m> \ Modified mesh routine for use with TeXmacs. Also
  included are modified back-end plot routines using gnuplot to transfer the
  embedded Postscript to TeXmacs. High level interfaces like plot work as
  expected.

  <with|color|blue|polyout.m> \ Modified polynomial printer to use a LaTeX
  formula.

  <with|color|blue|scheme.m> \ Executes a string as a Scheme expression via
  TeXmacs.

  <with|color|blue|num2scm.m> Converts a number (scalar) to a TeXmacs Scheme
  expression.

  <with|color|blue|mat2scm.m> \ Converts a matrix to a TeXmacs Scheme
  expression.

  <with|color|blue|str2scm.m> \ Converts a string to a TeXmacs Scheme
  expression.

  <with|color|blue|struct2bullet.m> Converts a structure to a bulleted
  TeXmacs list.

  <with|color|blue|struct2tree.m> \ Converts a structure to a TeXmacs tree.
  The leaves of the tree are "switchable" and can be switched to the variable
  name or content for easy traversal of complicated Octave structures.\ 

  <with|color|blue|struct2scm.m> Converts a structure to a TeXmacs Scheme
  expression by calling either struct2bullet or struct2tree, depending on the
  user's configuration. The TMSTRUCT global Octave variable is used. If
  TMSTRUCT = 0 a tree is used, else a bulleted list is used.

  <with|color|blue|list2scm.m> \ Converts a list to a TeXmacs Scheme
  expression.\ 

  <with|color|blue|obj2scm.m> Front end interface to the other converters to
  convert an arbitrary Octave variable to a TeXmacs Scheme expression. In
  most cases, user programs should use this function.

  \;

  The appearance of the TeXmacs output, namely the colors used, can be
  customized by setting the appropriate Octave global variables. These global
  variables should be defined in your ~/.octaverc or in the system-wide
  octaverc:

  \;

  global TMSTRUCT=0; ## Use tree output for structures.

  global TMCOLORS=["black"; "red"; "magenta"; "orange"; "green"; "blue";];

  global TMCOLIDX=6; ## number of colors

  \;

  The TMCOLORS array lists the available colors, in order, to be used by
  struct2bullet.m it will change to each color in turn as it indents the
  bulleted list. Additionally, the first color in the list is used as the
  color for the leaves of struct2tree.m output.\ 

  As an example of using these functions and of their behavior, consider the
  following sample Octave session:

  <\session|octave|default>
    <\output>
      GNU Octave, version 2.1.36 (i686-pc-linux-gnu).

      Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002 John W. Eaton.

      This is free software; see the source code for copying conditions.

      There is ABSOLUTELY NO WARRANTY; not even for MERCHANTIBILITY or

      FITNESS FOR A PARTICULAR PURPOSE.

      \;

      Report bugs to \<less\>bug-octave@bevo.che.wisc.edu\<gtr\>.

      \;
    </output>

    <input|octave\<gtr\> |<\var_expand|math>
      <with|color|blue|M.mat=<matrix|<tformat|<table|<row|<cell|1>|<cell|3>|<cell|5>>|<row|<cell|5>|<cell|8>|<cell|0>>>>>;
      \ M.l=list([1,2,3],2,3); M.branch.i=3; M.branch.j=4;>
    </var_expand>>

    <input|octave\<gtr\> |<\var_expand|math>
      <with|color|blue|tmdisp(M);> <with|color|black|## M.mat has been
      <rprime|''>switched<rprime|''>>
    </var_expand>>

    <\output>
      <tree|<with|color|red|<with|mode|math|<big|triangleup>>>|<hide_expand|switch|<with|color|yellow|<with|mode|math|formula
      style|true|<with|color|blue|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|3>>|<cell|<with|mode|math|5>>>|<row|<cell|<with|mode|math|5>>|<cell|<with|mode|math|8>>|<cell|<with|mode|math|0>>>>>>>>>|<\tuple>
        <with|color|red|mat>
      </tuple|<tmarker>>>|<\hide_expand|switch>
        <with|color|red|l>
      </hide_expand|<tuple|<tmarker>|<with|color|yellow|<\expand|enumerate-numeric>
        <item><with|mode|math|formula style|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|2>>|<cell|<with|mode|math|3>>>>>>>

        <item><with|mode|math|2>

        <item><with|mode|math|3>
      </expand>>>>|<tree|<with|color|red|branch>|<\hide_expand|switch>
        <with|color|red|i>
      </hide_expand|<tuple|<tmarker>|<with|color|yellow|<with|mode|math|3>>>>|<\hide_expand|switch>
        <with|color|red|j>
      </hide_expand|<tuple|<tmarker>|<with|color|yellow|<with|mode|math|4>>>>>>

      \;
    </output>

    <input|octave\<gtr\> |<\var_expand|math>
      <with|color|blue|TMSTRUCT=1;>
    </var_expand>>

    <input|octave\<gtr\> |<\var_expand|math>
      <with|color|blue|tmdisp(M)>
    </var_expand>>

    <\output>
      <\expand|itemize-arrow>
        <item><with|color|red|mat = ><with|mode|math|formula
        style|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|3>>|<cell|<with|mode|math|5>>>|<row|<cell|<with|mode|math|5>>|<cell|<with|mode|math|8>>|<cell|<with|mode|math|0>>>>>>>

        <item><with|color|red|l = ><\expand|enumerate-numeric>
          <item><with|mode|math|formula style|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|2>>|<cell|<with|mode|math|3>>>>>>>

          <item><with|mode|math|2>

          <item><with|mode|math|3>
        </expand>

        <item><with|color|red|branch = ><\itemize>
          <item><with|color|black|i = ><with|mode|math|3>

          <item><with|color|white|<with|color|black|j = >><with|mode|math|4>
        </itemize>
      </expand>

      \;
    </output>

    <input|octave\<gtr\> |<\var_expand|math>
      <with|color|blue|tmpolyout([4,5,6])>
    </var_expand>>

    <\output>
      <\with|mode|math>
        4\<cdot\>s<rsup|2>+5\<cdot\>s<rsup|1>+6
      </with>

      \;
    </output>

    <\input|octave\<gtr\> >
      X=0:.1:3;
    </input>

    <\input|octave\<gtr\> >
      Y=sin(X);
    </input>

    <\input|octave\<gtr\> >
      plot(X,Y);
    </input>

    <\output>
      \;

      \;

      <postscript|<tuple|<raw_data|%!PS-Adobe-2.0 EPSF-2.0\n%%Title:
      /tmp/tmplot.eps\n%%Creator: gnuplot 3.7 patchlevel 2\n%%CreationDate:
      Tue May 20 13:00:51 2003\n%%DocumentFonts: (atend)\n%%BoundingBox: 50
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
      UL\nLTb\n574 280 M\n63 0 V\n2725 0 R\n-63 0 V\n stroke\n490 280 M\n[
      [(Helvetica) 140.0 0.0 true true ( 0)]\n] -46.7 MRshow\n574 487 M\n63 0
      V\n2725 0 R\n-63 0 V\n stroke\n490 487 M\n[ [(Helvetica) 140.0 0.0 true
      true ( 0.1)]\n] -46.7 MRshow\n574 694 M\n63 0 V\n2725 0 R\n-63 0 V\n
      stroke\n490 694 M\n[ [(Helvetica) 140.0 0.0 true true ( 0.2)]\n] -46.7
      MRshow\n574 902 M\n63 0 V\n2725 0 R\n-63 0 V\n stroke\n490 902 M\n[
      [(Helvetica) 140.0 0.0 true true ( 0.3)]\n] -46.7 MRshow\n574 1109
      M\n63 0 V\n2725 0 R\n-63 0 V\n stroke\n490 1109 M\n[ [(Helvetica) 140.0
      0.0 true true ( 0.4)]\n] -46.7 MRshow\n574 1316 M\n63 0 V\n2725 0
      R\n-63 0 V\n stroke\n490 1316 M\n[ [(Helvetica) 140.0 0.0 true true (
      0.5)]\n] -46.7 MRshow\n574 1523 M\n63 0 V\n2725 0 R\n-63 0 V\n
      stroke\n490 1523 M\n[ [(Helvetica) 140.0 0.0 true true ( 0.6)]\n] -46.7
      MRshow\n574 1730 M\n63 0 V\n2725 0 R\n-63 0 V\n stroke\n490 1730 M\n[
      [(Helvetica) 140.0 0.0 true true ( 0.7)]\n] -46.7 MRshow\n574 1938
      M\n63 0 V\n2725 0 R\n-63 0 V\n stroke\n490 1938 M\n[ [(Helvetica) 140.0
      0.0 true true ( 0.8)]\n] -46.7 MRshow\n574 2145 M\n63 0 V\n2725 0
      R\n-63 0 V\n stroke\n490 2145 M\n[ [(Helvetica) 140.0 0.0 true true (
      0.9)]\n] -46.7 MRshow\n574 2352 M\n63 0 V\n2725 0 R\n-63 0 V\n
      stroke\n490 2352 M\n[ [(Helvetica) 140.0 0.0 true true ( 1)]\n] -46.7
      MRshow\n574 280 M\n0 63 V\n0 2009 R\n0 -63 V\n stroke\n574 140 M\n[
      [(Helvetica) 140.0 0.0 true true ( 0)]\n] -46.7 MCshow\n1039 280 M\n0
      63 V\n0 2009 R\n0 -63 V\n stroke\n1039 140 M\n[ [(Helvetica) 140.0 0.0
      true true ( 0.5)]\n] -46.7 MCshow\n1503 280 M\n0 63 V\n0 2009 R\n0 -63
      V\n stroke\n1503 140 M\n[ [(Helvetica) 140.0 0.0 true true ( 1)]\n]
      -46.7 MCshow\n1968 280 M\n0 63 V\n0 2009 R\n0 -63 V\n stroke\n1968 140
      M\n[ [(Helvetica) 140.0 0.0 true true ( 1.5)]\n] -46.7 MCshow\n2433 280
      M\n0 63 V\n0 2009 R\n0 -63 V\n stroke\n2433 140 M\n[ [(Helvetica) 140.0
      0.0 true true ( 2)]\n] -46.7 MCshow\n2897 280 M\n0 63 V\n0 2009 R\n0
      -63 V\n stroke\n2897 140 M\n[ [(Helvetica) 140.0 0.0 true true (
      2.5)]\n] -46.7 MCshow\n3362 280 M\n0 63 V\n0 2009 R\n0 -63 V\n
      stroke\n3362 140 M\n[ [(Helvetica) 140.0 0.0 true true ( 3)]\n] -46.7
      MCshow\n1.000 UL\nLTb\n574 280 M\n2788 0 V\n0 2072 V\n-2788 0 V\n574
      280 L\n1.000 UL\nLT0\n2711 2219 M\n[ [(Helvetica) 140.0 0.0 true true
      (line 1)]\n] -46.7 MRshow\n2795 2219 M\n399 0 V\n574 280 M\n93 207
      V\n93 205 V\n93 200 V\n93 195 V\n93 186 V\n93 177 V\n93 165 V\n92 151
      V\n93 137 V\n93 121 V\n93 103 V\n93 84 V\n93 65 V\n93 46 V\n93 25 V\n93
      4 V\n93 -16 V\n93 -37 V\n93 -57 V\n93 -77 V\n93 -95 V\n93 -114 V\n92
      -130 V\n93 -145 V\n93 -160 V\n93 -172 V\n93 -182 V\n93 -192 V\n93 -198
      V\n93 -204 V\nstroke\ngrestore\nend\nshowpage\n>|ps>||||||>

      \;
    </output>

    octave\<gtr\>\ 

    \;
  </session>

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

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|<value|toc-dots><pageref|toc-1>>

      <with|left margin|<quote|6fn>|font size|<quote|0.84>|<value|toc-dots><pageref|toc-2>>
    </associate>
  </collection>
</auxiliary>
