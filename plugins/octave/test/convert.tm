<TeXmacs|1.99.12>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Tests on octave/convert>

  <section|num2scm>

  <subsection|int32/int42/float scalar>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      0
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (0))
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      0.0
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (0.0))
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      int32 (0)
    <|unfolded-io>
      <with|mode|math|0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (int32 (0)))
    <|unfolded-io>
      <with|mode|math|0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      int64 (0)
    <|unfolded-io>
      <with|mode|math|0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (int64 (0)))
    <|unfolded-io>
      <with|mode|math|0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      1
    <|unfolded-io>
      <with|mode|math|1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (1))
    <|unfolded-io>
      <with|mode|math|1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      -1
    <|unfolded-io>
      <with|mode|math|-1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-1))
    <|unfolded-io>
      <with|mode|math|-1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      0.1
    <|unfolded-io>
      <with|mode|math|0.1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (0.1))
    <|unfolded-io>
      <with|mode|math|0.1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      -0.1
    <|unfolded-io>
      <with|mode|math|-0.1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-0.1))
    <|unfolded-io>
      <with|mode|math|-0.1>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <subsection|complex scalar>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      3 + 42i
    <|unfolded-io>
      <with|mode|math|3+42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (3 + 42i))
    <|unfolded-io>
      <with|mode|math|3+42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      3 - 42i
    <|unfolded-io>
      <with|mode|math|3-42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (3-42i))
    <|unfolded-io>
      <with|mode|math|3-42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      42i
    <|unfolded-io>
      <with|mode|math|42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (42i))
    <|unfolded-io>
      <with|mode|math|42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      -42i
    <|unfolded-io>
      <with|mode|math|-42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-42i))
    <|unfolded-io>
      <with|mode|math|-42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <section|mat2scm>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      zeros(3, 3)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      ones(3, 3)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (mat2scm (zeros (3, 3)))
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (mat2scm (ones (3, 3)))
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      zeros(1, 1)
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      zeros(2, 2)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>>>>>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <section|str2scm>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      "Hello World"
    <|unfolded-io>
      ret = (with "color" "blue" "Hello World")

      <with|color|blue|Hello World>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <section|struct2scm>

  <\session|octave|default>
    <\input>
      octave\<gtr\>\ 
    <|input>
      M.mat=[1, 3, 5; 5, 8, 0]; \ M.l=[1,2,3]; M.branch.i=3; M.branch.j=4;
    </input>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      M
    <|unfolded-io>
      <tree|<with|color|black|<with|mode|math|<big|triangleup>>>|<with|color|blue|<with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|3>>|<cell|<with|mode|math|5>>>|<row|<cell|<with|mode|math|5>>|<cell|<with|mode|math|8>>|<cell|<with|mode|math|0.0>>>>>>>>|<with|color|blue|<with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|2>>|<cell|<with|mode|math|3>>>>>>>>|<tree|<with|color|black|branch>|<with|color|blue|<with|mode|math|3>>|<with|color|blue|<with|mode|math|4>>>>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  \;

  <tmdoc-copyright|2020|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>