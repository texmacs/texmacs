<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Example of a <name|CoCoa> session>

  Here follows a sample session taken from
  <href|http://cocoa.dima.unige.it/tutorials>.

  <\session|cocoa5|default>
    <\output>
      \ \ \ ______ \ \ \ \ \ ______ \ \ \ \ \ ___ \ \ \ \ \ \ \ \ ______

      \ \ / ____/___ \ / ____/___ \ / \ \ \| \ \ \ \ \ \ / ____/

      \ / / \ \ / __ `/ / \ \ / __ `/ /\| \|______/___ ` \ 

      / /___/ /_/ / /___/ /_/ / ___ /_____/___/ / \ 

      `____/`____/`____/`____/_/ \ \|_\| \ \ \ /_____/ \ \ 

      Starting interactive session

      \;
    </output>

    <\unfolded-io|Cocoa5] >
      1;
    <|unfolded-io>
      \ 1

      \;
    </unfolded-io>
  </session>

  Euclid's well-known algorithm (originally for INTEGERS). In essence the
  last non-zero remainder in the sequence is the GCD of A and B:

  <\session|cocoa5|default>
    <\unfolded-io|Cocoa5] >
      <\code>
        Define EuclideanAlgm(A, B)

        \ \ While B \<less\>\<gtr\> 0 Do

        \ \ \ \ R := mod(A, B);

        \ \ \ \ A := B;

        \ \ \ \ B := R;

        \ \ EndWhile;

        \ \ Return A;

        EndDefine; -- EuclideanAlgm
      </code>
    <|unfolded-io>
      \ 
    </unfolded-io>

    <\unfolded-io|Cocoa5] >
      E := EuclideanAlgm(123456, 67890);
    <|unfolded-io>
      \ 
    </unfolded-io>

    <\unfolded-io|Cocoa5] >
      PrintLn "The GCD of 123456 and 67890 is ", E;
    <|unfolded-io>
      \ The GCD of 123456 and 67890 is 6

      \;
    </unfolded-io>
  </session>

  <tmdoc-copyright|2014|François Poulain, Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>