<TeXmacs|1.0.7.11>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\tmdoc-title>
    Example <name|Qcl> session
  </tmdoc-title>

  Here follows an example <name|Qcl> session, which was started using
  <menu|Insert|Session|Qcl>.

  <\session|qcl|default>
    <\output>
      QCL Quantum Computation Language (32 qubits, seed 1051277574)

      [0/32] <math|1<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|0>|\<rangle\>>>>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      qureg a[1];
    </input>

    <\unfolded-io|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      Rot(pi/4,a);
    <|unfolded-io>
      [1/32] <math|0.92388<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|0>|\<rangle\>>>-0.38268<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|1>|\<rangle\>>>>
    </unfolded-io>

    <\unfolded-io|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      Mix(a);
    <|unfolded-io>
      [1/32] <math|0.38268<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|0>|\<rangle\>>>+0.92388<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|1>|\<rangle\>>>>
    </unfolded-io>

    <\unfolded-io|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      dump;
    <|unfolded-io>
      <\with|color|blue>
        <\verbatim>
          STATE: 1 / 32 qubits allocated, 31 / 32 qubits free
        </verbatim>
      </with>

      <\math>
        0.38268<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|0>|\<rangle\>>>+0.92388<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|1>|\<rangle\>>>
      </math>
    </unfolded-io>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      include "shor.qcl";
    </input>

    <\unfolded-io|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      operator dft(qureg q) { const n=#q; int i; int j;for i=1 to n { for j=1
      to i-1 { if q[n-1] and q[n-j] {Phase(pi/2^(i-j));}} H(q[n-1]);}
      flip(q); }
    <|unfolded-io>
      <\with|color|red>
        <\verbatim>
          at "operator dft(qureg q) { c ...":
        </verbatim>
      </with>

      <\with|color|red>
        <\verbatim>
          illegal scope: Global symbol dft already defined
        </verbatim>
      </with>
    </unfolded-io>

    <\unfolded-io|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      dft(a);
    <|unfolded-io>
      [1/32] <math|0.70711<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|0>|\<rangle\>>>+0.70711<hspace|0.25spc><with|color|magenta|<around|\||<with|math-font-family|rm|1>|\<rangle\>>>>
    </unfolded-io>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      \;
    </input>
  </session>

  <tmdoc-copyright|2003|Chu-Ching Huang>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>