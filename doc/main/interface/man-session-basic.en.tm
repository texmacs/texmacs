<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creating sessions>

  A session can be started from the <apply|menu|Text|Session> menu. A session
  consists of a sequence of input and output fields and possible text between
  them. When pressing <key|<expand|key-return>> inside an input field of a
  session, the text inside the environment is evaluated and the result is
  displayed in an output field.

  When entering a command in a session, the application attempts to execute
  it. Several commands may be launched concurrently in the same document, but
  the output will only be active in the session where the cursor is and at
  the place of the cursor. Therefore, we recommend to use different buffers
  for parallel executions.

  For each type of extern application, one may choose between sharing a
  single process by different sessions, or launching a separate process for
  each different session. More precisely, when inserting a session using
  <apply|menu|Text|Session|Other>, you may specify both a ``session type''
  (Shell, Pari, Maxima, <abbr|etc.>) and a ``session name'' (the default name
  is ``default''). Sessions with different names correspond to different
  processes and sessions with the same name share a common process.

  In order to finish the process which underlies a given session, you may use
  <apply|menu|Session|Close session>. When pressing <key|<expand|key-return>>
  in the input of a non-connected system, the system will be restarted
  automatically. You may also use <apply|menu|Session|Interrupt execution> in
  order to interrupt the execution of a command. However, several
  applications do not support this feature.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
    <associate|shrinking factor|4>
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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Other>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
