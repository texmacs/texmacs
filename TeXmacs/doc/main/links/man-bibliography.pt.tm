<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Compilando uma bibliografia>

  Atualmente, o <apply|TeXmacs> usa o <verbatim|bibtex> para compilar
  bibliografias. O mecanismo para compilar uma bibliografia automaticamente é
  o seguinte:

  <\itemize>
    <item>Escreva um arquivo <verbatim|.bib> com todas as suas referências
    bibliográficas. Este arquivo deve ser formatado como uma bibliografia
    padrão para o <apply|LaTeX>.

    <item>Use <apply|menu|Insert|Link|Citation> e
    <apply|menu|Insert|Link|Invisible citation> para inserir citações, que
    correspondem a entradas no seu arquivo <verbatim|.bib>.

    <item>No lugar em a sua bibliografia deve ser compilada, clique em
    <apply|menu|Insert|Automatic|Bibliography>. Você deve responder à pergunta
    com um estilo do <verbatim|bibtex> (como <verbatim|plain>,
    <verbatim|alpha>, <verbatim|abbrv>, etc.) e seu arquivo <verbatim|.bib>.

    <item>Use <apply|menu|Document|Update|Bibliography> para compilar sua
    bibliografia.
  </itemize>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

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
    <associate|language|portuguese>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Citação>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Citação
      invisível>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Automático>|<with|font
      family|<quote|ss>|Bibliografia>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Atualizar>|<with|font
      family|<quote|ss>|Bibliografia>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
