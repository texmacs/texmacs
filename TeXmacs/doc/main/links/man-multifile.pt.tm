<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Livros e documentos com múltiplos arquivos>

  Quando um documento ficar muito grande, você pode querer dividi-lo em
  partes menores. Isto faz com que as partes menores sejam mais facilmente
  reutilizáveis em outros trabalhos e ainda melhora o desempenho do editor.
  Um arquivo completo pode ser inserido em outro usando
  <apply|menu|Insert|Link|Include>. Para acelerar o processamento dos
  arquivos incluídos, são guardadas cópias pré-processadas destes arquivos.
  Para atualizar todos os documentos inclusos, você deve usar
  <apply|menu|Tools|Update|Inclusions>.

  Ao escrever um livro, normalmente escrevemos cada capítulo em arquivos
  individuais <verbatim|c1.tm>, <verbatim|c2.tm> até <verbatim|cn.tm>. Em
  seguida, criamos um arquivo <verbatim|book.tm> para o livro completo, no
  qual os arquivos <verbatim|c1.tm>, <verbatim|c2.tm> até <verbatim|cn.tm>
  são inseridos com o mecanismo descrito acima. O sumário, bibliografia, etc.
  são, em geral, criadas no arquivo <verbatim|book.tm>.

  Para ver corretamente as referências cruzadas aos outros capítulos durante
  a edição de um capítulo <verbatim|ci.tm> em particular, devemos especificar
  <verbatim|book.tm> como o ``arquivo mestre'' para os arquivos
  <verbatim|c1.tm> a <verbatim|cn.tm> usando
  <apply|menu|Document|Master|Attach>. Na implementação corrente, os números
  dos capítulos não são corrigidos automaticamente com este esquema, e você
  deve atualizar manualmente a variável do ambiente <verbatim|chapternr> no
  início de cada capítulo para obter a numeração correta durante a edição.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Incluir>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ferramentas>|<with|font
      family|<quote|ss>|Atualizar>|<with|font
      family|<quote|ss>|Inclusões>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Mestre>|<with|font
      family|<quote|ss>|Vincular>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
