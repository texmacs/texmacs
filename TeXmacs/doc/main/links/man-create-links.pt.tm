<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Criando rótulos, ligações e referencias>

  Você pode criar um novo rótulo, que é inicializado desativado, usando
  \ <shortcut|(make-label)> ou <apply|menu|Insert|Link|Label> e uma referência
  para este rótulo usando <shortcut|(make 'reference)> ou
  <apply|menu|Insert|Link|Reference>. Tome cuidado e insira o rótulo em um
  ponto no qual seu número será correto. Quando rotular seções, por exemplo,
  a posição recomendada é logo após o nome da seção. Quando você rotular
  equações, o local recomendado é dentro da equação, em seu início.

  É possível criar hiper ligações para outros documentos usando
  <key|inactive \<gtr\>> ou <apply|menu|Insert|Link|Hyperlink>. O primeiro
  campo da hiper ligação é o texto associado, que é exibido em azul quando a
  hiper ligação está ativa. O segundo campo contém o nome de um documento,
  que pode inclusive estar na rede. Como é usual para hiper ligações, uma
  ligação da forma <verbatim|#<with|font shape|italic|label>> aponta para o
  mesmo documento e uma ligação da forma <verbatim|<with|font
  shape|italic|url>#<with|font shape|italic|label>> aponta para um rótulo no
  documento localizado na <verbatim|<with|font shape|italic|url>>.

  Da mesma forma, uma ação pode ser associada a um trecho de texto ou gráfico
  usando <key|inactive *> ou <apply|menu|Insert|Link|Action>. O segundo
  campo agora contém um script Guile/Scheme, que é executado sempre que você
  clica duplamente naquele texto, desde que o mesmo esteja ativo. Por motivos
  de segurança, a execução destes scripts não é automática. O comportamento
  padrão é perguntar para você se você aceita a execução; isto pode ser
  alterado em \ <apply|menu|Options|Security>. Note que o comando
  Guile/Scheme

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  executa <verbatim|shell-command> como um comando do seu shell.

  Finalmente, você pode incluir outros documentos diretamente dentro de um
  dado documento usando <key|inactive i> ou
  <apply|menu|Insert|Link|Include>. Isto permite, por exemplo, que seja
  incluída uma listagem de um programa no seu texto, de forma que alterações
  no programa sejam automaticamente refletidas no texto.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
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
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Etiqueta>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Referência>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Hiperlink>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Ação>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Opções>|<with|font
      family|<quote|ss>|Segurança>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Incluir>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
