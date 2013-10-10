<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Marcação Padrão>

  Uma variedades de marcações padrão são definidas em <tmdtd|std-markup>.
  Todas as etiquetas de conteúdo textual a seguir aceitam um argumento. A
  maioria pode ser encontrada no menu <menu|Insert|Content tag>.

  <\explain|<markup|strong>>
    Indica uma região <strong|importante> do texto. Você pode introduzir esta
    etiqueta com <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<markup|em>>
    Emfatiza um trecho do texto como em ``a coisa <em|verdadeira>''. Esta
    etiqueta corresponde à entrada de menu <menu|Insert|Content
    tag|Emphasize>.
  </explain>

  <\explain|<markup|dfn>>
    Para definições como ``um <dfn|gnu> é um bicho cabeludo''. Esta etiqueta
    corresponde a <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<markup|samp>>
    Uma seqüência de caracteres literais como <samp|ae> a ligatura æ. Você
    pode inserir esta etiqueta com <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<markup|name>>
    O nome de alguma coisa coisa ou conceinto, como o sistema operacional
    <name|Linux>. Esta etiqueta pode ser inserida com <menu|Insert|Content
    tag|Name>.
  </explain>

  <\explain|<markup|person>>
    O nome de uma pessoa como <name|Joris>. Esta etiqueta corresponde a
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<markup|cite*>>
    Uma citação bibliográfica como um livro ou uma revista, por exemplo:
    <cite*|Moby Dick>, de Melville. Esta etiqueta, que encontra-se em
    <menu|Insert|Content tag|Cite>, não deve ser confundida com
    <markup|cite>. A última também é usada para citações, porém o argumento
    desta refere-se a uma entrada em um banco de dados de referências
    bibliográficas.
  </explain>

  <\explain|<markup|abbr>>
    Uma abreviação. Por exemplo, eu trabalho no <abbr|C.N.R.S.> Um abreviação
    é criada com <menu|Insert|Content tag|Abbreviation> ou com o atalho de
    teclado <key|text a>.
  </explain>

  <\explain|<markup|acronym>>
    Um acrônimo é uma abreviação formada com a primeira letra de cada palavra
    de uma frase, como <acronym|HTML> ou <acronym|IBM>. Em particular, as
    letras não separadas por pontos. Você pode inserir um acrônimo com
    <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<markup|verbatim>>
    Texto literal como a saída de um programa de computador. Por exemplo: o
    programa disse: <verbatim|hello>. Você pode digitar texto literal com
    <menu|Insert|Content tag|Verbatim>. Esta etiqueta também pode ser usada
    como um ambiente para um trecho com vários parágrafos.
  </explain>

  <\explain|<markup|kbd>>
    Texto que deveser digitado no teclado. Por exemplo: por favor tecle
    <kbd|return>. Esta etiqueta corresponde à entrada do menu
    <menu|Insert|Content tag|Keyboard>.
  </explain>

  <\explain|<markup|code*>>
    Código fonte de um programa de computador, como em ``<code*|cout
    \<less\>\<less\> 1+1;> imprime <verbatim|2>''. Esta etiqueta é inserida
    com <menu|Insert|Content tag|Code>. Para trechos mais longos de código,
    você deve usar o ambiente <markup|code>.
  </explain>

  <\explain|<markup|var>>
    Variáveis em um programa de computador, como <verbatim|cp <var|src-file>
    <var|dest-file>>. Esta etiqueta corresponde à entrada de menu
    <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<markup|math>>
    Esta é a etiqueta que será usada no futuro para matemática dentro de
    texto normal. Por exemplo: <math|sin<rsup|2> x+cos<rsup|2> x=1> é bem
    conhecida.
  </explain>

  <\explain|<markup|op>>
    Esta é a etiqueta que pode ser usada dentro de expressões matemáticas
    para indicar que um operador deve ser considerado por si só, sem qualquer
    argumento. Por exemplo: a operação <math|<op|+>> é uma função de
    <math|\<bbb-R\><rsup|2>> em <math|\<bbb-R\>>. Esta etiqueta pode
    tornar-se obsoleta.
  </explain>

  <\explain|<markup|tt>>
    É uma etiqueta de marcação física que existe para compatibilidade com
    <name|HTML>, mas não recomendamos seu uso. use.
  </explain>

  A seguir listamos os ambientes padrão:

  <\explain|<markup|verbatim>>
    Descrito acima.
  </explain>

  <\explain|<markup|code>>
    Similar a <markup|code*>, mas para trechos de código com várias linhas.
  </explain>

  <\explain|<markup|quote>>
    Ambiente para citações curtas (um parágrafo).
  </explain>

  <\explain|<markup|quotation>>
    Ambiente para citações longas (vários parágrafos).
  </explain>

  <\explain|<markup|verse>>
    Ambiente para poesia.
  </explain>

  <\explain|<markup|center>>
    Esta é uma etiqueta de marcação física para centralizar uma ou várias
    linhas de texto. Existe para compatibilidade com <name|HTML>, mas não
    recomendamos seu uso.
  </explain>

  Alguns ambientes padrão para texto tabular são

  <\explain|<markup|tabular*>>
    Tabelas centradas.
  </explain>

  <\explain|<markup|block>>
    Tabelas alinhadas à esquerda com uma moldura padrão de <verbatim|1ln> de
    largura.
  </explain>

  <\explain|<markup|block*>>
    Tabelas centradas com uma moldura padrão de <verbatim|1ln> de largura.
  </explain>

  As etiquetas sequintes não aceitam argumentos:

  <\explain|<markup|TeXmacs>>
    O logotipo do <TeXmacs>.
  </explain>

  <\explain|<markup|TeX>>
    O logotipo <TeX>.
  </explain>

  <\explain|<markup|LaTeX>>
    O logotipo do <LaTeX>.
  </explain>

  <\explain|<markup|hflush>>
    Usada por desenvolvedores para empurrar o texto para a direita na
    definição de ambientes.
  </explain>

  <\explain|<markup|hrule>>
    Uma linha horizontal como a que aparece abaixo:

    <hrule>
  </explain>

  Todas as etiquetas abaixo aceitam um ou mais argumentos.

  <\explain|<markup|overline>>
    Para <overline|grifado acima>, que pode se extender por várias linhas.
  </explain>

  <\explain|<markup|underline>>
    Para <underline|texto sublinhado>, que pode se extender por várias
    linhas.
  </explain>

  <\explain|<markup|fold>>
    Macro com dois argumentos. O primeiro é exibido e o segundo ignorado:
    este macro corresponde a apresentação ``dobrada'' de um trecho do
    documento associado com um breve título ou resumo. O segundo argumento
    pode ser mostrado com <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<markup|switch>>
    Macro com dois argumentos <var|x> e <var|y>, onde <var|y> é um conjunto
    de representações possíveis para a escolha e <var|x> a representação
    atual. As teclas de função <key|F9>, <key|F10>, <key|F11> e <key|F12>
    podem ser usadas para alternar entre as diferentes representações.
  </explain>

  <\explain|<markup|phantom>>
    Função com um argumento <var|x>. Esta etiqueta ocupa tanto espaço quanto
    o argumento <var|x> ocuparia quando tipografado, porém <var|x> não é
    exibido. Por exemplo, o texto ``fantasma'' como um argumento para
    <markup|phantom> fornece ``<phantom|fantasma>''.
  </explain>

  <\explain|<markup|set-header>>
    Função com um argumento para mudar de forma permanente o cabeçalho.
    Perceba que algumas etiquetas no arquivo de estilo, como etiquetas de
    sessão, podem sobrepor-se a esta definição manual.
  </explain>

  <\explain|<markup|set-footer>>
    Função com um argumento para alterar de forma permanente o rodapé.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
  </collection>
</initial>