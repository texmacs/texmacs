<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Marcação Padrão>

  Uma variedades de marcações padrão são definidas em <tmdtd|std-markup>.
  Todas as etiquetas de conteúdo textual a seguir aceitam um argumento. A
  maioria pode ser encontrada no menu <apply|menu|Insert|Content tag>.

  <\description>
    <expand|item*|<markup|strong>>Indica uma região <strong|importante> do
    texto. Você pode introduzir esta etiqueta com <apply|menu|Insert|Content
    tag|Strong>.

    <expand|item*|<markup|em>>Emfatiza um trecho do texto como em ``a coisa
    <em|verdadeira>''. Esta etiqueta corresponde à entrada de menu
    <apply|menu|Insert|Content tag|Emphasize>.

    <expand|item*|<markup|dfn>>Para definições como ``um <dfn|gnu> é um bicho
    cabeludo''. Esta etiqueta corresponde a <apply|menu|Insert|Content
    tag|Definition>.

    <expand|item*|<markup|samp>>Uma seqüência de caracteres literais como
    <samp|ae> a ligatura æ. Você pode inserir esta etiqueta com
    <apply|menu|Insert|Content tag|Sample>.

    <expand|item*|<markup|name>>O nome de alguma coisa coisa ou conceinto,
    como o sistema operacional <name|Linux>. Esta etiqueta pode ser inserida
    com <apply|menu|Insert|Content tag|Name>.

    <expand|item*|<markup|person>>O nome de uma pessoa como <name|Joris>.
    Esta etiqueta corresponde a <apply|menu|Insert|Content tag|Person>.

    <expand|item*|<markup|cite*>>Uma citação bibliográfica como um livro ou
    uma revista, por exemplo: <expand|cite*|Moby Dick>, de Melville. Esta
    etiqueta, que encontra-se em <apply|menu|Insert|Content tag|Cite>, não deve
    ser confundida com <markup|cite>. A última também é usada para citações,
    porém o argumento desta refere-se a uma entrada em um banco de dados de
    referências bibliográficas.

    <expand|item*|<markup|abbr>>Uma abreviação. Por exemplo, eu trabalho no
    <abbr|C.N.R.S.> Um abreviação é criada com <apply|menu|Insert|Content
    tag|Abbreviation> ou com o atalho de teclado <key|text a>.

    <expand|item*|<markup|acronym>>Um acrônimo é uma abreviação formada com a
    primeira letra de cada palavra de uma frase, como <acronym|HTML> ou
    <acronym|IBM>. Em particular, as letras não separadas por pontos. Você
    pode inserir um acrônimo com <apply|menu|Insert|Content tag|Acronym>.

    <expand|item*|<markup|verbatim>>Texto literal como a saída de um programa
    de computador. Por exemplo: o programa disse: <verbatim|hello>. Você pode
    digitar texto literal com <apply|menu|Insert|Content tag|Verbatim>. Esta
    etiqueta também pode ser usada como um ambiente para um trecho com vários
    parágrafos.

    <expand|item*|<markup|kbd>>Texto que deveser digitado no teclado. Por
    exemplo: por favor tecle <kbd|return>. Esta etiqueta corresponde à
    entrada do menu <apply|menu|Insert|Content tag|Keyboard>.

    <expand|item*|<markup|code*>>Código fonte de um programa de computador,
    como em ``<expand|code*|cout \<less\>\<less\> 1+1;> imprime
    <verbatim|2>''. Esta etiqueta é inserida com <apply|menu|Insert|Content
    tag|Code>. Para trechos mais longos de código, você deve usar o ambiente
    <markup|code>.

    <expand|item*|<markup|var>>Variáveis em um programa de computador, como
    <verbatim|cp <var|src-file> <var|dest-file>>. Esta etiqueta corresponde à
    entrada de menu <apply|menu|Insert|Content tag|Variable>.

    <expand|item*|<markup|math>>Esta é a etiqueta que será usada no futuro
    para matemática dentro de texto normal. Por exemplo: <math|sin<rsup|2>
    x+cos<rsup|2> x=1> é bem conhecida.

    <expand|item*|<markup|op>>Esta é a etiqueta que pode ser usada dentro de
    expressões matemáticas para indicar que um operador deve ser considerado
    por si só, sem qualquer argumento. Por exemplo: a operação <math|<op|+>>
    é uma função de <with|mode|math|\<bbb-R\><rsup|2>> em
    <with|mode|math|\<bbb-R\>>. Esta etiqueta pode tornar-se obsoleta.

    <expand|item*|<markup|tt>>É uma etiqueta de marcação física que existe
    para compatibilidade com <name|HTML>, mas não recomendamos seu uso. use.
  </description>

  A seguir listamos os ambientes padrão:

  <\description>
    <expand|item*|<markup|verbatim>>Descrito acima.

    <expand|item*|<markup|code>>Similar a <markup|code*>, mas para trechos de
    código com várias linhas.

    <expand|item*|<markup|quote>>Ambiente para citações curtas (um
    parágrafo).

    <expand|item*|<markup|quotation>>Ambiente para citações longas (vários
    parágrafos).

    <expand|item*|<markup|verse>>Ambiente para poesia.

    <expand|item*|<markup|center>>Esta é uma etiqueta de marcação física para
    centralizar uma ou várias linhas de texto. Existe para compatibilidade
    com <name|HTML>, mas não recomendamos seu uso.
  </description>

  Alguns ambientes padrão para texto tabular são

  <\description>
    <expand|item*|<markup|tabular*>>Tabelas centradas.

    <expand|item*|<markup|block>>Tabelas alinhadas à esquerda com uma moldura
    padrão de <verbatim|1ln> de largura.

    <expand|item*|<markup|block*>>Tabelas centradas com uma moldura padrão de
    <verbatim|1ln> de largura.
  </description>

  As etiquetas sequintes não aceitam argumentos:

  <\description>
    <expand|item*|<markup|TeXmacs>>O logotipo do <TeXmacs>.

    <expand|item*|<markup|TeX>>O logotipo <TeX>.

    <expand|item*|<markup|LaTeX>>O logotipo do <LaTeX>.

    <expand|item*|<markup|hflush>>Usada por desenvolvedores para empurrar o
    texto para a direita na definição de ambientes.

    <expand|item*|<markup|hrule>>Uma linha horizontal como a que aparece
    abaixo:

    <value|hrule>
  </description>

  Todas as etiquetas abaixo aceitam um ou mais argumentos.

  <\description>
    <expand|item*|<markup|overline>>Para <overline|grifado acima>, que pode
    se extender por várias linhas.

    <expand|item*|<markup|underline>>Para <underline|texto sublinhado>, que
    pode se extender por várias linhas.

    <expand|item*|<markup|fold>>Macro com dois argumentos. O primeiro é
    exibido e o segundo ignorado: este macro corresponde a apresentação
    ``dobrada'' de um trecho do documento associado com um breve título ou
    resumo. O segundo argumento pode ser mostrado com
    <apply|menu|Insert|Switch|Unfold>.

    <expand|item*|<markup|switch>>Macro com dois argumentos <var|x> e
    <var|y>, onde <var|y> é um conjunto de representações possíveis para a
    escolha e <var|x> a representação atual. As teclas de função <key|F9>,
    <key|F10>, <key|F11> e <key|F12> podem ser usadas para alternar entre as
    diferentes representações.

    <expand|item*|<markup|phantom>>Função com um argumento <var|x>. Esta
    etiqueta ocupa tanto espaço quanto o argumento <var|x> ocuparia quando
    tipografado, porém <var|x> não é exibido. Por exemplo, o texto
    ``fantasma'' como um argumento para <markup|phantom> fornece
    ``<apply|phantom|fantasma>''.

    <expand|item*|<markup|set-header>>Função com um argumento para mudar de
    forma permanente o cabeçalho. Perceba que algumas etiquetas no arquivo de
    estilo, como etiquetas de sessão, podem sobrepor-se a esta definição
    manual.

    <expand|item*|<markup|set-footer>>Função com um argumento para alterar de
    forma permanente o rodapé.
  </description>

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-50|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-51|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-52|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-53|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-54|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-55|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-56|<tuple|<uninit>|?>>
    <associate|idx-46|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-57|<tuple|<uninit>|?>>
    <associate|idx-47|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-48|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-49|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-markup>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|strong>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Forte>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Destacar>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dfn>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Definição>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|samp>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Amostra>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|name>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Nome>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|person>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Pessoa>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite*>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Citar>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|abbr>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Abreviação>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|acronym>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Acrônimo>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Verbatim>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Código>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|var>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Tag de conteúdo>|<with|font
      family|<quote|ss>|Variável>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|math>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|op>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tt>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quote>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quotation>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verse>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|center>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular*>>|<pageref|idx-41>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-42>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block*>>|<pageref|idx-43>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeXmacs>>|<pageref|idx-44>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeX>>|<pageref|idx-45>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|LaTeX>>|<pageref|idx-46>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hflush>>|<pageref|idx-47>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hrule>>|<pageref|idx-48>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|overline>>|<pageref|idx-49>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|underline>>|<pageref|idx-50>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|fold>>|<pageref|idx-51>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Switch>|<with|font
      family|<quote|ss>|Unfold>>|<pageref|idx-52>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|switch>>|<pageref|idx-53>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-54>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-55>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-header>>|<pageref|idx-56>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-footer>>|<pageref|idx-57>>
    </associate>
  </collection>
</auxiliary>
