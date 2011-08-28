<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambiente matemáticos>

  A <abbr|d.t.d.> <tmdtd|env-math> especifica os ambiente matemático que
  podem ser usados no modo texto. Em outras palavra, os ambientes devem ser
  usados no modo texto, porém seus corpos contém fórmulas matemáticas ou
  tabelas de fórmulas matemáticas.

  <\description>
    <expand|item*|<markup|equation>>Uma equação numerada.

    <expand|item*|<markup|equation*>>Uma equação sem número.

    <expand|item*|<markup|eqnarray>>Uma matriz de equações numeradas (não
    deve ser usada ainda).

    <expand|item*|<markup|eqnarray*>>Uma matriz de equações não numeradas.
  </description>

  Dentro do ambiente <markup|eqnarray*>, você pode usar a etiqueta
  <markup|eqnumber> para numerar a equação

  <\warning>
    A numeração de equações dentro de tabelas ainda não é exatamente como
    deveria. Em particular, a etiqueta <markup|eqnarray> é equivalente a
    <markup|eqnarray*> no momento. Mais tarde, quando a etiqueta
    <markup|eqnarray> for implementada corretamente, você também terá uma
    etiqueta <markup|nonumber> para suprimir a numeração de uma equação, e um
    arquivo de estilo para numerar as equações do lado esquerdo.
  </warning>

  <\warning>
    Não há ainda uma opção para numerar as equações do lado esquerdo da
    página. Ainda assim, você pode manualmente usar a etiqueta
    <markup|leqnumber> para obter o mesmo efeito. Também existe a etiqueta
    <markup|nextnumber> que mostra o próximo número e incrementa o contador
    de equações.
  </warning>

  <\warning>
    Nós não encorajamos o uso dos ambientes do AMS-<TeX> <verbatim|align>,
    <verbatim|gather> e <verbatim|split>. Ainda assim, eles estão disponíveis
    sob os nomes de <markup|align>, <markup|gather>, <markup|eqsplit>,
    juntamente com suas variantes <markup|align*>, <markup|gather*> e
    <markup|eqsplit*>. Nós planejamos fornecer no futuro ambientes mais
    poderosos.
  </warning>

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
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|3|?>>
    <associate|idx-15|<tuple|3|?>>
    <associate|idx-16|<tuple|3|?>>
    <associate|idx-17|<tuple|3|?>>
    <associate|idx-18|<tuple|3|?>>
    <associate|idx-19|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-math>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnumber>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nonumber>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|leqnumber>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nextnumber>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align*>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather*>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit*>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
