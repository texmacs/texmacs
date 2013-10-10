<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambiente matemáticos>

  A <abbr|d.t.d.> <tmdtd|env-math> especifica os ambiente matemático que
  podem ser usados no modo texto. Em outras palavra, os ambientes devem ser
  usados no modo texto, porém seus corpos contém fórmulas matemáticas ou
  tabelas de fórmulas matemáticas.

  <\explain|<markup|equation>>
    Uma equação numerada.
  </explain>

  <\explain|<markup|equation*>>
    Uma equação sem número.
  </explain>

  <\explain|<markup|eqnarray>>
    Uma matriz de equações numeradas (não deve ser usada ainda).
  </explain>

  <\explain|<markup|eqnarray*>>
    Uma matriz de equações não numeradas.
  </explain>

  Dentro do ambiente <markup|eqnarray*>, você pode usar a etiqueta
  <markup|eq-number> para numerar a equação

  <\warning>
    A numeração de equações dentro de tabelas ainda não é exatamente como
    deveria. Em particular, a etiqueta <markup|eqnarray> é equivalente a
    <markup|eqnarray*> no momento. Mais tarde, quando a etiqueta
    <markup|eqnarray> for implementada corretamente, você também terá uma
    etiqueta <markup|no-number> para suprimir a numeração de uma equação, e
    um arquivo de estilo para numerar as equações do lado esquerdo.
  </warning>

  <\warning>
    Não há ainda uma opção para numerar as equações do lado esquerdo da
    página. Ainda assim, você pode manualmente usar a etiqueta
    <markup|leq-number> para obter o mesmo efeito. Também existe a etiqueta
    <markup|next-number> que mostra o próximo número e incrementa o contador
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