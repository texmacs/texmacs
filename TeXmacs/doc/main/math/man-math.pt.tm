<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formulas matemáticas>

  Para digitar fórmulas matemáticas, é necessário que você entre no ``modo
  matemático'' digitando a tecla <key|$> ou inserindo uma equação (usando
  <apply|menu|Insert|Mathematics|Equation>). No modo matemático, você tem
  acesso a comando específicos e combinações de teclas para digitar símbolos
  matemáticos e fórmulas. Por exemplo, o prefixo <prefix|M-A-> pode ser usado para
  inserir símbolos gregos, (lembre-se que <prefix|M-A-> é equivalente a <prefix|math:greek>,
  <key|escape escape escape> ou
  <prefix|A-C->).

  Este editor privilegia a digitação de fórmulas matemáticas que façam
  sentido matemático. Esta característica, que será mais desenvolvida em
  versões posteriores, é útil também para a comunicação com pacotes de
  álgebra computacional. No presente, você deve, por exemplo, digitar
  explicitamente o símbolo de multiplicação entre os símbolos
  <with|mode|math|a> e <with|mode|math|b>. Digitar <key|a b> produz
  <with|mode|math|mode|text|ab> e não <with|mode|math|a*b>.

  <\traverse>
    <apply|branch|Principais ferramentas matemáticas|keyboard/man-main.pt.tm>

    <apply|branch|Símbolos matemáticos|keyboard/man-symbols.pt.tm>

    <apply|branch|Operadores grandes|keyboard/man-big.pt.tm>

    <apply|branch|Delimitadores grandes|keyboard/man-large.pt.tm>

    <apply|branch|Acentos largos|keyboard/man-wide.pt.tm>
  </traverse>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Matemáticos>|<with|font
      family|<quote|ss>|Equação>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
