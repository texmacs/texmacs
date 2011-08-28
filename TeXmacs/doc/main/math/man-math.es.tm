<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Fórmulas matemáticas>

  A fin de teclear fórmulas matemáticas, debe primer entrar en ``modo
  matemático'' presionando la tecla <key|$>- o mediante la inserción de una
  ecuación (usando <apply|menu|Insert|Mathematics|Equation>). En modo
  matemático, puede tener comandos específicos y combinaciones de teclas para
  teclaer símbolos matemáticos y fórmulas. Por ejemplo el prefijo <prefix|M-A->
  puede ser usado a fin de ingresar símbolos griegos (recuerdo que <prefix|M-A->
  es equivalente a <prefix|math:greek>, <key|escape escape escape> o <prefix|A-C->).

  El editor favorece teclear matemáticas con un cierto significado. Esta
  característica será desarrollada aún más en las versiones futuras, es útil
  cuando se comunica con un paquete de álgebra computacional. En este
  momento, usted debería por ejemplo explícitamente teclear la multiplicación
  <key|*> entre símbolos <with|mode|math|a> y <with|mode|math|b>. Por
  defecto, teclear <key|a b> producirá <with|mode|math|ab> y no
  <with|mode|math|a b>.

  <\traverse>
    <apply|branch|Principales constructos matemáticos
    |keyboard/man-main.es.tm>

    <apply|branch|Símbolos matemáticos|keyboard/man-symbols.es.tm>

    <apply|branch|Operadores grandes|keyboard/man-big.es.tm>

    <apply|branch|Delimitadores grandes|keyboard/man-large.es.tm>

    <apply|branch|Acentos anchos|keyboard/man-wide.es.tm>
  </traverse>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

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
    <associate|language|spanish>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Matemáticas>|<with|font
      family|<quote|ss>|Ecuación>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
