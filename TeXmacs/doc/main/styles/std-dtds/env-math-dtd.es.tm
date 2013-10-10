<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Entornos matemáticos>

  El d.t.d. <tmdtd|env-math> espeficica cuales entornos matemáticos pueden
  ser usados dentro del modo de texto. En otras palabras, los entornos
  deberían ser usados detro del mode de texto, pero sus cuerpos pueden
  contener fórmulas matemáticas o tablas de fórmulas matemáticas.

  <\explain|<markup|equation>>
    Una ecuación numerada.
  </explain>

  <\explain|<markup|equation*>>
    Una ecuación no numerada.
  </explain>

  <\explain|<markup|eqnarray>>
    \ Un arreglo de ecuaciones numeradas. (no debe sería usarse aún).
  </explain>

  <\explain|<markup|eqnarray*>>
    Un arreglo de ecuaciones no numeradas.
  </explain>

  Dentro del entorno <markup|eqnarray*>, usted puede usar la etiqueta
  <markup|eq-number> a fin de numerar una ecuación.

  <\warning>
    La numeración de ecuaciones dentro de tablas no es aún como debería ser.
    En particular, la etiqueta <markup|eqnarray> es equivalente a
    <markup|eqnarray*> en el momento. Después, cuando la etiqueta
    <markup|eqnarray> sea implementada correctamente, usted también dispondrá
    de una etiqueta <markup|no-number> a fin de suprimir el número de una
    ecuación y un paquete de estilo para numerar ecuaciones al lado
    izquierdo.
  </warning>

  <\warning>
    No hay opción disponible para numerar ecuaciones a al lado izquierdo aún.
    Sin embargo, puede usar la etiqueta manual <markup|leq-number> para esto.
    También tiene la etiqueta <markup|next-number> el cual directamente
    muestra el siguiente número e incremente el contador de ecuaciones.
  </warning>

  <\warning>
    No alentamos el uso de los entornos AMS-<TeX>, <verbatim|align>,
    <verbatim|gather> y <verbatim|split>. No obstante, están disponibles bajo
    los nombres <markup|align>, <markup|gather>, <markup|eqsplit> junto con
    sus variantes <markup|align*>, <markup|gather*> and <markup|eqsplit*>. En
    el futuro, planeamos proveer entornos más poderosos.
  </warning>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|spanish>
  </collection>
</initial>