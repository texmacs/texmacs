<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Etiquetamiento especial para programas y sesiones>

  El <tmdtd|program> <abbr|d.t.d.> mainly provides the following environments
  for computer algebra sessions:

  <\explain|<markup|session>>
    Macro con tres argumentoss: el lenguaje de algebra computacional, el
    nombre de la sesión y el cuerpo de la sesión en sí mismo.
  </explain>

  <\explain|<markup|input>>
    Macro con dos argumentos: un <em|prompt> y la entrada input en sí misma.
  </explain>

  <\explain|<markup|output>>
    Macro con el cuerpo de la salida como su argumento.
  </explain>

  De hecho, estos entornos son basados en entornos de la forma
  <markup|<em|lan>-session>, <markup|<em|lan>-input> y
  <markup|<em|lan>-output> para cada lenguaje individual <verbatim|<em|lan>>.

  El <abbr|d.t.d.> <tmdtd|program> también probee algún ediquetado para el
  esquema de los programas de comptuadora. Sin embargo, esas etiquetas deben
  ser considerada muy inestables, puesto que pensamos reemplazarlas por un
  conjunto de etiquetas más detalladas:

  <\explain|<markup|algorithm>>
    Macro con dos argumentos: el nombre del algoritmo y el algoritmo en sí
    mismo, junto con su posible especificación.
  </explain>

  <\explain|<markup|body>>
    El cuerpo real del algoritmo.
  </explain>

  <\explain|<markup|indent>>
    Para identar una parte de un algoritmo.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|El permiso está garantizado para copiar, distribuir y/o
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