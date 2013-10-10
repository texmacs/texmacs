<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Títulos estándar>

  El d.t.d. <tmdtd|header-title> provee etiqeutas para la información del
  título. Las siguientes etiquetas de alto nivel sólo pueden ser usados
  cuando son encapsuladas dentro de la etiqueta <markup|make-title>:

  <\explain|<markup|title>>
    Espeficica un título para el documento.
  </explain>

  <\explain|<markup|author>>
    Especifica uno o varios autores para el documento.
  </explain>

  <\explain|<markup|address>>
    Especifica la dirección del autor.
  </explain>

  <\explain|<markup|address-block>>
    Especifica una dirección de un autor (en caso de múltiples direcciones).
  </explain>

  <\explain|<markup|title-email>>
    Especifica la dirección de correo electrónico del autor.
  </explain>

  <\explain|<markup|title-date>>
    Especifica la fecha de creación del artículo.
  </explain>

  Los campos <markup|title> y <markup|author> usan las etiquetas
  <markup|header-title> y <markup|header-author> para especificar el título y
  la cabecera corrientes. Usted puede sobreescribir estos reusando
  <markup|header-title> y <markup|header-author> respectivamente. Las
  etiquetas anteriores también dependen de las siguientes etiquetas de bajo
  nivel para su disposición física:

  <\explain|<markup|title*>>
    Macro con un argumento que especifica la disposición física de los
    títulos.
  </explain>

  <\explain|<markup|author*>>
    Macro con un argumento que especifica la disposición física de los
    autores.
  </explain>

  <\explain|<markup|address*>>
    Macro con un argumento que especifica la disposición física de las
    direcciones.
  </explain>

  <\explain|<markup|title-email*>>
    Macro con un argumento que especifica la disposición física de las
    direcciones de correo electrónico.
  </explain>

  <\explain|<markup|title-date*>>
    Macro con un argumento que especifica la disposición física de las fechas
    de creación.
  </explain>

  El d.t.d. <tmdtd|header-title> también<abbr|> defines la etiqueta
  <markup|abstract> para resúmenes de documentos.

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