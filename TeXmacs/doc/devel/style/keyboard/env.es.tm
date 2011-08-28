<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macros, funciones y variables de entorno >

  Las principales combinaciones de teclas que usted debe saber para escribir
  archivos de estilo son las siguientes:

  <\description>
    <expand|item*|<key|M-=>>crea una nueva asignación. El primer
    argumento es el nombre de un nuevo comando y el segundo una expresión.

    <expand|item*|<key|M-w>>permite cambiar localmente una o más
    variables de entorno. Las sentencias <with|font family|tt|with> son de la
    forma <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>,
    donde los <with|mode|math|x<rsub|i>> son los nombres de las variables,
    los <with|mode|math|a<rsub|i>> son sus valores locales, y
    <with|mode|math|b> es el texto sobre el que se aplica el entorno local.

    <expand|item*|<key|M-m>>crea un macro. Los argumentos se
    insertan utilizando la tecla <key|tab>.

    <expand|item*|<key|M-f>>crea una función. Los argumentos se
    insertan utilizando la tecla <key|tab>.

    <expand|item*|<key|inactive #>>obtiene el valor de un argumento del
    macro.

    <expand|item*|<key|inactive v>>obtiene el valor de una variable de
    entorno.

    <expand|item*|<key|inactive e>>expande el macro con cero o más
    argumentos.

    <expand|item*|<key|inactive a>>aplica una función a cero o más
    argumentos.
  </description>

  Más precisamente, cuando se evalua una expasión de macro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> creada por
  <key|inactive e>, las siguientes acciones son llevadas a cabo:

  <\itemize>
    <item>Si <with|mode|math|a> no es una cadena ni un macro, entonces
    <with|mode|math|a> se evalúa una sola vez. Esto produce o bien un nombre
    de macro o una expresión de macro <with|mode|math|f>.

    <item>Si obtenemos el nombre de un macro, entonces reemplazamos
    <with|mode|math|f> por el valor de la variable de entorno
    <with|mode|math|f>. Si después de esto <with|mode|math|f >no es todavía
    un macro, entonces devolvemos <with|mode|math|f>.

    <item>Sean <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> los argumentos
    de <with|mode|math|f> y <with|mode|math|b> su cuerpo (se desechan los
    argumentos superfluos; los argumentos faltantes toman la cadena vacía
    como su valor por defecto). Entonces sustituímos
    <with|mode|math|x<rsub|i>> por cada <with|mode|math|y<rsub|i>> en
    <with|mode|math|b> y devolvemos el resultado que ha sido evaluado.
  </itemize>

  Las funciones son similares a los macros, salvo en que los argumentos de
  las aplicaciones de una función son evaluados y no pueden ser editados
  directamente (primero necesitas desactivar la aplicación de la función,
  después editar los argumentos y por último, reactivar). También,
  <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> se consideran ahora como
  variables de entorno locales, a las que se atribuyen
  <with|mode|math|x<rsub|1>\<ldots\>x<rsub|n>> como sus valores. Estas
  variables locales no se recuerdan cuando una función devuelve una función
  que involucra esas variables.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
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
