<TeXmacs|1.0.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Escribiendo un texto sencillo>

  Una vez que haya dado un nombre al documento, puede empezar a escribir el
  texto. Posteriormente, explicaremos como escribir carácteres especiales,
  los cuales son necesarios para la escritura de textos en otros idiomas. El
  siguiente es un texto sencillo, el cual usted puede escribir como un
  ejercicio:

  <big-figure|<screenshot|simple-1.en.png>|Escribiendo un texto sencillo
  usando <TeXmacs>.>

  Cuando usted termine con la escritura, recomendamos que primero guarde el
  documento, usando la opción <with|font-series|bold|guardar buffer> del
  botón <icon|tm_save.png> de la barra de menus o presionando la tecla
  <key|F3>. Un mensaje en la parte baja de la ventana confirmará la
  operación.\ 

  \;

  <big-figure|<screenshot|simple-2.en.png>|Sólo salvamos el texto por
  seguridad.>

  Por su seguridad, <TeXmacs> actualmente hace una
  <with|font-shape|italic|copia de seguridad> de su documento cada dos
  minutos. Si usted olvida guardar su documento antes de cerrar <TeXmacs>, o
  si su computador se apaga por alguna razón, entonces a usted se le
  \ sugerirá si quiere recuperar los últimos cambios del documento no
  guardado tan pronto lo intente cargar. De nuevo, un mensaje en la parte
  inferior de la ventana confirma \ el autoguardado:\ 

  <big-figure|<screenshot|simple-3.en.png>|<TeXmacs> hace una copia de
  seguridad del documento cada dos minutos>\ 

  Cuando usted haya terminado de escribir, usualmente querá imprimir su
  documento. Esto se puede hacer seleccionando la opción <menu|Print all> del
  botón <icon|tm_print.png>de la barra de menus, o presionando la tecla
  <key|F4>. Antes de imprimir, usted puede configurar su impresora usando el
  menu \ <menu|File|Page setup>. Usted puede especificar el comando de
  impresión (como <verbatim|lpr>), el tipo de papel de la impresora (como
  <verbatim|a4> en Europa o <verbatim|carta> en U.S.A.) y la presición de la
  impresora en puntos por pulgada (por defecto es <verbatim|600>).\ 

  Para recuperar el texto después de haber cerrado <TeXmacs>, primero tiene
  que seleccionar la opción <menu|Load buffer> del botón
  <icon|tm_load.png>del menu o presionar la tecla <key|F2>. Después, puede
  seleccionar el archivo usando el manejador de archivos. En nuestro ejemplo,
  el archivo <verbatim|test.tm> aparece en el manejador de archivos y podemos
  recuperarlo rápidamente haciendo doble click sobre él.\ 

  \;

  <big-figure|<screenshot|load.en.png>|Recuperando el texto sencillo desde el
  disco>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven|Juan Pablo Romero Bernal>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>

  <tmdoc-license|Se garantiza el permiso de copiar, distribuir y/o modificar
  este documento bajo los términos de la GNU Free Documentation License,
  Versión 1.1 o cualquier versión posterior publicada por la Free Software
  Foundation, sin Secciones Invariantes, sin Textos de Portada, y sin Textos
  de contraportada. Una copia de la licencia está incluída en la sección
  titulada: "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|spanish>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|3|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|3|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|idx-3|<tuple|3|?>>
    <associate|gly-4|<tuple|4|?>>
    <associate|idx-4|<tuple|3|?>>
    <associate|idx-5|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Imprimir
      todo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Fichero>|<with|font-family|<quote|ss>|Configuración
      de página>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Cargar
      búfer>>|<pageref|idx-3>>
    </associate>
    <\associate|figure>
      <tuple|normal|Escribiendo un texto sencillo usando
      TeXmacs.|<pageref|gly-1>>

      <tuple|normal|Sólo salvamos el texto por seguridad.|<pageref|gly-2>>

      <tuple|normal|TeXmacs hace una copia de seguridad del documento cada
      dos minutos|<pageref|gly-3>>

      <tuple|normal|Recuperando el texto sencillo desde el
      disco|<pageref|gly-4>>
    </associate>
  </collection>
</auxiliary>