Este repositorio contiene un análisis cuantitativo para el examen de optativo de Análisis de datos estadísticos en R en la Universidad Alberto Hurtado.
Estudiante: Fátima Lusangi Vargas.
Profesora: Valentina Andrade.
Ayudantes: Nicolás Godoy y Dafne Jaime.
Fecha: 22/11/2021

 ABSTRACT: El objetivo de este análisis cuantitativo poblacional es conocer como se distribuye y compone el empleo con contrato escrito en Chile 2020, respecto de la composición de la duración de el contrato, distribución sexual, etaria y análisis salarial neto. Para ello se procesarán datos secundarios de la [Encuesta Suplementaria de Ingresos 2020](https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-suplementaria-de-ingresos) de donde extraemos los datos filtrando a aquellas personas que responden tener un empleo y que este sea por contrato escrito, que además reporte su salario neto y duración de contrato.
Los principales resultados confirman brecha salarial entre mujeres y hombres con empleos de contrato escrito, la edad y la duración de contrato influyen en el salario neto de las personas. 

Fuera de las carpetas podras ver
- Rproject (que contiene todas las demás carpetas) llamado: examen-FatimaUAH.Rproj
- Archivo README.md (este que lees como guía)

En las carpetas de esta repositorio podrá encontrar:

R:
- Documento Datos: 01_proc.1 que corresponde al script de procesamiento de la base de datos ESI 2020.
- Documento RMarkdown Lusangi-examen.Rmd que contiene el procesamiento de análisis y resultado de los datos.
. Documento html Lusangi-examen.html que es el documento a presentar con los resultados de esta investigación.

Input: 
carpeta data: debido a que los datos se cargaron con [URL](https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/spss_esi/2020/esi-2020---personas.sav?sfvrsn=a730ce58_4&download=true) se ha dejado un documento pdf que contiene el link en caso de que prefiera extraerse de ahí.

Output:
carpeta data:
contiene el archivo "datos_proc_esi2020.rds" que corresponde a la base de datos ya procesada, con la que se harán los análisis en el RMarkdown contenido en la carpeta R.


