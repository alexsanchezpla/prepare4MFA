## List of functions implemented in the package

- samplesTable(x)
  - Descripción: Construye una tabla con información sobre las muestras en un marco de datos.
  - Depende de: Ninguna función.

- varsTable(x)
  - Descripción: Construye una tabla con información sobre las variables en un marco de datos.
  - Depende de: Ninguna función.

- extractVarNames(varsAndGroupsDF, label)
  - Descripción: Extrae el subconjunto de nombres de variables asociados con una etiqueta dada en un marco de datos.
  - Depende de: Ninguna función.

- creaGrups(myDf, GroupsNames)
  - Descripción: Crea grupos de variables para su uso en un análisis factorial múltiple.
  - Depende de: extractVarNames(varsAndGroupsDF, label).

- MultMerge2(lst, all.x, all.y, by)
  - Descripción: Combina varios marcos de datos en uno solo, manteniendo todas las filas y columnas únicas de cada uno.
  - Depende de: Ninguna función.

- extractGroup(x, pos, vecOfSizes)
  - Descripción: Extrae un subgrupo de columnas de una matriz de datos.
  - Depende de: Ninguna función.

- showText(aText)
  - Descripción: Muestra un texto en la consola con un formato especial.
  - Depende de: Ninguna función.

- checkFactorialStructure(mylistOfGroups)
  - Descripción: Comprueba la estructura factorial de los grupos creados.
  - Depende de: MultMerge2(lst, all.x, all.y, by).

- showGroupsinList(alistOfVars)
  - Descripción: Muestra información sobre los grupos en una lista de variables.
  - Depende de: Ninguna función.
