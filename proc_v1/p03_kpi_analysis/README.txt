p03_kpi_analysis - Fortran KPI Analysis Tool

Este programa recibe una lista de archivos `.for` y genera un resumen CSV con KPIs clave:
- Total de líneas
- Total de palabras clave
- Palabras clave únicas
- Estimación de estructuras anidadas (IF, DO, THEN, ENDIF)
- Conteo de estructuras complejas (COMMON, ENTRY, EQUIVALENCE)
- Score total de complejidad

Uso:

1. Crear una lista con los archivos:
   ls ../repo/*.for > lista.txt

2. Ejecutar:
   ./kpi_loop lista.txt > resumen_kpis.csv
