# Auditoría documental del repositorio — 15 de noviembre de 2026

**Estado actualizado el 18 de noviembre de 2026.** Esta nota describe el inventario de la versión pública `v1.0.0` y distingue los análisis completos de las verificaciones técnicas adicionales. La actualización documental no altera bases de datos, scripts ni gráficos.

- **DOI de la versión v1.0.0:** https://doi.org/10.5281/zenodo.24255906
- **DOI general de todas las versiones:** https://doi.org/10.5281/zenodo.24255907
- **Repositorio:** https://github.com/rafaelramirezt-del/lobomycosis-systematic-review

## Inventario comprobado

| Elemento | Cantidad | Estado |
| --- | ---: | --- |
| Scripts R | 14 (7 en inglés y 7 en español) | Completos |
| Figuras analíticas PNG | 14 | Presentes |
| Figuras analíticas TIFF | 14 | Presentes |
| Presentaciones PRISMA editables | 2 | Inglés y español |
| Archivo PRISMA TIFF inglés | 1 | Presente |
| Bases de extracción Excel | 2 | 98 publicaciones y 256 registros individuales en cada idioma |
| Auditorías Excel | 14 | Presentes; una hoja por archivo |
| Resúmenes CSV de figuras R | 14 | Presentes |

**Las Figuras S1, S2 y S3 están completas en inglés y español.** Cada figura analítica (Figuras 2–5 y S1–S3) dispone de script, imagen PNG, imagen TIFF, resumen CSV y Excel de auditoría. La Figura 1 (PRISMA) se elaboró por separado. S1 corresponde al tiempo de evolución, S2 a los métodos diagnósticos y S3 a la disponibilidad de información.

## Auditoría de resultados

Los 14 libros de auditoría registran **146 controles `PASS` en inglés y 131 `PASS` más 15 `COINCIDE` en español**, para un total de **292 comprobaciones satisfactorias**. No se encontraron controles `CHECK`, `FAIL` o `ERROR` en los archivos almacenados. Los autores ejecutaron los scripts en RStudio; estas verificaciones no deben confundirse con una reproducción por terceros en un entorno limpio.

- Figura S1: 93 tiempos de evolución exactos para medianas e IQR; tres duraciones expresadas como límites inferiores se describieron aparte.
- Figura 3: diagrama aluvial construido con 46 registros completos.
- Análisis de sensibilidad: exclusión de 103 registros del ensayo LBM-054.
- **ND = not documented / no documentado:** información ausente o insuficientemente descrita en las publicaciones originales.

## Estado de la publicación

La versión `v1.0.0` está archivada en Zenodo. Los scripts disponen de licencia MIT y el material original de datos y documentación de licencia CC BY 4.0, con metadatos de citación en `CITATION.cff`.

Los documentos Word de las Tablas 1 y 2 y las suplementarias S1–S3 se mantienen fuera del paquete de análisis archivado, como materiales editoriales del manuscrito. Su ausencia del paquete no significa que falte un análisis.

El diccionario `docs/data_dictionary_draft.csv` está identificado como **borrador**; no debe anunciarse como definitivo sin validación.

## Comprobaciones adicionales, no análisis pendientes

- La reproducción independiente en una instalación limpia de R con versiones de paquetes registradas podría fortalecer la validación externa.
- Las copias editoriales de figuras para *PLOS ONE*, de ser necesarias, deben derivarse de las imágenes maestras sin modificarlas.
- Los derechos de materiales de terceros deben respetarse en cualquier reutilización.

**Conclusión:** los siete análisis de figuras por idioma, sus archivos gráficos y sus auditorías están completos. Esta actualización corrige únicamente la descripción documental. Los cambios posteriores en `main` no alteran el registro inmutable de Zenodo v1.0.0.
