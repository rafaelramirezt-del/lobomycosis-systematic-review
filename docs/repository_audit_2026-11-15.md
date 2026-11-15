# Revisión del repositorio — 15 de noviembre de 2026

**Archivo revisado:** `lobomycosis-systematic-review(1).7z`, 112 entradas (87 archivos). Esta revisión actualiza la auditoría anterior tras la incorporación de `Español_Spanish/` y del Excel faltante de la Figura S2. No se ejecutó R de forma independiente.

## Inventario verificado

| Elemento | Cantidad | Estado |
| --- | ---: | --- |
| Scripts R | 14 (7 en inglés, 7 en español) | Presentes |
| Figuras analíticas PNG | 14 | Presentes |
| Figuras analíticas TIFF | 14 | Presentes; 1.200 dpi |
| PRISMA PowerPoint | 2 | Inglés y español |
| PRISMA TIFF inglés | 1 | Presente; 220 dpi |
| Bases Excel originales | 2 | Presentes; 98 publicaciones y 256 registros cada una |
| Auditorías Excel | 14 | Una hoja por archivo |
| Resúmenes CSV de las figuras R | 14 | Presentes |

Se comprobaron las marcas almacenadas en los 14 Excel: **146 `PASS` en inglés y 131 `PASS` + 15 `COINCIDE` en español (292 comprobaciones satisfactorias)**; no se identificaron controles `CHECK`, `FAIL` o `ERROR`. Un `INFO` por idioma documenta un dato adicional. Estos controles no sustituyen una ejecución completa de R ni una comprobación visual exhaustiva de cada figura.

Las dos bases definitivas contienen **cero menciones de «Cultivo» o «Culture»** en sus hojas de publicaciones e individuales. La Figura suplementaria de matriz de evidencia se llama correctamente **S3**.

## Correcciones documentales propuestas

- Sustituir el `README.md` principal: el anterior describía rutas obsoletas y afirmaba incorrectamente que faltaba S2 en español.
- Actualizar `Español_Spanish/LEEME.md`, `scripts/README.md`, `figures/README.md` y `docs/analysis_manifest.csv` para reflejar las rutas actuales.
- Normalizar los dos nombres derivados de `fig02_geo_es.R`: `fig02_geo_es_summary.csv` y `fig02_geo_es_audit.xlsx`. Se trata solo de nombres; las cifras, la figura y la base permanecen iguales.
- Retirar del paquete final `Español_Spanish/figs01_duracion/.Rhistory` y `Español_Spanish/figs03_evidencia/.Rhistory`.

## Pendientes de la publicación pública

- Comprobar, idealmente en una instalación limpia de R/RStudio, los scripts y el resultado gráfico frente a los TIFF maestros.
- Verificar disponibilidad y versiones de paquetes, así como el origen y derechos de la cartografía Natural Earth.
- Revisar los posibles cuasi-identificadores de los registros individuales y obtener el visto bueno de coautores para redistribuir los datos.
- Elegir una licencia de distribución y preparar `CITATION.cff` con autoría/título definitivos.
- Valorar copias de envío a *PLOS ONE*, sin sobrescribir las figuras maestras: la Figura 3 TIFF inglesa mide 11,44 MB y la PRISMA inglesa está a 220 dpi.
- Revisar el borrador `docs/data_dictionary_draft.csv`; no presentarlo como diccionario final sin validación de sus campos.
- Si se desean publicar tablas suplementarias S1–S3 como archivos independientes, incorporarlas antes del registro definitivo de Zenodo.

**Conclusión:** el inventario de gráficos, bases, scripts y auditorías está completo. La estructura es apta para cerrar la documentación, pero todavía falta la revisión de derechos, licencia y una prueba independiente antes de declarar una versión pública definitiva.
