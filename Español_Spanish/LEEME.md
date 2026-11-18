# Reproducir las figuras en español

Esta carpeta contiene la versión española de las bases, los siete scripts R y los siete conjuntos de figuras analíticas de la revisión sistemática de lobomicosis. **Las Figuras S1, S2 y S3 están completas** y cuentan con sus imágenes, resúmenes y auditorías. Es un complemento del repositorio principal en inglés.

**Versión pública archivada:** https://doi.org/10.5281/zenodo.24255906

## Requisitos

- R y RStudio, con la fuente **Arial** instalada.
- Los paquetes de R utilizados por las figuras. Para instalarlos una sola vez, ejecuta en la consola de R:

```r
install.packages(c(
  "readxl", "openxlsx", "ragg", "systemfonts", "ggplot2",
  "dplyr", "tidyr", "sf", "rnaturalearth", "rnaturalearthdata",
  "ggalluvial", "cowplot", "scales", "ggrepel", "patchwork"
))
```

## Ejecutar una figura

1. Abre en RStudio el archivo `.R` de la figura que quieras reproducir.
2. Haz clic en **Source** para ejecutar el script completo.
3. Cuando aparezca la ventana de selección (`file.choose()`), elige **`raw/Lobomycosis_base_datos_ES.xlsx`** dentro de `Español_Spanish/`.
4. Revisa los resultados y las comprobaciones del Excel de auditoría.

**Cada script genera exactamente cuatro archivos junto al propio `.R`**: una imagen `.png`, una imagen `.tiff`, un resumen `_summary.csv` y un Excel `_audit.xlsx` con una sola hoja. La base original no se modifica. Si ya existen archivos de una ejecución anterior, se puede añadir un sufijo `_run01`, `_run02`, etc.

## Figuras disponibles

| Figura | Script en español |
| --- | --- |
| Figura 2 — Distribución geográfica | `fig02_geo_es.R` |
| Figura 3 — Mapa y diagrama aluvial | `fig03_geo_alluvial_es.R` |
| Figura 4 — Localización anatómica y morfología | `fig04_clinical_es.R` |
| Figura 5 — Tratamientos y desenlaces | `fig05_treatment_es.R` |
| Figura S1 — Tiempo de evolución y distribución clínica | `figs01_duration_es.R` |
| Figura S2 — Publicaciones y métodos diagnósticos | `figs02_diagnosis_es.R` |
| Figura S3 — Matriz de disponibilidad de información | `figs03_evidence_es.R` |

La **Figura 1 (PRISMA)** se preparó por separado y no requiere un script de R. Los scripts y sus resultados pueden encontrarse dentro de las subcarpetas de cada figura.

## Notas para interpretar los resultados

- **ND = no documentado** (*not documented*): información ausente o descrita insuficientemente en las publicaciones originales; no representa cero ni una respuesta clínica negativa.
- Los denominadores cambian según la variable y el nivel de análisis (publicación o registro individual).
- Los resultados corresponden a una síntesis descriptiva de información publicada, no a estimaciones de incidencia ni a comparaciones causales de tratamientos.

Para la descripción completa del estudio, consulta el [`README.md`](../README.md) de la carpeta principal del repositorio. La versión en inglés constituye la documentación principal del repositorio; esta carpeta permite reproducir los análisis en español.

## Reutilización y citación

Los scripts se publican con licencia **MIT** y las bases, figuras y documentación originales con **CC BY 4.0**. Consulta los archivos `LICENSE`, `LICENSE-DATA.md` y `CITATION.cff` de la carpeta principal. Puedes reutilizar el material citando el repositorio y los estudios originales de los que provienen los datos.
