# FIGURA 2. Distribución geográfica de casos reportados y publicaciones
# La figura compara los casos reportados por país de adquisición (A) y las publicaciones por país de estudio (B).
# Entrada: hoja de publicaciones de Lobomycosis_base_datos_ES.xlsx, seleccionada mediante file.choose().
# Salidas: PNG, TIFF, CSV y Excel de control (una hoja), junto al script .R.

# 1. Paquetes y configuración ------------------------------------------------------------
FIGURE_DPI <- 1200
FIGURE_WIDTH_CM <- 32
FIGURE_HEIGHT_CM <- 24
FONT_FAMILY <- "Arial"

# Se conserva la resolución y el tamaño del script original; el TIFF final para la revista se revisará por separado.
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "sf", "rnaturalearth",
  "ragg", "systemfonts", "cowplot", "openxlsx"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Faltan paquetes: ", paste(missing_packages, collapse = ", "),
       "\nInstálalos una sola vez con install.packages() y vuelve a ejecutar el script.")
}
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(cowplot)
})

if (!any(tolower(systemfonts::system_fonts()$family) == "arial")) {
  stop("No se encontró Arial en este equipo.")
}

# 2. Selección de la base y carpeta de salida ---------------------------------------------
# Identificar la carpeta del script que se está ejecutando.
# Así las salidas quedan junto al .R, aunque la base de datos esté en otro lugar.
obtener_carpeta_script <- function() {
  rutas <- vapply(sys.frames(), function(marco) {
    if (is.null(marco$ofile)) NA_character_ else as.character(marco$ofile)[1]
  }, character(1))
  rutas <- rutas[!is.na(rutas) & nzchar(rutas)]
  if (length(rutas) && file.exists(tail(rutas, 1))) {
    return(dirname(normalizePath(tail(rutas, 1), winslash = "/")))
  }

  args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(args)) {
    ruta <- sub("^--file=", "", args[1])
    if (file.exists(ruta)) return(dirname(normalizePath(ruta, winslash = "/")))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    editor <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(editor) && length(editor$path) == 1 &&
        !is.na(editor$path) && nzchar(editor$path) && file.exists(editor$path)) {
      return(dirname(normalizePath(editor$path, winslash = "/")))
    }
  }
  stop("No se pudo identificar la carpeta del script. Guarde el .R y ejecútelo con Source en RStudio.")
}
script_dir <- obtener_carpeta_script()
output_dir <- script_dir

# En RStudio se selecciona manualmente el Excel español en cada ejecución.
# Para ejecución automática puede usarse LOBOMYCOSIS_DATA o data/raw del repositorio.
if (interactive()) {
  message("Seleccione Lobomycosis_base_datos.xlsx (base en español).")
  excel_file <- file.choose()
} else {
  archivo_indicado <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (nzchar(archivo_indicado)) {
    excel_file <- archivo_indicado
  } else {
    excel_file <- file.path(script_dir, "..", "raw", "Lobomycosis_base_datos_ES.xlsx")
  }
}
if (!file.exists(excel_file)) stop("No existe el archivo: ", excel_file)
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
message("Las salidas se guardarán junto al script: ", output_dir)

# 3. Lectura y verificación de datos ---------------------------------------------------------------
# La hoja se identifica por su nombre real; no se renombran columnas del Excel original.
sheet_name <- "Base de datos WORD"
required_columns <- c(
  "ID_estudio", "Country_of_study", "N_total_casos_lobomicosis", "Pais_adquisicion"
)
if (!(sheet_name %in% readxl::excel_sheets(excel_file))) {
  stop("No se encontró la hoja: ", sheet_name)
}
publications <- readxl::read_excel(excel_file, sheet = sheet_name,
                                    col_types = "text")
names(publications) <- trimws(names(publications))
if (!all(required_columns %in% names(publications))) {
  stop("Faltan columnas: ",
       paste(setdiff(required_columns, names(publications)), collapse = ", "))
}
publications[] <- lapply(publications, function(x) trimws(as.character(x)))

study_ids <- publications[["ID_estudio"]]
if (anyNA(study_ids) || any(!nzchar(study_ids)) || anyDuplicated(study_ids)) {
  stop("Los identificadores de estudio están vacíos o repetidos.")
}

# ND indica número de casos no disponible; no se interpreta como cero.
case_text <- publications[["N_total_casos_lobomicosis"]]
missing_cases <- is.na(case_text) | case_text == "" | toupper(case_text) == "ND"
case_numbers <- suppressWarnings(as.numeric(case_text))
if (any(!missing_cases &
        (is.na(case_numbers) | !is.finite(case_numbers) |
           case_numbers < 0 | case_numbers != floor(case_numbers)))) {
  stop("El número total de casos debe ser entero no negativo o ND.")
}
case_numbers[missing_cases] <- NA_real_
publications$case_count <- case_numbers

# 4. Preparación de los recuentos por país ---------------------------------------------------------------
# Se reconocen las formas españolas e inglesas de los países, pues algunos campos del Excel están en inglés.
country_catalog <- data.frame(
  country_es = c("Bolivia", "Brasil", "Colombia", "Ecuador", "Guayana Francesa",
                 "Grecia", "Guyana", "India", "México", "Panamá", "Perú",
                 "Palestina", "Surinam", "Venezuela", "Canadá", "Francia",
                 "Alemania", "Italia", "Reino Unido", "Estados Unidos"),
  country_en = c("Bolivia", "Brazil", "Colombia", "Ecuador", "French Guiana",
                 "Greece", "Guyana", "India", "Mexico", "Panama", "Peru",
                 "Palestine", "Suriname", "Venezuela", "Canada", "France",
                 "Germany", "Italy", "United Kingdom", "United States"),
  ISO3 = c("BOL", "BRA", "COL", "ECU", "GUF", "GRC", "GUY", "IND",
           "MEX", "PAN", "PER", "PSE", "SUR", "VEN", "CAN", "FRA",
           "DEU", "ITA", "GBR", "USA"),
  lon = c(-64, -53, -73, -78, -53, 24, -59, 79, -103, -80,
          -75, 35.2, -56, -65, -105, 2, 10, 12, -3, -100),
  lat = c(-17, -11, 4, -2, 4, 39, 5, 23, 24, 9,
          -10, 31.9, 4, 8, 58, 47, 51, 43, 55, 39),
  stringsAsFactors = FALSE
)
country_catalog$label <- country_catalog$country_es

normalize_country <- function(x) {
  tolower(trimws(iconv(x, to = "ASCII//TRANSLIT")))
}
country_keys <- c(country_catalog$country_es, country_catalog$country_en)
iso_dictionary <- setNames(rep(country_catalog$ISO3, 2),
                           normalize_country(country_keys))
iso_dictionary["united states of america"] <- "USA"

country_to_iso <- function(x) {
  key <- normalize_country(x)
  iso <- unname(iso_dictionary[key])
  iso[is.na(x) | key %in% c("", "nd")] <- "ND"
  if (anyNA(iso)) {
    stop("País no reconocido en el Excel: ", paste(unique(x[is.na(iso)]), collapse = ", "))
  }
  iso
}
publications$study_iso <- country_to_iso(publications$Country_of_study)

# Los desgloses de países solo se distribuyen cuando la publicación informa recuentos explícitos.
allocations <- dplyr::bind_rows(lapply(seq_len(nrow(publications)), function(i) {
  country_value <- publications[["Pais_adquisicion"]][i]
  reported_cases <- publications$case_count[i]
  study_id <- publications[["ID_estudio"]][i]

  if (is.na(country_value) || !nzchar(country_value) ||
      toupper(country_value) == "ND") {
    return(data.frame(Study_ID = study_id, ISO3 = "ND",
                      Cases = reported_cases))
  }
  if (grepl("/", country_value, fixed = TRUE) ||
      grepl("(", country_value, fixed = TRUE)) {
    parts <- strsplit(country_value, "/", fixed = TRUE)[[1]]
    parts_data <- lapply(parts, function(part) {
      match <- regmatches(part, regexec(
        "^\\s*(.*?)\\s*\\(([0-9]+)\\)\\s*$", part, perl = TRUE
      ))[[1]]
      if (length(match) != 3) {
        stop("Desglose de países no válido en estudio ", study_id, ": ", country_value)
      }
      data.frame(Study_ID = study_id,
                 ISO3 = country_to_iso(match[2]),
                 Cases = as.numeric(match[3]))
    })
    result <- dplyr::bind_rows(parts_data)
    if (is.na(reported_cases) || sum(result$Cases) != reported_cases) {
      stop("El desglose no coincide con el total de casos del estudio ", study_id)
    }
    return(result)
  }
  data.frame(Study_ID = study_id,
             ISO3 = country_to_iso(country_value),
             Cases = reported_cases)
}))

cases_by_country <- allocations |>
  dplyr::filter(ISO3 != "ND", !is.na(Cases)) |>
  dplyr::group_by(ISO3) |>
  dplyr::summarise(reported_cases = sum(Cases), .groups = "drop")
publications_by_country <- publications |>
  dplyr::filter(study_iso != "ND") |>
  dplyr::count(ISO3 = study_iso, name = "publications")
country_summary <- country_catalog |>
  dplyr::left_join(cases_by_country, by = "ISO3") |>
  dplyr::left_join(publications_by_country, by = "ISO3") |>
  dplyr::mutate(dplyr::across(c(reported_cases, publications),
                              ~tidyr::replace_na(.x, 0)))

reported_total <- sum(publications$case_count, na.rm = TRUE)
known_country_total <- sum(country_summary$reported_cases)
unknown_country_total <- sum(allocations$Cases[allocations$ISO3 == "ND"],
                             na.rm = TRUE)
stopifnot(known_country_total + unknown_country_total == reported_total)
# Se verifican los totales aprobados del manuscrito antes de dibujar la figura.
if (nrow(publications) != 98 || reported_total != 1463 ||
    known_country_total != 1254 || sum(country_summary$publications) != 98) {
  stop("Los recuentos difieren del manuscrito: revise el archivo seleccionado.")
}

# 5. Cartografía ----------------------------------------------------------------
# Se utiliza Natural Earth (escala 1:50 m), igual que en el script original. Se requiere conexión a Internet.
message("Obteniendo cartografía de Natural Earth...")
world <- rnaturalearth::ne_download(
  scale = 50, type = "map_units", category = "cultural",
  returnclass = "sf", destdir = tempdir()
)
names(world) <- tolower(names(world))
world <- sf::st_transform(world, 4326)
world <- sf::st_wrap_dateline(world,
  options = c("WRAPDATELINE=YES", "DATELINEOFFSET=10"), quiet = TRUE)
world$ISO3 <- as.character(world$iso_a3)
if ("iso_a3_eh" %in% names(world)) {
  missing_iso <- is.na(world$ISO3) | world$ISO3 == "-99"
  world$ISO3[missing_iso] <- as.character(world$iso_a3_eh[missing_iso])
}
is_geo_unit <- function(pattern) {
  grepl(pattern, world$name, ignore.case = TRUE) |
    grepl(pattern, world$geounit, ignore.case = TRUE)
}
world$ISO3[which(is_geo_unit("French Guiana"))] <- "GUF"
world$ISO3[which(is_geo_unit("Palestine|West Bank|Gaza"))] <- "PSE"
world$ISO3[which(is_geo_unit("France") & world$continent == "Europe")] <- "FRA"
if ("sovereignt" %in% names(world)) {
  world$ISO3[which(world$sovereignt == "United Kingdom" &
                   world$continent == "Europe")] <- "GBR"
}
world <- world |> dplyr::filter(is.na(continent) | continent != "Antarctica")
missing_shapes <- setdiff(country_summary$ISO3, world$ISO3)
if (length(missing_shapes)) {
  stop("Faltan geometrías para: ", paste(missing_shapes, collapse = ", "))
}
WORLD_CRS <- "+proj=eck4 +lon_0=0 +datum=WGS84 +units=m +no_defs"
world_projected <- sf::st_make_valid(sf::st_transform(world, WORLD_CRS))
country_shapes <- world_projected |>
  dplyr::group_by(ISO3) |>
  dplyr::summarise(.groups = "drop") |>
  dplyr::left_join(country_summary, by = "ISO3")
points <- sf::st_as_sf(country_summary, coords = c("lon", "lat"),
                       crs = 4326, remove = FALSE)
point_coords <- sf::st_coordinates(sf::st_transform(points, WORLD_CRS))
map_points <- sf::st_drop_geometry(points) |>
  dplyr::mutate(X = point_coords[, 1], Y = point_coords[, 2])

# 6. Construcción de la figura ---------------------------------------------------------------
case_classes <- c("1–5", "6–9", "10–83", "≥84")
case_colors <- c("1–5" = "#FC8D59", "6–9" = "#EF6548",
                 "10–83" = "#D7301F", "≥84" = "#7F0000")
publication_classes <- c("1–3", "4–8", "9–32", "≥33")
publication_colors <- c("#F7DD9C", "#D79A38", "#AF7012", "#71430B")

case_class <- function(n) {
  ifelse(n <= 5, case_classes[1],
         ifelse(n <= 9, case_classes[2],
                ifelse(n <= 83, case_classes[3], case_classes[4])))
}
country_shapes$case_class <- factor(case_class(country_shapes$reported_cases),
                                    levels = case_classes)
country_shapes$publication_class <- cut(
  country_shapes$publications, breaks = c(0, 3, 8, 32, Inf),
  labels = publication_classes, include.lowest = TRUE, right = TRUE
)

map_theme <- function() {
  ggplot2::theme_void(base_family = FONT_FAMILY, base_size = 11) +
    ggplot2::theme(
      text = ggplot2::element_text(family = FONT_FAMILY, face = "plain"),
      plot.title = ggplot2::element_text(size = 18, face = "bold",
                                         hjust = 0, margin = ggplot2::margin(b = 5)),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}
map_background <- function() {
  ggplot() +
    geom_sf(data = world_projected, fill = "#EFEEED",
            colour = "#B8B4B1", linewidth = .12) +
    coord_sf(crs = WORLD_CRS, datum = NA, expand = FALSE) +
    map_theme()
}

# Las etiquetas muestran los países principales y usan punto para los miles.
number_label <- function(n) {
  format(n, big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}
add_country_labels <- function(plot, data, value_column, max_labels) {
  labels <- data[data[[value_column]] > 0, ]
  labels <- head(labels[order(-labels[[value_column]]), ], max_labels)
  anchors <- data.frame(
    ISO3 = c("BRA", "COL", "VEN", "BOL", "ECU", "PAN", "MEX",
             "GUY", "SUR", "USA", "GUF"),
    lon = c(-18, -110, -35, -106, -107, -110, -114, -37, -35, -145, -34),
    lat = c(-27, 0, 17, -25, -12, 13, 27, 8, 1, 32, -5)
  )
  labels <- dplyr::left_join(labels, anchors, by = "ISO3", suffix = c("", "_anchor"))
  labels$lon_anchor[is.na(labels$lon_anchor)] <-
    labels$lon[is.na(labels$lon_anchor)] + 15
  labels$lat_anchor[is.na(labels$lat_anchor)] <-
    labels$lat[is.na(labels$lat_anchor)]
  anchor_points <- sf::st_as_sf(labels, coords = c("lon_anchor", "lat_anchor"),
                                crs = 4326)
  anchor_xy <- sf::st_coordinates(sf::st_transform(anchor_points, WORLD_CRS))
  labels$LX <- anchor_xy[, 1]
  labels$LY <- anchor_xy[, 2]
  labels$label <- paste0(labels$label, ": ", number_label(labels[[value_column]]))
  plot +
    geom_segment(data = labels,
                 aes(x = LX, y = LY, xend = X, yend = Y),
                 colour = "#817985", linewidth = .22,
                 arrow = grid::arrow(length = grid::unit(1.2, "mm"), type = "open")) +
    geom_text(data = labels, aes(x = LX, y = LY, label = label),
              family = FONT_FAMILY, fontface = "plain", size = 3.8)
}

# Se conservan las clases, colores y posiciones originales de ambas leyendas.
make_legend <- function(title, categories, colors) {
  legend <- cowplot::ggdraw() +
    cowplot::draw_label(title, x = .50, y = .70, hjust = .50, vjust = 1,
                        fontfamily = FONT_FAMILY, fontface = "bold", size = 11.5)
  positions <- seq(.55, .25, length.out = length(categories))
  for (i in seq_along(categories)) {
    legend <- legend + annotation_custom(
      grid::rectGrob(gp = grid::gpar(fill = colors[i], col = NA)),
      xmin = .20, xmax = .37, ymin = positions[i] - .019,
      ymax = positions[i] + .019
    )
    legend <- legend + cowplot::draw_label(
      categories[i], x = .43, y = positions[i] + .025, hjust = 0,
      fontfamily = FONT_FAMILY, fontface = "plain", size = 10.5
    )
  }
  legend
}

panel_a <- map_background() +
  geom_sf(data = country_shapes |> dplyr::filter(reported_cases > 0),
          aes(fill = case_class), colour = "#B8B4B1", linewidth = .12) +
  scale_fill_manual(values = case_colors, drop = FALSE, guide = "none") +
  labs(title = "A.")
panel_a <- add_country_labels(panel_a, map_points, "reported_cases", 5)

panel_b <- map_background() +
  geom_sf(data = country_shapes |> dplyr::filter(publications > 0),
          aes(fill = publication_class), colour = "#B8B4B1", linewidth = .12) +
  scale_fill_manual(values = setNames(publication_colors, publication_classes),
                    drop = FALSE, guide = "none") +
  labs(title = "B.")
panel_b <- add_country_labels(panel_b, map_points, "publications", 4)

legend_a <- make_legend(
  paste0("Casos reportados\n(n = ", number_label(known_country_total), ")"),
  case_classes, unname(case_colors)
)
legend_b <- make_legend(
  paste0("Número de publicaciones\n(n = ", number_label(sum(country_summary$publications)), ")"),
  publication_classes, publication_colors
)

# Dos mapas con la misma distribución de paneles y leyendas de la figura aprobada.
figure_02 <- cowplot::ggdraw() +
  cowplot::draw_plot(panel_a, x = 0, y = .505, width = .78, height = .49) +
  cowplot::draw_plot(legend_a, x = .78, y = .505, width = .22, height = .49) +
  cowplot::draw_plot(panel_b, x = 0, y = .005, width = .78, height = .49) +
  cowplot::draw_plot(legend_b, x = .78, y = .005, width = .22, height = .49)

# 7. Exportación de Figura 2 y CSV --------------------------------------------------------------------
# Conservar las salidas anteriores si se ejecuta nuevamente el script.
nombre_figura <- "fig02_geo_es"
nombre_resumen <- "fig02_geo_es_summary"
nombre_control <- "fig02_geo_es_audit"
sufijo <- ""
numero_ejecucion <- 0L
repeat {
  rutas_previstas <- file.path(output_dir, c(
    paste0(nombre_figura, sufijo, ".png"),
    paste0(nombre_figura, sufijo, ".tiff"),
    paste0(nombre_resumen, sufijo, ".csv"),
    paste0(nombre_control, sufijo, ".xlsx")
  ))
  if (!any(file.exists(rutas_previstas))) break
  numero_ejecucion <- numero_ejecucion + 1L
  sufijo <- paste0("_run", sprintf("%02d", numero_ejecucion))
}
if (nzchar(sufijo)) message("Se conservan las salidas previas; sufijo nuevo: ", sufijo)

# Exportar la figura aprobada sin modificar la base original.
# La salida se guarda por separado; nunca se sobrescribe el Excel seleccionado.
export_figure <- function(file_path, file_type) {
  if (file_type == "png") {
    ragg::agg_png(file_path, width = FIGURE_WIDTH_CM,
                  height = FIGURE_HEIGHT_CM, units = "cm", res = FIGURE_DPI,
                  background = "white")
  } else {
    ragg::agg_tiff(file_path, width = FIGURE_WIDTH_CM,
                   height = FIGURE_HEIGHT_CM, units = "cm", res = FIGURE_DPI,
                   compression = "lzw", background = "white")
  }
  tryCatch(print(figure_02), finally = grDevices::dev.off())
  message("Guardado: ", file_path)
}
output_base <- file.path(output_dir, paste0(nombre_figura, sufijo))
export_figure(paste0(output_base, ".png"), "png")
export_figure(paste0(output_base, ".tiff"), "tiff")

# El CSV permite verificar los recuentos por país sin duplicar la base original.
summary_file <- file.path(output_dir, paste0(nombre_resumen, sufijo, ".csv"))
write.csv(
  country_summary[, c("label", "ISO3", "reported_cases", "publications")],
  summary_file, row.names = FALSE, fileEncoding = "UTF-8"
)
message("Publicaciones: ", nrow(publications), " | ",
        "Casos reportados: ", number_label(reported_total), " | ",
        "Casos con país asignado: ", number_label(known_country_total))
message("Archivos guardados en: ", output_dir)

# 8. Excel de control con una sola hoja ---------------------------------------------------------------
# Reúne los valores de ambos mapas y verifica los totales del manuscrito.
total_publicaciones <- nrow(publications)
resumen_paises <- country_summary |>
  dplyr::transmute(
    Pais = label, ISO3 = ISO3,
    Casos_reportados = as.integer(reported_cases),
    Porcentaje_casos = round(100 * reported_cases / known_country_total, 1),
    Publicaciones = as.integer(publications),
    Porcentaje_publicaciones = round(100 * publications / total_publicaciones, 1)
  )
resumen_paises <- rbind(resumen_paises, data.frame(
  Pais = "TOTAL", ISO3 = "",
  Casos_reportados = as.integer(known_country_total), Porcentaje_casos = 100,
  Publicaciones = nrow(publications), Porcentaje_publicaciones = 100
))

publicaciones_con_pais <- sum(publications$Pais_adquisicion != "ND" &
  !is.na(publications$Pais_adquisicion) & nzchar(publications$Pais_adquisicion))
publicaciones_sin_pais <- nrow(publications) - publicaciones_con_pais
publicaciones_multipais <- sum(grepl("/", publications$Pais_adquisicion,
                                     fixed = TRUE), na.rm = TRUE)

verificaciones <- data.frame(
  Indicador = c(
    "Publicaciones incluidas", "Publicaciones con número de casos",
    "Publicaciones sin número de casos", "Suma de casos reportados",
    "Casos con país de adquisición conocido", "Casos sin país de adquisición",
    "Publicaciones con país de adquisición", "Publicaciones sin país de adquisición",
    "Publicaciones realizadas en Brasil", "Publicaciones realizadas en Colombia",
    "Publicaciones realizadas en Venezuela", "Casos adquiridos en Brasil",
    "Casos adquiridos en Colombia", "Casos adquiridos en Venezuela",
    "Suma de asignaciones por país", "Publicaciones con desglose explícito entre países"
  ),
  Observado = c(
    nrow(publications), sum(!is.na(publications$case_count)),
    sum(is.na(publications$case_count)), reported_total,
    known_country_total, unknown_country_total,
    publicaciones_con_pais, publicaciones_sin_pais,
    country_summary$publications[country_summary$ISO3 == "BRA"],
    country_summary$publications[country_summary$ISO3 == "COL"],
    country_summary$publications[country_summary$ISO3 == "VEN"],
    country_summary$reported_cases[country_summary$ISO3 == "BRA"],
    country_summary$reported_cases[country_summary$ISO3 == "COL"],
    country_summary$reported_cases[country_summary$ISO3 == "VEN"],
    known_country_total + unknown_country_total,
    publicaciones_multipais
  ),
  Manuscrito = c(98, 94, 4, 1463, 1254, 209, 83, 15,
                 63, 9, 5, 1133, 83, 9, 1463, NA_real_),
  stringsAsFactors = FALSE
)
verificaciones$Estado <- ifelse(is.na(verificaciones$Manuscrito), "INFO",
  ifelse(verificaciones$Observado == verificaciones$Manuscrito, "COINCIDE", "REVISAR"))
if (any(verificaciones$Estado == "REVISAR")) {
  stop("Los controles no coinciden con el manuscrito. Revise la base antes de exportar.")
}
verificaciones$Nota <- c(
  "Una fila por publicación", "Número de casos disponible", "Número de casos ND",
  "Suma acumulada; no necesariamente pacientes únicos",
  "Denominador del mapa A", "No representados en el mapa A",
  "País informado a nivel de publicación", "ND a nivel de publicación",
  "Mapa B", "Mapa B", "Mapa B", "Mapa A", "Mapa A", "Mapa A",
  "Casos con país + casos sin país", "Dato descriptivo, sin objetivo en el manuscrito"
)

libro <- openxlsx::createWorkbook()
hoja <- "Control figura 02"
openxlsx::addWorksheet(libro, hoja, gridLines = FALSE)
estilo_titulo <- openxlsx::createStyle(fontSize = 15, textDecoration = "bold",
  fgFill = "#E8EDF4", fontColour = "#243447", valign = "center")
estilo_cabecera <- openxlsx::createStyle(textDecoration = "bold",
  fgFill = "#DDE6EF", border = "Bottom", valign = "center")
estilo_seccion <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold",
  fgFill = "#EEF2F6")
estilo_total <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E8EDF4")
estilo_entero <- openxlsx::createStyle(numFmt = "#,##0")
estilo_porcentaje <- openxlsx::createStyle(numFmt = "0.0")

openxlsx::writeData(libro, hoja, "FIGURA 2 — VERIFICACIÓN DE RECUENTOS POR PAÍS", startRow = 1)
openxlsx::mergeCells(libro, hoja, cols = 1:6, rows = 1)
openxlsx::addStyle(libro, hoja, estilo_titulo, rows = 1, cols = 1:6, gridExpand = TRUE)
openxlsx::setRowHeights(libro, hoja, rows = 1, heights = 28)
openxlsx::writeData(libro, hoja, paste("Base utilizada:", basename(excel_file)),
                    startRow = 2, startCol = 1)
openxlsx::writeData(libro, hoja,
  "A: casos por país de adquisición; B: publicaciones por país del estudio",
  startRow = 3, startCol = 1)
openxlsx::writeData(libro, hoja, resumen_paises, startRow = 5, startCol = 1,
                    headerStyle = estilo_cabecera, withFilter = TRUE)
fila_total <- 5 + nrow(resumen_paises)
openxlsx::addStyle(libro, hoja, estilo_total, rows = fila_total,
                    cols = 1:6, gridExpand = TRUE)
openxlsx::addStyle(libro, hoja, estilo_entero,
  rows = 6:fila_total, cols = c(3, 5), gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(libro, hoja, estilo_porcentaje,
  rows = 6:fila_total, cols = c(4, 6), gridExpand = TRUE, stack = TRUE)

fila_titulo_control <- fila_total + 3
fila_tabla_control <- fila_titulo_control + 1
openxlsx::writeData(libro, hoja, "CONTROLES FRENTE AL MANUSCRITO",
                    startRow = fila_titulo_control)
openxlsx::addStyle(libro, hoja, estilo_seccion, rows = fila_titulo_control,
                    cols = 1:6, gridExpand = TRUE)
openxlsx::writeData(libro, hoja, verificaciones,
  startRow = fila_tabla_control, headerStyle = estilo_cabecera)
openxlsx::addStyle(libro, hoja, estilo_entero,
  rows = (fila_tabla_control + 1):(fila_tabla_control + nrow(verificaciones)),
  cols = 2:3, gridExpand = TRUE, stack = TRUE)

fila_notas <- fila_tabla_control + nrow(verificaciones) + 4
openxlsx::writeData(libro, hoja, "INTERPRETACIÓN Y TRATAMIENTO DE DATOS",
                    startRow = fila_notas)
openxlsx::addStyle(libro, hoja, estilo_seccion, rows = fila_notas,
                    cols = 1:6, gridExpand = TRUE)
notas <- c(
  "La suma de casos por publicación no necesariamente corresponde a pacientes únicos.",
  "El país de adquisición (A) y el país donde se realizó el estudio (B) son variables diferentes.",
  "Los casos solo se distribuyen entre países cuando la publicación informa un desglose numérico.",
  "Los recuentos ND se excluyen de la suma; los casos sin país quedan sin asignar.",
  "Un cero en el resumen indica que no se asignaron casos o publicaciones a ese país.",
  "La verificación compara los recuentos con el manuscrito; no sustituye la revisión de los artículos originales."
)
openxlsx::writeData(libro, hoja, data.frame(Notas = notas),
                    startRow = fila_notas + 1, colNames = FALSE)
openxlsx::setColWidths(libro, hoja, cols = 1, widths = 51)
openxlsx::setColWidths(libro, hoja, cols = 2, widths = 17)
openxlsx::setColWidths(libro, hoja, cols = 3:6, widths = c(20, 22, 19, 22))
openxlsx::freezePane(libro, hoja, firstActiveRow = 6)
archivo_resumen_excel <- file.path(output_dir, paste0(nombre_control, sufijo, ".xlsx"))
openxlsx::saveWorkbook(libro, archivo_resumen_excel, overwrite = FALSE)
message("Excel de una hoja guardado: ", archivo_resumen_excel)

message("Figura 2 terminada. Los cuatro archivos están en: ", output_dir)
