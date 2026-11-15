# FIGURA SUPLEMENTARIA S2. Publicaciones y métodos diagnósticos informados
# Panel A: número de publicaciones por año y tipo de estudio.
# Panel B: métodos informados por periodo; una publicación puede reportar varios métodos.
# Cultivo se excluye de la figura por indicación del tutor; la base original no se modifica.

# 1. Paquetes y configuración -----------------------------------------------------------
DPI <- 1200
FIGURE_WIDTH_CM <- 42
FIGURE_HEIGHT_CM <- 26
FONT_FAMILY <- "Arial"
TEXT_SIZE <- 14
AXIS_WIDTH <- 1.5
SHOW_NOTES <- FALSE
EXCEL_FILE <- NULL  # NULL abre file.choose(); también puede indicarse una ruta.

source_sheet <- "Base de datos WORD"
id_column <- "ID_estudio"
year_column <- "Ano_publicacion"
type_column <- "Tipo_estudio"
method_column <- "Metodo_diagnostico"
output_stem <- "figs02_diagnosis_es"

study_colors <- c(
  "Reporte de caso" = "#A9C19F", "Estudio de laboratorio" = "#BBA0D0",
  "Serie de casos" = "#E6CC93", "Estudio observacional" = "#CC9C9F",
  "Ensayo clínico" = "#A3ABAD")

# Estos son los únicos métodos del panel B; cultivo se excluye de la figura.
method_names <- c("Histopatología", "Examen directo", "Clínico",
                  "Inmunohistoquímica", "Molecular", "Serología",
                  "Microscopía electrónica", "Inmunofluorescencia", "ND")
excluded_method <- "Cultivo"

required_packages <- c("readxl", "ragg", "systemfonts", "openxlsx")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Faltan paquetes: ", paste(missing_packages, collapse = ", "),
       ". Instálelos una vez con install.packages() antes de ejecutar.")
}
fonts <- systemfonts::system_fonts()
font_regular <- fonts[tolower(fonts$family) == "arial" & !fonts$italic &
  !grepl("bold|heavy|black", fonts$style, ignore.case = TRUE), ]
font_bold <- fonts[tolower(fonts$family) == "arial" & !fonts$italic &
  grepl("bold", fonts$style, ignore.case = TRUE), ]
if (!nrow(font_regular) || !nrow(font_bold)) stop("Se requieren Arial regular y Arial Bold.")

# 2. Selección del Excel y carpeta de salida ---------------------------------------------------------
# El Excel puede estar en otra carpeta; las cuatro salidas quedan junto al archivo .R.
find_script_directory <- function() {
  source_files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
  }, character(1))
  source_files <- source_files[!is.na(source_files) & nzchar(source_files)]
  if (length(source_files) && file.exists(tail(source_files, 1))) {
    return(dirname(normalizePath(tail(source_files, 1), winslash = "/")))
  }
  args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(args)) {
    candidate <- sub("^--file=", "", args[1])
    if (file.exists(candidate)) return(dirname(normalizePath(candidate, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    candidate <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                          error = function(e) "")
    if (length(candidate) == 1L && !is.na(candidate) && nzchar(candidate) &&
        file.exists(candidate)) {
      return(dirname(normalizePath(candidate, winslash = "/")))
    }
  }
  stop("Guarde el script y ejecútelo con Source en RStudio.")
}
output_dir <- find_script_directory()
if (!is.null(EXCEL_FILE)) {
  excel_file <- EXCEL_FILE
} else if (interactive()) {
  message("Seleccione la base en español: Lobomycosis_base_datos.xlsx")
  excel_file <- file.choose()
} else {
  excel_file <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (!nzchar(excel_file)) {
    excel_file <- file.path(output_dir, "..", "data", "raw", "Lobomycosis_base_datos.xlsx")
  }
}
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
if (tolower(tools::file_ext(excel_file)) != "xlsx") stop("Seleccione un Excel .xlsx.")
if (!source_sheet %in% readxl::excel_sheets(excel_file)) stop("No se encontró la hoja: ", source_sheet)
message("Carpeta de salida (ubicación del script): ", output_dir)

# 3. Lectura y validación de publicaciones -------------------------------------------------------
publications <- as.data.frame(suppressMessages(readxl::read_excel(
  excel_file, sheet = source_sheet, col_types = "text", .name_repair = "unique")))
names(publications) <- trimws(names(publications))
required_columns <- c(id_column, year_column, type_column, method_column)
if (!all(required_columns %in% names(publications))) {
  stop("Faltan columnas: ", paste(setdiff(required_columns, names(publications)), collapse = ", "))
}
clean_text <- function(x) {
  value <- trimws(gsub("\u00a0", " ", as.character(x), fixed = TRUE))
  value[is.na(value)] <- ""
  gsub("[[:space:]]+", " ", value)
}
normalize_text <- function(x) {
  value <- iconv(tolower(clean_text(x)), to = "ASCII//TRANSLIT")
  value[is.na(value)] <- ""
  value
}
missing_values <- c("", "nd", "nr", "n.d.", "n.r.", "n/d", "n/r", "na", "n/a", "-",
                    "no informado", "no reportado", "no disponible", "none", "nan")
available <- function(x) !normalize_text(x) %in% missing_values
publications[[id_column]] <- clean_text(publications[[id_column]])
publications <- publications[available(publications[[id_column]]), , drop = FALSE]
if (!nrow(publications) || anyDuplicated(publications[[id_column]])) {
  stop("Hay identificadores de publicaciones vacíos o duplicados.")
}
if (any(!grepl("^[0-9]{4}$", clean_text(publications[[year_column]])))) {
  stop("Años de publicación inválidos.")
}
publications$year <- as.integer(publications[[year_column]])
publications$study_type <- clean_text(publications[[type_column]])
if (any(!publications$study_type %in% names(study_colors))) {
  stop("Tipos de estudio no reconocidos: ", paste(setdiff(unique(publications$study_type),
        names(study_colors)), collapse = "; "))
}
total_publications <- nrow(publications)
years <- seq(min(publications$year), max(publications$year))
study_types <- names(study_colors)
annual <- table(factor(publications$year, levels = years),
                factor(publications$study_type, levels = study_types))
stopifnot(sum(annual) == total_publications)
type_counts <- colSums(annual)

# 4. Periodos y métodos diagnósticos -------------------------------------------------------
periods <- data.frame(start = c(min(1962L, min(years)), 2000L, 2010L, 2020L),
                      end = c(1999L, 2009L, 2019L, max(2026L, max(years))))
periods$n <- vapply(seq_len(nrow(periods)), function(j) {
  sum(publications$year >= periods$start[j] & publications$year <= periods$end[j])
}, integer(1))
periods <- periods[periods$n > 0, , drop = FALSE]
periods$period <- paste0(periods$start, "–", periods$end)
stopifnot(sum(periods$n) == total_publications)

# La exclusión afecta solo a los métodos mostrados; no a la base original.
parse_methods <- function(value) {
  if (!available(value)) return("ND")
  parts <- unique(clean_text(strsplit(clean_text(value), "/", fixed = TRUE)[[1]]))
  parts <- parts[available(parts)]
  unknown <- parts[!normalize_text(parts) %in%
    normalize_text(c(method_names, excluded_method))]
  if (length(unknown)) stop("Método diagnóstico no reconocido: ", paste(unique(unknown), collapse = "; "))
  # No se incluye cultivo en el mapa de calor ni en los recuentos de métodos mostrados.
  parts <- parts[normalize_text(parts) != normalize_text(excluded_method)]
  if (!length(parts)) return(character(0))
  method_names[match(normalize_text(parts), normalize_text(method_names))]
}
method_sets <- lapply(publications[[method_column]], parse_methods)
raw_method_sets <- lapply(publications[[method_column]], function(value) {
  if (!available(value)) return(character(0))
  unique(clean_text(strsplit(clean_text(value), "/", fixed = TRUE)[[1]]))
})
excluded_flags <- vapply(raw_method_sets, function(parts)
  any(normalize_text(parts) == normalize_text(excluded_method)), logical(1))
# ND indica método no disponible; no significa ausencia del procedimiento.
nd_flags <- vapply(method_sets, function(parts) identical(parts, "ND"), logical(1))
method_count <- matrix(0L, nrow = length(method_names), ncol = nrow(periods),
  dimnames = list(method_names, periods$period))
for (j in seq_len(nrow(periods))) {
  selected <- which(publications$year >= periods$start[j] &
                    publications$year <= periods$end[j])
  for (i in seq_along(method_names)) {
    method_count[i, j] <- sum(vapply(method_sets[selected], function(values)
      method_names[i] %in% values, logical(1)))
  }
}
method_percent <- sweep(method_count, 2, periods$n, "/") * 100
stopifnot(all(method_percent >= 0 & method_percent <= 100),
          sum(type_counts) == total_publications,
          !excluded_method %in% rownames(method_count))

# 5. Dibujo de la figura -------------------------------------------------------
# Se conservan diseño, colores y fuentes; solo se elimina la fila de cultivo.
draw_figure <- function() {
  grid::grid.newpage()
  txt <- function(s, x, y, size = TEXT_SIZE, just = "left", bold = FALSE,
                  color = "#171717", rot = 0) {
    grid::grid.text(s, x, y, just = just, rot = rot,
      gp = grid::gpar(fontfamily = FONT_FAMILY, fontsize = size,
        fontface = if (bold) "bold" else "plain", col = color, lineheight = 1.05))
  }
  line <- function(x0, x1, y0, y1, color = "#242424", width = AXIS_WIDTH) {
    grid::grid.lines(c(x0, x1), c(y0, y1),
      gp = grid::gpar(col = color, lwd = width))
  }
  rect <- function(x0, x1, y0, y1, color, border = "#555555", width = .3) {
    grid::grid.rect((x0 + x1) / 2, (y0 + y1) / 2,
      width = x1 - x0, height = y1 - y0,
      gp = grid::gpar(fill = color, col = border, lwd = width))
  }
  A <- c(.065, .495); B <- c(.675, .960)
  bottom <- .320; top <- .860
  txt("A.", A[1], .935, 18, bold = TRUE)
  txt("B.", B[1], .935, 18, bold = TRUE)
  max_n <- max(rowSums(annual))
  y_max <- max_n + 1
  y_ticks <- pretty(c(0, y_max), n = 7)
  y_ticks <- unique(round(y_ticks[y_ticks >= 0 & y_ticks <= y_max]))
  xx <- function(y) A[1] + diff(A) * (y - min(years) + 1.5) / (length(years) + 2)
  yy <- function(v) bottom + (top - bottom) * v / y_max
  for (v in y_ticks) {
    line(A[1], A[2], yy(v), yy(v), "#E0E0E0", .6)
    line(A[1] - .003, A[1], yy(v), yy(v), width = 1)
    txt(as.character(v), A[1] - .008, yy(v), size = 13, just = "right")
  }
  bar_width <- diff(A) / (length(years) + 2) * .83
  for (i in seq_along(years)) {
    cumulative <- 0
    for (j in seq_along(study_types)) {
      count <- annual[i, j]
      if (count > 0) rect(xx(years[i]) - bar_width / 2,
        xx(years[i]) + bar_width / 2, yy(cumulative), yy(cumulative + count),
        study_colors[j])
      cumulative <- cumulative + count
    }
  }
  x_ticks <- sort(unique(c(min(years),
    seq(ceiling(min(years) / 10) * 10,
        floor(max(years) / 10) * 10, by = 10), max(years))))
  if (length(x_ticks) > 2) {
    x_ticks <- x_ticks[!(x_ticks != min(years) & x_ticks != max(years) &
                        (x_ticks - min(years) < 4 | max(years) - x_ticks < 4))]
  }
  for (v in x_ticks) {
    line(xx(v), xx(v), bottom, bottom - .005, width = 1)
    txt(as.character(v), xx(v), bottom - .022, size = 12, just = "centre")
  }
  line(A[1], A[2], bottom, bottom)
  line(A[1], A[1], bottom, top)
  txt("Número de publicaciones", .025, mean(c(bottom, top)), just = "centre", rot = 90)
  txt("Año de publicación", mean(A), bottom - .060, just = "centre")
  txt(paste0("Publicaciones por año y diseño", " (n = ", total_publications, ")"),
      mean(A), .215, just = "centre", bold = TRUE)
  for (j in seq_along(study_types)) {
    column <- (j - 1L) %/% 3L; row <- (j - 1L) %% 3L
    x <- A[1] + column * .235; y <- .151 - row * .029
    rect(x, x + .011, y - .006, y + .006, study_colors[j], width = .5)
    txt(paste0(study_types[j], " (n = ", type_counts[j], ")"), x + .017, y, size = 11.5)
  }
  # Cada celda es n/N (%), usando las publicaciones del periodo como denominador.
  cell_width <- diff(B) / ncol(method_count)
  cell_height <- (top - bottom) / nrow(method_count)
  scale_colors <- grDevices::colorRampPalette(c("#FFFFFF", "#D5BCE5", "#76518D"))(101)
  for (i in seq_len(nrow(method_count))) {
    y1 <- top - (i - 1L) * cell_height; y0 <- y1 - cell_height
    txt(method_names[i], B[1] - .008, mean(c(y0, y1)), size = 12, just = "right")
    for (j in seq_len(ncol(method_count))) {
      x0 <- B[1] + (j - 1L) * cell_width; x1 <- x0 + cell_width
      pct <- method_percent[i, j]
      rect(x0, x1, y0, y1, scale_colors[round(pct) + 1L], "#D9D9D9", .6)
      label <- paste0(method_count[i, j], "/", periods$n[j], "\n(",
                      gsub(".", ",", sprintf("%.1f", pct), fixed = TRUE), " %)")
      txt(label, mean(c(x0, x1)), mean(c(y0, y1)), size = 11,
          just = "centre", color = if (pct > 62) "white" else "#171717")
    }
  }
  line(B[1], B[2], bottom, bottom)
  line(B[1], B[1], bottom, top)
  for (j in seq_len(nrow(periods))) {
    txt(paste0(periods$period[j], "\n(n = ", periods$n[j], ")"),
        B[1] + (j - .5) * cell_width, bottom - .034, size = 12, just = "centre")
  }
  txt(paste0("Métodos informados por periodo", " (n = ", total_publications, ")"),
      mean(B), .215, just = "centre", bold = TRUE)
  color_bar <- c(B[1] + .04, B[2] - .025)
  for (i in 0:99) {
    rect(color_bar[1] + diff(color_bar) * i / 100,
         color_bar[1] + diff(color_bar) * (i + 1) / 100,
         .135, .153, scale_colors[i + 1L], border = NA)
  }
  for (v in c(0, 25, 50, 75, 100)) {
    x <- color_bar[1] + diff(color_bar) * v / 100
    line(x, x, .135, .130, width = .7)
    txt(as.character(v), x, .116, size = 11, just = "centre")
  }
  txt("Publicaciones del periodo (%)", mean(color_bar), .086, size = 12, just = "centre")
  if (SHOW_NOTES) {
    txt("Una publicación puede informar varios métodos; los porcentajes pueden sumar más del 100%.", A[1], .045, size = 10)
    txt("Cultivo se excluye del panel B; no se modificó la base de datos.", A[1], .023, size = 10)
  }
}

# 6. Recuentos y controles del manuscrito -------------------------------------------------------
# Los valores de referencia proceden de la Tabla 1, los resultados diagnósticos y la Figura S2 del manuscrito.
expected_type <- setNames(c(48L, 23L, 16L, 10L, 1L), study_types)
expected_period <- c(18L, 24L, 30L, 26L)
expected_method <- setNames(c(91L, 28L, 12L, 16L, 9L, 3L, 2L, 2L, 3L), method_names)
method_totals <- rowSums(method_count)
check_table <- data.frame(
  indicator = c("total_publications", paste0("type: ", study_types),
                paste0("period: ", periods$period),
                paste0("method: ", method_names), "methods_available", "molecular_2020_2026"),
  expected = c(98L, unname(expected_type), expected_period,
               unname(expected_method), 95L, 6L),
  observed = c(total_publications, unname(type_counts), periods$n,
               unname(method_totals), sum(!nd_flags),
               sum(vapply(seq_len(total_publications), function(i)
                 publications$year[i] >= 2020 && publications$year[i] <= 2026 &&
                 method_names[5] %in% method_sets[[i]], logical(1)))),
  stringsAsFactors = FALSE)
check_table$status <- ifelse(check_table$observed == check_table$expected, "PASS", "CHECK")
if (any(check_table$status == "CHECK")) warning("Algunos resultados difieren del manuscrito. Revise el Excel de auditoría.")

# Un solo CSV reúne recuentos anuales, totales por periodo y métodos.
annual_summary <- as.data.frame(as.table(annual), stringsAsFactors = FALSE)
names(annual_summary) <- c("year", "study_type", "n")
annual_summary$year <- as.integer(as.character(annual_summary$year))
annual_summary <- annual_summary[annual_summary$n > 0, , drop = FALSE]
annual_summary$denominator <- total_publications
annual_summary$percentage <- 100 * annual_summary$n / total_publications
annual_summary$period <- ""
annual_summary$diagnostic_method <- ""
annual_summary$section <- "publication_year_study_type"

method_summary <- expand.grid(method = method_names, period = periods$period,
                              stringsAsFactors = FALSE)
method_summary$n <- as.integer(mapply(function(method, period)
  method_count[method, period], method_summary$method, method_summary$period))
method_summary$denominator <- periods$n[match(method_summary$period, periods$period)]
method_summary$percentage <- 100 * method_summary$n / method_summary$denominator
method_summary$year <- NA_integer_
method_summary$study_type <- ""
method_summary$diagnostic_method <- method_summary$method
method_summary$section <- "diagnostic_method_by_period"
period_summary <- data.frame(section = "period_total", year = NA_integer_,
  study_type = "", diagnostic_method = "", period = periods$period,
  n = periods$n, denominator = total_publications,
  percentage = 100 * periods$n / total_publications, stringsAsFactors = FALSE)

summary_columns <- c("section", "year", "study_type", "diagnostic_method",
                     "period", "n", "denominator", "percentage")
summary_csv <- rbind(annual_summary[, summary_columns],
                     period_summary[, summary_columns],
                     method_summary[, summary_columns])

# Se conserva la trazabilidad por publicación, incluidas las menciones originales de cultivo.
trace <- data.frame(
  publication_id = publications[[id_column]],
  year = publications$year,
  study_type = publications$study_type,
  diagnostic_method_original = clean_text(publications[[method_column]]),
  diagnostic_method_displayed = vapply(method_sets, function(values)
    if (length(values)) paste(values, collapse = "/") else "ND", character(1)),
  excluded_method_mentioned = excluded_flags,
  missing_diagnostic_method = nd_flags,
  stringsAsFactors = FALSE)

# 7. Exportar los cuatro archivos -------------------------------------------------------
# Se protegen las ejecuciones anteriores con sufijos _run01, _run02, etc.
run_suffix <- ""
run_number <- 0L
repeat {
  output_paths <- file.path(output_dir, paste0(output_stem, run_suffix,
    c(".png", ".tiff", "_summary.csv", "_audit.xlsx")))
  if (!any(file.exists(output_paths))) break
  run_number <- run_number + 1L
  run_suffix <- sprintf("_run%02d", run_number)
}
export_figure <- function(path, type) {
  if (type == "png") {
    ragg::agg_png(path, width = FIGURE_WIDTH_CM, height = FIGURE_HEIGHT_CM,
                  units = "cm", res = DPI, background = "white")
  } else {
    ragg::agg_tiff(path, width = FIGURE_WIDTH_CM, height = FIGURE_HEIGHT_CM,
                   units = "cm", res = DPI, background = "white", compression = "lzw")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_figure()
}
export_figure(output_paths[1], "png")
export_figure(output_paths[2], "tiff")
# En español, el CSV usa punto y coma y coma decimal para abrirlo en Excel.
summary_export <- summary_csv
names(summary_export) <- c("seccion", "anio", "tipo_estudio", "metodo_diagnostico",
                           "periodo", "n", "denominador", "porcentaje")
summary_export$seccion <- c("publicaciones_por_anio_y_tipo", "total_por_periodo",
                            "metodo_por_periodo")[match(summary_export$seccion,
  c("publication_year_study_type", "period_total", "diagnostic_method_by_period"))]
utils::write.csv2(summary_export, output_paths[3], row.names = FALSE,
                  na = "", fileEncoding = "UTF-8")

# Todas las verificaciones, tablas y registros están en una sola hoja Excel.
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Audit")
section_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DFEAE5")
header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EAECEF")
title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 14,
                                      fgFill = "#DFEAE5")
put_section <- function(title, row) {
  openxlsx::writeData(wb, "Audit", title, startRow = row, startCol = 1)
  openxlsx::addStyle(wb, "Audit", section_style,
                     rows = row, cols = 1:8, gridExpand = TRUE)
}
put_table <- function(table, row) {
  openxlsx::writeData(wb, "Audit", table, startRow = row, startCol = 1)
  openxlsx::addStyle(wb, "Audit", header_style, rows = row,
                     cols = seq_len(ncol(table)), gridExpand = TRUE)
  row + nrow(table) + 3L
}
openxlsx::writeData(wb, "Audit", "Figura suplementaria S2 — auditoría de publicaciones y diagnóstico", startRow = 1)
openxlsx::addStyle(wb, "Audit", title_style,
                   rows = 1, cols = 1:8, gridExpand = TRUE)
metadata <- data.frame(
  source_workbook = basename(excel_file), source_sheet = source_sheet,
  total_publications = total_publications,
  excluded_method = excluded_method,
  mentions_in_source = sum(excluded_flags),
  displayed_in_panel_b = FALSE,
  note = "Cultivo no aparece en la figura ni en los recuentos de métodos mostrados; sus menciones originales se conservan para trazabilidad.", stringsAsFactors = FALSE)
row <- 3L
names(metadata) <- c("Base", "Hoja", "Publicaciones", "Metodo_excluido",
                     "Menciones_en_base", "Mostrado_en_figura", "Observacion")
row <- put_table(metadata, row)
put_section("CONTROLES DEL MANUSCRITO", row); row <- row + 1L
checks_export <- check_table
names(checks_export) <- c("Indicador", "Esperado", "Obtenido", "Estado")
row <- put_table(checks_export, row)
type_table <- data.frame(study_type = study_types, n = unname(type_counts),
  percentage = 100 * unname(type_counts) / total_publications)
names(type_table) <- c("Tipo_de_estudio", "N", "Porcentaje")
put_section("TIPOS DE ESTUDIO", row); row <- row + 1L
row <- put_table(type_table, row)
put_section("PUBLICACIONES POR PERIODO", row); row <- row + 1L
period_table <- periods[, c("period", "n")]
names(period_table) <- c("Periodo", "Publicaciones")
row <- put_table(period_table, row)
method_table <- data.frame(method = method_names, total = as.integer(rowSums(method_count)),
                           method_count, check.names = FALSE)
names(method_table)[1:2] <- c("Metodo", "Total")
put_section("MÉTODOS DIAGNÓSTICOS — RECUENTOS", row); row <- row + 1L
row <- put_table(method_table, row)
put_section("MÉTODOS DIAGNÓSTICOS — PORCENTAJES", row); row <- row + 1L
percent_table <- data.frame(method = method_names, round(method_percent, 1), check.names = FALSE)
names(percent_table)[1] <- "Metodo"
row <- put_table(percent_table, row)
put_section("PUBLICACIONES POR AÑO Y TIPO DE ESTUDIO", row); row <- row + 1L
annual_table <- data.frame(year = years, annual, check.names = FALSE)
names(annual_table)[1] <- "Anio"
row <- put_table(annual_table, row)
put_section("TRAZABILIDAD POR PUBLICACIÓN", row); row <- row + 1L
trace_header_row <- row
names(trace) <- c("ID_estudio", "Anio", "Tipo_estudio", "Metodo_original",
                  "Metodos_mostrados", "Cultivo_en_base", "Metodo_ND")
row <- put_table(trace, row)
openxlsx::freezePane(wb, "Audit", firstActiveRow = trace_header_row + 1L)
openxlsx::setColWidths(wb, "Audit", cols = 1:8,
                       widths = c(31, 28, 25, 39, 27, 22, 20, 20))
openxlsx::saveWorkbook(wb, output_paths[4], overwrite = FALSE)

if (!all(file.exists(output_paths)) || any(file.info(output_paths)$size <= 0)) {
  stop("No se pudieron generar todos los archivos.")
}
message("Cuatro archivos guardados junto al script:", "\n", paste(basename(output_paths), collapse = "\n"))
