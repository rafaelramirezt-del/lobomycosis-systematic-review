# FIGURA SUPLEMENTARIA S3. Disponibilidad de información por publicación
# El panel A presenta los porcentajes de disponibilidad por variable.
# El panel B muestra la matriz de estudios ordenados por año de publicación.
# Trauma y comorbilidades se verifican con registros individuales vinculados.
# Cada versión genera cuatro archivos junto al script, sin modificar el Excel.

# 1. Configuración y comprobación de paquetes ---------------------------------------------
DPI <- 1200
FIGURE_WIDTH_CM <- 28
FIGURE_HEIGHT_CM <- 48
FONT_FAMILY <- "Arial"
COLOR_AVAILABLE <- "#D5BCE5"
COLOR_MISSING <- "#E1DBD3"
EXCEL_FILE <- NULL  # NULL abre file.choose(); también admite una ruta.

PUBLICATION_SHEET <- "Base de datos WORD"
INDIVIDUAL_SHEET <- "Datos a nivel individual"
PUB_ID <- "ID_estudio"
IND_STUDY_ID <- "ID_estudio"
IND_RECORD_ID <- "ID_registro"
YEAR_COLUMN <- "Ano_publicacion"
COUNTRY_COLUMN <- "Country_of_study"
OUTPUT_STEM <- "figs03_evidence_es"

variable_names <- c("Sexo", "Edad", "País de adquisición", "Región de adquisición",
  "Ocupación", "Exposición", "Trauma", "Localización anatómica",
  "Distribución clínica", "Morfología", "Tiempo de evolución",
  "Comorbilidades", "Métodos diagnósticos", "Tratamiento")
block_names <- c(rep("Epidemiología", 6), rep("Clínica", 6),
                 "Diagnóstico", "Tratamiento")
# Se utiliza la información por publicación, salvo trauma y comorbilidades.
publication_columns <- c("Sex_F_M", "Age, years (min–max)", "Pais_adquisicion",
  "Región probable de adquisición", "Ocupación", "Exposición",
  NA_character_, "Localización anatómica", "Distribucion_clinica",
  "Morfología de las lesiones", "Disease duration, years", NA_character_,
  "Metodo_diagnostico", "Tratamientos reportados")
individual_columns <- c("Sexo", "Edad_anos", "Probable country of acquisition",
  "Región probable de adquisición", "Ocupación y grupo poblacional", "Exposición",
  "Antecedente de trauma", "Localización anatómica", "Distribucion_clinica",
  "Morfología de las lesiones", "Tiempo de evolución, años", "Comorbilidades y estado inmunitario",
  "Metodo_diagnostico_principal", "Tratamiento_quirurgico")
packages <- c("readxl", "ggplot2", "patchwork", "ragg", "systemfonts", "openxlsx")
not_installed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(not_installed)) stop("Faltan paquetes de R: ", paste(not_installed, collapse = ", "))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(patchwork))

fonts <- systemfonts::system_fonts()
regular <- fonts[tolower(fonts$family) == tolower(FONT_FAMILY) & !fonts$italic &
                   !grepl("bold|heavy|black", fonts$style, ignore.case = TRUE), ]
if (!nrow(regular)) stop("Se requiere la fuente Arial regular instalada en el equipo.")

# 2. Seleccionar la base y definir las salidas junto al script ----------------------------
find_script_directory <- function() {
  candidate_files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
  }, character(1))
  candidate_files <- candidate_files[!is.na(candidate_files) & nzchar(candidate_files)]
  if (length(candidate_files) && file.exists(tail(candidate_files, 1))) {
    return(dirname(normalizePath(tail(candidate_files, 1), winslash = "/")))
  }
  arguments <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arguments)) {
    script_file <- sub("^--file=", "", arguments[1])
    if (file.exists(script_file)) return(dirname(normalizePath(script_file, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    script_file <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                            error = function(e) "")
    if (length(script_file) == 1L && !is.na(script_file) && nzchar(script_file) &&
        file.exists(script_file)) {
      return(dirname(normalizePath(script_file, winslash = "/")))
    }
  }
  stop("Guarde el script y ejecútelo completo mediante Source en RStudio.")
}
output_dir <- find_script_directory()
if (is.null(EXCEL_FILE)) {
  message("Seleccione la base en español: Lobomycosis_base_datos.xlsx")
  excel_file <- file.choose()
} else {
  excel_file <- EXCEL_FILE
}
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
if (tolower(tools::file_ext(excel_file)) != "xlsx") stop("Seleccione un archivo Excel .xlsx.")
sheets <- readxl::excel_sheets(excel_file)
if (!all(c(PUBLICATION_SHEET, INDIVIDUAL_SHEET) %in% sheets)) {
  stop("Faltan hojas requeridas: ", paste(setdiff(c(PUBLICATION_SHEET, INDIVIDUAL_SHEET), sheets), collapse = ", "))
}
read_sheet <- function(sheet_name) {
  dat <- as.data.frame(suppressMessages(readxl::read_excel(
    excel_file, sheet = sheet_name, col_types = "text", .name_repair = "unique")),
    stringsAsFactors = FALSE, check.names = FALSE)
  names(dat) <- trimws(names(dat))
  dat
}
publications <- read_sheet(PUBLICATION_SHEET)
individuals <- read_sheet(INDIVIDUAL_SHEET)
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
missing_values <- c("", "nd", "nr", "n.d.", "n.r.", "n/d", "n/r", "na", "n/a",
                    "-", "no informado", "no reportado", "no disponible",
                    "no descrito", "no especificado", "none", "nan")
available <- function(x) !normalize_text(x) %in% missing_values

required_publication <- unique(c(PUB_ID, YEAR_COLUMN, COUNTRY_COLUMN,
                                 publication_columns[!is.na(publication_columns)]))
required_individual <- unique(c(IND_RECORD_ID, IND_STUDY_ID,
                                individual_columns[c(7, 12)]))
if (!all(required_publication %in% names(publications))) {
  stop("Faltan columnas en la hoja de publicaciones: ", paste(setdiff(required_publication, names(publications)), collapse = ", "))
}
if (!all(required_individual %in% names(individuals))) {
  stop("Faltan columnas en la hoja de registros individuales: ", paste(setdiff(required_individual, names(individuals)), collapse = ", "))
}
publications[[PUB_ID]] <- clean_text(publications[[PUB_ID]])
individuals[[IND_RECORD_ID]] <- clean_text(individuals[[IND_RECORD_ID]])
individuals[[IND_STUDY_ID]] <- clean_text(individuals[[IND_STUDY_ID]])
publications <- publications[grepl("^LBM-[0-9]+$", publications[[PUB_ID]]), , drop = FALSE]
individuals <- individuals[grepl("^LBM-[0-9]+$", individuals[[IND_STUDY_ID]]), , drop = FALSE]
if (!nrow(publications) || anyDuplicated(publications[[PUB_ID]])) stop("Los identificadores de publicaciones son inválidos o están repetidos.")
if (anyDuplicated(individuals[[IND_RECORD_ID]]) || any(!available(individuals[[IND_RECORD_ID]]))) {
  stop("Hay identificadores de casos vacíos o repetidos.")
}
if (any(!individuals[[IND_STUDY_ID]] %in% publications[[PUB_ID]])) {
  stop("Hay registros individuales vinculados a una publicación desconocida.")
}
publications$year_for_figure <- suppressWarnings(as.integer(publications[[YEAR_COLUMN]]))
if (anyNA(publications$year_for_figure)) stop("Hay años de publicación inválidos.")
id_number <- as.integer(sub("^LBM-", "", publications[[PUB_ID]]))
publications <- publications[order(publications$year_for_figure, id_number), , drop = FALSE]
rownames(publications) <- NULL
n_publications <- nrow(publications)
n_individuals <- nrow(individuals)
n_variables <- length(variable_names)
publications$figure_row <- seq.int(n_publications, 1L)

# 3. Contar la disponibilidad de cada variable una vez por publicación --------------------
# Solo trauma y comorbilidades se obtienen de los casos individuales; dichos registros
# no sustituyen los datos ausentes de las otras doce variables por publicación.
individuals_by_study <- split(individuals, individuals[[IND_STUDY_ID]])
availability_matrix <- matrix(0L, nrow = n_publications, ncol = n_variables,
                              dimnames = list(publications[[PUB_ID]], variable_names))
for (j in seq_len(n_variables)) {
  study_column <- publication_columns[j]
  if (!is.na(study_column)) {
    is_reported <- available(publications[[study_column]])
  } else {
    individual_column <- individual_columns[j]
    is_reported <- vapply(publications[[PUB_ID]], function(study_id) {
      cases <- individuals_by_study[[study_id]]
      if (is.null(cases) || !nrow(cases)) return(FALSE)
      any(available(cases[[individual_column]]))
    }, logical(1))
  }
  availability_matrix[, j] <- as.integer(is_reported)
}
reported_counts <- colSums(availability_matrix)
missing_counts <- n_publications - reported_counts
percentages <- 100 * reported_counts / n_publications
stopifnot(all(reported_counts + missing_counts == n_publications),
          length(availability_matrix) == n_publications * n_variables)
variable_summary <- data.frame(
  variable = variable_names,
  publications_with_information = as.integer(reported_counts),
  publications_without_information = as.integer(missing_counts),
  denominator = n_publications,
  percentage = round(as.numeric(percentages), 1),
  source_level = ifelse(is.na(publication_columns), "Datos individuales", "Publicaciones"),
  stringsAsFactors = FALSE, check.names = FALSE)

long_data <- do.call(rbind, lapply(seq_len(n_variables), function(j) {
  data.frame(publication_id = publications[[PUB_ID]],
             publication_year = publications$year_for_figure,
             figure_row = publications$figure_row,
             variable_index = j, variable = variable_names[j],
             is_reported = availability_matrix[, j] == 1L,
             state = ifelse(availability_matrix[, j] == 1L, "Información disponible", "ND"),
             stringsAsFactors = FALSE)
}))
long_data$state <- factor(long_data$state, levels = c("Información disponible", "ND"))

# 4. Construir los dos paneles de la matriz original --------------------------------------
# Las barras superiores y la matriz continua de 98 × 14 conservan la disposición original.
block_headings <- block_names
if (n_variables >= 2L && block_names[13] == "Diagnóstico" &&
    block_names[14] == "Tratamiento") {
  block_headings[13:14] <- "Diagnóstico y\ntratamiento"
}
block_centers <- do.call(rbind, lapply(unique(block_headings), function(block_label) {
  data.frame(variable_index = mean(which(block_headings == block_label)),
             label = block_label, stringsAsFactors = FALSE)
}))
figure_theme <- ggplot2::theme_classic(base_family = FONT_FAMILY, base_size = 10) + ggplot2::theme(
  text = ggplot2::element_text(family = FONT_FAMILY, face = "plain", colour = "black"),
  axis.title = ggplot2::element_text(size = 10, face = "plain"),
  axis.text = ggplot2::element_text(size = 9, colour = "black"),
  panel.grid = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(4, 5, 4, 4),
  legend.title = ggplot2::element_blank(),
  legend.text = ggplot2::element_text(size = 10, face = "plain"),
  legend.key = ggplot2::element_rect(colour = "black", linewidth = 0.25),
  legend.key.width = grid::unit(4, "mm"),
  legend.key.height = grid::unit(4, "mm"),
  legend.spacing.x = grid::unit(3, "mm")
)
format_pct <- function(x) {
  label <- sprintf("%.1f", x)
  if ("es" == "es") label <- gsub(".", ",", label, fixed = TRUE)
  label
}
bar_plot <- ggplot2::ggplot(variable_summary, ggplot2::aes(x = seq_len(n_variables), y = percentages)) +
  ggplot2::geom_col(fill = COLOR_AVAILABLE, colour = "black", linewidth = 0.3, width = 0.72) +
  ggplot2::geom_text(ggplot2::aes(y = percentages + 2.4,
                      label = vapply(percentages, format_pct, character(1))),
                     family = FONT_FAMILY, fontface = "plain", colour = "black",
                     size = 3, vjust = 0) +
  ggplot2::scale_x_continuous(limits = c(0.5, n_variables + 0.5), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0, 111), breaks = c(0, 25, 50, 75, 100),
                              expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = "Publicaciones con información (%)", tag = "A.") + figure_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(colour = "#DDDDDD", linewidth = 0.15),
                 plot.tag = ggplot2::element_text(family = FONT_FAMILY, face = "plain", size = 11))

# La matriz es continua: sin bordes de celda ni divisiones entre bloques.
matrix_plot <- ggplot2::ggplot(long_data,
                              ggplot2::aes(x = variable_index, y = figure_row, fill = state)) +
  ggplot2::geom_raster(interpolate = FALSE) +
  ggplot2::scale_fill_manual(values = c("Información disponible" = COLOR_AVAILABLE,
                                        "ND" = COLOR_MISSING),
                             labels = c("Información disponible", "ND: dato no disponible"), drop = FALSE) +
  ggplot2::scale_x_continuous(limits = c(0.5, n_variables + 0.5),
                              breaks = seq_len(n_variables), labels = variable_names,
                              expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0.5, n_publications + 0.5),
                              breaks = publications$figure_row,
                              labels = paste0(publications[[PUB_ID]], "  ·  ",
                                              publications$year_for_figure),
                              expand = c(0, 0)) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::geom_text(data = block_centers,
                     ggplot2::aes(x = variable_index, y = Inf, label = label),
                     inherit.aes = FALSE, family = FONT_FAMILY, fontface = "plain",
                     colour = "black", size = 3.3, lineheight = 0.95, vjust = -0.6) +
  ggplot2::labs(x = NULL, y = "Publicaciones ordenadas por año", tag = "B.") + figure_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
  ggplot2::theme(axis.line = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
                 axis.text.y = ggplot2::element_text(size = 8.6),
                 panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.3),
                 plot.tag = ggplot2::element_text(family = FONT_FAMILY, face = "plain", size = 11),
                 plot.margin = ggplot2::margin(28, 5, 4, 4))

figure <- (bar_plot / matrix_plot) +
  patchwork::plot_layout(heights = c(1.8, max(10, n_publications * 0.154)), guides = "collect") +
  patchwork::plot_annotation(
    title = paste0("Disponibilidad de información por publicación y variable (n = ", n_publications, ")"),
    theme = ggplot2::theme(plot.title = ggplot2::element_text(
      family = FONT_FAMILY, face = "plain", size = 12),
      plot.margin = ggplot2::margin(7, 7, 7, 7)))
figure <- figure & ggplot2::theme(
  legend.position = "top", legend.direction = "horizontal",
  legend.box.just = "left", legend.justification = "left")

# 5. Auditoría de los totales y de la disponibilidad por publicación ----------------------
reference_counts <- c(86L, 82L, 83L, 78L, 50L, 48L, 21L, 81L, 79L,
                      77L, 66L, 20L, 95L, 57L)
check_names <- c("Publicaciones incluidas", "Registros individuales", "Variables",
                 "Celdas publicación-variable", "Celdas con información", "Celdas sin información",
                 paste0("Información disponible: ", variable_names))
observed <- c(n_publications, n_individuals, n_variables,
              n_publications * n_variables, sum(reported_counts), sum(missing_counts),
              as.integer(reported_counts))
expected <- c(98L, 256L, 14L, 1372L, 923L, 449L, reference_counts)
checks <- data.frame(check = check_names, observed = observed, expected = expected,
                     result = ifelse(observed == expected, "PASS", "FAIL"),
                     stringsAsFactors = FALSE)
stopifnot(nrow(checks) == 20L)

source_dictionary <- data.frame(
  variable = variable_names,
  source_level = ifelse(is.na(publication_columns), "Datos individuales", "Publicaciones"),
  publication_column = ifelse(is.na(publication_columns), "", publication_columns),
  individual_column = ifelse(is.na(publication_columns), individual_columns, ""),
  stringsAsFactors = FALSE)

trace <- data.frame(
  publication_id = publications[[PUB_ID]],
  publication_year = publications$year_for_figure,
  country_of_study = clean_text(publications[[COUNTRY_COLUMN]]),
  stringsAsFactors = FALSE)
trace_availability <- as.data.frame(availability_matrix, optional = TRUE)
names(trace_availability) <- variable_names
trace <- cbind(trace, trace_availability)
trace$variables_with_information <- rowSums(availability_matrix)
trace$variables_without_information <- n_variables - trace$variables_with_information
stopifnot(sum(trace$variables_with_information) == sum(reported_counts),
          sum(trace$variables_without_information) == sum(missing_counts))

# 6. Exportar solo PNG, TIFF, CSV y Excel de auditoría de una hoja -------------------------
# Los sufijos _run01, _run02... evitan sobrescribir archivos anteriores.
run_suffix <- ""
run_number <- 0L
repeat {
  output_paths <- file.path(output_dir, paste0(OUTPUT_STEM, run_suffix,
    c(".png", ".tiff", "_summary.csv", "_audit.xlsx")))
  if (!any(file.exists(output_paths))) break
  run_number <- run_number + 1L
  run_suffix <- sprintf("_run%02d", run_number)
}
export_graphic <- function(path, type) {
  if (type == "png") {
    ragg::agg_png(path, width = FIGURE_WIDTH_CM, height = FIGURE_HEIGHT_CM,
                  units = "cm", res = DPI, background = "white", snap_rect = TRUE)
  } else {
    ragg::agg_tiff(path, width = FIGURE_WIDTH_CM, height = FIGURE_HEIGHT_CM,
                   units = "cm", res = DPI, background = "white",
                   compression = "lzw", snap_rect = TRUE)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  print(figure)
}
export_graphic(output_paths[1], "png")
export_graphic(output_paths[2], "tiff")
utils::write.csv(variable_summary, output_paths[3], row.names = FALSE,
                 na = "", fileEncoding = "UTF-8")

workbook <- openxlsx::createWorkbook()
audit_sheet <- "Auditoria"
openxlsx::addWorksheet(workbook, audit_sheet)
title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 14, fgFill = "#E9DDF2")
section_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E9DDF2")
header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EAECEF")
section <- function(text, row) {
  openxlsx::writeData(workbook, audit_sheet, text, startRow = row, startCol = 1)
  openxlsx::addStyle(workbook, audit_sheet, section_style,
                     rows = row, cols = 1:8, gridExpand = TRUE)
}
write_table <- function(data, start_row) {
  openxlsx::writeData(workbook, audit_sheet, data, startRow = start_row, startCol = 1)
  openxlsx::addStyle(workbook, audit_sheet, header_style,
                     rows = start_row, cols = seq_len(ncol(data)), gridExpand = TRUE)
  start_row + nrow(data) + 3L
}
openxlsx::writeData(workbook, audit_sheet, "Figura suplementaria S3 — auditoría de disponibilidad de información", startRow = 1)
openxlsx::addStyle(workbook, audit_sheet, title_style,
                   rows = 1, cols = 1:8, gridExpand = TRUE)
meta <- data.frame(
  source_workbook = basename(excel_file), publication_sheet = PUBLICATION_SHEET,
  individual_sheet = INDIVIDUAL_SHEET, total_publications = n_publications,
  individual_records = n_individuals, variables = n_variables,
  data_cells = n_publications * n_variables,
  reported_cells = sum(reported_counts), stringsAsFactors = FALSE)
row <- 3L
row <- write_table(meta, row)
section("CONTROLES DE REFERENCIA — VERIFICADOS EN AMBAS BASES", row); row <- row + 1L
row <- write_table(checks, row)
section("RESUMEN POR VARIABLE (DENOMINADOR: PUBLICACIONES)", row); row <- row + 1L
row <- write_table(variable_summary, row)
section("COLUMNAS FUENTE Y UNIDAD DE MEDICIÓN", row); row <- row + 1L
row <- write_table(source_dictionary, row)
section("MATRIZ DE DISPONIBILIDAD POR PUBLICACIÓN (1 = DISPONIBLE; 0 = ND)", row); row <- row + 1L
row <- write_table(trace, row)
openxlsx::freezePane(workbook, audit_sheet, firstActiveRow = 3L)
openxlsx::setColWidths(workbook, audit_sheet,
                       cols = 1:(n_variables + 5L), widths = 22)
openxlsx::setColWidths(workbook, audit_sheet, cols = c(1, 3, 4),
                       widths = c(35, 27, 31))
openxlsx::saveWorkbook(workbook, output_paths[4], overwrite = FALSE)

if (!all(file.exists(output_paths)) || any(file.info(output_paths)$size <= 0L)) {
  stop("Falta algún archivo de salida o está vacío.")
}
message("Se guardaron cuatro archivos junto al script:", "\n", paste(basename(output_paths), collapse = "\n"))
message("Publicaciones / casos; celdas informadas / celdas totales; controles aprobados: ", n_publications, " / ", n_individuals,
        "; ", sum(reported_counts), " / ", n_publications * n_variables,
        "; ", sum(checks$result == "PASS"), " / ", nrow(checks))
if (any(checks$result == "FAIL")) warning("La base difiere de los valores previamente verificados. Revise la auditoría.")
if (interactive()) print(figure)
