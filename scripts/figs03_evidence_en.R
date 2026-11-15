# SUPPLEMENTARY FIGURE S3. Evidence availability across included publications
# Panel A shows the proportion of publications with each type of information.
# Panel B shows study-level availability ordered by publication year.
# Trauma and comorbidities are scored using linked individual records only.
# Each version exports four files beside the script; neither workbook is modified.

# 1. Settings and package checks ---------------------------------------------------------
DPI <- 1200
FIGURE_WIDTH_CM <- 28
FIGURE_HEIGHT_CM <- 48
FONT_FAMILY <- "Arial"
COLOR_AVAILABLE <- "#D5BCE5"
COLOR_MISSING <- "#E1DBD3"
EXCEL_FILE <- NULL  # NULL opens file.choose(); an explicit path can also be used.

PUBLICATION_SHEET <- "Publication-level data"
INDIVIDUAL_SHEET <- "Individual-level data"
PUB_ID <- "Study_ID"
IND_STUDY_ID <- "Study_ID"
IND_RECORD_ID <- "Record_ID"
YEAR_COLUMN <- "Publication_year"
COUNTRY_COLUMN <- "Country_of_study"
OUTPUT_STEM <- "figs03_evidence_en"

variable_names <- c("Sex", "Age", "Country of acquisition", "Region of acquisition",
  "Occupation", "Exposure", "Trauma", "Anatomical location",
  "Clinical distribution", "Lesion morphology", "Disease duration",
  "Comorbidities", "Diagnostic methods", "Treatment")
block_names <- c(rep("Epidemiology", 6), rep("Clinical", 6),
                 "Diagnosis", "Treatment")
# Study-level columns define availability, except trauma and comorbidities.
publication_columns <- c("Sex_F_M", "Age_years_min_max", "Country_of_acquisition",
  "Probable_region_of_acquisition", "Occupation", "Exposure",
  NA_character_, "Anatomical_location", "Clinical_distribution",
  "Lesion_morphology", "Disease_duration_years", NA_character_,
  "Diagnostic_method", "Reported_treatments")
individual_columns <- c("Sex", "Age_years", "Probable_country_of_acquisition",
  "Probable_region_of_acquisition", "Occupation_and_population_group", "Exposure",
  "History_of_trauma", "Anatomical_location", "Clinical_distribution",
  "Lesion_morphology", "Disease_duration_years", "Comorbidities_and_immune_status",
  "Primary_diagnostic_method", "Surgical_treatment")
packages <- c("readxl", "ggplot2", "patchwork", "ragg", "systemfonts", "openxlsx")
not_installed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(not_installed)) stop("Required R packages are missing: ", paste(not_installed, collapse = ", "))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(patchwork))

fonts <- systemfonts::system_fonts()
regular <- fonts[tolower(fonts$family) == tolower(FONT_FAMILY) & !fonts$italic &
                   !grepl("bold|heavy|black", fonts$style, ignore.case = TRUE), ]
if (!nrow(regular)) stop("Arial Regular must be installed on your computer.")

# 2. Read the source Excel; save the four outputs beside this R script --------------------
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
  stop("Save this script and run the complete file using Source in RStudio.")
}
output_dir <- find_script_directory()
if (is.null(EXCEL_FILE)) {
  message("Select the English workbook: Lobomycosis_database.xlsx")
  excel_file <- file.choose()
} else {
  excel_file <- EXCEL_FILE
}
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
if (tolower(tools::file_ext(excel_file)) != "xlsx") stop("Select an .xlsx workbook.")
sheets <- readxl::excel_sheets(excel_file)
if (!all(c(PUBLICATION_SHEET, INDIVIDUAL_SHEET) %in% sheets)) {
  stop("Missing required worksheet(s): ", paste(setdiff(c(PUBLICATION_SHEET, INDIVIDUAL_SHEET), sheets), collapse = ", "))
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
  stop("Missing publication-level columns: ", paste(setdiff(required_publication, names(publications)), collapse = ", "))
}
if (!all(required_individual %in% names(individuals))) {
  stop("Missing individual-level columns: ", paste(setdiff(required_individual, names(individuals)), collapse = ", "))
}
publications[[PUB_ID]] <- clean_text(publications[[PUB_ID]])
individuals[[IND_RECORD_ID]] <- clean_text(individuals[[IND_RECORD_ID]])
individuals[[IND_STUDY_ID]] <- clean_text(individuals[[IND_STUDY_ID]])
publications <- publications[grepl("^LBM-[0-9]+$", publications[[PUB_ID]]), , drop = FALSE]
individuals <- individuals[grepl("^LBM-[0-9]+$", individuals[[IND_STUDY_ID]]), , drop = FALSE]
if (!nrow(publications) || anyDuplicated(publications[[PUB_ID]])) stop("Publication IDs are missing or duplicated.")
if (anyDuplicated(individuals[[IND_RECORD_ID]]) || any(!available(individuals[[IND_RECORD_ID]]))) {
  stop("Individual record IDs are missing or duplicated.")
}
if (any(!individuals[[IND_STUDY_ID]] %in% publications[[PUB_ID]])) {
  stop("Some individual records reference an unknown publication.")
}
publications$year_for_figure <- suppressWarnings(as.integer(publications[[YEAR_COLUMN]]))
if (anyNA(publications$year_for_figure)) stop("Invalid publication years.")
id_number <- as.integer(sub("^LBM-", "", publications[[PUB_ID]]))
publications <- publications[order(publications$year_for_figure, id_number), , drop = FALSE]
rownames(publications) <- NULL
n_publications <- nrow(publications)
n_individuals <- nrow(individuals)
n_variables <- length(variable_names)
publications$figure_row <- seq.int(n_publications, 1L)

# 3. Score availability once per study and variable --------------------------------------
# Individual records are used only for trauma and comorbidities; no individual
# entry fills a missing publication-level field for the other 12 variables.
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
  source_level = ifelse(is.na(publication_columns), "Individual-level data", "Publication-level data"),
  stringsAsFactors = FALSE, check.names = FALSE)

long_data <- do.call(rbind, lapply(seq_len(n_variables), function(j) {
  data.frame(publication_id = publications[[PUB_ID]],
             publication_year = publications$year_for_figure,
             figure_row = publications$figure_row,
             variable_index = j, variable = variable_names[j],
             is_reported = availability_matrix[, j] == 1L,
             state = ifelse(availability_matrix[, j] == 1L, "Information available", "ND"),
             stringsAsFactors = FALSE)
}))
long_data$state <- factor(long_data$state, levels = c("Information available", "ND"))

# 4. Draw the original two-panel evidence matrix -----------------------------------------
# The upper bar plot and 98-by-14 uninterrupted raster use the original layout.
block_headings <- block_names
if (n_variables >= 2L && block_names[13] == "Diagnosis" &&
    block_names[14] == "Treatment") {
  block_headings[13:14] <- "Diagnosis and\ntreatment"
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
  if ("en" == "es") label <- gsub(".", ",", label, fixed = TRUE)
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
  ggplot2::labs(x = NULL, y = "Publications with information (%)", tag = "A.") + figure_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(colour = "#DDDDDD", linewidth = 0.15),
                 plot.tag = ggplot2::element_text(family = FONT_FAMILY, face = "plain", size = 11))

# Raster is deliberately not separated with cell boundaries or block dividers.
matrix_plot <- ggplot2::ggplot(long_data,
                              ggplot2::aes(x = variable_index, y = figure_row, fill = state)) +
  ggplot2::geom_raster(interpolate = FALSE) +
  ggplot2::scale_fill_manual(values = c("Information available" = COLOR_AVAILABLE,
                                        "ND" = COLOR_MISSING),
                             labels = c("Information available", "ND: information unavailable"), drop = FALSE) +
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
  ggplot2::labs(x = NULL, y = "Publications ordered by year", tag = "B.") + figure_theme +
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
    title = paste0("Information availability by publication and variable (n = ", n_publications, ")"),
    theme = ggplot2::theme(plot.title = ggplot2::element_text(
      family = FONT_FAMILY, face = "plain", size = 12),
      plot.margin = ggplot2::margin(7, 7, 7, 7)))
figure <- figure & ggplot2::theme(
  legend.position = "top", legend.direction = "horizontal",
  legend.box.just = "left", legend.justification = "left")

# 5. Source audit: variable totals and each publication's available fields --------------
reference_counts <- c(86L, 82L, 83L, 78L, 50L, 48L, 21L, 81L, 79L,
                      77L, 66L, 20L, 95L, 57L)
check_names <- c("Included publications", "Individual records", "Variables",
                 "Publication-variable cells", "Available cells", "Missing cells",
                 paste0("Information available: ", variable_names))
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
  source_level = ifelse(is.na(publication_columns), "Individual-level data", "Publication-level data"),
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

# 6. Export only PNG, TIFF, CSV and a one-sheet Excel audit -------------------------------
# _run01, _run02... avoid overwriting previous completed runs.
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
audit_sheet <- "Audit"
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
openxlsx::writeData(workbook, audit_sheet, "Supplementary Figure S3 — evidence availability and source audit", startRow = 1)
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
section("REFERENCE COUNTS — VERIFIED AGAINST BOTH ORIGINAL DATABASES", row); row <- row + 1L
row <- write_table(checks, row)
section("VARIABLE SUMMARY (PUBLICATION DENOMINATOR)", row); row <- row + 1L
row <- write_table(variable_summary, row)
section("SOURCE COLUMNS AND LEVEL OF MEASUREMENT", row); row <- row + 1L
row <- write_table(source_dictionary, row)
section("PUBLICATION-LEVEL AVAILABILITY MATRIX (1 = AVAILABLE; 0 = ND)", row); row <- row + 1L
row <- write_table(trace, row)
openxlsx::freezePane(workbook, audit_sheet, firstActiveRow = 3L)
openxlsx::setColWidths(workbook, audit_sheet,
                       cols = 1:(n_variables + 5L), widths = 22)
openxlsx::setColWidths(workbook, audit_sheet, cols = c(1, 3, 4),
                       widths = c(35, 27, 31))
openxlsx::saveWorkbook(workbook, output_paths[4], overwrite = FALSE)

if (!all(file.exists(output_paths)) || any(file.info(output_paths)$size <= 0L)) {
  stop("An output file is missing or empty.")
}
message("Four files saved beside this R script:", "\n", paste(basename(output_paths), collapse = "\n"))
message("Publications / individual records; available cells / all cells; checks passed: ", n_publications, " / ", n_individuals,
        "; ", sum(reported_counts), " / ", n_publications * n_variables,
        "; ", sum(checks$result == "PASS"), " / ", nrow(checks))
if (any(checks$result == "FAIL")) warning("The source data differ from the independently verified baseline. Inspect the audit.")
if (interactive()) print(figure)
