# FIGURE 4. Anatomical distribution and lesion morphology
# Panel A summarizes anatomical sites; panel B summarizes lesion morphology.
# Each category is counted once per record, and percentages use available records.
# Input: the English individual-level worksheet in Lobomycosis_database.xlsx.
# Outputs: PNG, TIFF, a summary CSV and a one-sheet audit workbook beside this script.

# 1. Packages and configuration -------------------------------------------------------------
DPI <- 1200
FIGURE_WIDTH_CM <- 42
FIGURE_HEIGHT_CM <- 26
FONT_FAMILY <- "Arial"
TEXT_SIZE <- 14
AXIS_WIDTH <- 1.5
AXIS_COLOR <- "#242424"
LOCATION_COLOR <- "#A9C19F"
MORPHOLOGY_COLOR <- "#BBA0D0"
SHOW_NOTE <- FALSE
EXCEL_FILE <- NULL  # NULL opens file.choose(); a file path may also be supplied.

required_packages <- c("readxl", "ragg", "systemfonts", "openxlsx")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       "\nInstall them once using install.packages(), then run the script again.")
}

# The approved figure uses Arial regular and bold, without replacing either font.
installed_fonts <- systemfonts::system_fonts()
regular_font <- installed_fonts[
  tolower(installed_fonts$family) == "arial" & !installed_fonts$italic &
    !grepl("bold|heavy|black", installed_fonts$style, ignore.case = TRUE),
]
bold_font <- installed_fonts[
  tolower(installed_fonts$family) == "arial" & !installed_fonts$italic &
    grepl("bold", installed_fonts$style, ignore.case = TRUE),
]
if (!nrow(regular_font) || !nrow(bold_font)) stop("Arial Regular and Arial Bold are required to reproduce the original figure.")

# 2. Select the source workbook -------------------------------------------------------------
# Source() in RStudio provides the script path; the selected workbook may be elsewhere.
find_script_directory <- function() {
  loaded <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
  }, character(1))
  loaded <- loaded[!is.na(loaded) & nzchar(loaded)]
  if (length(loaded) && file.exists(tail(loaded, 1))) {
    return(dirname(normalizePath(tail(loaded, 1), winslash = "/")))
  }
  command_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(command_file)) {
    candidate <- sub("^--file=", "", command_file[1])
    if (file.exists(candidate)) return(dirname(normalizePath(candidate, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    candidate <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                          error = function(e) "")
    if (length(candidate) == 1 && !is.na(candidate) &&
        nzchar(candidate) && file.exists(candidate)) {
      return(dirname(normalizePath(candidate, winslash = "/")))
    }
  }
  stop("Save this script and execute it using Source in RStudio.")
}
output_dir <- find_script_directory()
output_stem <- "fig04_clinical_en"

if (!is.null(EXCEL_FILE)) {
  excel_file <- EXCEL_FILE
} else if (interactive()) {
  message("Select Lobomycosis_database.xlsx (English database).")
  excel_file <- file.choose()
} else {
  excel_file <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (!nzchar(excel_file)) {
    excel_file <- file.path(output_dir, "..", "data", "raw", "Lobomycosis_database.xlsx")
  }
}
if (!file.exists(excel_file)) stop("The selected Excel file could not be found.")
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
if (tolower(tools::file_ext(excel_file)) != "xlsx") stop("Expected an .xlsx file.")
message("Files will be saved beside the script: ", output_dir)

# 3. Read and validate individual records -------------------------------------------------------------
sheet_name <- "Individual-level data"
id_column <- "Record_ID"
location_column <- "Anatomical_location"
morphology_column <- "Lesion_morphology"
required_columns <- c(id_column, location_column, morphology_column)
if (!(sheet_name %in% readxl::excel_sheets(excel_file))) {
  stop("Worksheet not found: ", sheet_name)
}
records <- as.data.frame(suppressMessages(readxl::read_excel(
  excel_file, sheet = sheet_name, col_types = "text", .name_repair = "unique"
)))
names(records) <- trimws(names(records))
if (!all(required_columns %in% names(records))) {
  stop("Missing required columns: ", paste(setdiff(required_columns, names(records)), collapse = ", "))
}

clean_text <- function(x) {
  x <- trimws(gsub("\u00a0", " ", as.character(x), fixed = TRUE))
  x[is.na(x)] <- ""
  gsub("[[:space:]]+", " ", x)
}
normalize_text <- function(x) {
  x <- iconv(tolower(clean_text(x)), to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x
}
missing_values <- c("", "nd", "nr", "n.d.", "n.r.", "n/d", "n/r", "na",
                    "n/a", "-", "no informado", "no reportado", "no disponible",
                    "no descrito", "no especificado", "none", "nan")
records[[id_column]] <- clean_text(records[[id_column]])
records <- records[!normalize_text(records[[id_column]]) %in% missing_values,
                   , drop = FALSE]
if (!nrow(records)) stop("No individual records were found.")
if (anyDuplicated(records[[id_column]])) stop("Record identifiers are missing or duplicated.")
total_records <- nrow(records)

# 4. Summarize each variable -------------------------------------------------------------
# Values separated by "/" are separate categories, but a repeated value
# within the same record must not be counted twice.
split_categories <- function(value) {
  if (normalize_text(value) %in% missing_values) return(character(0))
  parts <- clean_text(strsplit(clean_text(value), "/", fixed = TRUE)[[1]])
  unique(parts[!normalize_text(parts) %in% missing_values])
}
summarize_variable <- function(column, label, preferred_order) {
  items <- lapply(records[[column]], split_categories)
  available <- lengths(items) > 0L
  available_n <- sum(available)
  if (!available_n) stop("No available data for: ", column)
  counts <- table(unlist(items[available], use.names = FALSE))
  result <- data.frame(category = names(counts), n = as.integer(counts),
                       stringsAsFactors = FALSE)
  result$percent <- 100 * result$n / available_n
  tie_order <- match(result$category, preferred_order)
  tie_order[is.na(tie_order)] <- length(preferred_order) + 1L
  result <- result[order(-result$n, tie_order, result$category), , drop = FALSE]
  rownames(result) <- NULL
  nd_n <- total_records - available_n
  stopifnot(available_n + nd_n == total_records, all(result$n <= available_n))
  list(name = label, data = result, available = available_n, nd = nd_n,
       available_flags = available, category_sets = items)
}
location_order <- c("Lower limb", "Upper limb", "Trunk", "Head and neck", "Extremities", "Lymph nodes", "Genital region")
morphology_order <- c("Nodular", "Keloid-like", "Plaque", "Ulcerative", "Tumoral", "Papular", "Verrucous", "Cicatricial", "Infiltrative", "Crusted", "Macular", "Hyperkeratotic")
location <- summarize_variable(location_column, "Anatomical location", location_order)
morphology <- summarize_variable(morphology_column, "Lesion morphology", morphology_order)

# Reference counts come from Table 2 and Figure 4 of the approved manuscript.
location_expected <- setNames(c(66, 52, 44, 34, 4, 3, 2), location_order)
morphology_expected <- setNames(c(87, 65, 40, 28, 18, 15, 13, 8, 4, 3, 2, 2), morphology_order)

# Preserve the original height unless there are more than 12 category rows.
plot_height_cm <- FIGURE_HEIGHT_CM
n_rows_max <- max(nrow(location$data), nrow(morphology$data))
if (n_rows_max > 12L) plot_height_cm <- max(plot_height_cm, 26 + (n_rows_max - 12L) * 1.3)

# 5. Construct the approved figure -------------------------------------------------------------
# The original grid layout, panel spacing, bars and font sizes are retained.
# Only the visible category labels and decimal separators differ by language.
format_percent <- function(value) {
  label <- sprintf("%.1f", value)
  label <- gsub(",", ".", label, fixed = TRUE)
  label
}
format_count <- function(n, pct) {
  paste0(n, " (", format_percent(pct), "%)")
}

draw_figure <- function() {
  grid::grid.newpage()
  draw_text <- function(text, x, y, size = TEXT_SIZE, align = "left", bold = FALSE) {
    grid::grid.text(text, x, y, just = align,
      gp = grid::gpar(fontfamily = FONT_FAMILY, fontsize = size,
                      fontface = if (bold) "bold" else "plain",
                      col = "#171717", lineheight = 1.1))
  }
  draw_line <- function(x0, x1, y0, y1, color = "#DDDDDD", width = .6) {
    grid::grid.lines(c(x0, x1), c(y0, y1),
                     gp = grid::gpar(col = color, lwd = width))
  }
  draw_panel <- function(summary, bounds, letter, fill_color) {
    top <- .865
    bottom <- .240
    step <- (top - bottom) / nrow(summary$data)
    y_positions <- top - (seq_len(nrow(summary$data)) - .5) * step
    bar_height <- step * .60
    x_for <- function(value) bounds[1] + diff(bounds) * value / 100

    draw_text(letter, bounds[1], .935, 18, bold = TRUE)
    for (tick in c(0, 25, 50, 75, 100)) {
      x <- x_for(tick)
      draw_line(x, x, bottom, top, "#E0E0E0", .6)
      draw_line(x, x, bottom, bottom - .006, AXIS_COLOR, 1.2)
      draw_text(as.character(tick), x, bottom - .022, align = "centre")
    }
    for (i in seq_len(nrow(summary$data))) {
      entry <- summary$data[i, ]
      width <- x_for(entry$percent) - bounds[1]
      grid::grid.rect(bounds[1] + width / 2, y_positions[i],
        width = width, height = bar_height,
        gp = grid::gpar(fill = fill_color, col = "#343434", lwd = .7))
      wrapped_label <- paste(strwrap(entry$category, width = 26), collapse = "\n")
      draw_text(wrapped_label, bounds[1] - .008, y_positions[i], align = "right")
      count_label <- format_count(entry$n, entry$percent)
      label_width <- grid::convertWidth(grid::grobWidth(grid::textGrob(
        count_label, gp = grid::gpar(fontfamily = FONT_FAMILY, fontsize = 13)
      )), "npc", valueOnly = TRUE)
      label_fits <- x_for(entry$percent) + .008 + label_width <= bounds[2]
      draw_text(count_label,
        x_for(entry$percent) + if (label_fits) .008 else -.008,
        y_positions[i], size = 13,
        align = if (label_fits) "left" else "right")
    }
    draw_line(bounds[1], bounds[2], bottom, bottom, AXIS_COLOR, AXIS_WIDTH)
    draw_line(bounds[1], bounds[1], bottom, top, AXIS_COLOR, AXIS_WIDTH)
    draw_text("Records with information (%)", mean(bounds), bottom - .060, align = "centre")
    draw_text(paste0(summary$name, " (n = ", summary$available, ")"),
              mean(bounds), .120, align = "centre", bold = TRUE)
    draw_text(paste0("ND (n = ", summary$nd, ")"),
              mean(bounds), .085, align = "centre")
  }
  draw_panel(location, c(.160, .460), "A.", LOCATION_COLOR)
  draw_panel(morphology, c(.655, .955), "B.", MORPHOLOGY_COLOR)
  if (SHOW_NOTE) draw_text("One record can have multiple locations or morphologies; percentages may exceed 100% in total.", .160, .030, size = 11)
}

# 6. Export figures and summary -------------------------------------------------------------
# A suffix protects previously generated outputs without changing the source Excel.
run_suffix <- ""
run_number <- 0L
repeat {
  expected_files <- file.path(output_dir, paste0(output_stem, run_suffix,
                                                  c(".png", ".tiff", "_summary.csv", "_audit.xlsx")))
  if (!any(file.exists(expected_files))) break
  run_number <- run_number + 1L
  run_suffix <- sprintf("_run%02d", run_number)
}

export_figure <- function(path, type) {
  if (type == "png") {
    ragg::agg_png(path, width = FIGURE_WIDTH_CM, height = plot_height_cm,
                  units = "cm", res = DPI, background = "white")
  } else {
    ragg::agg_tiff(path, width = FIGURE_WIDTH_CM, height = plot_height_cm,
                   units = "cm", res = DPI, background = "white", compression = "lzw")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_figure()
}

png_path <- file.path(output_dir, paste0(output_stem, run_suffix, ".png"))
tiff_path <- file.path(output_dir, paste0(output_stem, run_suffix, ".tiff"))
csv_path <- file.path(output_dir, paste0(output_stem, run_suffix, "_summary.csv"))
audit_path <- file.path(output_dir, paste0(output_stem, run_suffix, "_audit.xlsx"))

message("Exporting Figure 4 at ", DPI, " dpi...")
export_figure(png_path, "png")
export_figure(tiff_path, "tiff")

# The CSV is a compact machine-readable summary of both panels.
summary_csv <- rbind(
  data.frame(panel = "A", variable = location$name, location$data,
             available_n = location$available, nd_n = location$nd),
  data.frame(panel = "B", variable = morphology$name, morphology$data,
             available_n = morphology$available, nd_n = morphology$nd)
)
names(summary_csv) <- c("Panel", "Variable", "Category", "Count",
                        "Percent (%)", "Available n", "ND n")
utils::write.csv(summary_csv, csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

# 7. Create the single-sheet audit -------------------------------------------------------------
# One worksheet combines the manuscript checks, category frequencies and
# record-level source values; no extra Excel files or sheets are created.
get_actual <- function(summary, category) {
  value <- summary$data$n[match(category, summary$data$category)]
  if (is.na(value)) 0L else as.integer(value)
}

metrics <- c("Total individual records", "Anatomical location available", "Anatomical location ND",
             "Lesion morphology available", "Lesion morphology ND", "Total anatomical category assignments", "Total morphology category assignments")
expected_values <- c(256, 152, 104, 125, 131, 205, 285)
actual_values <- c(total_records, location$available, location$nd,
                   morphology$available, morphology$nd,
                   sum(location$data$n), sum(morphology$data$n))

checks <- data.frame(metric = metrics, expected = expected_values,
                     actual = actual_values, stringsAsFactors = FALSE)
for (category in location_order) {
  checks <- rbind(checks, data.frame(
    metric = paste0("A: ", category), expected = as.integer(location_expected[[category]]),
    actual = get_actual(location, category)))
}
for (category in morphology_order) {
  checks <- rbind(checks, data.frame(
    metric = paste0("B: ", category), expected = as.integer(morphology_expected[[category]]),
    actual = get_actual(morphology, category)))
}
checks$status <- ifelse(checks$expected == checks$actual, "PASS", "CHECK")
names(checks) <- c("Metric", "Manuscript", "Database", "Status")

# Preserve unexpected labels in the frequency table; do not silently recode them.
missing_or_extra <- c(
  setdiff(location$data$category, location_order),
  setdiff(location_order, location$data$category),
  setdiff(morphology$data$category, morphology_order),
  setdiff(morphology_order, morphology$data$category)
)
if (length(missing_or_extra)) message("Unexpected or missing categories: ", paste(unique(missing_or_extra), collapse = "; "))

traceability <- data.frame(
  id = records[[id_column]],
  original_location = clean_text(records[[location_column]]),
  location_available = ifelse(location$available_flags, "Yes", "ND"),
  location_categories = lengths(location$category_sets),
  original_morphology = clean_text(records[[morphology_column]]),
  morphology_available = ifelse(morphology$available_flags, "Yes", "ND"),
  morphology_categories = lengths(morphology$category_sets),
  stringsAsFactors = FALSE
)
names(traceability) <- c("Record ID", "Original anatomical location", "Anatomical location available",
                         "Number of categories A", "Original lesion morphology", "Lesion morphology available",
                         "Number of categories B")

workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(workbook, "Audit")
sheet <- "Audit"
openxlsx::writeData(workbook, sheet, "FIGURE 4 | SOURCE-DATA AUDIT", startRow = 1, startCol = 1)
openxlsx::writeData(workbook, sheet, paste0("Source workbook: ", basename(excel_file)),
                    startRow = 2, startCol = 1)
openxlsx::writeData(workbook, sheet, "A. MAIN COUNTS AND MANUSCRIPT CHECKS", startRow = 4, startCol = 1)
openxlsx::writeData(workbook, sheet, checks, startRow = 5, startCol = 1)
frequency_start <- 5 + nrow(checks) + 3
openxlsx::writeData(workbook, sheet, "B. CATEGORY FREQUENCIES (MULTIPLE RESPONSES ALLOWED)", startRow = frequency_start, startCol = 1)
openxlsx::writeData(workbook, sheet, summary_csv, startRow = frequency_start + 1, startCol = 1)
trace_start <- frequency_start + nrow(summary_csv) + 5
openxlsx::writeData(workbook, sheet, "C. RECORD-LEVEL TRACEABILITY", startRow = trace_start, startCol = 1)
openxlsx::writeData(workbook, sheet, traceability, startRow = trace_start + 1, startCol = 1)
note_start <- trace_start + nrow(traceability) + 4
openxlsx::writeData(workbook, sheet, "Percentages use the records with information for each variable; totals across categories may exceed 100%.", startRow = note_start, startCol = 1)
openxlsx::writeData(workbook, sheet, "ND indicates unavailable information. Duplicate mentions of the same category in one record are counted once.", startRow = note_start + 1, startCol = 1)
openxlsx::writeData(workbook, sheet, "Checks against Table 2 and Figure 4 of the manuscript; not an independent statistical validation.", startRow = note_start + 2, startCol = 1)

header_style <- openxlsx::createStyle(fgFill = "#35465B", fontColour = "#FFFFFF",
                                     textDecoration = "bold")
section_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E8EDF2")
for (row in c(1, 4, frequency_start, trace_start)) {
  openxlsx::addStyle(workbook, sheet, section_style,
                     rows = row, cols = 1:7, gridExpand = TRUE, stack = TRUE)
}
for (row in c(5, frequency_start + 1, trace_start + 1)) {
  openxlsx::addStyle(workbook, sheet, header_style,
                     rows = row, cols = 1:7, gridExpand = TRUE, stack = TRUE)
}
openxlsx::setColWidths(workbook, sheet, cols = 1, widths = 41)
openxlsx::setColWidths(workbook, sheet, cols = 2:4, widths = 24)
openxlsx::setColWidths(workbook, sheet, cols = 5, widths = 47)
openxlsx::setColWidths(workbook, sheet, cols = 6:7, widths = 22)
openxlsx::freezePane(workbook, sheet, firstActiveRow = 6)
openxlsx::saveWorkbook(workbook, audit_path, overwrite = FALSE)

outputs <- c(png_path, tiff_path, csv_path, audit_path)
if (!all(file.exists(outputs)) || any(file.info(outputs)$size <= 0)) {
  stop("One or more outputs were not created correctly.")
}
message("The four outputs were created: \n", paste(basename(outputs), collapse = "\n"))
message("Manuscript comparison: ", sum(checks[[4]] == "PASS"), "/", nrow(checks), " PASS")
