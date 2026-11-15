# SUPPLEMENTARY FIGURE S1. Disease duration by clinical distribution
# Panel A shows exact durations as boxplots and individual records; panel B shows lower bounds separately.
# The figure is descriptive: censored lower bounds are not treated as exact durations.
# Input: Lobomycosis_database.xlsx; output: PNG, TIFF, summary CSV, one-sheet Excel audit.

# 1. Packages and configuration -------------------------------------------------
DPI <- 1200
FIGURE_WIDTH_CM <- 42
FIGURE_HEIGHT_CM <- 26
FONT_FAMILY <- "Arial"
TEXT_SIZE <- 14
AXIS_WIDTH <- 1.5
AXIS_COLOR <- "#242424"
GROUP_COLORS <- c("Localized" = "#A9C19F", "Disseminated" = "#BBA0D0")
SHOW_NOTES <- FALSE
JITTER_SEED <- 1999
EXCEL_FILE <- NULL  # NULL opens file.choose(); a path is also accepted.

required_packages <- c("readxl", "ragg", "systemfonts", "openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing packages: ",
  paste(missing_packages, collapse = ", "),
  ". Install once with install.packages(), then rerun.")

fonts <- systemfonts::system_fonts()
regular <- fonts[tolower(fonts$family) == "arial" & !fonts$italic &
                 !grepl("bold|heavy|black", fonts$style, ignore.case = TRUE), ]
bold <- fonts[tolower(fonts$family) == "arial" & !fonts$italic &
              grepl("bold", fonts$style, ignore.case = TRUE), ]
if (!nrow(regular) || !nrow(bold)) stop("Arial Regular and Bold fonts are required.")

# 2. Select the Excel workbook -------------------------------------------------
# The source Excel may be anywhere; outputs go beside this saved .R file.
find_script_directory <- function() {
  files <- vapply(sys.frames(), function(fr) {
    if (is.null(fr$ofile)) NA_character_ else as.character(fr$ofile)[1]
  }, character(1))
  files <- files[!is.na(files) & nzchar(files)]
  if (length(files) && file.exists(tail(files, 1)))
    return(dirname(normalizePath(tail(files, 1), winslash = "/")))
  args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(args)) {
    candidate <- sub("^--file=", "", args[1])
    if (file.exists(candidate)) return(dirname(normalizePath(candidate, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    candidate <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (length(candidate) == 1 && !is.na(candidate) && nzchar(candidate) && file.exists(candidate))
      return(dirname(normalizePath(candidate, winslash = "/")))
  }
  stop("Save the script and run it with Source in RStudio.")
}
output_dir <- find_script_directory()
output_stem <- "figs01_duration_en"

if (!is.null(EXCEL_FILE)) {
  excel_file <- EXCEL_FILE
} else if (interactive()) {
  message("Select the English database: Lobomycosis_database.xlsx")
  excel_file <- file.choose()
} else {
  excel_file <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (!nzchar(excel_file))
    excel_file <- file.path(output_dir, "..", "data", "raw", "Lobomycosis_database.xlsx")
}
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
if (tolower(tools::file_ext(excel_file)) != "xlsx") stop("An .xlsx source is required.")
message("Outputs beside script: ", output_dir)

# 3. Read and validate individual-level records --------------------------
source_sheet <- "Individual-level data"
id_col <- "Record_ID"
clinical_col <- "Clinical_distribution"
duration_col <- "Disease_duration_years"
if (!source_sheet %in% readxl::excel_sheets(excel_file)) stop("Missing worksheet: ", source_sheet)
records <- as.data.frame(suppressMessages(readxl::read_excel(
  excel_file, sheet = source_sheet, col_types = "text", .name_repair = "unique")))
names(records) <- trimws(names(records))
needed <- c(id_col, clinical_col, duration_col)
if (!all(needed %in% names(records))) stop("Missing columns: ",
  paste(setdiff(needed, names(records)), collapse = ", "))

clean_text <- function(x) {
  y <- trimws(gsub("\u00a0", " ", as.character(x), fixed = TRUE))
  y[is.na(y)] <- ""
  gsub("[[:space:]]+", " ", y)
}
normalize_text <- function(x) {
  y <- iconv(tolower(clean_text(x)), to = "ASCII//TRANSLIT")
  y[is.na(y)] <- ""
  y
}
missing_values <- c("", "nd", "nr", "n.d.", "n.r.", "n/d", "n/r", "na", "n/a", "-",
                    "no informado", "no reportado", "no disponible", "no descrito",
                    "no especificado", "none", "nan")
records[[id_col]] <- clean_text(records[[id_col]])
records <- records[!normalize_text(records[[id_col]]) %in% missing_values, , drop = FALSE]
if (!nrow(records) || anyDuplicated(records[[id_col]])) stop("Missing or duplicated record IDs.")
n_total <- nrow(records)

# Retain the undifferentiated clinical category without merging it with disseminated cases.
clinical_keys <- c("Localized", "Disseminated", "Multifocal/disseminated, not differentiated", "ND")
normalized_keys <- normalize_text(clinical_keys)
raw_clinical <- clean_text(records[[clinical_col]])
raw_clinical[normalize_text(raw_clinical) %in% missing_values] <- "ND"
key <- normalize_text(raw_clinical)
if (any(!key %in% normalized_keys)) stop("Unrecognized clinical categories: ",
  paste(unique(raw_clinical[!key %in% normalized_keys]), collapse = "; "))
records$clinical <- clinical_keys[match(key, normalized_keys)]
records$raw_duration <- clean_text(records[[duration_col]])
records$years <- NA_real_
records$operator <- "ND"
for (i in seq_len(n_total)) {
  value <- records$raw_duration[i]
  if (normalize_text(value) %in% missing_values) next
  value <- gsub("[[:space:]]+", "", value)
  value <- gsub(",", ".", value, fixed = TRUE)
  value <- gsub(">=", "≥", value, fixed = TRUE)
  if (!grepl("^(≥|>)?[0-9]+(\\.[0-9]+)?$", value))
    stop("Unrecognized duration at ", records[[id_col]][i], ": ", records$raw_duration[i])
  records$operator[i] <- if (startsWith(value, "≥")) "≥" else if (startsWith(value, ">")) ">" else "="
  records$years[i] <- as.numeric(sub("^[≥>]", "", value))
}

# 4. Separate exact values from lower bounds ---------------------------
# Lower bounds are displayed, but never included in medians or quartiles.
groups <- c("Localized", "Disseminated")
plotted <- records[records$clinical %in% groups, , drop = FALSE]
exact <- plotted[plotted$operator == "=", , drop = FALSE]
lower <- plotted[plotted$operator %in% c(">", "≥"), , drop = FALSE]
if (!nrow(exact)) stop("No exact durations available.")
group_summary <- do.call(rbind, lapply(groups, function(group) {
  d <- plotted[plotted$clinical == group, , drop = FALSE]
  values <- d$years[d$operator == "="]
  qq <- if (length(values)) unname(stats::quantile(values, c(.25, .50, .75), type = 7)) else rep(NA_real_, 3)
  data.frame(group = group, total = nrow(d), exact = length(values),
             lower = sum(d$operator %in% c(">", "≥")),
             nd = sum(d$operator == "ND"), q1 = qq[1], median = qq[2], q3 = qq[3],
             stringsAsFactors = FALSE)
}))
stopifnot(all(group_summary$exact + group_summary$lower + group_summary$nd == group_summary$total),
          sum(group_summary$exact) == nrow(exact), sum(group_summary$lower) == nrow(lower))
all_quartiles <- unname(stats::quantile(exact$years, probs = c(.25, .50, .75), type = 7))
message("Individual records: ", n_total,
        "; exact durations: ", nrow(exact),
        "; lower bounds: ", nrow(lower))
print(group_summary, row.names = FALSE)

max_value <- max(c(exact$years, lower$years), na.rm = TRUE)
y_max <- max(10, ceiling(max_value * 1.10 / 10) * 10)
y_ticks <- pretty(c(0, y_max), n = 7)
y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_max]
number_label <- function(x) format(x, trim = TRUE, scientific = FALSE, decimal.mark = ".")

# 5. Draw the original two-panel figure --------------------------------
# A fixed seed changes only horizontal point positions, never the measured durations.
draw_figure <- function() {
  grid::grid.newpage()
  set.seed(JITTER_SEED)
  txt <- function(s, x, y, size = TEXT_SIZE, just = "left", bold = FALSE, rot = 0) {
    grid::grid.text(s, x, y, just = just, rot = rot,
      gp = grid::gpar(fontfamily = FONT_FAMILY, fontsize = size,
                      fontface = if (bold) "bold" else "plain",
                      col = "#171717", lineheight = 1.1))
  }
  line <- function(x0, x1, y0, y1, color = AXIS_COLOR, width = 1) {
    grid::grid.lines(c(x0, x1), c(y0, y1), gp = grid::gpar(col = color, lwd = width))
  }
  A <- c(.090, .635)
  B <- c(.755, .960)
  bottom <- .260
  top <- .870
  yy <- function(v) bottom + (top - bottom) * v / y_max
  centers <- function(panel) panel[1] + diff(panel) * c(.25, .75)
  for (panel in list(A, B)) {
    for (v in y_ticks) {
      line(panel[1], panel[2], yy(v), yy(v), "#E0E0E0", .6)
      line(panel[1] - .003, panel[1], yy(v), yy(v), AXIS_COLOR, 1.1)
      txt(number_label(v), panel[1] - .007, yy(v), size = 13, just = "right")
    }
    line(panel[1], panel[2], bottom, bottom, AXIS_COLOR, AXIS_WIDTH)
    line(panel[1], panel[1], bottom, top, AXIS_COLOR, AXIS_WIDTH)
  }
  txt("A.", A[1], .935, 18, bold = TRUE)
  txt("B.", B[1], .935, 18, bold = TRUE)
  txt("Disease duration (years)", .025, mean(c(bottom, top)), just = "centre", rot = 90)
  xa <- centers(A); xb <- centers(B)
  for (j in seq_along(groups)) {
    group <- groups[j]
    q <- group_summary[j, ]
    color <- GROUP_COLORS[group]
    values <- exact$years[exact$clinical == group]
    if (length(values)) {
      iqr <- q$q3 - q$q1
      whiskers <- range(values[values >= q$q1 - 1.5 * iqr & values <= q$q3 + 1.5 * iqr])
      width <- diff(A) * .18
      line(xa[j], xa[j], yy(whiskers[1]), yy(whiskers[2]), "#555555", 1.2)
      for (end in whiskers)
        line(xa[j] - width / 4, xa[j] + width / 4, yy(end), yy(end), "#555555", 1.2)
      grid::grid.rect(xa[j], mean(c(yy(q$q1), yy(q$q3))), width = width,
        height = yy(q$q3) - yy(q$q1),
        gp = grid::gpar(fill = grDevices::adjustcolor(color, alpha.f = .45),
                        col = "#555555", lwd = 1))
      line(xa[j] - width / 2, xa[j] + width / 2, yy(q$median), yy(q$median), AXIS_COLOR, 2)
      point_x <- xa[j] + stats::runif(length(values), -width * .42, width * .42)
      grid::grid.circle(x = grid::unit(point_x, "npc"), y = grid::unit(yy(values), "npc"),
        r = grid::unit(1.05, "mm"),
        gp = grid::gpar(fill = grDevices::adjustcolor(color, alpha.f = .85),
                        col = "#555555", lwd = .6))
      txt(paste0("Median: ", number_label(q$median), " years", "\n",
                 "IQR: ", number_label(q$q1), "–", number_label(q$q3)),
          xa[j], top - .028, size = 13, just = "centre")
    } else txt("No exact durations", xa[j], top - .028, size = 13, just = "centre")
    txt(group, xa[j], bottom - .030, just = "centre")
    txt(paste0("Exact: n = ", q$exact, " | ND: n = ", q$nd),
        xa[j], bottom - .060, size = 12, just = "centre")
    bounds <- lower[lower$clinical == group, , drop = FALSE]
    if (nrow(bounds)) {
      for (value in unique(bounds$years)) {
        idx <- which(bounds$years == value)
        offsets <- if (length(idx) > 1L) seq(-.012, .012, length.out = length(idx)) else 0
        for (k in seq_along(idx)) {
          x <- xb[j] + offsets[k]
          y <- yy(value)
          grid::grid.polygon(x = grid::unit(x, "npc") + grid::unit(c(-1.1, 1.1, 0), "mm"),
            y = grid::unit(y, "npc") + grid::unit(c(-.9, -.9, 1.1), "mm"),
            gp = grid::gpar(fill = "white", col = color, lwd = 2))
          txt(paste0(bounds$operator[idx[k]], number_label(value)), x + .010, y, size = 13)
        }
      }
    }
    txt(group, xb[j], bottom - .030, size = 13, just = "centre")
    txt(paste0("n = ", q$lower), xb[j], bottom - .060, size = 12, just = "centre")
  }
  if (!nrow(lower)) txt("No lower bounds", mean(B), mean(c(top, bottom)), just = "centre")
  txt(paste0("Exact values (n = ", nrow(exact), ")"), mean(A), .120, bold = TRUE, just = "centre")
  txt(paste0("Lower bounds (n = ", nrow(lower), ")"), mean(B), .120, bold = TRUE, just = "centre")
  if (SHOW_NOTES) {
    txt("Box: IQR and median; points: exact durations. Lower bounds are shown only in panel B.", A[1], .060, size = 10)
    txt("The undifferentiated category is excluded from the boxplot.", A[1], .030, size = 10)
  }
}

# 6. Audit of denominators and manuscript values ----------------------
# Each source record is retained in the audit, including records not plotted.
checks <- data.frame(
  indicator = c("individual_records", "clinical_information", "localized_total", "disseminated_total",
    "undifferentiated_total", "clinical_nd", "duration_available", "exact_total", "lower_total",
    "localized_exact", "disseminated_exact", "localized_lower", "disseminated_lower",
    "localized_median", "localized_q1", "localized_q3", "disseminated_median", "disseminated_q1",
    "disseminated_q3", "all_exact_median", "all_exact_q1", "all_exact_q3",
    "exact_min", "exact_max"),
  manuscript = c(256, 233, 171, 36, 26, 23, 96, 93, 3, 69, 24, 2, 1,
                 10, 3, 17, 25, 15, 32.5, 12, 5, 21, 0.58, 60),
  database = c(n_total, sum(records$clinical != "ND"),
    sum(records$clinical == groups[1]), sum(records$clinical == groups[2]),
    sum(records$clinical == clinical_keys[3]), sum(records$clinical == "ND"),
    sum(records$operator != "ND"), nrow(exact), nrow(lower),
    group_summary$exact[1], group_summary$exact[2],
    group_summary$lower[1], group_summary$lower[2],
    group_summary$median[1], group_summary$q1[1], group_summary$q3[1],
    group_summary$median[2], group_summary$q1[2], group_summary$q3[2],
    all_quartiles[2], all_quartiles[1], all_quartiles[3],
    min(exact$years), max(exact$years)), stringsAsFactors = FALSE)
checks$status <- ifelse(abs(checks$manuscript - checks$database) < 1e-8, "PASS", "CHECK")
if (any(checks$status == "CHECK")) warning("Some checks differ from the manuscript; inspect the audit Excel.")

# The summary CSV stores the numbers underlying both panels, not patient-level data.
summary <- data.frame(
  section = c(rep("group_summary", nrow(group_summary) * 7), rep("manuscript_checks", nrow(checks))),
  group = c(rep(group_summary$group, each = 7), rep("All", nrow(checks))),
  metric = c(rep(c("total", "exact", "lower_bounds", "nd", "q1", "median", "q3"), times = nrow(group_summary)),
             checks$indicator),
  value = c(as.vector(t(as.matrix(group_summary[, c("total", "exact", "lower", "nd", "q1", "median", "q3")]))), checks$database),
  reference = c(rep(NA_real_, nrow(group_summary) * 7), checks$manuscript),
  status = c(rep("INFO", nrow(group_summary) * 7), checks$status), stringsAsFactors = FALSE)

# Classify, but do not discard, source records in the traceability table.
trace <- data.frame(
  record_id = records[[id_col]], clinical_distribution = records$clinical,
  duration_original = ifelse(normalize_text(records$raw_duration) %in% missing_values, "ND", records$raw_duration),
  duration_type = ifelse(records$operator == "=", "Exact",
                  ifelse(records$operator == "ND", "ND", "Lower bound")),
  years = records$years, operator = records$operator,
  panel = ifelse(!records$clinical %in% groups, "Not plotted",
                 ifelse(records$operator == "=", "A", ifelse(records$operator == "ND", "Not plotted", "B"))),
  reason = ifelse(!records$clinical %in% groups, "Clinical distribution not plotted",
                  ifelse(records$operator == "ND", "Duration unavailable", "")),
  stringsAsFactors = FALSE)

# 7. Export four files beside the script -----------------------------
# Previous results are preserved by adding _run01, _run02, etc.
suffix <- ""
run <- 0L
repeat {
  paths <- file.path(output_dir, paste0(output_stem, suffix,
                  c(".png", ".tiff", "_summary.csv", "_audit.xlsx")))
  if (!any(file.exists(paths))) break
  run <- run + 1L
  suffix <- sprintf("_run%02d", run)
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
export_figure(paths[1], "png")
export_figure(paths[2], "tiff")
utils::write.csv(summary, paths[3], row.names = FALSE, na = "", fileEncoding = "UTF-8")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Audit")
style_title <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#E7EAEF")
style_section <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DEE8E2")
style_header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EBEBEB")
openxlsx::writeData(wb, "Audit", "Supplementary Figure S1 — duration audit", startRow = 1, startCol = 1)
openxlsx::mergeCells(wb, "Audit", cols = 1:8, rows = 1)
openxlsx::addStyle(wb, "Audit", style_title, rows = 1, cols = 1:8, gridExpand = TRUE)
source_info <- data.frame(basename(excel_file), source_sheet, stringsAsFactors = FALSE)
names(source_info) <- c("Source workbook", "Source worksheet")
openxlsx::writeData(wb, "Audit", source_info, startRow = 3, startCol = 1)
openxlsx::writeData(wb, "Audit", "GROUP SUMMARY", startRow = 6, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_section, rows = 6, cols = 1:8, gridExpand = TRUE)
openxlsx::writeData(wb, "Audit", group_summary, startRow = 7, startCol = 1, withFilter = FALSE)
openxlsx::addStyle(wb, "Audit", style_header, rows = 7, cols = 1:8, gridExpand = TRUE)
checks_row <- 12
openxlsx::writeData(wb, "Audit", "MANUSCRIPT CHECKS", startRow = checks_row, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_section, rows = checks_row, cols = 1:8, gridExpand = TRUE)
openxlsx::writeData(wb, "Audit", checks, startRow = checks_row + 1, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_header, rows = checks_row + 1, cols = 1:4, gridExpand = TRUE)
lower_row <- checks_row + nrow(checks) + 4
openxlsx::writeData(wb, "Audit", "LOWER-BOUND RECORDS", startRow = lower_row, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_section, rows = lower_row, cols = 1:8, gridExpand = TRUE)
limits_table <- lower[, c(id_col, "clinical", "raw_duration", "operator", "years"), drop = FALSE]
openxlsx::writeData(wb, "Audit", limits_table, startRow = lower_row + 1, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_header, rows = lower_row + 1, cols = 1:5, gridExpand = TRUE)
trace_row <- lower_row + nrow(lower) + 4
openxlsx::writeData(wb, "Audit", "RECORD-LEVEL TRACEABILITY", startRow = trace_row, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_section, rows = trace_row, cols = 1:8, gridExpand = TRUE)
openxlsx::writeData(wb, "Audit", trace, startRow = trace_row + 1, startCol = 1)
openxlsx::addStyle(wb, "Audit", style_header, rows = trace_row + 1, cols = 1:8, gridExpand = TRUE)
openxlsx::setColWidths(wb, "Audit", cols = 1:8,
                        widths = c(33, 39, 27, 22, 16, 14, 27, 38))
openxlsx::freezePane(wb, "Audit", firstActiveRow = trace_row + 2)
openxlsx::saveWorkbook(wb, paths[4], overwrite = FALSE)

if (!all(file.exists(paths)) || any(file.info(paths)$size <= 0)) stop("One or more output files could not be written.")
message("Saved four files beside the script:", "\n",
        paste(basename(paths), collapse = "\n"))
