# FIGURE 2. Geographical distribution of reported cases and publications
# The figure compares reported cases by country of acquisition (A) and publications by country of study (B).
# Input: publication-level worksheet of Lobomycosis_database.xlsx, selected with file.choose().
# Outputs: PNG, TIFF, CSV and a one-sheet Excel audit, saved beside this .R file.

# 1. Packages and configuration ------------------------------------------------------------
FIGURE_DPI <- 1200
FIGURE_WIDTH_CM <- 32
FIGURE_HEIGHT_CM <- 24
FONT_FAMILY <- "Arial"

# The original script resolution and dimensions are retained. Journal-ready TIFF specifications will be checked separately.
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "sf", "rnaturalearth",
  "ragg", "systemfonts", "cowplot", "openxlsx"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       "\nInstall them once with install.packages() and run the script again.")
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
  stop("Arial was not found on this computer.")
}

# 2. Select the database and output folder -------------------------------------------------
# Find the directory containing the currently executed script.
# This keeps figure exports next to the .R file, even when the database is stored elsewhere.
find_script_directory <- function() {
  script_files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
  }, character(1))
  script_files <- script_files[!is.na(script_files) & nzchar(script_files)]
  if (length(script_files) && file.exists(tail(script_files, 1))) {
    return(dirname(normalizePath(tail(script_files, 1), winslash = "/")))
  }

  args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(args)) {
    path <- sub("^--file=", "", args[1])
    if (file.exists(path)) return(dirname(normalizePath(path, winslash = "/")))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    editor <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(editor) && length(editor$path) == 1 &&
        !is.na(editor$path) && nzchar(editor$path) && file.exists(editor$path)) {
      return(dirname(normalizePath(editor$path, winslash = "/")))
    }
  }
  stop("Could not identify the script folder. Save the .R file and run it with Source in RStudio.")
}
script_dir <- find_script_directory()
output_dir <- script_dir

# RStudio: select the English Excel file manually each time you run the script.
# Noninteractive runs can use LOBOMYCOSIS_DATA or data/raw in the repository.
if (interactive()) {
  message("Select Lobomycosis_database.xlsx (English database).")
  excel_file <- file.choose()
} else {
  chosen_file <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (nzchar(chosen_file)) {
    excel_file <- chosen_file
  } else {
    excel_file <- file.path(script_dir, "..", "data", "raw", "Lobomycosis_database.xlsx")
  }
}
if (!file.exists(excel_file)) stop("File not found: ", excel_file)
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
message("Outputs will be saved beside the script: ", output_dir)

# 3. Data import and checks ---------------------------------------------------------------
# The actual English worksheet and column names are used without changing the original workbook.
sheet_name <- "Publication-level data"
required_columns <- c(
  "Study_ID", "Country_of_study", "Total_lobomycosis_cases", "Country_of_acquisition"
)
if (!(sheet_name %in% readxl::excel_sheets(excel_file))) {
  stop("Worksheet not found: ", sheet_name)
}
publications <- readxl::read_excel(excel_file, sheet = sheet_name,
                                    col_types = "text")
names(publications) <- trimws(names(publications))
if (!all(required_columns %in% names(publications))) {
  stop("Missing columns: ",
       paste(setdiff(required_columns, names(publications)), collapse = ", "))
}
publications[] <- lapply(publications, function(x) trimws(as.character(x)))

study_ids <- publications[["Study_ID"]]
if (anyNA(study_ids) || any(!nzchar(study_ids)) || anyDuplicated(study_ids)) {
  stop("Study identifiers are missing or duplicated.")
}

# ND is treated as an unavailable case count, not as zero.
case_text <- publications[["Total_lobomycosis_cases"]]
missing_cases <- is.na(case_text) | case_text == "" | toupper(case_text) == "ND"
case_numbers <- suppressWarnings(as.numeric(case_text))
if (any(!missing_cases &
        (is.na(case_numbers) | !is.finite(case_numbers) |
           case_numbers < 0 | case_numbers != floor(case_numbers)))) {
  stop("Total lobomycosis cases must be nonnegative integers or ND.")
}
case_numbers[missing_cases] <- NA_real_
publications$case_count <- case_numbers

# 4. Country-level counts ---------------------------------------------------------------
# Country names are matched to ISO3 codes using English and Spanish aliases for source consistency.
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
country_catalog$label <- country_catalog$country_en

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
    stop("Unrecognized country in the database: ", paste(unique(x[is.na(iso)]), collapse = ", "))
  }
  iso
}
publications$study_iso <- country_to_iso(publications$Country_of_study)

# Multi-country case counts are split only when explicit numbers are reported for each country.
allocations <- dplyr::bind_rows(lapply(seq_len(nrow(publications)), function(i) {
  country_value <- publications[["Country_of_acquisition"]][i]
  reported_cases <- publications$case_count[i]
  study_id <- publications[["Study_ID"]][i]

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
        stop("Invalid country breakdown in study ", study_id, ": ", country_value)
      }
      data.frame(Study_ID = study_id,
                 ISO3 = country_to_iso(match[2]),
                 Cases = as.numeric(match[3]))
    })
    result <- dplyr::bind_rows(parts_data)
    if (is.na(reported_cases) || sum(result$Cases) != reported_cases) {
      stop("Country breakdown does not match the total for study ", study_id)
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
# Approved manuscript totals are checked before plotting to avoid silent discrepancies.
if (nrow(publications) != 98 || reported_total != 1463 ||
    known_country_total != 1254 || sum(country_summary$publications) != 98) {
  stop("Counts differ from the approved manuscript. Check the selected workbook.")
}

# 5. Map data ----------------------------------------------------------------
# Natural Earth map units at 1:50m scale are used as in the original script. Internet access is required.
message("Retrieving Natural Earth map data...")
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
  stop("Missing map geometries for: ", paste(missing_shapes, collapse = ", "))
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

# 6. Figure construction ---------------------------------------------------------------
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

# Main countries are labeled; commas separate thousands in the English figure.
number_label <- function(n) {
  format(n, big.mark = ",", decimal.mark = ".",
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

# The original class breaks, colors, and legend positions are retained.
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
  paste0("Reported cases\n(n = ", number_label(known_country_total), ")"),
  case_classes, unname(case_colors)
)
legend_b <- make_legend(
  paste0("Number of publications\n(n = ", number_label(sum(country_summary$publications)), ")"),
  publication_classes, publication_colors
)

# The two approved panels are arranged with the original map and legend proportions.
figure_02 <- cowplot::ggdraw() +
  cowplot::draw_plot(panel_a, x = 0, y = .505, width = .78, height = .49) +
  cowplot::draw_plot(legend_a, x = .78, y = .505, width = .22, height = .49) +
  cowplot::draw_plot(panel_b, x = 0, y = .005, width = .78, height = .49) +
  cowplot::draw_plot(legend_b, x = .78, y = .005, width = .22, height = .49)

# 7. Export of Figure 2 and CSV --------------------------------------------------------------------
# Keep previous exports when the script is re-run in the same folder.
figure_stem <- "fig02_geo_en"
summary_stem <- "fig02_geo_en_summary"
audit_stem <- "fig02_geo_en_audit"
run_suffix <- ""
run_number <- 0L
repeat {
  expected_paths <- file.path(output_dir, c(
    paste0(figure_stem, run_suffix, ".png"),
    paste0(figure_stem, run_suffix, ".tiff"),
    paste0(summary_stem, run_suffix, ".csv"),
    paste0(audit_stem, run_suffix, ".xlsx")
  ))
  if (!any(file.exists(expected_paths))) break
  run_number <- run_number + 1L
  run_suffix <- paste0("_run", sprintf("%02d", run_number))
}
if (nzchar(run_suffix)) message("Existing outputs preserved; this run uses suffix: ", run_suffix)

# Export the approved figure without modifying the source workbook.
# Outputs are written separately; the selected source workbook is never overwritten.
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
  message("Saved: ", file_path)
}
output_base <- file.path(output_dir, paste0(figure_stem, run_suffix))
export_figure(paste0(output_base, ".png"), "png")
export_figure(paste0(output_base, ".tiff"), "tiff")

# The CSV provides a country-level check without redistributing the source workbook.
summary_file <- file.path(output_dir, paste0(summary_stem, run_suffix, ".csv"))
write.csv(
  country_summary[, c("label", "ISO3", "reported_cases", "publications")],
  summary_file, row.names = FALSE, fileEncoding = "UTF-8"
)
message("Publications: ", nrow(publications), " | ",
        "Reported cases: ", number_label(reported_total), " | ",
        "Cases with known country: ", number_label(known_country_total))
message("Files saved in: ", output_dir)

# 8. One-sheet Excel audit ---------------------------------------------------------------
# The audit groups all values behind Figure 2 in one worksheet.
publication_total <- nrow(publications)
country_audit <- country_summary |>
  dplyr::transmute(
    Country = label, ISO3 = ISO3,
    Reported_cases = as.integer(reported_cases),
    Case_share_percent = round(100 * reported_cases / known_country_total, 1),
    Publications = as.integer(publications),
    Publication_share_percent = round(100 * publications / publication_total, 1)
  )
country_audit <- rbind(country_audit, data.frame(
  Country = "TOTAL", ISO3 = "",
  Reported_cases = as.integer(known_country_total), Case_share_percent = 100,
  Publications = nrow(publications), Publication_share_percent = 100
))

country_known_publications <- sum(publications$Country_of_acquisition != "ND" &
  !is.na(publications$Country_of_acquisition) &
  nzchar(publications$Country_of_acquisition))
country_unknown_publications <- nrow(publications) - country_known_publications
multi_country_studies <- sum(grepl("/", publications$Country_of_acquisition, fixed = TRUE), na.rm = TRUE)

checks <- data.frame(
  Indicator = c(
    "Included publications", "Publications reporting a case count",
    "Publications without a case count", "Reported cases across publications",
    "Cases with known country of acquisition", "Cases without country of acquisition",
    "Publications with acquisition country reported", "Publications without acquisition country",
    "Publications conducted in Brazil", "Publications conducted in Colombia",
    "Publications conducted in Venezuela", "Reported cases acquired in Brazil",
    "Reported cases acquired in Colombia", "Reported cases acquired in Venezuela",
    "Total country allocations", "Publications with explicit multi-country case breakdown"
  ),
  Observed = c(
    nrow(publications), sum(!is.na(publications$case_count)),
    sum(is.na(publications$case_count)), reported_total,
    known_country_total, unknown_country_total,
    country_known_publications, country_unknown_publications,
    country_summary$publications[country_summary$ISO3 == "BRA"],
    country_summary$publications[country_summary$ISO3 == "COL"],
    country_summary$publications[country_summary$ISO3 == "VEN"],
    country_summary$reported_cases[country_summary$ISO3 == "BRA"],
    country_summary$reported_cases[country_summary$ISO3 == "COL"],
    country_summary$reported_cases[country_summary$ISO3 == "VEN"],
    known_country_total + unknown_country_total,
    multi_country_studies
  ),
  Manuscript = c(98, 94, 4, 1463, 1254, 209, 83, 15,
                 63, 9, 5, 1133, 83, 9, 1463, NA_real_),
  stringsAsFactors = FALSE
)
checks$Status <- ifelse(is.na(checks$Manuscript), "INFO",
                        ifelse(checks$Observed == checks$Manuscript, "PASS", "CHECK"))
if (any(checks$Status == "CHECK")) {
  stop("The country audit differs from the manuscript. Review the workbook before exporting.")
}
checks$Notes <- c(
  "One row per publication", "Available case counts", "ND case counts",
  "A cumulative count, not necessarily unique patients",
  "Map A denominator", "Not mapped in panel A",
  "Country of acquisition available at publication level", "ND at publication level",
  "Map B", "Map B", "Map B", "Map A", "Map A", "Map A",
  "Known + unknown = all reported cases", "Descriptive count, not a manuscript target"
)

wb <- openxlsx::createWorkbook()
sheet <- "Figure 02 audit"
openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
title_style <- openxlsx::createStyle(fontSize = 15, textDecoration = "bold",
  fgFill = "#E8EDF4", fontColour = "#243447", valign = "center")
header_style <- openxlsx::createStyle(textDecoration = "bold",
  fgFill = "#DDE6EF", border = "Bottom", valign = "center")
section_style <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold",
  fgFill = "#EEF2F6")
total_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E8EDF4")
numeric_style <- openxlsx::createStyle(numFmt = "#,##0")
percent_style <- openxlsx::createStyle(numFmt = "0.0")

openxlsx::writeData(wb, sheet, "FIGURE 2 — COUNTRY-LEVEL AUDIT", startRow = 1, startCol = 1)
openxlsx::mergeCells(wb, sheet, cols = 1:6, rows = 1)
openxlsx::addStyle(wb, sheet, title_style, rows = 1, cols = 1:6, gridExpand = TRUE)
openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 28)
openxlsx::writeData(wb, sheet, paste("Source workbook:", basename(excel_file)),
                     startRow = 2, startCol = 1)
openxlsx::writeData(wb, sheet,
  "A: reported cases by acquisition country; B: publications by country of study",
  startRow = 3, startCol = 1)
openxlsx::writeData(wb, sheet, country_audit, startRow = 5, startCol = 1,
  headerStyle = header_style, withFilter = TRUE)
total_row <- 5 + nrow(country_audit)
openxlsx::addStyle(wb, sheet, total_style, rows = total_row, cols = 1:6, gridExpand = TRUE)
openxlsx::addStyle(wb, sheet, numeric_style,
  rows = 6:total_row, cols = c(3, 5), gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, sheet, percent_style,
  rows = 6:total_row, cols = c(4, 6), gridExpand = TRUE, stack = TRUE)

checks_title_row <- total_row + 3
checks_table_row <- checks_title_row + 1
openxlsx::writeData(wb, sheet, "CHECKS AGAINST THE MANUSCRIPT", startRow = checks_title_row)
openxlsx::addStyle(wb, sheet, section_style, rows = checks_title_row,
                    cols = 1:6, gridExpand = TRUE)
openxlsx::writeData(wb, sheet, checks,
  startRow = checks_table_row, headerStyle = header_style)
openxlsx::addStyle(wb, sheet, numeric_style,
  rows = (checks_table_row + 1):(checks_table_row + nrow(checks)),
  cols = 2:3, gridExpand = TRUE, stack = TRUE)

notes_start <- checks_table_row + nrow(checks) + 4
openxlsx::writeData(wb, sheet, "INTERPRETATION AND DATA HANDLING", startRow = notes_start)
openxlsx::addStyle(wb, sheet, section_style, rows = notes_start,
                    cols = 1:6, gridExpand = TRUE)
notes <- c(
  "Reported cases are summed from publications and may not represent unique patients.",
  "Acquisition country (A) and country of study (B) describe different variables.",
  "A country breakdown is allocated only if counts are explicitly available in the source.",
  "ND case counts are excluded from the sum; unavailable acquisition countries remain unassigned.",
  "A zero in the country table means no cases or publications were allocated to that country.",
  "Check statuses refer to the counts specified in the manuscript, not to a formal re-analysis of the original reports."
)
openxlsx::writeData(wb, sheet, data.frame(Notes = notes),
                    startRow = notes_start + 1, colNames = FALSE)
openxlsx::setColWidths(wb, sheet, cols = 1, widths = 51)
openxlsx::setColWidths(wb, sheet, cols = 2, widths = 17)
openxlsx::setColWidths(wb, sheet, cols = 3:6, widths = c(20, 22, 19, 22))
openxlsx::freezePane(wb, sheet, firstActiveRow = 6)
excel_summary <- file.path(output_dir, paste0(audit_stem, run_suffix, ".xlsx"))
openxlsx::saveWorkbook(wb, excel_summary, overwrite = FALSE)
message("Saved one-sheet Excel audit: ", excel_summary)

message("Completed Figure 2. All four outputs are in: ", output_dir)
