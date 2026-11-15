# FIGURE 5. Treatment response, recurrence and follow-up in lobomycosis
# The analysis uses individual records with a treatment linked to the outcome.
# The clinical trial is shown separately from other publications.
# Treatment groups are descriptive; no comparative effectiveness is estimated.
# Input: Lobomycosis_database.xlsx. Outputs: PNG, TIFF, CSV and one-sheet XLSX beside the script.

# 1. Packages and settings ------------------------------------------------------
DPI <- 1200
WIDTH_CM <- 42
HEIGHT_CM <- 28
FONT <- "Arial"
TEXT_PT <- 14
EXCEL_FILE <- NULL  # NULL opens file.choose(); a fixed path is also accepted.

packages <- c("readxl", "ragg", "systemfonts", "openxlsx")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ",
  paste(missing_packages, collapse = ", "), ". ", "Install these packages once with install.packages() and run the script again.")

fonts <- systemfonts::system_fonts()
if (!any(tolower(fonts$family) == "arial" & !fonts$italic)) stop("Required font Arial was not found.")

# 2. Choose the source workbook --------------------------------------------------
# Output files follow the script, even when the source Excel is selected elsewhere.
script_folder <- function() {
  files <- vapply(sys.frames(), function(f) {
    if (is.null(f$ofile)) NA_character_ else as.character(f$ofile)[1]
  }, character(1))
  files <- files[!is.na(files) & nzchar(files)]
  if (length(files) && file.exists(tail(files, 1)))
    return(dirname(normalizePath(tail(files, 1), winslash = "/")))
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg) && file.exists(sub("^--file=", "", arg[1])))
    return(dirname(normalizePath(sub("^--file=", "", arg[1]), winslash = "/")))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (length(p) == 1 && !is.na(p) && nzchar(p) && file.exists(p))
      return(dirname(normalizePath(p, winslash = "/")))
  }
  stop("Save this script and run the entire file with Source in RStudio.")
}
output_dir <- script_folder()
file_stem <- "fig05_treatment_en"
if (!is.null(EXCEL_FILE)) {
  excel_file <- EXCEL_FILE
} else if (interactive()) {
  message("Select Lobomycosis_database.xlsx (English database).")
  excel_file <- file.choose()
} else {
  excel_file <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (!nzchar(excel_file))
    excel_file <- file.path(output_dir, "..", "data", "raw", "Lobomycosis_database.xlsx")
}
if (!file.exists(excel_file)) stop("The Excel file could not be found.")
excel_file <- normalizePath(excel_file, winslash = "/", mustWork = TRUE)
if (tolower(tools::file_ext(excel_file)) != "xlsx") stop("Expected an .xlsx workbook.")
message("The outputs will be saved beside this R script: ", output_dir)

# 3. Import and validate the data ----------------------------------------------------
sheet_ind <- "Individual-level data"
sheet_pub <- "Publication-level data"
col_id <- "Record_ID"; col_study <- "Study_ID"; col_design <- "Study_type"
col_treatment <- "Treatment_linked_to_outcome"; col_response <- "Clinical_response"
col_recurrence <- "Recurrence"; col_follow <- "Follow_up_months"
required_sheets <- c(sheet_ind, sheet_pub)
if (!all(required_sheets %in% readxl::excel_sheets(excel_file)))
  stop("Missing worksheets: ", paste(setdiff(required_sheets, readxl::excel_sheets(excel_file)), collapse = ", "))
read_sheet <- function(name) {
  x <- as.data.frame(suppressMessages(readxl::read_excel(
    excel_file, sheet = name, col_types = "text", .name_repair = "unique")))
  names(x) <- trimws(names(x))
  x
}
individual <- read_sheet(sheet_ind)
studies <- read_sheet(sheet_pub)
needed_ind <- c(col_id, col_study, col_treatment, col_response, col_recurrence, col_follow)
needed_pub <- c(col_study, col_design)
if (!all(needed_ind %in% names(individual)))
  stop("Missing required columns: ", paste(setdiff(needed_ind, names(individual)), collapse = ", "))
if (!all(needed_pub %in% names(studies)))
  stop("Missing required columns: ", paste(setdiff(needed_pub, names(studies)), collapse = ", "))
clean <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("[[:space:]]+", " ", gsub("\u00a0", " ", x, fixed = TRUE)))
}
normal <- function(x) {
  y <- iconv(tolower(clean(x)), to = "ASCII//TRANSLIT")
  y[is.na(y)] <- ""
  y
}
missing_codes <- c("", "nd", "nr", "n/d", "n/r", "n/a", "na", "-",
                   "no disponible", "no informado", "no reportado", "not reported")
has_data <- function(x) !(normal(x) %in% missing_codes)
as_nd <- function(x) { x <- clean(x); x[!has_data(x)] <- "ND"; x }
individual <- individual[has_data(individual[[col_id]]), , drop = FALSE]
studies <- studies[has_data(studies[[col_study]]), , drop = FALSE]
individual[[col_id]] <- clean(individual[[col_id]])
individual[[col_study]] <- clean(individual[[col_study]])
studies[[col_study]] <- clean(studies[[col_study]])
if (!nrow(individual) || anyDuplicated(individual[[col_id]])) stop("Missing or duplicated individual record identifiers.")
if (!nrow(studies) || anyDuplicated(studies[[col_study]])) stop("Duplicated study identifiers.")
if (any(!individual[[col_study]] %in% studies[[col_study]])) stop("Individual records refer to unknown Study_ID values.")
for (key in c(col_treatment, col_response, col_recurrence, col_follow))
  individual[[key]] <- as_nd(individual[[key]])

# Clinical trials are identified from study design, rather than hard-coding a record ID.
trial_ids <- studies[[col_study]][grepl(
  "clinical trial|ensayo clinico|interventional study|estudio de intervencion",
  normal(studies[[col_design]]))]
individual$is_trial <- individual[[col_study]] %in% trial_ids
individual$included <- has_data(individual[[col_treatment]])
total_records <- nrow(individual)
records <- individual[individual$included, , drop = FALSE]
if (!nrow(records)) stop("No treatment-outcome links were found.")

# 4. Treatment groups and outcomes -----------------------------------------------------
response_levels <- c("Complete clinical response", "Partial response", "Clinical stabilization", "No response", "Clinical progression", "ND")
recurrence_levels <- c("No", "Yes", "ND")
response_lookup <- setNames(response_levels, normal(response_levels))
recurrence_lookup <- setNames(recurrence_levels, normal(recurrence_levels))
response_lookup["nd"] <- "ND"
recurrence_lookup["nd"] <- "ND"
recurrence_lookup["si"] <- "Yes"
recurrence_lookup["yes"] <- "Yes"
response_standard <- function(x) {
  y <- unname(response_lookup[normal(x)])
  if (anyNA(y)) stop("Unrecognized clinical response: ", paste(unique(x[is.na(y)]), collapse = "; "))
  y
}
recurrence_standard <- function(x) {
  y <- unname(recurrence_lookup[normal(x)])
  if (anyNA(y)) stop("Unrecognized recurrence: ", paste(unique(x[is.na(y)]), collapse = "; "))
  y
}
records$response <- response_standard(records[[col_response]])
records$recurrence <- recurrence_standard(records[[col_recurrence]])
group_names <- c("MDT", "MDT + itraconazole", "Control", "Local procedures", "Local + systemic", "Itraconazole", "Clofazimine + itraconazole", "Posaconazole", "Other pharmacological treatments", "No treatment")
group_labels <- c("Rifampicin + clofazimine\n+ dapsone", "Rifampicin + clofazimine\n+ dapsone + itraconazole", "Control", "Local procedures\nwithout systemic drugs", "Local procedures\n+ systemic drugs", "Itraconazole", "Clofazimine + itraconazole", "Posaconazole", "Other pharmacological\ntreatments", "No treatment")
names(group_labels) <- group_names

# Only the treatment linked to the reported outcome determines the group.
# Combined local and systemic procedures remain one record, not two.
treatment_group <- function(value) {
  y <- normal(value)
  if (grepl("^control group|^control", y)) return(group_names[3])
  if (startsWith(y, "mdt"))
    return(if (grepl("itraconazole", y, fixed=TRUE)) group_names[2] else group_names[1])
  if (y %in% c("no treatment", "sin tratamiento")) return(group_names[10])
  local <- grepl("excision|resection|surg|cauter|cryo|otoplast|laser|graft|flap", y)
  drug <- grepl("itraconazole|clofazimine|amphotericin|posaconazole|trimethoprim|dapsone|rifampicin|ketoconazole", y)
  if (local) return(if (drug) group_names[5] else group_names[4])
  if (y == "itraconazole") return(group_names[6])
  if (grepl("clofazimine", y, fixed=TRUE) && grepl("itraconazole", y, fixed=TRUE) &&
      !grepl("amphotericin|posaconazole|dapsone|rifampicin|ketoconazole", y)) return(group_names[7])
  if (y == "posaconazole") return(group_names[8])
  group_names[9]
}
records$group <- vapply(records[[col_treatment]], treatment_group, character(1))
records$block <- ifelse(records$is_trial,
                        paste0("Clinical trial", ": ", records[[col_study]]), "Other studies")
blocks <- c(paste0("Clinical trial", ": ", sort(unique(records[[col_study]][records$is_trial]))),
            if (any(!records$is_trial)) "Other studies")
rows <- list()
for (block in blocks) for (group in group_names) {
  indices <- which(records$block == block & records$group == group)
  if (length(indices)) rows[[length(rows) + 1L]] <- list(block = block, group = group, index = indices)
}
if (!length(rows)) stop("No treatment groups could be constructed.")

# 5. Follow-up duration ---------------------------------------------------
# Exact durations are circles; > and ≥ are lower bounds displayed as triangles.
months <- rep(NA_real_, nrow(records))
operator <- rep("ND", nrow(records))
for (i in seq_len(nrow(records))) {
  value <- records[[col_follow]][i]
  if (!has_data(value)) next
  value <- gsub("\\s+", "", gsub(",", ".", value, fixed = TRUE), perl = TRUE)
  value <- sub("^>=", "≥", value)
  if (!grepl("^(≥|>)?[0-9]+(\\.[0-9]+)?$", value))
    stop("Unrecognized follow-up duration for record ", records[[col_id]][i], ": ", records[[col_follow]][i])
  operator[i] <- if (startsWith(value, "≥")) "≥" else if (startsWith(value, ">")) ">" else "="
  months[i] <- as.numeric(sub("^[≥>]", "", value))
  if (!is.finite(months[i]) || months[i] <= 0) stop("Follow-up months must be positive for the logarithmic scale.")
}
records$months <- months
records$operator <- operator
month_range <- if (any(is.finite(months))) range(months[is.finite(months)]) else c(3,180)
month_limits <- c(min(2, month_range[1]/1.25), max(210, month_range[2]*1.12))
month_ticks <- c(1,3,6,12,24,60,180,360,600)
month_ticks <- month_ticks[month_ticks >= month_limits[1] & month_ticks <= month_limits[2]]
if (length(month_ticks) > 7)
  month_ticks <- month_ticks[unique(round(seq(1,length(month_ticks),length.out=7)))]

# 6. Descriptive counts ---------------------------------------------------
response_colors <- setNames(c("#A9C19F","#BBA0D0","#E6CC93","#CC9C9F","#A3ABAD","#E8E8E8"),
                            response_levels)
recurrence_colors <- setNames(c("#BBA0D0","#CC9C9F","#E8E8E8"), recurrence_levels)
summary_rows <- do.call(rbind, lapply(rows, function(row) {
  d <- records[row$index, , drop = FALSE]
  data.frame(block = row$block, group = row$group, n = nrow(d),
             complete = sum(d$response == response_levels[1]),
             partial = sum(d$response == response_levels[2]),
             stable = sum(d$response == response_levels[3]),
             no_response = sum(d$response == response_levels[4]),
             progressive = sum(d$response == response_levels[5]),
             response_nd = sum(d$response == "ND"),
             recurrence_no = sum(d$recurrence == recurrence_levels[1]),
             recurrence_yes = sum(d$recurrence == recurrence_levels[2]),
             recurrence_nd = sum(d$recurrence == "ND"),
             follow_exact = sum(d$operator == "="),
             follow_lower_bound = sum(d$operator %in% c(">","≥")),
             follow_nd = sum(d$operator == "ND"))
}))
stopifnot(sum(summary_rows$n) == nrow(records),
          all(rowSums(summary_rows[,4:9]) == summary_rows$n),
          all(rowSums(summary_rows[,10:12]) == summary_rows$n),
          all(rowSums(summary_rows[,13:15]) == summary_rows$n))

# Preserve the original panel layout and leave titles and notes outside the figure.
row_count <- length(rows)
line_count <- vapply(rows, function(row) {
  length(strsplit(group_labels[row$group], "\n")[[1]]) + 1L
}, integer(1))
min_mm <- sum(pmax(14, line_count * TEXT_PT * 25.4/72 * 1.1 + 5)) +
  7 * max(0, length(blocks)-1)
plot_height <- max(HEIGHT_CM, min_mm / (10 * .605))

# 7. Draw the approved panels ------------------------------------------------------
draw_figure <- function() {
  grid::grid.newpage()
  black <- "#242424"
  text_at <- function(s, x, y, size = TEXT_PT, align="left", color="#171717", bold=FALSE) {
    grid::grid.text(s, x, y, just=align,
      gp=grid::gpar(fontfamily=FONT, fontsize=size,
                    fontface=if (bold) "bold" else "plain", col=color, lineheight=1.1))
  }
  rect_at <- function(x0,x1,y0,y1,fill,border=NA,width=.6) {
    grid::grid.rect((x0+x1)/2,(y0+y1)/2,width=x1-x0,height=y1-y0,
      gp=grid::gpar(fill=fill,col=border,lwd=width))
  }
  line_at <- function(x0,x1,y0,y1,color=black,width=.6) {
    grid::grid.lines(c(x0,x1),c(y0,y1),gp=grid::gpar(col=color,lwd=width))
  }
  dot_at <- function(x,y,color,kind="=",size=2) {
    if (kind == "=") {
      grid::grid.circle(x,y,r=grid::unit(size/2,"mm"),
        gp=grid::gpar(fill=color,col=black,lwd=.65))
    } else {
      grid::grid.polygon(
        x=grid::unit(x,"npc")+grid::unit(c(-.85,-.85,1),"mm"),
        y=grid::unit(y,"npc")+grid::unit(c(-.9,.9,0),"mm"),
        gp=grid::gpar(fill=color,col=black,lwd=.65))
    }
  }
  bar_count <- function(count,x0,x1,y,height) {
    s <- as.character(count)
    grob <- grid::textGrob(s, gp=grid::gpar(fontfamily=FONT,fontsize=TEXT_PT))
    w <- grid::convertWidth(grid::grobWidth(grob),"npc",valueOnly=TRUE)
    text_size <- TEXT_PT * min(1,(x1-x0)*.88/w)
    if (text_size >= 12) {
      text_at(s,(x0+x1)/2,y,text_size,align="centre")
    } else {
      center <- (x0+x1)/2; label_y <- y+height/2+
        grid::convertHeight(grid::unit(3,"mm"),"npc",valueOnly=TRUE)
      line_at(center,center,y+height/2,label_y-.003,black,.6)
      text_at(s,center,label_y,12,align="centre")
    }
  }

  A <- c(.285,.505); B <- c(.555,.735); C <- c(.775,.970)
  top <- .875; bottom <- .270
  row_labels <- vapply(seq_along(rows),function(i)
    paste0(group_labels[rows[[i]]$group],"\n(n = ",summary_rows$n[i],")"),character(1))
  label_heights <- vapply(row_labels,function(s) {
    grob <- grid::textGrob(s,gp=grid::gpar(fontfamily=FONT,fontsize=TEXT_PT,lineheight=1.1))
    grid::convertHeight(grid::grobHeight(grob),"npc",valueOnly=TRUE)
  },numeric(1))
  mm <- function(v) grid::convertHeight(grid::unit(v,"mm"),"npc",valueOnly=TRUE)
  weights <- pmax(mm(14),label_heights+mm(5))
  block_switch <- c(FALSE,vapply(seq_len(row_count)[-1],function(i)
    rows[[i]]$block != rows[[i-1]]$block,logical(1)))
  gap <- mm(7)
  available_height <- top-bottom-sum(block_switch)*gap
  if (available_height < sum(weights))
    stop("More space is required for the treatment labels: increase HEIGHT_CM.")
  row_heights <- weights * available_height / sum(weights)
  yy <- numeric(row_count); cursor <- top
  for (i in seq_len(row_count)) {
    if (block_switch[i]) cursor <- cursor-gap
    yy[i] <- cursor-row_heights[i]/2
    cursor <- cursor-row_heights[i]
  }
  bar_height <- min(mm(9),min(row_heights)*.54)
  pct_x <- function(panel,v) panel[1]+diff(panel)*v/100
  month_x <- function(v) C[1]+diff(C)*(log10(v)-log10(month_limits[1]))/
    diff(log10(month_limits))

  text_at("A.",A[1],.935,18,bold=TRUE)
  text_at("B.",B[1],.935,18,bold=TRUE)
  text_at("C.",C[1],.935,18,bold=TRUE)
  for (block in blocks) {
    jj <- which(vapply(rows,function(row) row$block==block,logical(1)))
    y0 <- yy[max(jj)]-row_heights[max(jj)]/2
    y1 <- yy[min(jj)]+row_heights[min(jj)]/2
    is_trial_block <- startsWith(block,paste0("Clinical trial",": "))
    if (is_trial_block) for (panel in list(A,B,C))
      rect_at(panel[1],panel[2],y0,y1,"#F6F4F8")
    block_n <- sum(summary_rows$n[jj])
    block_name <- if (is_trial_block) "Clinical trial" else "Other studies"
    block_detail <- if (is_trial_block)
      paste0(sub(paste0("^","Clinical trial",": "),"",block),"\n(n = ",block_n,")")
      else paste0("(n = ",block_n,")")
    top_label <- y1-.010
    distance <- mm(6)
    text_at(block_name,.018,top_label,14,align=c("left","top"),bold=TRUE)
    text_at(block_detail,.018,top_label-distance,14,align=c("left","top"))
    if (max(jj)<row_count) line_at(.018,.984,y0-gap/2,y0-gap/2,"#383038",1.2)
  }
  for (panel in list(A,B)) {
    for (tick in c(0,25,50,75,100)) {
      x <- pct_x(panel,tick)
      line_at(x,x,bottom,top,"#DDDDDD",.45)
      line_at(x,x,bottom,bottom-.006,"#777777",.55)
      text_at(as.character(tick),x,bottom-.019,14,align="centre")
    }
    line_at(panel[1],panel[2],bottom,bottom,black,1.1)
    text_at("Records (%)",mean(panel),bottom-.049,14,align="centre")
  }
  for (tick in month_ticks) {
    x <- month_x(tick)
    line_at(x,x,bottom,top,"#DDDDDD",.45)
    line_at(x,x,bottom,bottom-.006,"#777777",.55)
    text_at(format(tick,trim=TRUE),x,bottom-.019,14,align="centre")
  }
  line_at(C[1],C[2],bottom,bottom,black,1.1)
  text_at("Months (log scale)",mean(C),bottom-.049,14,align="centre")

  for (i in seq_len(row_count)) {
    row <- rows[[i]]
    d <- records[row$index,,drop=FALSE]
    nn <- nrow(d); y <- yy[i]; height <- row_heights[i]
    text_at(row_labels[i],A[1]-.012,y,TEXT_PT,align="right")
    start <- 0
    for (cat in names(response_colors)) {
      count <- sum(d$response==cat)
      if (!count) next
      width <- 100*count/nn
      rect_at(pct_x(A,start),pct_x(A,start+width),y-bar_height/2,y+bar_height/2,
        response_colors[cat],black,.6)
      bar_count(count,pct_x(A,start),pct_x(A,start+width),y,bar_height)
      start <- start+width
    }
    stopifnot(abs(start-100)<1e-8)
    start <- 0
    for (cat in names(recurrence_colors)) {
      count <- sum(d$recurrence==cat)
      if (!count) next
      width <- 100*count/nn
      rect_at(pct_x(B,start),pct_x(B,start+width),y-bar_height/2,y+bar_height/2,
        recurrence_colors[cat],black,.6)
      bar_count(count,pct_x(B,start),pct_x(B,start+width),y,bar_height)
      start <- start+width
    }
    stopifnot(abs(start-100)<1e-8)
    known <- d[is.finite(d$months),,drop=FALSE]
    if (nrow(known)) {
      if (startsWith(row$block,paste0("Clinical trial",": "))) {
        unique_values <- unique(known[c("months","operator","recurrence")])
        offsets <- if (nrow(unique_values)>1) seq(-height*.22,height*.22,length.out=nrow(unique_values)) else 0
        for (j in seq_len(nrow(unique_values))) {
          q <- unique_values[j,]
          count <- sum(known$months==q$months & known$operator==q$operator & known$recurrence==q$recurrence)
          dot_at(month_x(q$months),y+offsets[j],recurrence_colors[q$recurrence],q$operator)
          label <- paste0(if(q$operator=="=") "" else q$operator,
            format(q$months,trim=TRUE)," (n = ",count,")")
          right <- month_x(q$months)<C[2]-.055
          text_at(label,month_x(q$months)+if(right).006 else -.006,y+offsets[j],11,
            align=if(right) "left" else "right")
        }
      } else {
        for (value in unique(known$months)) {
          one_month <- known[known$months==value,,drop=FALSE]
          offsets <- if(nrow(one_month)>1) seq(-height*.22,height*.22,length.out=nrow(one_month)) else 0
          for (j in seq_len(nrow(one_month)))
            dot_at(month_x(value),y+offsets[j],recurrence_colors[one_month$recurrence[j]],one_month$operator[j])
        }
      }
    }
    text_at(paste0("ND (n = ",sum(!is.finite(d$months)),")"),C[2]-.004,
      y-height*.34,12,align="right",color="#555555")
  }
  # The three legends retain the positions and color encoding of the approved image.
  text_at(paste0("Clinical response"," (n = ",nrow(records),")"),A[1],.195,14,bold=TRUE)
  response_short <- c("Complete clinical", "Partial", "Stabilization", "No response", "Progression", "ND")
  for (j in seq_along(response_short)) {
    column <- (j-1) %% 2; row <- (j-1) %/% 2
    x <- A[1]+column*.125; y <- .147-row*.026
    rect_at(x,x+.011,y-.006,y+.006,unname(response_colors[j]),black,.6)
    text_at(response_short[j],x+.016,y,14)
  }
  text_at(paste0("Recurrence"," (n = ",nrow(records),")"),B[1],.195,14,bold=TRUE)
  recurrence_short <- c("No recurrence reported", "Recurrence reported", "ND")
  for (j in seq_along(recurrence_short)) {
    y <- .147-(j-1)*.026
    rect_at(B[1],B[1]+.011,y-.006,y+.006,unname(recurrence_colors[j]),black,.6)
    text_at(recurrence_short[j],B[1]+.016,y,14)
  }
  text_at(paste0("Follow-up"," (n = ",nrow(records),")"),C[1],.195,14,bold=TRUE)
  dot_at(C[1]+.005,.147,"white","=")
  text_at("Exact follow-up",C[1]+.019,.147,14)
  dot_at(C[1]+.005,.121,"white",">")
  text_at("Lower bound (> or ≥)",C[1]+.019,.121,14)
}

# 8. Export the four outputs ------------------------------------------------------
# Preserve previous exports by adding run01, run02, etc. when necessary.
suffix <- ""; run <- 0L
repeat {
  names_out <- file.path(output_dir,c(
    paste0(file_stem,suffix,".png"), paste0(file_stem,suffix,".tiff"),
    paste0(file_stem,suffix,"_summary.csv"), paste0(file_stem,suffix,"_audit.xlsx")))
  if (!any(file.exists(names_out))) break
  run <- run + 1L
  suffix <- sprintf("_run%02d",run)
}
png_path <- names_out[1]; tiff_path <- names_out[2]
csv_path <- names_out[3]; audit_path <- names_out[4]
export_image <- function(path,format) {
  if (format=="png") {
    ragg::agg_png(path,width=WIDTH_CM,height=plot_height,units="cm",res=DPI,background="white")
  } else {
    ragg::agg_tiff(path,width=WIDTH_CM,height=plot_height,units="cm",res=DPI,
                   background="white",compression="lzw")
  }
  on.exit(grDevices::dev.off(),add=TRUE)
  draw_figure()
}
export_image(png_path,"png")
export_image(tiff_path,"tiff")

summary_csv <- summary_rows
percent_columns <- c("complete", "partial", "stable", "no_response", "progressive",
                     "response_nd", "recurrence_no", "recurrence_yes", "recurrence_nd")
percentages <- as.data.frame(lapply(summary_rows[percent_columns], function(x)
  round(100 * x / summary_rows$n, 1)))
names(percentages) <- paste0(names(percentages), "_pct")
summary_csv <- cbind(summary_csv, percentages)
names(summary_csv)[1:15] <- c("Block", "Treatment group", "Records", "Complete response", "Partial response", "Clinical stabilization", "No response", "Clinical progression", "Response ND", "Recurrence no", "Recurrence yes", "Recurrence ND", "Follow-up exact", "Follow-up lower bound", "Follow-up ND")
utils::write.csv(summary_csv,csv_path,row.names=FALSE,na="",fileEncoding="UTF-8")

# 9. Audit of the results ------------------------------------------------------
# Comparisons below use the results currently reported in the manuscript.
# Any discrepancy is flagged rather than silently corrected.
clinical_total <- vapply(response_levels,function(v)sum(records$response==v),integer(1))
rec_total <- vapply(recurrence_levels,function(v)sum(records$recurrence==v),integer(1))
by_group <- function(group,trial_filter) sum(records$group==group & records$is_trial==trial_filter)
expected <- c(256,153,103,103,50,38,67,4,35,6,3,110,4,39,48,105,
              54,26,23,16,16,6,3,2,7)
actual <- c(total_records,nrow(records),total_records-nrow(records),
            sum(records$is_trial),sum(!records$is_trial),
            clinical_total[1],clinical_total[2],clinical_total[3],
            clinical_total[4],clinical_total[5],clinical_total[6],
            rec_total[3],rec_total[2],rec_total[1],
            sum(is.finite(records$months)),sum(!is.finite(records$months)),
            by_group(group_names[1],TRUE),by_group(group_names[2],TRUE),
            by_group(group_names[3],TRUE),by_group(group_names[4],FALSE),
            by_group(group_names[5],FALSE),by_group(group_names[6],FALSE),
            by_group(group_names[7],FALSE),by_group(group_names[8],FALSE),
            by_group(group_names[9],FALSE))
checks <- data.frame(Metric=c("Individual records", "Treatment-outcome linked records", "Records not included", "Clinical trial records", "Other-study records", "Complete clinical responses", "Partial responses", "Clinical stabilization", "No response", "Clinical progression", "Response ND", "Recurrence not reported", "Recurrence yes", "Recurrence no", "Follow-up available", "Follow-up ND", "Trial: MDT", "Trial: MDT plus itraconazole", "Trial: control", "Other studies: local procedures", "Other studies: local plus systemic", "Other studies: itraconazole", "Other studies: clofazimine plus itraconazole", "Other studies: posaconazole", "Other studies: other medications"),Manuscript=expected,Database=as.integer(actual),
                     Status=ifelse(expected==actual,"PASS","CHECK"),check.names=FALSE)

# Record-level audit includes the 103 excluded records and the original cell contents.
group_by_id <- setNames(records$group,records[[col_id]])
response_by_id <- setNames(records$response,records[[col_id]])
rec_by_id <- setNames(records$recurrence,records[[col_id]])
months_by_id <- setNames(records$months,records[[col_id]])
operator_by_id <- setNames(records$operator,records[[col_id]])
ids <- individual[[col_id]]
trace <- data.frame(
  id=ids, study=individual[[col_study]],
  trial=ifelse(individual$is_trial,"Yes","No"),
  included=ifelse(individual$included,"Yes","No"),
  treatment=individual[[col_treatment]],
  group=unname(group_by_id[ids]),
  response=unname(response_by_id[ids]),
  recurrence=unname(rec_by_id[ids]),
  follow_reported=individual[[col_follow]],
  months=as.numeric(unname(months_by_id[ids])),
  operator=unname(operator_by_id[ids]),
  stringsAsFactors=FALSE,check.names=FALSE)
trace$group[is.na(trace$group)] <- "ND"
trace$response[is.na(trace$response)] <- "ND"
trace$recurrence[is.na(trace$recurrence)] <- "ND"
trace$operator[is.na(trace$operator)] <- "ND"
trace$months[!is.finite(trace$months)] <- NA_real_
names(trace) <- c("Record ID", "Study ID", "Clinical trial", "Included", "Treatment linked to outcome", "Treatment group", "Clinical response", "Recurrence", "Follow-up reported", "Follow-up months", "Follow-up operator")

book <- openxlsx::createWorkbook()
openxlsx::addWorksheet(book,"Audit")
sh <- "Audit"
openxlsx::writeData(book,sh,"FIGURE 5 | DATA AND MANUSCRIPT AUDIT",startRow=1)
openxlsx::writeData(book,sh,paste0("Source: ",basename(excel_file)),startRow=2)
openxlsx::writeData(book,sh,"A. MANUSCRIPT CHECKS",startRow=4)
openxlsx::writeData(book,sh,checks,startRow=5)
group_start <- 5+nrow(checks)+3
openxlsx::writeData(book,sh,"B. TREATMENT GROUP COUNTS",startRow=group_start)
openxlsx::writeData(book,sh,summary_csv,startRow=group_start+1)
trace_start <- group_start+1+nrow(summary_csv)+3
openxlsx::writeData(book,sh,"C. RECORD-LEVEL TRACEABILITY",startRow=trace_start)
openxlsx::writeData(book,sh,trace,startRow=trace_start+1)
notes_start <- trace_start+1+nrow(trace)+3
openxlsx::writeData(book,sh,"Follow-up duration is not the duration of treatment. Lower bounds are not treated as exact durations.",startRow=notes_start)
openxlsx::writeData(book,sh,"Missing values are coded ND; no missing outcomes are imputed.",startRow=notes_start+1)
openxlsx::writeData(book,sh,"Counts are descriptive and are checked against Table 2 and Figure 5 of the manuscript.",startRow=notes_start+2)
section_style <- openxlsx::createStyle(fgFill="#E7EDF2",textDecoration="bold")
header_style <- openxlsx::createStyle(fgFill="#34465A",fontColour="#FFFFFF",textDecoration="bold")
for (r in c(1,4,group_start,trace_start))
  openxlsx::addStyle(book,sh,section_style,rows=r,cols=1:24,gridExpand=TRUE,stack=TRUE)
for (r in c(5,group_start+1,trace_start+1))
  openxlsx::addStyle(book,sh,header_style,rows=r,cols=1:24,gridExpand=TRUE,stack=TRUE)
openxlsx::setColWidths(book,sh,cols=1,widths=38)
openxlsx::setColWidths(book,sh,cols=2:5,widths=21)
openxlsx::setColWidths(book,sh,cols=6,widths=31)
openxlsx::setColWidths(book,sh,cols=7:24,widths=20)
openxlsx::freezePane(book,sh,firstActiveRow=6)
openxlsx::saveWorkbook(book,audit_path,overwrite=FALSE)

stopifnot(all(file.exists(names_out)),all(file.info(names_out)$size>0))
message("Four files created:","\n",paste(basename(names_out),collapse="\n"))
message("Figure 5: records used in the analysis: ",nrow(records)," / ",total_records)
message("Audit checks: ",sum(checks$Status=="PASS"),"/",nrow(checks)," PASS")
if (any(checks$Status=="CHECK")) warning("Manuscript checks require review. Inspect audit XLSX.")
