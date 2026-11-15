# FIGURA 3. Distribución geográfica y patrones epidemiológicos y clínicos
# A: registros individuales por país y región probable de adquisición.
# B: exposición, cuartiles de edad y distribución clínica (registros completos).
# Entrada: Lobomycosis_base_datos.xlsx, seleccionado con file.choose().
# Salidas: PNG, TIFF, CSV y Excel de auditoría de una sola hoja, junto al script.

# 1. Paquetes y configuración ---------------------------------------------------------
DPI <- 1200
ANCHO_CM <- 36
ALTO_CM <- 24
FUENTE <- "Arial"
ALFA_CONEXIONES <- 0.55
GROSOR_CONEXIONES <- 0.45
ALFA_VERDE <- 0.46
ALFA_MORADO <- 0.72
DIAMETRO_MAPA_MM <- 22
ARCHIVO_EXCEL <- NULL
ARCHIVO_MUNDO <- ""        # Optional local Natural Earth .gpkg/.shp file
ARCHIVO_REGIONES <- ""     # Optional verified regional boundaries, with a Region field
ARCHIVO_DELIMITACIONES <- "" # Optional workbook with geographic WKT boundaries
CATEGORIAS_CLINICAS <- NULL
CORRECCION_GUYANA_REVISADA <- TRUE
COLORES_REGION <- c("Amazonía" = "#B7C9AD", "Escudo Guayanés" = "#D5BCE5",
                   "Centroamérica" = "#E5C995", "Otras regiones" = "#B3B3B3")
COLORES_CONEXIONES <- c("Amazonía" = "#53735A", "Escudo Guayanés" = "#765584",
                        "Centroamérica" = "#92723C", "Otras regiones" = "#5D5D5D")

# Los paquetes faltantes se instalan por separado, solo una vez.
paquetes <- c("readxl", "dplyr", "tidyr", "ggplot2", "sf", "ggalluvial",
              "ragg", "systemfonts", "cowplot", "scales", "ggrepel",
              "rnaturalearth", "openxlsx")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) stop("Faltan paquetes: ", paste(faltan, collapse = ", "),
                         "\ninstall.packages(c(", paste(sprintf('"%s"', faltan), collapse = ", "), "))")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))
if (!any(tolower(systemfonts::system_fonts()$family) == tolower(FUENTE)))
  stop("No se encontró la fuente Arial en el equipo.")

# 2. Selección del Excel y carpeta de salida ---------------------------------------------
# Al ejecutar con Source, las salidas quedan junto al .R, no junto al Excel.
find_script_directory <- function() {
  scripts <- vapply(sys.frames(), function(f) {
    if (is.null(f$ofile)) NA_character_ else as.character(f$ofile)[1]
  }, character(1))
  scripts <- scripts[!is.na(scripts) & nzchar(scripts)]
  if (length(scripts) && file.exists(tail(scripts, 1)))
    return(dirname(normalizePath(tail(scripts, 1), winslash = "/")))
  args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(args)) {
    f <- sub("^--file=", "", args[1])
    if (file.exists(f)) return(dirname(normalizePath(f, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (length(p) == 1 && !is.na(p) && nzchar(p) && file.exists(p))
      return(dirname(normalizePath(p, winslash = "/")))
  }
  stop("Save the script and run it with Source in RStudio to identify its folder.")
}
carpeta_salida <- find_script_directory()
prefijo <- "fig03_geo_alluvial_es"
if (!is.null(ARCHIVO_EXCEL)) {
  archivo_excel <- ARCHIVO_EXCEL
} else if (interactive()) {
  message("Seleccione Lobomycosis_base_datos.xlsx (base en español).")
  archivo_excel <- file.choose()
} else {
  archivo_excel <- Sys.getenv("LOBOMYCOSIS_DATA", unset = "")
  if (!nzchar(archivo_excel))
    archivo_excel <- file.path(carpeta_salida, "..", "data", "raw", "Lobomycosis_base_datos.xlsx")
}
archivo_excel <- normalizePath(archivo_excel, winslash = "/", mustWork = TRUE)
message("Los archivos de salida se guardarán junto al script: ", carpeta_salida)

# 3. Lectura y comprobación de registros individuales ---------------------------------------------
hoja <- "Datos a nivel individual"
requeridas <- c("ID_registro", "Exposición", "Probable country of acquisition",
                "Región probable de adquisición", "Edad_anos", "Distribucion_clinica")
buscar_hoja <- function(ruta, columnas) {
  hojas <- readxl::excel_sheets(ruta)
  encontradas <- vapply(hojas, function(h) {
    nombres <- names(suppressMessages(readxl::read_excel(ruta, sheet = h, n_max = 0)))
    all(columnas %in% trimws(nombres))
  }, logical(1))
  if (sum(encontradas) != 1) stop("Expected exactly one worksheet with: ", paste(columnas, collapse = ", "))
  hojas[encontradas]
}
if (!(hoja %in% readxl::excel_sheets(archivo_excel)))
  stop("Worksheet not found: ", hoja)
base <- as.data.frame(suppressMessages(readxl::read_excel(
  archivo_excel, sheet = hoja, col_types = "text", .name_repair = "unique")))
names(base) <- trimws(names(base))
if (!all(requeridas %in% names(base)))
  stop("Missing columns: ", paste(setdiff(requeridas, names(base)), collapse = ", "))
limpiar <- function(x) {
  x <- trimws(gsub("\u00a0", " ", as.character(x), fixed = TRUE))
  x[is.na(x)] <- ""
  x
}
normalizar <- function(x) {
  z <- iconv(tolower(limpiar(x)), to = "ASCII//TRANSLIT")
  z[is.na(z)] <- ""
  z
}
ausentes <- c("", "nd", "nr", "n/d", "n/r", "na", "n/a", "-",
              "no disponible", "no informado", "no reportado", "not reported", "not available")
disponible <- function(x) !normalizar(x) %in% ausentes
base[] <- lapply(base, limpiar)
# Se normalizan referencias internas; el archivo original no se modifica.
base$ID_registro <- base[["ID_registro"]]
if (any(!grepl("^LBM-[0-9]+-", base$ID_registro)) ||
    any(!nzchar(base$ID_registro)) || anyDuplicated(base$ID_registro))
  stop("The record identifiers are missing, invalid or duplicated.")
total_base <- nrow(base)
individuales <- base
individuales$Pais_adquisicion <- individuales[["Probable country of acquisition"]]
# Keep the previous reviewed correction only if an older database still contains it.
if (CORRECCION_GUYANA_REVISADA) {
  revisar <- normalizar(individuales$Pais_adquisicion) == "guyana/venezuela"
  individuales$Pais_adquisicion[revisar] <- "Guyana"
  if (any(revisar)) message("Reviewed Guyana assignment applied in memory: ", sum(revisar))
}
individuales$Region <- individuales[["Región probable de adquisición"]]
individuales$Region[!disponible(individuales$Region)] <- "ND"
catalogo <- data.frame(
  Pais = c("Bolivia","Brasil","Colombia","Ecuador","Guayana Francesa","Grecia",
            "Guyana","India","México","Panamá","Perú","Palestina","Surinam",
            "Venezuela","Canadá","Francia","Alemania","Italia","Reino Unido","Estados Unidos"),
  ISO3 = c("BOL","BRA","COL","ECU","GUF","GRC","GUY","IND","MEX","PAN",
           "PER","PSE","SUR","VEN","CAN","FRA","DEU","ITA","GBR","USA"),
  Nombre_EN = c("Bolivia","Brazil","Colombia","Ecuador","French Guiana","Greece",
                "Guyana","India","Mexico","Panama","Peru","Palestine","Suriname",
                "Venezuela","Canada","France","Germany","Italy","United Kingdom","United States"),
  Lon = c(-64,-53,-73,-78,-53,24,-59,79,-103,-80,-75,35.2,-56,-65,-105,2,10,12,-3,-100),
  Lat = c(-17,-11,4,-2,4,39,5,23,24,9,-10,31.9,4,8,58,47,51,43,55,39))
dic <- setNames(rep(catalogo$ISO3, 2), normalizar(c(catalogo$Pais, catalogo$Nombre_EN)))
dic["surinam"] <- "SUR"
dic["surinamee"] <- "SUR" # Spelling variant found in the English source workbook.
dic["guayana francesa"] <- "GUF"
dic["united states of america"] <- "USA"
a_iso <- function(x) {
  claves <- normalizar(x)
  codigos <- unname(dic[claves])
  codigos[!disponible(x)] <- "ND"
  if (anyNA(codigos)) stop("Unrecognized country: ", paste(unique(x[is.na(codigos)]), collapse = ", "))
  codigos
}

# 4. CONTEOS PARA EL MAPA -------------------------------------------------
individuales$Asignacion <- ifelse(!disponible(individuales$Pais_adquisicion), "ND",
  ifelse(grepl("/", individuales$Pais_adquisicion, fixed = TRUE), "País ambiguo", "País único"))
individuales$ISO3 <- "ND"
con_pais <- individuales$Asignacion == "País único"
individuales$ISO3[con_pais] <- a_iso(individuales$Pais_adquisicion[con_pais])
conteos <- individuales %>% filter(Asignacion == "País único") %>% count(ISO3, name = "Individuales")
resumen <- catalogo %>% left_join(conteos, by = "ISO3") %>%
  mutate(Individuales = replace_na(Individuales, 0L))
regiones <- individuales %>% count(Region, name = "N")
max_ind <- max(resumen$Individuales)
if (max_ind < 4) stop("No hay suficientes valores para cuatro clases geométricas de círculos.")
limites_ind <- c(0, floor(exp(seq(log(1), log(max_ind), length.out = 5))[-1]))
limites_ind[5] <- max_ind
if (any(diff(limites_ind) <= 0)) stop("Las cuatro clases geométricas no son distintas.")
etiquetas_ind <- paste0(limites_ind[1:4] + 1, "–", limites_ind[2:5])
pal_ind <- c("#4B146B", "#81449D", "#B68DCA", "#E1CBEE")
resumen$Color <- pal_ind[as.integer(cut(resumen$Individuales, breaks = limites_ind,
                                      include.lowest = TRUE, right = TRUE))]
verde <- function(n) scales::col_numeric(c("#DCEFD4", "#88BF83", "#337D48", "#155832"),
  domain = c(0, log1p(max(regiones$N))))(log1p(n))

# 5. CARTOGRAFÍA ---------------------------------------------------------
# Natural Earth a escala 1:50 millones; la descarga se mantiene en la carpeta temporal de R.
cargar_mundo <- function() {
  if (nzchar(ARCHIVO_MUNDO)) {
    if (!file.exists(ARCHIVO_MUNDO)) stop("Map file not found: ", ARCHIVO_MUNDO)
    return(sf::st_read(ARCHIVO_MUNDO, quiet = TRUE))
  }
  if (requireNamespace("rnaturalearthdata", quietly = TRUE)) {
    mapa_env <- new.env(parent = emptyenv())
    suppressWarnings(utils::data("map_units50", package = "rnaturalearthdata", envir = mapa_env))
    if (exists("map_units50", envir = mapa_env, inherits = FALSE)) {
      return(sf::st_as_sf(get("map_units50", envir = mapa_env, inherits = FALSE)))
    }
  }
  message("Recuperando cartografía de Natural Earth a escala 1:50 millones...")
  rnaturalearth::ne_download(scale = 50, type = "map_units", category = "cultural",
                            returnclass = "sf", destdir = tempdir())
}
mundo <- cargar_mundo()
if (is.na(sf::st_crs(mundo))) stop("La cartografía necesita CRS declarado.")
mundo <- sf::st_transform(mundo, 4326)
CRS_REGION <- "+proj=laea +lat_0=-4 +lon_0=-70 +datum=WGS84 +units=m +no_defs"
caja <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(
  c(xmin = -115, ymin = -38, xmax = -32, ymax = 30), crs = sf::st_crs(4326))), CRS_REGION)
america_ll <- suppressWarnings(sf::st_crop(mundo, sf::st_bbox(
  c(xmin = -115, ymin = -38, xmax = -32, ymax = 30), crs = sf::st_crs(4326))))
america <- sf::st_make_valid(sf::st_transform(america_ll, CRS_REGION))
limites <- sf::st_bbox(caja)
bocetos <- list(
 "Amazonía" = c(-79,-2,-75,2,-69,3,-65,2,-59,1,-53,-1,-48,-2,-49,-6,-53,-11,-59,-15,-65,-14,-69,-12,-74,-11,-77,-6),
 "Orinoquía" = c(-74,4,-72,8,-67,10,-62,9,-61,7,-64,4,-69,2,-72,2),
 "Escudo Guayanés" = c(-65,3,-64,7,-60,8,-56,6,-51,4,-50,1,-55,0,-59,1),
 "Centroamérica" = c(-93,18,-86,17,-83,13,-76,10,-77,7,-84,8,-89,13),
 "Pacífico colombiano" = c(-79,1,-78,4,-77.3,8,-76.5,8,-76,4,-77,1),
 "Sudeste de México" = c(-97,19,-92,22,-86,21,-87,17,-92,15,-95,16),
 "Caribe" = c(-88,25,-75,25,-57,18,-60,10,-75,10,-88,17),
 "Sudeste de Brasil" = c(-49,-19,-43,-17,-39,-20,-44,-25,-49,-24))
suavizar <- function(v) {
  p <- matrix(v,ncol = 2,byrow = TRUE); m <- nrow(p); salida <- list()
  for (i in seq_len(m)) {
    ix <- ((c(i-1,i,i+1,i+2)-1) %% m)+1
    p0 <- p[ix[1],];p1 <- p[ix[2],];p2 <- p[ix[3],];p3 <- p[ix[4],]
    salida[[i]] <- t(vapply(seq(0,.95,by = .05),function(t) .5*((2*p1)+(-p0+p2)*t+(2*p0-5*p1+4*p2-p3)*t^2+(-p0+3*p1-3*p2+p3)*t^3),numeric(2)))
  }
  xy <- do.call(rbind,salida);rbind(xy,xy[1,])
}

# Prioridad: capa externa > WKT del Excel > bocetos del script original.
columnas_geo <- c("Region_delimitada", "Limite_WKT", "EPSG", "Tipo_limite")
geo_excel <- data.frame()
geo_base <- individuales
if (nzchar(ARCHIVO_DELIMITACIONES)) {
  hg <- buscar_hoja(ARCHIVO_DELIMITACIONES, columnas_geo)
  geo_base <- as.data.frame(readxl::read_excel(ARCHIVO_DELIMITACIONES, sheet = hg, col_types = "text"))
}
if (all(columnas_geo %in% names(geo_base))) {
  geo_excel <- geo_base %>% select(all_of(columnas_geo)) %>%
    filter(!is.na(Region_delimitada), !is.na(Limite_WKT), Limite_WKT != "ND", Limite_WKT != "") %>% distinct()
  if (anyDuplicated(geo_excel$Region_delimitada)) stop("Hay varias delimitaciones para una misma región.")
}
if (nzchar(ARCHIVO_REGIONES)) {
  regiones_sf <- sf::st_read(ARCHIVO_REGIONES, quiet = TRUE)
  if (!"Region" %in% names(regiones_sf) || is.na(sf::st_crs(regiones_sf)))
    stop("La capa de regiones necesita columna Region y CRS.")
  regiones_sf <- regiones_sf %>% select(Region) %>% group_by(Region) %>% summarise(.groups = "drop")
  esquematico <- FALSE
} else if (nrow(geo_excel)) {
  if (any(is.na(geo_excel$EPSG) | geo_excel$EPSG != "4326")) stop("El WKT debe tener EPSG 4326.")
  regiones_sf <- sf::st_sf(Region = geo_excel$Region_delimitada,
                         geometry = sf::st_as_sfc(geo_excel$Limite_WKT, crs = 4326))
  esquematico <- any(is.na(geo_excel$Tipo_limite) | geo_excel$Tipo_limite != "Verificado")
} else {
  esquematico <- TRUE
  regiones_sf <- sf::st_sf(Region = names(bocetos), geometry = sf::st_sfc(
    lapply(bocetos, function(v) sf::st_polygon(list(suavizar(v)))), crs = 4326))
}
regiones_sf <- regiones_sf %>% left_join(regiones, by = "Region") %>% filter(!is.na(N), Region != "ND")
regiones_sf <- sf::st_make_valid(sf::st_transform(regiones_sf, CRS_REGION))
regiones_recorte <- suppressWarnings(sf::st_intersection(regiones_sf,
                                           sf::st_union(sf::st_geometry(america))))
regiones_recorte$Color <- verde(regiones_recorte$N)
ley_reg <- regiones_sf %>% sf::st_drop_geometry() %>% distinct(Region, N) %>% arrange(desc(N))
pts <- sf::st_as_sf(resumen, coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)
xy <- sf::st_coordinates(sf::st_transform(pts, CRS_REGION))
puntos <- sf::st_drop_geometry(pts) %>% mutate(X = xy[,1], Y = xy[,2])
pts_america <- puntos %>% filter(Lon >= -115, Lon <= -32, Lat >= -38, Lat <= 30, Individuales > 0)
pts_america <- pts_america[order(-pts_america$Individuales), ]
n_mapa <- sum(pts_america$Individuales)

# 6. PREPARAR EL FLUJO CON EDAD Y LAS CATEGORÍAS DE LA BASE ----------------
# B contiene exclusivamente registros con las cuatro variables informadas.
# La región sigue definiendo los colores, pero deja de ser un eje del flujo.
datos <- data.frame(
  ID_registro = limpiar(base$ID_registro),
  Exposicion_original = limpiar(base[["Exposición"]]),
  Region_original = limpiar(base[["Región probable de adquisición"]]),
  Edad_original = limpiar(base[["Edad_anos"]]),
  Distribucion = limpiar(base[["Distribucion_clinica"]]),
  stringsAsFactors = FALSE
)
categorias_celda <- unique(datos$Distribucion[disponible(datos$Distribucion)])
if (!length(categorias_celda)) stop(
  "Distribucion_clinica no contiene categorías clínicas distintas de ND.")
if (is.null(CATEGORIAS_CLINICAS)) {
  CATEGORIAS_CLINICAS <- categorias_celda
} else {
  faltantes_orden <- setdiff(categorias_celda, CATEGORIAS_CLINICAS)
  CATEGORIAS_CLINICAS <- unique(c(CATEGORIAS_CLINICAS, faltantes_orden))
}
message("Categorías clínicas tomadas literalmente de la columna: ",
        paste(CATEGORIAS_CLINICAS, collapse = "; "), ".")
datos$Edad_anos <- suppressWarnings(as.numeric(
  sub(",", ".", datos$Edad_original, fixed = TRUE)))
edad_invalida <- disponible(datos$Edad_original) &
  (!is.finite(datos$Edad_anos) | datos$Edad_anos < 0)
if (any(edad_invalida)) stop(
  "Edad_anos debe contener una edad individual numérica o ND. Revise: ",
  paste(datos$ID_registro[edad_invalida], collapse = ", "))
# Una región combinada no identifica una sola región para el color de B.
region_ambigua <- disponible(datos$Region_original) & grepl("/", datos$Region_original)
exposicion_aceptada <- disponible(datos$Exposicion_original)
completos <- exposicion_aceptada & disponible(datos$Region_original) &
  disponible(datos$Distribucion) & !region_ambigua & is.finite(datos$Edad_anos)
# Auditoría: una fila por registro, con todas las causas de exclusión aplicables.
auditoria <- datos
auditoria$Incluido_B <- completos
auditoria$Exposicion_ND <- !disponible(datos$Exposicion_original)
auditoria$Motivo_exclusion <- vapply(seq_len(nrow(datos)), function(i) {
  razones <- c(
    if (!exposicion_aceptada[i]) "Exposición no informada",
    if (!disponible(datos$Region_original[i])) "Región no informada",
    if (region_ambigua[i]) "Región múltiple",
    if (!disponible(datos$Distribucion[i])) "Distribución clínica no informada",
    if (!is.finite(datos$Edad_anos[i])) "Edad no informada")
  paste(razones, collapse = "; ")
}, character(1))
datos <- datos[completos, , drop = FALSE]
n <- nrow(datos)
if (!n) stop("No hay registros con región única, edad y distribución clínica informadas.")
# Los cuantiles se calculan después del filtro de B. Con edades enteras, un
# cuantil fraccionario se asigna al entero inferior, sin dividir edades iguales.
cuartiles_edad <- unname(stats::quantile(datos$Edad_anos,
  probs = c(.25, .50, .75), type = 7, na.rm = TRUE))
if (all(datos$Edad_anos == floor(datos$Edad_anos))) {
  limites_edad <- floor(cuartiles_edad)
  if (any(diff(limites_edad) <= 0)) stop(
    "Los cuartiles de edad coinciden: no se pueden formar cuatro grupos distintos.")
  EDAD_ETIQUETAS <- paste0(c(min(datos$Edad_anos), limites_edad + 1), "–",
    c(limites_edad, max(datos$Edad_anos)), " años")
  datos$Edad <- cut(datos$Edad_anos, breaks = c(-Inf, limites_edad, Inf),
    labels = EDAD_ETIQUETAS, right = TRUE, include.lowest = TRUE)
} else {
  if (any(diff(cuartiles_edad) <= 0)) stop(
    "Los cuartiles de edad coinciden: no se pueden formar cuatro grupos distintos.")
  formato_edad <- function(x) format(x, trim = TRUE, scientific = FALSE, digits = 8)
  EDAD_ETIQUETAS <- c(paste0("≤", formato_edad(cuartiles_edad[1]), " años"),
    paste0(">", formato_edad(cuartiles_edad[1:2]), "–≤",
           formato_edad(cuartiles_edad[2:3]), " años"),
    paste0(">", formato_edad(cuartiles_edad[3]), " años"))
  datos$Edad <- cut(datos$Edad_anos, breaks = c(-Inf, cuartiles_edad, Inf),
    labels = EDAD_ETIQUETAS, right = TRUE, include.lowest = TRUE)
}
if (anyNA(datos$Edad)) stop("Hay edades fuera de los cuatro grupos calculados.")
message("Cuartiles de edad en B (P25, P50, P75): ",
  paste(cuartiles_edad, collapse = ", "), " años.")

# 6.1 AGRUPACIONES EXPLÍCITAS PARA LA FIGURA -------------------------------
# Exposiciones separadas por / se conservan como un único caso combinado.
# Estas agrupaciones afectan solo a la figura, no a la base original.
etiquetas_exposicion <- c(
  "Ocupacional forestal" = "Forestal",
  "Ocupacional agrícola" = "Agrícola",
  "Ocupacional minera" = "Minera",
  "Acuática dulce" = "Acuática",
  "Acuática marina" = "Acuática",
  "Acuática" = "Acuática",
  "Ambiental rural o forestal" = "Ambiental",
  "Ambiental forestal" = "Ambiental",
  "Ambiental rural" = "Ambiental",
  "Ocupacional pesquera o fluvial" = "Pesquera/fluvial",
  "Viaje" = "Viaje",
  "Ocupacional de laboratorio" = "Laboratorio"
)
datos$Exposicion <- datos$Exposicion_original
multiple <- grepl("/", datos$Exposicion_original)
datos$Exposicion[multiple] <- "Combinada"
simple <- !multiple
recodificada <- unname(etiquetas_exposicion[datos$Exposicion_original[simple]])
# Si aparece una categoría nueva simple, se conserva con su nombre original.
recodificada[is.na(recodificada)] <- datos$Exposicion_original[simple][is.na(recodificada)]
datos$Exposicion[simple] <- recodificada
datos$Region <- ifelse(datos$Region_original %in% c("Amazonía", "Escudo Guayanés", "Centroamérica"),
                        datos$Region_original, "Otras regiones")
orden_exp <- c("Forestal", "Agrícola", "Combinada", "Minera", "Ambiental", "Acuática",
                "Pesquera/fluvial", "Viaje", "Laboratorio")
orden_exp <- unique(c(orden_exp[orden_exp %in% datos$Exposicion], datos$Exposicion))
datos$Exposicion <- factor(datos$Exposicion, levels = orden_exp)
datos$Region <- factor(datos$Region, levels = names(COLORES_REGION))
datos$Distribucion <- factor(datos$Distribucion, levels = CATEGORIAS_CLINICAS)

# Contar las combinaciones observadas. No se expande un registro en varias filas.
flujos <- aggregate(rep(1L, n),
                    by = list(Exposicion = datos$Exposicion, Edad = datos$Edad,
                              Distribucion = datos$Distribucion, Region = datos$Region), FUN = sum)
names(flujos)[5] <- "Casos"
stopifnot(sum(flujos$Casos) == n, !anyNA(flujos$Edad), !anyNA(flujos$Distribucion))
message("Base individual: ", total_base, " casos.")
message("Casos incluidos en el diagrama: ", n, "; excluidos por información insuficiente: ", total_base - n, ".")
conteos_edad_B <- data.frame(Edad = EDAD_ETIQUETAS,
  N = as.integer(table(factor(datos$Edad, levels = EDAD_ETIQUETAS))))
conteos_clinica_B <- data.frame(Distribucion = CATEGORIAS_CLINICAS,
  N = as.integer(table(factor(datos$Distribucion, levels = CATEGORIAS_CLINICAS))))
message("Edades incluidas en B:")
print(conteos_edad_B, row.names = FALSE)
message("Distribución clínica en B (incluye categorías con cero casos):")
print(conteos_clinica_B, row.names = FALSE)
categorias_sin_B <- conteos_clinica_B$Distribucion[conteos_clinica_B$N == 0]
if (length(categorias_sin_B)) message(
  "Sin casos tras el filtro de B: ",
  paste(categorias_sin_B, collapse = ", "),
  ". No se dibujan nodos ni flujos artificiales.")
message("Registros adicionales excluidos por edad ND: ",
        sum(exposicion_aceptada & disponible(base[["Región probable de adquisición"]]) &
            disponible(base[["Distribucion_clinica"]]) & !region_ambigua &
            !is.finite(auditoria$Edad_anos)))
message("Exposición no informada incluida en B: 0 (criterio de selección).")
message("Regiones agrupadas en Otras regiones: ",
        paste(sort(unique(datos$Region_original[datos$Region == "Otras regiones"])), collapse = "; "))




# 7. MAPA, LEYENDAS INFERIORES Y DIAGRAMA ---------------------------------
tema_mapa <- theme_void(base_family = FUENTE, base_size = 11) + theme(
  text = element_text(family = FUENTE, face = "plain", colour = "black"),
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.grid = element_blank(), plot.margin = margin(4, 4, 4, 4))
mapa <- ggplot() + geom_sf(data = america, fill = "#EFEEED", colour = "#B8B4B1", linewidth = .15) +
  geom_sf(data = regiones_recorte,
    fill = grDevices::adjustcolor(regiones_recorte$Color, alpha.f = ALFA_VERDE), colour = NA) +
  geom_point(data = pts_america, aes(X, Y, size = Individuales), shape = 21,
    fill = grDevices::adjustcolor(pts_america$Color, alpha.f = ALFA_MORADO),
    colour = "#542371", stroke = .3, show.legend = FALSE) +
  scale_size_area(max_size = DIAMETRO_MAPA_MM, limits = c(0, max_ind), guide = "none") +
  coord_sf(crs = CRS_REGION, xlim = c(limites["xmin"], limites["xmax"]),
    ylim = c(limites["ymin"], limites["ymax"]), datum = NA, expand = FALSE, clip = "off") + tema_mapa
anclas <- data.frame(ISO3 = c("BRA","COL","VEN","BOL","ECU","PAN","MEX","GUY","SUR","GUF"),
  Lon = c(-36,-101,-46,-101,-105,-104,-108,-40,-38,-36),
  Lat = c(-28,1,18,-25,-12,13,28,8,0,-6))
et <- left_join(pts_america, anclas, by = "ISO3", suffix = c("", "_ancla"))
et$Lon_ancla[is.na(et$Lon_ancla)] <- et$Lon[is.na(et$Lon_ancla)] + 12
et$Lat_ancla[is.na(et$Lat_ancla)] <- et$Lat[is.na(et$Lat_ancla)]
coord_et <- sf::st_coordinates(sf::st_transform(
  sf::st_as_sf(et, coords = c("Lon_ancla", "Lat_ancla"), crs = 4326), CRS_REGION))
dx <- unname(limites["xmax"] - limites["xmin"])
dy <- unname(limites["ymax"] - limites["ymin"])
et$LX <- pmax(unname(limites["xmin"]) + .13 * dx,
              pmin(unname(limites["xmax"]) - .14 * dx, coord_et[,1]))
et$LY <- pmax(unname(limites["ymin"]) + .06 * dy,
              pmin(unname(limites["ymax"]) - .06 * dy, coord_et[,2]))
et$Etiqueta <- paste0(et$Pais, " (n = ", et$Individuales, ")")
mapa <- mapa + geom_segment(data = et, aes(x = LX, y = LY, xend = X, yend = Y),
  colour = "#817985", linewidth = .22,
  arrow = grid::arrow(length = grid::unit(1.1, "mm"), type = "open")) +
  ggrepel::geom_text_repel(data = et, aes(LX, LY, label = Etiqueta),
    family = FUENTE, fontface = "plain", size = 4.1, seed = 170,
    min.segment.length = Inf, max.overlaps = Inf, box.padding = .25,
    xlim = c(limites["xmin"], limites["xmax"]), ylim = c(limites["ymin"], limites["ymax"]))

# Cuatro clases geométricas: círculos proporcionales al límite superior de clase.
# Mapa y leyenda usan el mismo diámetro, borde y transparencia.
dc <- data.frame(N = limites_ind[2:5], Etiqueta = etiquetas_ind,
                 X = c(.12,.60,.12,.60), Y = c(.78,.78,.30,.30),
                 TX = c(.25,.73,.25,.73))
ley_casos <- ggplot(dc, aes(X,Y)) + geom_point(aes(size = N), shape = 21,
  fill = grDevices::adjustcolor(pal_ind, alpha.f = ALFA_MORADO), colour = "#542371", stroke = .3) +
  geom_text(aes(x = TX, label = Etiqueta), hjust = 0, family = FUENTE, size = 4.05) +
  scale_size_area(max_size = DIAMETRO_MAPA_MM, limits = c(0,max_ind), guide = "none") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE, clip = "off") +
  labs(title = "Casos individuales por país") + tema_mapa +
  theme(plot.title = element_text(size = 11, face = "bold", margin = margin(b = 5)))
ley_reg$Y <- seq(.85,.05, length.out = nrow(ley_reg))
ley_reg$Etiqueta <- paste0(ley_reg$Region, " (n = ", ley_reg$N, ")")
ley_reg$Color <- grDevices::adjustcolor(verde(ley_reg$N), alpha.f = ALFA_VERDE)
ley_regiones <- ggplot(ley_reg, aes(y = Y)) +
  geom_rect(aes(xmin = .02, xmax = .10, ymin = Y-.022, ymax = Y+.022),
             fill = ley_reg$Color, colour = NA) +
  geom_text(aes(x = .15, label = Etiqueta), hjust = 0, family = FUENTE, size = 3.85) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE, clip = "off") +
  labs(title = "Regiones representadas") + tema_mapa +
  theme(plot.title = element_text(size = 11, face = "bold", margin = margin(b = 5)))

# Orden cronológico de edades y orden clínico: de arriba hacia abajo.
# Los nodos conservan altura proporcional; sus etiquetas van dentro.
ancho_nodo <- .66
diagrama <- ggplot(flujos, aes(y = Casos, axis1 = Exposicion, axis2 = Edad,
                              axis3 = Distribucion)) +
  ggalluvial::geom_alluvium(aes(fill = Region), width = ancho_nodo, alpha = .72,
    colour = NA, reverse = TRUE, decreasing = NA, knot.pos = .4) +
  ggalluvial::geom_stratum(width = ancho_nodo, fill = "white", colour = "black",
    linewidth = .3, reverse = TRUE, decreasing = NA) +
  scale_fill_manual(values = COLORES_REGION, guide = "none") +
  scale_x_discrete(limits = c("Exposición", "Edad", "Distribución")) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(.65,3.35), ylim = c(0,n), expand = FALSE, clip = "off") +
  theme_void(base_family = FUENTE) + theme(plot.margin = margin(0,0,0,0))
nodos <- ggplot_build(diagrama)$data[[2]]
nodos$Nombre <- as.character(nodos$stratum)
nodos$Nombre_mostrado <- vapply(nodos$Nombre, function(x) {
  # El espacio tras / solo facilita el salto de línea de la etiqueta; no cambia
  # el valor leído de la celda ni la categoría utilizada en los conteos.
  x_para_salto <- gsub("/", "/ ", x, fixed = TRUE)
  paste(strwrap(x_para_salto, width = 20), collapse = "\n")
}, character(1))
etiqueta_larga <- grepl("\n", nodos$Nombre_mostrado, fixed = TRUE)
nodos$Etiqueta <- ifelse(nodos$count <= 2 & !etiqueta_larga,
  paste0(nodos$Nombre_mostrado, " (n = ", round(nodos$count), ")"),
  paste0(nodos$Nombre_mostrado, "\n(n = ", round(nodos$count), ")"))
nodos$Tamano <- ifelse(nodos$count <= 1, 2.65,
  ifelse(nodos$count <= 2, 3.0,
  ifelse(nodos$count <= 5, 3.6, 4.1)))
nodos$Tamano[etiqueta_larga] <- pmin(nodos$Tamano[etiqueta_larga], 3.15)
diagrama <- diagrama + geom_text(data = nodos,
  aes(x = x, y = y, label = Etiqueta, size = Tamano), inherit.aes = FALSE,
  family = FUENTE, lineheight = .95) + scale_size_identity()

# Las tres leyendas se dibujan bajo un encabezado común, a la misma altura.
lf <- data.frame(Region = names(COLORES_REGION),
  X = rep(.02,4), Y = c(.84,.63,.42,.21))
lf$Etiqueta <- paste0(lf$Region, " (n = ", as.integer(table(factor(datos$Region, levels = lf$Region))), ")")
ley_flujo <- ggplot(lf) +
  geom_rect(aes(xmin = X, xmax = X+.045, ymin = Y-.035, ymax = Y+.035),
    fill = grDevices::adjustcolor(unname(COLORES_REGION), alpha.f = .72),
    colour = "#777777", linewidth = .2) +
  geom_text(aes(x = X+.065, y = Y, label = Etiqueta), hjust = 0,
    family = FUENTE, size = 4.05) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE, clip = "off") +
  theme_void() + theme(plot.margin = margin(0,0,0,0))
ley_casos <- ley_casos + labs(title = NULL) + theme(plot.margin = margin(0,0,0,0))
ley_regiones <- ley_regiones + labs(title = NULL) + theme(plot.margin = margin(0,0,0,0))
mapa <- mapa + theme(plot.margin = margin(0,0,0,0))

# 8. CONEXIONES REALES ENTRE PAÍS Y EXPOSICIÓN ----------------------------
# Cada línea representa un registro incluido en B, no todos los registros de A.
# Se conserva una sola línea por ID; país ND se muestra como fuente no geográfica.
conexiones <- left_join(datos, individuales[,c("ID_registro","ISO3")], by = "ID_registro")
conexiones <- left_join(conexiones, puntos[,c("ISO3","X","Y")], by = "ISO3")
izq <- nodos[nodos$x == 1, c("stratum","ymin","ymax")]
names(izq)[1] <- "Exposicion"
izq$Exposicion <- as.character(izq$Exposicion)
conexiones$Exposicion <- as.character(conexiones$Exposicion)
conexiones <- left_join(conexiones, izq, by = "Exposicion")
conexiones <- conexiones %>% arrange(Exposicion, ISO3, ID_registro) %>%
  group_by(Exposicion) %>% mutate(Destino = ymin + row_number() - .5) %>% ungroup()
stopifnot(nrow(conexiones) == n, !anyDuplicated(conexiones$ID_registro),
          all(conexiones$Destino > conexiones$ymin), all(conexiones$Destino < conexiones$ymax))

# Rectángulos de composición en coordenadas normalizadas de la figura.
# La proporción física del mapa se respeta para alinear exactamente los orígenes.
area <- c(x=.015, y=.24, w=.39, h=.69)
w <- min(area['w']*ANCHO_CM, area['h']*ALTO_CM*dx/dy)/ANCHO_CM
h <- w*ANCHO_CM/(dx/dy)/ALTO_CM
m <- c(x=unname(area['x']+(area['w']-w)/2),
       y=unname(area['y']+(area['h']-h)/2), w=unname(w), h=unname(h))
b <- c(x=.445, y=.24, w=.535, h=.67)
conexiones$SX <- m['x'] + (conexiones$X-limites['xmin'])/dx*m['w']
conexiones$SY <- m['y'] + (conexiones$Y-limites['ymin'])/dy*m['h']
conexiones$TX <- b['x'] + ((1-ancho_nodo/2)-.65)/2.7*b['w']
conexiones$TY <- b['y'] + conexiones$Destino/n*b['h']
fuera <- !is.na(conexiones$X) &
  (conexiones$SX < m['x'] | conexiones$SX > m['x']+m['w'] |
   conexiones$SY < m['y'] | conexiones$SY > m['y']+m['h'])

# Contexto mundial completo para los países exteriores al recorte americano.
CRS_MUNDO <- "+proj=eck4 +lon_0=0 +datum=WGS84 +units=m +no_defs"
world <- sf::st_transform(sf::st_make_valid(mundo), CRS_MUNDO)
wb <- sf::st_bbox(world)
wi <- c(x=.018, y=.25, w=.16, h=.16*ANCHO_CM/ALTO_CM*
          unname((wb['ymax']-wb['ymin'])/(wb['xmax']-wb['xmin'])))
exterior <- resumen[!resumen$ISO3 %in% pts_america$ISO3 & resumen$Individuales > 0, , drop=FALSE]
# Las coordenadas del catálogo son puntos representativos del país.
wp <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(exterior,
      coords=c("Lon","Lat"), crs=4326), CRS_MUNDO))
exterior$WX <- wp[,1]; exterior$WY <- wp[,2]
exterior$Etiqueta <- paste0(exterior$Pais, " (n = ", exterior$Individuales, ")")
mini <- ggplot() + geom_sf(data=world, fill="#EFEEED", colour="#B8B4B1", linewidth=.10) +
  geom_point(data=exterior, aes(WX,WY), shape=21, size=2.3,
    fill="#81449D", colour="#542371", stroke=.25) +
  ggrepel::geom_text_repel(data=exterior, aes(WX,WY,label=Etiqueta),
    family=FUENTE, size=3.15, seed=17, min.segment.length=0,
    box.padding=.25, max.overlaps=Inf) +
  coord_sf(crs=CRS_MUNDO, xlim=c(wb['xmin'],wb['xmax']),
    ylim=c(wb['ymin'],wb['ymax']), expand=FALSE, datum=NA, clip="off") +
  theme_void() + theme(plot.margin=margin(0,0,0,0))
if (any(fuera)) {
  j <- match(conexiones$ISO3[fuera], exterior$ISO3)
  conexiones$SX[fuera] <- wi['x']+(exterior$WX[j]-wb['xmin'])/(wb['xmax']-wb['xmin'])*wi['w']
  conexiones$SY[fuera] <- wi['y']+(exterior$WY[j]-wb['ymin'])/(wb['ymax']-wb['ymin'])*wi['h']
}
nd <- is.na(conexiones$SX) | is.na(conexiones$SY)
conexiones$SX[nd] <- .37; conexiones$SY[nd] <- .29
# Bézier cúbica: los extremos coinciden con el punto del país y el nodo.
curvas <- do.call(rbind, lapply(seq_len(n), function(i) {
  t <- seq(0,1,length.out=90); z <- conexiones[i,]
  avance <- z$TX-z$SX
  data.frame(ID=z$ID_registro, Color=unname(COLORES_CONEXIONES[as.character(z$Region)]),
    x=(1-t)^3*z$SX+3*(1-t)^2*t*(z$SX+.42*avance)+
      3*(1-t)*t^2*(z$TX-.22*avance)+t^3*z$TX,
    y=(1-t)^3*z$SY+3*(1-t)^2*t*z$SY+3*(1-t)*t^2*z$TY+t^3*z$TY)
}))
componer <- function() {
  anterior <- cowplot::set_null_device("agg")
  on.exit(cowplot::set_null_device(anterior), add=TRUE)
  f <- cowplot::ggdraw() + cowplot::draw_plot(mapa, m['x'],m['y'],m['w'],m['h']) +
    cowplot::draw_plot(mini, wi['x'],wi['y'],wi['w'],wi['h']) +
    cowplot::draw_label("Contexto mundial", x=wi['x'], y=wi['y']+wi['h']+.013,
      hjust=0, fontfamily=FUENTE, fontface="bold", size=13.5) +
    geom_path(data=curvas, aes(x,y,group=ID,colour=Color),
      inherit.aes=FALSE, linewidth=GROSOR_CONEXIONES, alpha=ALFA_CONEXIONES) + scale_colour_identity() +
    cowplot::draw_plot(diagrama, b['x'],b['y'],b['w'],b['h'])
  if (any(nd)) f <- f + geom_point(aes(x=.37,y=.29), shape=23, size=3,
    fill="white", colour="#542371", inherit.aes=FALSE) +
    cowplot::draw_label(paste0("País ND (n = ",sum(nd),")"), x=.37,y=.265,
      fontfamily=FUENTE,size=11.5)
  f <- f + cowplot::draw_label("A.", x=.018,y=.965,hjust=0,
      fontfamily=FUENTE,fontface="bold",size=18) +
    cowplot::draw_label("B.", x=.445,y=.965,hjust=0,
      fontfamily=FUENTE,fontface="bold",size=18)
  for (k in 1:3) f <- f + cowplot::draw_label(
    c("Exposición","Edad","Distribución clínica")[k],
    x=b['x']+(k-.65)/2.7*b['w'], y=.928, fontfamily=FUENTE,size=13.5)
  for (k in 1:3) f <- f + cowplot::draw_label(
    c(paste0("Casos individuales por país\n(n = ", sum(resumen$Individuales), ")"),
      paste0("Regiones representadas en A\n(n = ", sum(ley_reg$N), ")"),
      paste0("Región probable de adquisición en B\n(n = ",n,")"))[k],
    x=c(.02,.235,.49)[k], y=.210,hjust=0,fontfamily=FUENTE,
    fontface="bold",size=12.5,lineheight=.95)
  f + cowplot::draw_plot(ley_casos,.02,.035,.20,.145) +
    cowplot::draw_plot(ley_regiones,.235,.032,.23,.148) +
    cowplot::draw_plot(ley_flujo,.49,.035,.34,.145)
}
figura <- componer()

# 9. Exportación y auditoría --------------------------------------------------------------
# Se generan cuatro salidas; si existen, se usa _run01, _run02, etc.
sufijo <- ""
numero_ejecucion <- 0L
repeat {
  nombres <- paste0(prefijo, sufijo,
                    c(".png", ".tiff", "_summary.csv", "_audit.xlsx"))
  rutas <- file.path(carpeta_salida, nombres)
  if (!any(file.exists(rutas))) break
  numero_ejecucion <- numero_ejecucion + 1L
  sufijo <- sprintf("_run%02d", numero_ejecucion)
}
exportar_figura <- function(extension, destino) {
  if (extension == "png") {
    ragg::agg_png(destino, width = ANCHO_CM, height = ALTO_CM,
                  units = "cm", res = DPI, background = "white")
  } else {
    ragg::agg_tiff(destino, width = ANCHO_CM, height = ALTO_CM,
                   units = "cm", res = DPI, background = "white", compression = "lzw")
  }
  tryCatch(print(figura), finally = grDevices::dev.off())
}

# El CSV resume los recuentos de ambos paneles en formato legible por programas.
crear_resumen <- function(seccion, categorias, valores, denominador) {
  data.frame(section = seccion, category = as.character(categorias),
             n = as.integer(valores), denominator = as.integer(denominador),
             percent = round(100 * as.numeric(valores) / denominador, 1),
             stringsAsFactors = FALSE)
}
conocidos <- sum(resumen$Individuales)
region_conocida <- sum(regiones$N[regiones$Region != "ND"])
regiones_america <- sum(ley_reg$N)
conteos_exposicion <- as.data.frame(table(datos$Exposicion), stringsAsFactors = FALSE)
conteos_region_B <- as.data.frame(table(datos$Region), stringsAsFactors = FALSE)
resumen_csv <- dplyr::bind_rows(
  crear_resumen("General", c("Todos los registros individuales", "País disponible", "Región disponible",
                              "Regiones americanas representadas", "Registros completos en B"),
                c(total_base, conocidos, region_conocida, regiones_america, n), total_base),
  crear_resumen("País de adquisición", resumen$Pais[resumen$Individuales > 0],
                resumen$Individuales[resumen$Individuales > 0], total_base),
  crear_resumen("Región de adquisición", regiones$Region,
                regiones$N, total_base),
  crear_resumen("Panel B: exposición", conteos_exposicion[[1]], conteos_exposicion$Freq, n),
  crear_resumen("Panel B: grupos de edad", conteos_edad_B$Edad, conteos_edad_B$N, n),
  crear_resumen("Panel B: distribución clínica", conteos_clinica_B$Distribucion,
                conteos_clinica_B$N, n),
  crear_resumen("Panel B: regiones", conteos_region_B[[1]], conteos_region_B$Freq, n)
)

# Se comprueban los resultados principales frente al manuscrito antes de exportar.
controles <- data.frame(
  indicator = c("Individual records", "País disponible", "Región disponible",
                "American-region records", "Amazon-region records", "Panel B records",
                "Panel B: localized", "Panel B: disseminated", "Panel B: Amazon region",
                "Panel B: Guiana Shield", "Panel B: Central America", "Panel B: other regions",
                "Panel B: age P25", "Panel B: age P50", "Panel B: age P75"),
  observed = c(total_base, conocidos, region_conocida, regiones_america,
               regiones$N[regiones$Region == "Amazonía"], n,
               sum(as.character(datos$Distribucion) == "Localizada"),
               sum(as.character(datos$Distribucion) == "Diseminada"),
               sum(as.character(datos$Region) == "Amazonía"),
               sum(as.character(datos$Region) == "Escudo Guayanés"),
               sum(as.character(datos$Region) == "Centroamérica"),
               sum(as.character(datos$Region) == "Otras regiones"),
               cuartiles_edad[1], cuartiles_edad[2], cuartiles_edad[3]),
  expected = c(256, 233, 226, 224, 187, 46, 37, 9, 29, 8, 2, 7, 42, 55, 65),
  stringsAsFactors = FALSE
)
controles$status <- ifelse(controles$observed == controles$expected, "PASS", "CHECK")
if (anyNA(controles$observed) || any(controles$status != "PASS")) {
  print(controles)
  stop("Los resultados no coinciden con el manuscrito. Revise la tabla de controles.")
}

# El Excel de una hoja registra también la inclusión y exclusión de cada registro en B.
resumen_paises <- data.frame(country = resumen$Pais,
  reported_records = resumen$Individuales,
  percent_of_known_country = round(100 * resumen$Individuales / conocidos, 1))
resumen_regiones <- data.frame(region = regiones$Region, n = regiones$N,
  percent_of_all_records = round(100 * regiones$N / total_base, 1),
  percent_of_american_regions = ifelse(regiones$Region %in% ley_reg$Region,
                                      round(100 * regiones$N / regiones_america, 1), NA_real_))
seguimiento <- auditoria[, c("ID_registro", "Exposicion_original", "Region_original",
                            "Edad_original", "Distribucion", "Incluido_B", "Motivo_exclusion")]
names(seguimiento) <- c("record_id", "exposure", "region", "age_years",
                         "clinical_distribution", "included_panel_b", "exclusion_reason")

names(resumen_csv) <- c("seccion", "categoria", "n", "denominador", "porcentaje")
names(controles) <- c("indicador", "observado", "esperado", "estado")
names(resumen_paises) <- c("pais", "registros", "porcentaje_pais_conocido")
names(resumen_regiones) <- c("region", "n", "porcentaje_total", "porcentaje_regiones_americanas")
names(seguimiento) <- c("id_registro", "exposicion", "region", "edad_anos",
                        "distribucion_clinica", "incluido_B", "motivo_exclusion")

libro <- openxlsx::createWorkbook()
openxlsx::addWorksheet(libro, "Auditoria figura 3")
seccion_excel <- function(titulo, tabla, fila) {
  openxlsx::writeData(libro, 1, titulo, startRow = fila, startCol = 1)
  openxlsx::addStyle(libro, 1,
    openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                          fontColour = "#23354D", fgFill = "#DFE8ED"),
    rows = fila, cols = 1:max(7, ncol(tabla)), gridExpand = TRUE, stack = TRUE)
  openxlsx::writeData(libro, 1, tabla, startRow = fila + 1, startCol = 1,
                      headerStyle = openxlsx::createStyle(textDecoration = "bold",
                                                          fgFill = "#E9EDF2"))
  fila + nrow(tabla) + 4L
}
fila <- 1L
fila <- seccion_excel("CONTROLES FRENTE AL MANUSCRITO", controles, fila)
fila <- seccion_excel("RESUMEN DE LA FIGURA", resumen_csv, fila)
fila <- seccion_excel("RECUENTOS POR PAÍS – PANEL A", resumen_paises, fila)
fila <- seccion_excel("RECUENTOS POR REGIÓN – PANEL A", resumen_regiones, fila)
fila <- seccion_excel("INCLUSIÓN DE REGISTROS EN B (256 registros)", seguimiento, fila)
openxlsx::setColWidths(libro, 1, cols = 1:7, widths = c(29, 33, 23, 18, 35, 20, 49))
openxlsx::freezePane(libro, 1, firstActiveRow = 3)

# Los archivos solo se guardan si se superan los controles de datos.
exportar_figura("png", rutas[1])
exportar_figura("tiff", rutas[2])
utils::write.csv(resumen_csv, rutas[3], row.names = FALSE, na = "", fileEncoding = "UTF-8")
openxlsx::saveWorkbook(libro, rutas[4], overwrite = FALSE)
stopifnot(length(rutas) == 4L, all(file.exists(rutas)))
message("Figura 3 terminada. Se generaron cuatro archivos junto al script:")
print(basename(rutas))
if (interactive()) print(figura)
