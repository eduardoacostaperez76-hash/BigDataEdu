
# ============================================================
# ANALISIS ESTADISTICO AIRBNB FRANCIA
# Comparacion de cuatro regiones
# ============================================================

# 1. CONFIGURACION

options(
  na.print = "NA",
  OutDec = ",",
  scipen = 999
)

paquetes <- c("readr", "dplyr", "tidyr")

for (paquete in paquetes) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

library(readr)
library(dplyr)
library(tidyr)


# 2. SELECCIONAR EL ARCHIVO CSV

cat("\nANALISIS ESTADISTICO AIRBNB FRANCIA\n\n")
cat("Selecciona airbnb_francia_clean.csv\n")

archivo_csv <- file.choose()

datos <- readr::read_csv(
  archivo_csv,
  show_col_types = FALSE
)

cat("\nArchivo cargado:", nrow(datos), "filas\n")


# 3. COMPROBAR COLUMNAS

columnas_necesarias <- c(
  "region_origen",
  "room_type",
  "price",
  "accommodates",
  "bedrooms"
)

faltantes <- setdiff(columnas_necesarias, names(datos))

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan columnas:",
      paste(faltantes, collapse = ", ")
    )
  )
}


# 4. PREPARAR LAS VARIABLES

convertir_numerica <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  suppressWarnings(as.numeric(as.character(x)))
}

datos <- datos %>%
  mutate(
    region_origen = trimws(as.character(region_origen)),
    room_type = trimws(as.character(room_type)),
    price = convertir_numerica(price),
    accommodates = convertir_numerica(accommodates),
    bedrooms = convertir_numerica(bedrooms)
  ) %>%
  mutate(
    price = ifelse(
      is.finite(price) & price > 0,
      price,
      NA_real_
    ),
    accommodates = ifelse(
      is.finite(accommodates) & accommodates > 0,
      accommodates,
      NA_real_
    ),
    bedrooms = ifelse(
      is.finite(bedrooms) & bedrooms >= 0,
      bedrooms,
      NA_real_
    ),
    region_es = recode(
      region_origen,
      "Bordeaux" = "Burdeos",
      "Lyon" = "Lyon",
      "Paris" = "París",
      "Pays Basque" = "País Vasco francés",
      .default = region_origen
    )
  )


# 5. NUMERO DE ALOJAMIENTOS POR REGION

tabla_conteo <- datos %>%
  filter(!is.na(region_es), region_es != "") %>%
  count(region_es, name = "numero_alojamientos") %>%
  arrange(region_es)

cat("\nNUMERO DE ALOJAMIENTOS POR REGION\n\n")
print(as.data.frame(tabla_conteo), row.names = FALSE)


# 6. PREPARAR LOS DATOS PARA EL ANALISIS

etiquetas_variables <- c(
  price = "Precio por noche",
  accommodates = "Capacidad de huéspedes",
  bedrooms = "Número de dormitorios"
)

datos_largos <- datos %>%
  filter(!is.na(region_es), region_es != "") %>%
  select(
    region_es,
    room_type,
    all_of(names(etiquetas_variables))
  ) %>%
  pivot_longer(
    cols = all_of(names(etiquetas_variables)),
    names_to = "variable_original",
    values_to = "valor"
  ) %>%
  mutate(
    variable = unname(etiquetas_variables[variable_original])
  )


# 7. FUNCION DE ESTADISTICA DESCRIPTIVA

calcular_resumen <- function(x) {

  x <- x[is.finite(x)]
  n_validos <- length(x)

  if (n_validos == 0) {
    return(data.frame(
      n = 0L,
      media = NA_real_,
      mediana = NA_real_,
      desviacion_estandar = NA_real_,
      minimo = NA_real_,
      maximo = NA_real_
    ))
  }

  data.frame(
    n = n_validos,
    media = mean(x),
    mediana = median(x),
    desviacion_estandar = if (n_validos >= 2) sd(x) else NA_real_,
    minimo = min(x),
    maximo = max(x)
  )
}


# 8. TABLA 1: COMPARACION POR REGION

tabla_ciudades <- datos_largos %>%
  group_by(region_es, variable) %>%
  group_modify(~ calcular_resumen(.x$valor)) %>%
  ungroup() %>%
  arrange(variable, region_es)

cat("\n============================================\n")
cat("TABLA 1. COMPARACION ESTADISTICA POR REGION\n")
cat("============================================\n\n")

tabla_ciudades_mostrar <- tabla_ciudades %>%
  mutate(
    media = round(media, 2),
    mediana = round(mediana, 2),
    desviacion_estandar = round(desviacion_estandar, 2),
    minimo = round(minimo, 2),
    maximo = round(maximo, 2)
  )

print(as.data.frame(tabla_ciudades_mostrar), row.names = FALSE)


# 9. TABLA 2: COMPARACION POR REGION Y TIPOLOGIA

tabla_tipologias <- datos_largos %>%
  filter(!is.na(room_type), room_type != "") %>%
  group_by(region_es, room_type, variable) %>%
  group_modify(~ calcular_resumen(.x$valor)) %>%
  ungroup() %>%
  arrange(region_es, room_type, variable)

cat("\n============================================\n")
cat("TABLA 2. ESTADISTICAS POR REGION Y TIPOLOGIA\n")
cat("============================================\n\n")

tabla_tipologias_mostrar <- tabla_tipologias %>%
  mutate(
    media = round(media, 2),
    mediana = round(mediana, 2),
    desviacion_estandar = round(desviacion_estandar, 2),
    minimo = round(minimo, 2),
    maximo = round(maximo, 2)
  )

print(as.data.frame(tabla_tipologias_mostrar), row.names = FALSE)


# 10. SELECCIONAR LA CARPETA DE RESULTADOS

cat("\nSelecciona la carpeta donde guardar los resultados.\n")

carpeta_resultados <- utils::choose.dir(
  default = dirname(archivo_csv),
  caption = "Selecciona la carpeta de resultados"
)

if (
  length(carpeta_resultados) == 0 ||
  is.na(carpeta_resultados) ||
  carpeta_resultados == ""
) {
  carpeta_resultados <- dirname(archivo_csv)
  cat("Se utilizará la carpeta del CSV original.\n")
}


# 11. EXPORTAR PARA EXCEL EN ESPAÑOL
#
# write.csv2 utiliza:
#   - Punto y coma para separar columnas
#   - Coma para separar decimales
#   - UTF-8 para conservar caracteres acentuados
#
# Los resultados exportados conservan la precision numerica
# original; solo las tablas de consola se redondean.

utils::write.csv2(
  tabla_ciudades,
  file.path(carpeta_resultados, "01_estadisticas_por_region.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

utils::write.csv2(
  tabla_tipologias,
  file.path(carpeta_resultados, "02_estadisticas_region_tipologia.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

utils::write.csv2(
  tabla_conteo,
  file.path(carpeta_resultados, "03_numero_alojamientos_por_region.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# 12. COMPROBACION INDEPENDIENTE DE LAS MEDIAS

cat("\n============================================\n")
cat("COMPROBACION DE LAS MEDIAS DIRECTAMENTE DESDE LOS DATOS\n")
cat("============================================\n\n")

comprobacion <- datos %>%
  group_by(region_origen) %>%
  summarise(
    alojamientos = n(),
    media_capacidad = mean(accommodates, na.rm = TRUE),
    media_dormitorios = mean(bedrooms, na.rm = TRUE),
    media_precio = mean(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

print(as.data.frame(comprobacion), row.names = FALSE)


# 13. FINALIZACION

cat("\nANALISIS FINALIZADO\n\n")
cat("Archivos guardados en:\n")
cat(carpeta_resultados, "\n\n")

cat("Archivos creados:\n")
cat("01_estadisticas_por_region.csv\n")
cat("02_estadisticas_region_tipologia.csv\n")
cat("03_numero_alojamientos_por_region.csv\n")
