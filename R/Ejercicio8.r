# Cargamos el dataset preprocesado y limpio
df <- read.csv("airbnb_francia_master.csv", stringsAsFactors = FALSE)

cat("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):\n")

# 1. Limpiamos la columna price
df$price <- as.character(df$price)
df$price <- gsub("\\$", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- trimws(df$price)
df$price <- suppressWarnings(as.numeric(df$price))

# =====================================================================
# EJERCICIO 8: INDICADOR DE REFERENCIA
# Variables: precio (50 %), número de camas (30 %),
# valoración de reseñas (20 %).
# =====================================================================

cat("\n📌 8. RANKING DE ALOJAMIENTOS: INDICADOR DE REFERENCIA\n")

# 1. Creamos una copia del dataset
df_referencia <- df

# 2. Convertimos las variables a formato numérico
columnas_numericas <- c("price", "beds", "review_scores_rating")

for (columna in columnas_numericas) {
  df_referencia[[columna]] <- as.character(df_referencia[[columna]])
  df_referencia[[columna]] <- gsub("\\$", "", df_referencia[[columna]])
  df_referencia[[columna]] <- gsub(",", "", df_referencia[[columna]])
  df_referencia[[columna]] <- trimws(df_referencia[[columna]])
  df_referencia[[columna]] <- suppressWarnings(
    as.numeric(df_referencia[[columna]])
  )
}

# 3. Eliminamos registros sin datos válidos
df_referencia <- df_referencia[
  !is.na(df_referencia$price) &
    !is.na(df_referencia$beds) &
    !is.na(df_referencia$review_scores_rating) &
    df_referencia$price > 0 &
    df_referencia$beds > 0 &
    df_referencia$review_scores_rating > 0,
]

if (nrow(df_referencia) == 0) {

  cat("No hay alojamientos con datos válidos para calcular el indicador.\n")

} else {

  # 4. Normalizamos las variables entre 0 y 1

  # Precio: menor precio = mejor puntuación
  precio_min <- min(df_referencia$price)
  precio_max <- max(df_referencia$price)

  if (precio_max == precio_min) {
    df_referencia$score_precio <- 1.0
  } else {
    df_referencia$score_precio <-
      (precio_max - df_referencia$price) /
      (precio_max - precio_min)
  }

  # Número de camas: más camas = mejor puntuación
  camas_min <- min(df_referencia$beds)
  camas_max <- max(df_referencia$beds)

  if (camas_max == camas_min) {
    df_referencia$score_camas <- 1.0
  } else {
    df_referencia$score_camas <-
      (df_referencia$beds - camas_min) /
      (camas_max - camas_min)
  }

  # Reseñas: mejor valoración = mejor puntuación
  df_referencia$score_resenas <- pmax(
    0,
    pmin(df_referencia$review_scores_rating / 5, 1)
  )

  # 5. Calculamos el INDICADOR DE REFERENCIA sobre 100
  df_referencia$indicador_referencia <- 100 * (
    0.50 * df_referencia$score_precio +
      0.30 * df_referencia$score_camas +
      0.20 * df_referencia$score_resenas
  )

  # 6. Ordenamos los alojamientos de mayor a menor indicador
  ranking_referencia <- df_referencia[
    order(-df_referencia$indicador_referencia),
  ]

  # 7. Seleccionamos las columnas disponibles
  columnas_resultado <- c(
    "name",
    "region_origen",
    "price",
    "beds",
    "review_scores_rating",
    "indicador_referencia"
  )

  columnas_resultado <- columnas_resultado[
    columnas_resultado %in% names(ranking_referencia)
  ]

  cat("\n🏆 TOP 10 ALOJAMIENTOS SEGÚN EL INDICADOR DE REFERENCIA:\n")

  resultado_top10 <- head(
    ranking_referencia[, columnas_resultado, drop = FALSE],
    10
  )

  # Redondeamos las columnas numéricas a 2 decimales
  columnas_num <- sapply(resultado_top10, is.numeric)

  resultado_top10[columnas_num] <- lapply(
    resultado_top10[columnas_num],
    round,
    digits = 2
  )

  print(resultado_top10, row.names = FALSE)

  # 8. Identificamos el alojamiento mejor clasificado
  mejor <- ranking_referencia[1, ]

  cat("\n🥇 ALOJAMIENTO CON MEJOR INDICADOR DE REFERENCIA:\n")

  if ("name" %in% names(mejor)) {
    cat("Nombre:", as.character(mejor$name), "\n")
  }

  if ("region_origen" %in% names(mejor)) {
    cat("Región:", as.character(mejor$region_origen), "\n")
  }

  cat(sprintf("Precio: %.2f €\n", mejor$price))
  cat(sprintf("Número de camas: %.0f\n", mejor$beds))
  cat(sprintf("Valoración: %.2f/5\n", mejor$review_scores_rating))
  cat(sprintf(
    "Indicador de referencia: %.2f/100\n",
    mejor$indicador_referencia
  ))
}