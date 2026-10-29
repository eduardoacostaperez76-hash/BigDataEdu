# Cargamos el dataset preprocesado y limpio
df <- read.csv("airbnb_francia_master.csv", stringsAsFactors = FALSE)

cat("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):\n")

# 1. Limpiamos la columna price
df$price <- as.character(df$price)
df$price <- gsub("\\$", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- trimws(df$price)
df$price <- as.numeric(df$price)

# =====================================================================
# EJERCICIO 7: ALOJAMIENTO CON MEJOR RELACIÓN CALIDAD-PRECIO
# Objetivo: encontrar los alojamientos mejor valorados en relación
# con su precio.
# Criterios: precio y puntuación de las reseñas.
# =====================================================================

cat("\n📌 7. ALOJAMIENTOS CON MEJOR RELACIÓN CALIDAD-PRECIO:\n")

# 1. Creamos una copia del dataset original
df_calidad_precio <- df

# 2. Convertimos las variables a formato numérico
df_calidad_precio$price <- as.character(df_calidad_precio$price)
df_calidad_precio$price <- gsub("\\$", "", df_calidad_precio$price)
df_calidad_precio$price <- gsub(",", "", df_calidad_precio$price)
df_calidad_precio$price <- trimws(df_calidad_precio$price)
df_calidad_precio$price <- suppressWarnings(
  as.numeric(df_calidad_precio$price)
)

df_calidad_precio$review_scores_rating <- suppressWarnings(
  as.numeric(as.character(df_calidad_precio$review_scores_rating))
)

# 3. Filtramos los alojamientos con datos válidos
df_calidad_precio <- df_calidad_precio[
  !is.na(df_calidad_precio$price) &
    !is.na(df_calidad_precio$review_scores_rating) &
    df_calidad_precio$price > 0 &
    df_calidad_precio$review_scores_rating > 0,
]

# 4. Calculamos el índice de relación calidad-precio
# Cuanto mayor sea la puntuación por euro, mejor será la relación.
df_calidad_precio$calidad_precio <-
  df_calidad_precio$review_scores_rating /
  df_calidad_precio$price

# 5. Ordenamos los alojamientos de mejor a peor
ranking_calidad_precio <- df_calidad_precio[
  order(-df_calidad_precio$calidad_precio),
]

# 6. Seleccionamos las columnas que queremos mostrar
columnas_resultado <- c(
  "name",
  "region_origen",
  "price",
  "review_scores_rating",
  "calidad_precio"
)

# Mostramos únicamente las columnas disponibles en el dataset
columnas_resultado <- columnas_resultado[
  columnas_resultado %in% names(ranking_calidad_precio)
]

# 7. Mostramos los 10 alojamientos con mejor relación calidad-precio
cat("\n🏆 TOP 10 ALOJAMIENTOS CON MEJOR RELACIÓN CALIDAD-PRECIO:\n")

if (nrow(ranking_calidad_precio) > 0) {
  resultado_top10 <- head(
    ranking_calidad_precio[, columnas_resultado, drop = FALSE],
    10
  )

  # Redondeamos los valores numéricos a 3 decimales
  columnas_numericas <- sapply(resultado_top10, is.numeric)
  resultado_top10[columnas_numericas] <- lapply(
    resultado_top10[columnas_numericas],
    round,
    digits = 3
  )

  print(resultado_top10, row.names = FALSE)

  # 8. Destacamos el alojamiento que ocupa la primera posición
  mejor <- ranking_calidad_precio[1, ]

  cat("\n🥇 MEJOR ALOJAMIENTO SEGÚN LA RELACIÓN CALIDAD-PRECIO:\n")

  nombre_mejor <- if ("name" %in% names(mejor)) {
    as.character(mejor$name)
  } else {
    "Nombre no disponible"
  }

  cat("Nombre:", nombre_mejor, "\n")

  if ("region_origen" %in% names(mejor)) {
    cat("Región:", as.character(mejor$region_origen), "\n")
  }

  cat(sprintf("Precio: %.2f €\n", mejor$price))
  cat(sprintf("Valoración: %.2f/5\n", mejor$review_scores_rating))
  cat(sprintf("Índice calidad-precio: %.4f\n", mejor$calidad_precio))

} else {
  cat("No se encontraron alojamientos con datos válidos.\n")
}