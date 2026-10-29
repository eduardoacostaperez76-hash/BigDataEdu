# Cargamos el dataset preprocesado y limpio
df <- read.csv("airbnb_francia_master.csv", stringsAsFactors = FALSE)

cat("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):\n")

# Limpiamos la columna price:
# Eliminamos símbolos $, comas y espacios en blanco
df$price <- as.character(df$price)
df$price <- gsub("\\$", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- trimws(df$price)

# Convertimos price a formato numérico
df$price <- as.numeric(df$price)

# =====================================================================
# EJERCICIO 6: Alojamiento superbarato en habitación privada, con un
# precio inferior a 40 y con reseñas iguales o superiores a 4.2
# =====================================================================

cat("\n📌 6. Alojamiento superbarato (Filtro Cruzado Complejo):\n")

# Aplicamos los filtros
filtro_economico <- (
  df$room_type == "Private room" &
  df$price < 40 &
  df$review_scores_rating >= 4.2
)

# Seleccionamos las columnas deseadas
oportunidades <- df[
  !is.na(filtro_economico) & filtro_economico,
  c(
    "room_type",
    "neighbourhood_cleansed",
    "price",
    "review_scores_rating"
  )
]

# Mostramos el número de opciones encontradas
cat(
  "🎯 Se han encontrado",
  nrow(oportunidades),
  "opciones que cumplen los requisitos del inversor.\n"
)

# Mostramos únicamente las primeras 10 opciones
print(head(oportunidades, 10), row.names = FALSE)