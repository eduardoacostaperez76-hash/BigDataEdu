
# =====================================================================
# EJERCICIO 12: GRAFICO INTERACTIVO DE PRECIO VS VALORACION
# Airbnb Francia
# Visualizacion interactiva en Viewer de RStudio
# =====================================================================

cat("\nGRAFICO INTERACTIVO: PRECIO VS VALORACION AIRBNB\n")

# ---------------------------------------------------------------------
# 1. INSTALAR Y CARGAR EL PAQUETE
# ---------------------------------------------------------------------

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

library(plotly)

# ---------------------------------------------------------------------
# 2. CARGAR EL DATASET
# ---------------------------------------------------------------------

archivo <- "airbnb_francia_master.csv"

if (!file.exists(archivo)) {
  stop(
    paste(
      "No se encuentra el archivo:",
      archivo,
      "\nDirectorio actual:",
      getwd(),
      "\nComprueba la ubicacion del CSV."
    )
  )
}

df <- read.csv(
  archivo,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  check.names = FALSE
)

cat("Dataset cargado correctamente.\n")
cat("Numero de registros:", nrow(df), "\n")

# ---------------------------------------------------------------------
# 3. COMPROBAR LAS COLUMNAS NECESARIAS
# ---------------------------------------------------------------------

columnas_necesarias <- c(
  "price",
  "review_scores_rating",
  "room_type"
)

faltantes <- setdiff(
  columnas_necesarias,
  names(df)
)

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan estas columnas:",
      paste(faltantes, collapse = ", ")
    )
  )
}

# ---------------------------------------------------------------------
# 4. LIMPIAR Y CONVERTIR LOS PRECIOS
# ---------------------------------------------------------------------

df$price <- as.character(df$price)

df$price <- gsub(
  "[$€]",
  "",
  df$price
)

df$price <- gsub(
  ",",
  "",
  df$price,
  fixed = TRUE
)

df$price <- trimws(df$price)

df$price <- suppressWarnings(
  as.numeric(df$price)
)

# ---------------------------------------------------------------------
# 5. CONVERTIR LAS VALORACIONES A NUMERO
# ---------------------------------------------------------------------

df$review_scores_rating <- suppressWarnings(
  as.numeric(
    as.character(df$review_scores_rating)
  )
)

df$room_type <- trimws(
  as.character(df$room_type)
)

# ---------------------------------------------------------------------
# 6. FILTRAR LOS REGISTROS VALIDOS
# ---------------------------------------------------------------------

datos_grafico <- df[
  !is.na(df$price) &
    is.finite(df$price) &
    df$price > 0 &
    !is.na(df$review_scores_rating) &
    is.finite(df$review_scores_rating) &
    df$review_scores_rating > 0 &
    !is.na(df$room_type) &
    df$room_type != "",
]

# Detectamos si la escala de valoracion es 0-5 o 0-100.
# Si los datos estan en escala 0-100, los convertimos a 0-5
# cuando todas las valoraciones validas sean superiores a 5.

if (
  nrow(datos_grafico) > 0 &&
  max(datos_grafico$review_scores_rating, na.rm = TRUE) > 5 &&
  max(datos_grafico$review_scores_rating, na.rm = TRUE) <= 100
) {
  datos_grafico$review_scores_rating <-
    datos_grafico$review_scores_rating / 20
}

# Limitamos el grafico al percentil 99 del precio para reducir
# el efecto visual de los precios extremadamente altos.
if (nrow(datos_grafico) > 0) {

  limite_precio <- as.numeric(
    quantile(
      datos_grafico$price,
      probs = 0.99,
      na.rm = TRUE
    )
  )

  datos_grafico <- datos_grafico[
    datos_grafico$price <= limite_precio,
  ]
}

if (nrow(datos_grafico) == 0) {
  stop(
    "No hay alojamientos con precios y valoraciones validos."
  )
}

cat(
  "Alojamientos incluidos en el grafico:",
  nrow(datos_grafico),
  "\n"
)

# ---------------------------------------------------------------------
# 7. CREAR EL GRAFICO DE DISPERSION INTERACTIVO
# ---------------------------------------------------------------------

grafico <- plot_ly(
  data = datos_grafico,
  x = ~price,
  y = ~review_scores_rating,
  color = ~room_type,
  colors = c(
    "#2878B5",
    "#E87524",
    "#3A9D5D",
    "#9B59B6",
    "#D64F70"
  ),
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 7,
    opacity = 0.55
  ),
  text = ~paste0(
    "Tipo de alojamiento: ",
    room_type,
    "<br>Precio por noche: ",
    format(round(price, 2), nsmall = 2),
    " €",
    "<br>Valoracion: ",
    round(review_scores_rating, 2),
    " / 5"
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "Precio vs. valoracion de alojamientos Airbnb",
      x = 0.5
    ),
    xaxis = list(
      title = "Precio por noche (€)",
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Valoracion de las reseñas (escala 0-5)",
      range = c(0, 5),
      dtick = 0.5
    ),
    legend = list(
      title = list(
        text = "Tipo de alojamiento"
      )
    ),
    hovermode = "closest",
    template = "plotly_white"
  )

# ---------------------------------------------------------------------
# 8. MOSTRAR EL GRAFICO EN VIEWER
# ---------------------------------------------------------------------

print(grafico)

# ---------------------------------------------------------------------
# 9. GUARDAR UNA VERSION HTML INTERACTIVA
# ---------------------------------------------------------------------

archivo_html <- "ejercicio12_precio_vs_valoracion.html"

htmlwidgets::saveWidget(
  widget = grafico,
  file = archivo_html,
  selfcontained = TRUE
)

cat("\nGrafico interactivo generado correctamente.\n")
cat("Archivo HTML:", archivo_html, "\n")
cat(
  "Puedes abrir el archivo HTML en Chrome, Edge o Firefox.\n"
)