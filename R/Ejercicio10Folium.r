
# =====================================================================
# EJERCICIO 10: ÍNDICE DE PRECIOS AIRBNB CON MAPA INTERACTIVO
# Zonas: Burdeos, París, País Vasco francés y Lyon
# =====================================================================

cat("\nEJERCICIO 10: MAPA INTERACTIVO DEL ÍNDICE DE PRECIOS\n")

# ---------------------------------------------------------------------
# 0. INSTALAR Y CARGAR LOS PAQUETES
# ---------------------------------------------------------------------

paquetes <- c("leaflet", "htmlwidgets", "htmltools")

for (paquete in paquetes) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

library(leaflet)
library(htmlwidgets)
library(htmltools)

# ---------------------------------------------------------------------
# 1. CARGAR EL DATASET
# ---------------------------------------------------------------------

archivo <- "airbnb_francia_master.csv"

if (!file.exists(archivo)) {
  stop(paste("Error: no se encuentra el archivo", archivo))
}

df <- read.csv(
  archivo,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------------
# 2. COMPROBAR LAS COLUMNAS NECESARIAS
# ---------------------------------------------------------------------

columnas_necesarias <- c(
  "price",
  "region_origen",
  "room_type"
)

faltantes <- setdiff(columnas_necesarias, names(df))

if (length(faltantes) > 0) {
  stop(paste(
    "Faltan columnas en el CSV:",
    paste(faltantes, collapse = ", ")
  ))
}

# ---------------------------------------------------------------------
# 3. LIMPIAR LOS PRECIOS Y LOS DATOS
# ---------------------------------------------------------------------

df$price <- as.character(df$price)
df$price <- gsub("\\$", "", df$price)
df$price <- gsub("€", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- trimws(df$price)

df$price <- suppressWarnings(as.numeric(df$price))

df <- df[
  !is.na(df$price) & df$price > 0,
]

df$region_origen <- trimws(as.character(df$region_origen))
df$room_type <- trimws(as.character(df$room_type))

df <- df[
  !is.na(df$region_origen) &
    df$region_origen != "" &
    !is.na(df$room_type) &
    df$room_type != "",
]

df$region_normalizada <- tolower(df$region_origen)

# ---------------------------------------------------------------------
# 4. DEFINIR ZONAS, COORDENADAS Y COLORES
# ---------------------------------------------------------------------

zonas <- list(
  "Burdeos" = list(
    nombres = c("Burdeos", "Bordeaux"),
    coordenadas = c(44.8378, -0.5792),
    color = "blue"
  ),
  "París" = list(
    nombres = c("París", "Paris"),
    coordenadas = c(48.8566, 2.3522),
    color = "red"
  ),
  "País Vasco francés" = list(
    nombres = c(
      "País Vasco francés",
      "Pays Basque",
      "French Basque Country"
    ),
    coordenadas = c(43.4929, -1.4748),
    color = "green"
  ),
  "Lyon" = list(
    nombres = c("Lyon"),
    coordenadas = c(45.7640, 4.8357),
    color = "purple"
  )
)

# ---------------------------------------------------------------------
# 5. CALCULAR LOS ÍNDICES DE PRECIOS
# ---------------------------------------------------------------------

resultados <- list()

for (zona in names(zonas)) {

  info <- zonas[[zona]]
  nombres_normalizados <- tolower(info$nombres)

  datos_zona <- df[
    df$region_normalizada %in% nombres_normalizados,
  ]

  if (nrow(datos_zona) == 0) {
    cat(
      "\nAviso: no hay registros para ", zona,
      ". Comprueba los nombres de region_origen.\n",
      sep = ""
    )
    next
  }

  precio_medio_zona <- mean(datos_zona$price, na.rm = TRUE)

  tipos <- unique(datos_zona$room_type)

  resumen <- do.call(
    rbind,
    lapply(tipos, function(tipo) {

      datos_tipo <- datos_zona[
        datos_zona$room_type == tipo,
      ]

      data.frame(
        room_type = tipo,
        precio_medio = mean(datos_tipo$price, na.rm = TRUE),
        numero_alojamientos = nrow(datos_tipo),
        stringsAsFactors = FALSE
      )
    })
  )

  rownames(resumen) <- NULL

  resumen$indice_precio <-
    (resumen$precio_medio / precio_medio_zona) * 100

  resumen$zona <- zona

  resultados[[zona]] <- resumen
}

# ---------------------------------------------------------------------
# 6. COMPROBAR Y UNIR LOS RESULTADOS
# ---------------------------------------------------------------------

if (length(resultados) == 0) {
  stop(
    paste(
      "No se han encontrado datos para las zonas seleccionadas.",
      "Revisa los valores de region_origen del CSV."
    )
  )
}

indice_precios <- do.call(rbind, resultados)
rownames(indice_precios) <- NULL

indice_precios <- indice_precios[
  ,
  c(
    "zona",
    "room_type",
    "numero_alojamientos",
    "precio_medio",
    "indice_precio"
  )
]

indice_precios <- indice_precios[
  order(
    indice_precios$zona,
    -indice_precios$indice_precio
  ),
]

rownames(indice_precios) <- NULL

# ---------------------------------------------------------------------
# 7. MOSTRAR Y EXPORTAR LOS RESULTADOS
# ---------------------------------------------------------------------

cat("\nÍNDICE DE PRECIOS POR ZONA Y TIPO DE ALOJAMIENTO\n\n")

tabla_mostrar <- indice_precios

columnas_numericas <- c(
  "numero_alojamientos",
  "precio_medio",
  "indice_precio"
)

tabla_mostrar[columnas_numericas] <- lapply(
  tabla_mostrar[columnas_numericas],
  round,
  digits = 2
)

print(tabla_mostrar, row.names = FALSE)

archivo_csv <- "ejercicio10_indice_precios.csv"

write.csv(
  indice_precios,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nTabla guardada en: ", archivo_csv, "\n", sep = "")

# ---------------------------------------------------------------------
# 8. CREAR EL MAPA BASE DE FRANCIA
# ---------------------------------------------------------------------

mapa <- leaflet(
  options = leafletOptions(zoomControl = TRUE)
) |>
  addTiles(
    group = "OpenStreetMap",
    attribution = "&copy; OpenStreetMap contributors"
  ) |>
  setView(
    lng = 2.5,
    lat = 46.6,
    zoom = 6
  )

# ---------------------------------------------------------------------
# 9. AÑADIR MARCADORES Y VENTANAS INFORMATIVAS
# ---------------------------------------------------------------------

for (zona in names(zonas)) {

  info <- zonas[[zona]]

  datos_zona <- indice_precios[
    indice_precios$zona == zona,
  ]

  if (nrow(datos_zona) == 0) {
    next
  }

  nombres_normalizados <- tolower(info$nombres)

  datos_originales <- df[
    df$region_normalizada %in% nombres_normalizados,
  ]

  precio_medio_zona <- mean(datos_originales$price, na.rm = TRUE)
  numero_total <- nrow(datos_originales)

  # Construimos las filas de la tabla HTML.
  # htmlEscape protege el contenido de texto introducido en el HTML.
  filas_html <- vapply(
    seq_len(nrow(datos_zona)),
    function(i) {

      fila <- datos_zona[i, ]

      tipo <- as.character(
        htmltools::htmlEscape(fila$room_type)
      )

      sprintf(
        paste0(
          '<tr>',
          '<td style="padding:6px;border:1px solid #ddd;">%s</td>',
          '<td style="padding:6px;border:1px solid #ddd;">%.2f &euro;</td>',
          '<td style="padding:6px;border:1px solid #ddd;">%.1f</td>',
          '<td style="padding:6px;border:1px solid #ddd;">%d</td>',
          '</tr>'
        ),
        tipo,
        fila$precio_medio,
        fila$indice_precio,
        as.integer(fila$numero_alojamientos)
      )
    },
    character(1)
  )

  filas_html <- paste(filas_html, collapse = "\n")

  # Construimos la ventana emergente.
  # IMPORTANTE: los porcentajes CSS se escriben como %% en sprintf().
  contenido <- sprintf(
    paste0(
      '<div style="width:440px;max-width:100%%;',
      'font-family:Arial,sans-serif;font-size:13px;">',

      '<h3 style="color:%s;margin-bottom:8px;">%s</h3>',

      '<p><b>Precio medio general:</b> %.2f &euro;</p>',
      '<p><b>Número de alojamientos:</b> %d</p>',
      '<p><b>Índice de referencia:</b> 100</p>',

      '<div style="overflow-x:auto;">',
      '<table style="width:100%%;border-collapse:collapse;',
      'text-align:center;">',

      '<thead><tr style="background:#eeeeee;">',
      '<th style="padding:6px;border:1px solid #ddd;">Tipo</th>',
      '<th style="padding:6px;border:1px solid #ddd;">Precio medio</th>',
      '<th style="padding:6px;border:1px solid #ddd;">Índice</th>',
      '<th style="padding:6px;border:1px solid #ddd;">N.º</th>',
      '</tr></thead>',

      '<tbody>%s</tbody>',
      '</table></div>',

      '<p style="font-size:12px;color:#555;">',
      'Índice superior a 100: precio por encima ',
      'de la media de la zona.',
      '</p></div>'
    ),
    info$color,
    as.character(htmltools::htmlEscape(zona)),
    precio_medio_zona,
    numero_total,
    filas_html
  )

  # Añadimos el marcador con popup HTML
  mapa <- mapa |>
    addAwesomeMarkers(
      lng = info$coordenadas[2],
      lat = info$coordenadas[1],
      label = paste("Consultar precios:", zona),
      popup = htmltools::HTML(contenido),
      popupOptions = popupOptions(maxWidth = 500),
      icon = makeAwesomeIcon(
        icon = "home",
        library = "fa",
        markerColor = info$color
      ),
      group = zona
    )
}

# ---------------------------------------------------------------------
# 10. AÑADIR TÍTULO Y CONTROL DE CAPAS
# ---------------------------------------------------------------------

titulo <- htmltools::HTML(
  paste0(
    '<div style="position:fixed;top:10px;left:50%;',
    'transform:translateX(-50%);z-index:9999;',
    'background:white;padding:10px 14px;',
    'border:1px solid #ccc;border-radius:6px;',
    'box-shadow:0 2px 6px #aaa;',
    'font-family:Arial,sans-serif;font-size:15px;',
    'font-weight:bold;text-align:center;max-width:80%%;">',
    'Índice de precios Airbnb en Francia',
    '</div>'
  )
)

mapa <- mapa |>
  addControl(
    html = titulo,
    position = "topright"
  ) |>
  addLayersControl(
    overlayGroups = names(zonas),
    options = layersControlOptions(collapsed = TRUE)
  )

# Añadimos pantalla completa si leaflet.extras está instalado
if (requireNamespace("leaflet.extras", quietly = TRUE)) {
  mapa <- mapa |>
    leaflet.extras::addFullscreenControl()
}

# ---------------------------------------------------------------------
# 11. GUARDAR Y MOSTRAR EL MAPA
# ---------------------------------------------------------------------

archivo_html <- "ejercicio10_mapa_precios.html"

htmlwidgets::saveWidget(
  widget = mapa,
  file = archivo_html,
  selfcontained = TRUE
)

cat("\nArchivos generados correctamente:\n")
cat("- ", archivo_csv, "\n", sep = "")
cat("- ", archivo_html, "\n", sep = "")

cat(
  "\nAbre el archivo HTML en Chrome, Edge o Firefox ",
  "para explorar el mapa interactivo.\n",
  sep = ""
)

print(mapa)