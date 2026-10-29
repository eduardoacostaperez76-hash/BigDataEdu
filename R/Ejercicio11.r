
# =====================================================================
# EJERCICIO 12: INDICE DE PRECIOS AIRBNB CON MAPA INTERACTIVO
# Zonas: Burdeos, Paris, Pais Vasco frances y Lyon
# =====================================================================

cat("\nEJERCICIO 12: MAPA INTERACTIVO DEL INDICE DE PRECIOS\n")

# ---------------------------------------------------------------------
# 0. INSTALAR Y CARGAR LOS PAQUETES NECESARIOS
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
  stop(
    paste(
      "Error: no se encuentra el archivo",
      archivo,
      ". Comprueba el directorio de trabajo con getwd()."
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
# 2. COMPROBAR LAS COLUMNAS NECESARIAS
# ---------------------------------------------------------------------

columnas_necesarias <- c(
  "price",
  "region_origen",
  "room_type"
)

faltantes <- setdiff(columnas_necesarias, names(df))

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan columnas en el CSV:",
      paste(faltantes, collapse = ", ")
    )
  )
}

# ---------------------------------------------------------------------
# 3. LIMPIAR LOS PRECIOS Y LOS DATOS
# ---------------------------------------------------------------------

# Convertimos el precio a texto y eliminamos simbolos
df$price <- as.character(df$price)
df$price <- gsub("\\$", "", df$price)
df$price <- gsub("€", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- trimws(df$price)

# Convertimos el precio a numero
df$price <- suppressWarnings(
  as.numeric(df$price)
)

# Conservamos precios validos y positivos
df <- df[
  !is.na(df$price) & df$price > 0,
]

# Limpiamos regiones y tipos de alojamiento
df$region_origen <- trimws(
  as.character(df$region_origen)
)

df$room_type <- trimws(
  as.character(df$room_type)
)

# Eliminamos regiones y tipos vacios o ausentes
df <- df[
  !is.na(df$region_origen) &
    df$region_origen != "" &
    !is.na(df$room_type) &
    df$room_type != "",
]

# Normalizamos los nombres de las regiones
df$region_normalizada <- tolower(
  df$region_origen
)

cat(
  "Registros validos despues de la limpieza:",
  nrow(df),
  "\n"
)

# ---------------------------------------------------------------------
# 4. DEFINIR ZONAS, COORDENADAS Y COLORES
# ---------------------------------------------------------------------

zonas <- list(
  "Burdeos" = list(
    nombres = c("Burdeos", "Bordeaux"),
    latitud = 44.8378,
    longitud = -0.5792,
    color = "blue"
  ),

  "Paris" = list(
    nombres = c("Paris", "París"),
    latitud = 48.8566,
    longitud = 2.3522,
    color = "red"
  ),

  "Pais Vasco frances" = list(
    nombres = c(
      "Pais Vasco frances",
      "País Vasco francés",
      "Pays Basque",
      "French Basque Country"
    ),
    # Bayona como referencia geografica
    latitud = 43.4929,
    longitud = -1.4748,
    color = "green"
  ),

  "Lyon" = list(
    nombres = c("Lyon"),
    latitud = 45.7640,
    longitud = 4.8357,
    color = "purple"
  )
)

# ---------------------------------------------------------------------
# 5. CALCULAR LOS INDICES DE PRECIOS
# ---------------------------------------------------------------------

resultados <- list()

for (zona in names(zonas)) {

  info <- zonas[[zona]]

  nombres_normalizados <- tolower(
    info$nombres
  )

  datos_zona <- df[
    df$region_normalizada %in% nombres_normalizados,
  ]

  if (nrow(datos_zona) == 0) {

    cat(
      "\nAviso: no hay registros para ",
      zona,
      ". Comprueba los valores de region_origen.\n",
      sep = ""
    )

    next
  }

  # Precio medio general de la zona
  precio_medio_zona <- mean(
    datos_zona$price,
    na.rm = TRUE
  )

  # Obtenemos los tipos de alojamiento existentes
  tipos <- unique(datos_zona$room_type)

  # Calculamos el resumen de cada tipo
  resumen <- do.call(
    rbind,
    lapply(tipos, function(tipo) {

      datos_tipo <- datos_zona[
        datos_zona$room_type == tipo,
      ]

      data.frame(
        room_type = tipo,
        precio_medio = mean(
          datos_tipo$price,
          na.rm = TRUE
        ),
        numero_alojamientos = nrow(datos_tipo),
        stringsAsFactors = FALSE
      )
    })
  )

  rownames(resumen) <- NULL

  # Indice con base 100 en la media de la zona
  resumen$indice_precio <- (
    resumen$precio_medio / precio_medio_zona
  ) * 100

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
      "Revisa los nombres de la columna region_origen."
    )
  )
}

indice_precios <- do.call(
  rbind,
  resultados
)

rownames(indice_precios) <- NULL

# Seleccionamos las columnas que queremos mostrar
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

# Ordenamos por zona y de mayor a menor indice
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

cat(
  "\nINDICE DE PRECIOS POR ZONA Y TIPO DE ALOJAMIENTO\n\n"
)

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

print(
  tabla_mostrar,
  row.names = FALSE
)

# Exportamos la tabla
archivo_csv <- "ejercicio12_indice_precios.csv"

write.csv(
  indice_precios,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat(
  "\nTabla guardada en:",
  archivo_csv,
  "\n"
)

# ---------------------------------------------------------------------
# 8. CREAR EL MAPA BASE DE FRANCIA
# ---------------------------------------------------------------------

mapa <- leaflet(
  options = leafletOptions(
    zoomControl = TRUE
  )
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

  datos_resultado <- indice_precios[
    indice_precios$zona == zona,
  ]

  # Si no hay resultados para esta zona, no creamos marcador
  if (nrow(datos_resultado) == 0) {
    next
  }

  nombres_normalizados <- tolower(
    info$nombres
  )

  datos_originales <- df[
    df$region_normalizada %in% nombres_normalizados,
  ]

  precio_medio_zona <- mean(
    datos_originales$price,
    na.rm = TRUE
  )

  numero_total <- nrow(datos_originales)

  # Creamos las filas de la tabla HTML
  filas_html <- vapply(
    seq_len(nrow(datos_resultado)),
    function(i) {

      fila <- datos_resultado[i, ]

      tipo <- as.character(
        htmltools::htmlEscape(
          fila$room_type
        )
      )

      sprintf(
        paste0(
          "<tr>",
          '<td style="padding:6px;border:1px solid #ddd;">%s</td>',
          '<td style="padding:6px;border:1px solid #ddd;">%.2f &euro;</td>',
          '<td style="padding:6px;border:1px solid #ddd;">%.1f</td>',
          '<td style="padding:6px;border:1px solid #ddd;">%d</td>',
          "</tr>"
        ),
        tipo,
        fila$precio_medio,
        fila$indice_precio,
        as.integer(fila$numero_alojamientos)
      )
    },
    character(1)
  )

  filas_html <- paste(
    filas_html,
    collapse = "\n"
  )

  # Construimos la ventana emergente.
  # Los porcentajes literales dentro de sprintf se escriben como %%
  contenido <- sprintf(
    paste0(
      '<div style="width:440px;max-width:100%%;',
      'font-family:Arial,sans-serif;font-size:13px;">',

      '<h3 style="color:%s;margin-bottom:8px;">%s</h3>',

      '<p><b>Precio medio general:</b> %.2f &euro;</p>',
      '<p><b>Numero de alojamientos:</b> %d</p>',
      '<p><b>Indice de referencia:</b> 100</p>',

      '<div style="overflow-x:auto;">',

      '<table style="width:100%%;border-collapse:collapse;',
      'text-align:center;">',

      '<thead><tr style="background:#eeeeee;">',
      '<th style="padding:6px;border:1px solid #ddd;">Tipo</th>',
      '<th style="padding:6px;border:1px solid #ddd;">Precio medio</th>',
      '<th style="padding:6px;border:1px solid #ddd;">Indice</th>',
      '<th style="padding:6px;border:1px solid #ddd;">N.</th>',
      '</tr></thead>',

      '<tbody>%s</tbody>',
      '</table></div>',

      '<p style="font-size:12px;color:#555;">',
      'Indice superior a 100: precio por encima ',
      'de la media de la zona.',
      '</p></div>'
    ),

    info$color,
    as.character(htmltools::htmlEscape(zona)),
    precio_medio_zona,
    numero_total,
    filas_html
  )

  # Añadimos el marcador al mapa
  mapa <- mapa |>
    addAwesomeMarkers(
      lng = info$longitud,
      lat = info$latitud,
      label = paste(
        "Consultar precios:",
        zona
      ),
      popup = htmltools::HTML(contenido),
      popupOptions = popupOptions(
        maxWidth = 500
      ),
      icon = makeAwesomeIcon(
        icon = "home",
        library = "fa",
        markerColor = info$color
      ),
      group = zona
    )
}

# ---------------------------------------------------------------------
# 10. AÑADIR TITULO Y CONTROL DE CAPAS
# ---------------------------------------------------------------------

titulo <- htmltools::HTML(
  paste0(
    '<div style="background:white;padding:10px 14px;',
    'border:1px solid #ccc;border-radius:6px;',
    'box-shadow:0 2px 6px #aaa;',
    'font-family:Arial,sans-serif;font-size:15px;',
    'font-weight:bold;text-align:center;">',
    'Indice de precios Airbnb en Francia',
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
    options = layersControlOptions(
      collapsed = TRUE
    )
  )

# Control de pantalla completa, si el paquete esta instalado
if (requireNamespace(
  "leaflet.extras",
  quietly = TRUE
)) {
  mapa <- mapa |>
    leaflet.extras::addFullscreenControl()
}

# ---------------------------------------------------------------------
# 11. GUARDAR Y MOSTRAR EL MAPA
# ---------------------------------------------------------------------

archivo_html <- "ejercicio12_mapa_precios.html"

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

# Mostramos el mapa en RStudio
print(mapa)