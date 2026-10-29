# ============================================================
# PROYECTO: AIRBNB FRANCIA
# EJERCICIO 5: ANALISIS GEOGRAFICO DE ALOJAMIENTOS
# ============================================================

# 1. Cargar paquetes
library(tidyverse)
library(leaflet)
library(htmlwidgets)

# 2. Cargar los datos limpios
clean <- readr::read_csv(
  "C:/Users/8/Desktop/Francia/RStudio/airbnb_francia_clean.csv",
  show_col_types = FALSE
)

# 3. Preparar los datos geograficos
mapa_datos <- clean %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  ) %>%
  mutate(
    tipologia = recode(
      room_type,
      "Entire home/apt" = "Casa/apartamento",
      "Private room" = "Habitacion privada",
      "Shared room" = "Habitacion compartida",
      "Hotel room" = "Hotel"
    )
  )

# 4. Definir colores por tipologia
colores <- c(
  "Casa/apartamento" = "#2878B5",
  "Habitacion privada" = "#E67E22",
  "Habitacion compartida" = "#27AE60",
  "Hotel" = "#8E44AD"
)

# 5. Crear el mapa interactivo
library(leaflet)
library(dplyr)
library(htmlwidgets)

mapa_airbnb <- leaflet(mapa_datos) %>%
  addProviderTiles(
    providers$OpenStreetMap
  ) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.7,
    color = ~unname(colores[tipologia]),
    popup = ~paste0(
      "<b>Zona:</b> ", region_origen,
      "<br><b>Tipología:</b> ", tipologia,
      "<br><b>Barrio:</b> ", neighbourhood_cleansed
    ),
    clusterOptions = markerClusterOptions()
  ) %>%
  addLegend(
    position = "bottomright",
    colors = unname(colores),
    labels = names(colores),
    title = "Tipo de alojamiento"
  )

mapa_airbnb

# 6. Mostrar el mapa en RStudio
mapa_airbnb

# 7. Crear carpeta de resultados si no existe
dir.create(
  "results/mapas",
  recursive = TRUE,
  showWarnings = FALSE
)

# 8. Guardar el mapa interactivo en HTML
htmlwidgets::saveWidget(
  widget = mapa_airbnb,
  file = "results/mapas/mapa_airbnb_tipologias.html",
  selfcontained = TRUE
)

# 9. Confirmar que se ha guardado
file.exists(
  "results/mapas/mapa_airbnb_tipologias.html"
)

# 10. Mostrar el mapa en el Viewer de RStudio
print(mapa_airbnb, viewer = getOption("viewer"))