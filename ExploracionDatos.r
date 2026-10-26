
# ==============================================================================
# SCRIPT 3: BATERÍA DE CONSULTAS DE EXPLORACIÓN (EDA) - FRANCIA
# ==============================================================================

library(tidyverse)

# Cargamos el dataset maestro de Francia
df <- read_csv(
  "airbnb_francia_master.csv",
  show_col_types = FALSE
)

print("🔍 Dataset cargado. Iniciando batería de consultas de exploración...")


# ==============================================================================
# EJERCICIO 1: TOP 5 DE ALOJAMIENTOS MÁS CAROS DE FRANCIA
# ==============================================================================

print("\n📌 1. TOP 5 ALOJAMIENTOS MÁS CAROS DE FRANCIA:")

columnas_vista <- c(
  "name", "region_origen", "price", "room_type"
)

top_caros <- df %>%
  arrange(desc(price)) %>%
  select(all_of(columnas_vista)) %>%
  head(5)

print(as.data.frame(top_caros), row.names = FALSE)


# ==============================================================================
# EJERCICIO 2: PRECIO PROMEDIO Y TOTAL DE ALOJAMIENTOS POR REGIÓN
# ==============================================================================

print("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):")

# Eliminamos símbolos monetarios, comas y espacios
df <- df %>%
  mutate(
    price = as.character(price),
    price = str_replace_all(price, fixed("$"), ""),
    price = str_replace_all(price, fixed(","), ""),
    price = str_trim(price),
    price = na_if(price, ""),
    price = as.numeric(price)
  )

# Agrupamos por región y calculamos las métricas
resumen_region <- df %>%
  group_by(region_origen) %>%
  summarise(
    precio_promedio = mean(price, na.rm = TRUE),
    total_alojamientos = sum(!is.na(id)),
    .groups = "drop"
  ) %>%
  mutate(
    precio_promedio = round(precio_promedio, 2)
  ) %>%
  arrange(desc(precio_promedio))

print(resumen_region)


# ==============================================================================
# EJERCICIO 3: EL EFECTO SUPERHOST
# ==============================================================================

print("\n📌 3. COMPARATIVA: ¿SER SUPERHOST MARCA LA DIFERENCIA?")

# Filtramos alojamientos con puntuaciones válidas superiores a cero
df_con_resenas <- df %>%
  filter(
    !is.na(review_scores_rating),
    review_scores_rating > 0
  )

efecto_superhost <- df_con_resenas %>%
  group_by(host_is_superhost) %>%
  summarise(
    precio_medio = round(mean(price, na.rm = TRUE), 2),
    puntuacion_media = round(
      mean(review_scores_rating, na.rm = TRUE), 2
    ),
    total_propiedades = sum(!is.na(id)),
    .groups = "drop"
  )

print(efecto_superhost)


# ==============================================================================
# EJERCICIO 4: FILTRADO AVANZADO MULTICRITERIO
# Apartamentos completos, precio < 80, capacidad >= 4 y puntuación >= 4.5
# ==============================================================================

print("\n📌 4. BÚSQUEDA DE OPORTUNIDADES (Filtro Cruzado Complejo):")

filtros <- df %>%
  filter(
    room_type == "Entire home/apt",
    price < 80,
    accommodates >= 4,
    review_scores_rating >= 4.5
  )

oportunidades <- filtros %>%
  select(
    name,
    neighbourhood_cleansed,
    price,
    review_scores_rating
  )

print(paste(
  "🎯 Se han encontrado",
  nrow(oportunidades),
  "opciones que cumplen los requisitos del inversor."
))

# Mostramos las primeras cinco opciones
print(
  as.data.frame(head(oportunidades, 5)),
  row.names = FALSE
)


# ==============================================================================
# EJERCICIO 5: MATRIZ DE DISTRIBUCIÓN POR REGIÓN Y TIPO DE ALOJAMIENTO
# ==============================================================================

print("\n📌 5. MATRIZ DE DISTRIBUCIÓN: TIPO DE ALOJAMIENTO POR REGIÓN:")

matriz_tipos <- table(
  df$region_origen,
  df$room_type
)

print(matriz_tipos)


# ==============================================================================
# EJERCICIO 6: HABITACIONES PRIVADAS ECONÓMICAS CON BUENA VALORACIÓN
# Precio < 40 y puntuación >= 4.2
# ==============================================================================

print("\n📌 6. ALOJAMIENTO SUPERBARATO (Filtro Cruzado Complejo):")

filtro_economico <- df %>%
  filter(
    room_type == "Private room",
    price < 40,
    review_scores_rating >= 4.2
  )

oportunidades_economicas <- filtro_economico %>%
  select(
    room_type,
    neighbourhood_cleansed,
    price,
    review_scores_rating
  )

print(paste(
  "🎯 Se han encontrado",
  nrow(oportunidades_economicas),
  "opciones que cumplen los requisitos del inversor."
))

# Mostramos las primeras diez opciones
print(
  as.data.frame(head(oportunidades_economicas, 10)),
  row.names = FALSE
)


# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================

print("\n🎉 Batería de consultas de exploración de Francia finalizada.")