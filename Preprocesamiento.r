
# ==============================================================================
# SCRIPT 2: PREPROCESAMIENTO AVANZADO DE DATOS - FRANCIA
# ==============================================================================

library(tidyverse)

# Cargamos el dataset unificado generado en el paso anterior
print("🧹 Iniciando la fase avanzada de preprocesamiento de datos...")

df <- read_csv(
  "airbnb_francia_master.csv",
  show_col_types = FALSE
)


# ==============================================================================
# RETO 1: TRATAMIENTO DE VALORES NULOS (MISSING VALUES)
# ==============================================================================

print("\n1️⃣ Tratando valores nulos...")

# Imputamos bedrooms y beds con la mediana de cada región
df <- df %>%
  group_by(region_origen) %>%
  mutate(
    bedrooms = {
      mediana <- median(bedrooms, na.rm = TRUE)
      if (is.nan(mediana)) mediana <- NA_real_
      coalesce(bedrooms, mediana)
    },
    beds = {
      mediana <- median(beds, na.rm = TRUE)
      if (is.nan(mediana)) mediana <- NA_real_
      coalesce(beds, mediana)
    }
  ) %>%
  ungroup()

# Creamos una bandera para identificar alojamientos sin puntuación
df <- df %>%
  mutate(
    has_no_reviews = as.integer(is.na(review_scores_rating)),
    review_scores_rating = coalesce(review_scores_rating, 0),
    host_is_superhost = if_else(
      is.na(host_is_superhost),
      FALSE,
      host_is_superhost
    )
  )

# Contamos las habitaciones que siguen vacías
habitaciones_vacias <- sum(is.na(df$bedrooms))

print(paste(
  "   ✅ Nulos tratados. Habitaciones vacías restantes:",
  habitaciones_vacias
))


# ==============================================================================
# RETO 2: GESTIÓN DE OUTLIERS (VALORES ATÍPICOS EN PRECIOS)
# ==============================================================================

print("\n2️⃣ Filtrando valores atípicos (Outliers) en precios...")

# Registramos el número original de filas
filas_originales <- nrow(df)

# Convertimos el precio a numérico y aplicamos el rango de negocio
df <- df %>%
  mutate(
    price = as.character(price),
    price = str_replace_all(price, fixed("$"), ""),
    price = str_replace_all(price, fixed(","), ""),
    price = str_trim(price),
    price = na_if(price, ""),
    price = as.numeric(price)
  ) %>%
  filter(
    !is.na(price),
    price >= 10,
    price <= 1000
  )

# Calculamos cuántas filas se han eliminado
filas_filtradas <- filas_originales - nrow(df)

print(paste(
  "   ✅ Registros descartados:",
  filas_filtradas,
  "filas fuera del rango [10 - 1000]."
))


# ==============================================================================
# RETO 3: INGENIERÍA DE CARACTERÍSTICAS (FEATURE ENGINEERING)
# ==============================================================================

print("\n3️⃣ Ejecutando Ingeniería de Características...")

# Extraemos el número de baños desde bathrooms_text
# Ejemplos: "1.5 baths", "1 private bath", "Shared half-bath"

df <- df %>%
  mutate(
    bathrooms_text = coalesce(
      as.character(bathrooms_text),
      "1"
    ),
    bathrooms_text = str_to_lower(bathrooms_text),

    # Extraemos el primer número entero o decimal
    bathrooms_num = as.numeric(
      str_extract(bathrooms_text, "\\d+\\.?\\d*")
    ),

    # Si aparece "half" y no se ha encontrado un número, asignamos 0.5
    bathrooms_num = if_else(
      str_detect(bathrooms_text, "half") &
        is.na(bathrooms_num),
      0.5,
      bathrooms_num
    ),

    # Asignamos 1 a los valores que todavía estén vacíos
    bathrooms_num = coalesce(bathrooms_num, 1.0)
  ) %>%
  # Eliminamos la columna de texto original
  select(-bathrooms_text)

print(
  "   ✅ Columna numérica 'bathrooms_num' creada correctamente."
)


# ==============================================================================
# GUARDAR DATASET FINAL Y LIMPIO
# ==============================================================================

archivo_final <- "airbnb_francia_clean.csv"

write_csv(df, archivo_final)

print("\n🎉 ¡Preprocesamiento avanzado finalizado!")

print("📊 Dataset listo para análisis en R y Power BI.")

print(paste(
  "💾 Guardado como:",
  archivo_final,
  "| Total de filas:",
  nrow(df)
))