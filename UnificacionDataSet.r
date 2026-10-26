

# ==============================================================================
# SCRIPT 1: UNIFICACIÓN DE DATASETS DE AIRBNB - FRANCIA
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. CONFIGURACIÓN DEL ENTORNO
# ==============================================================================

# Ruta raíz donde están las carpetas de las regiones francesas
RUTA_BASE <- "C:/Users/8/Desktop/Francia"

# Columnas clave que queremos conservar
COLUMNAS_CLAVE <- c(
  "id", "listing_url", "name", "host_id", "host_name",
  "host_is_superhost", "neighbourhood_cleansed",
  "latitude", "longitude", "room_type", "accommodates",
  "bathrooms_text", "bedrooms", "beds", "price",
  "number_of_reviews", "review_scores_rating"
)

# Lista para almacenar los datasets de cada región
lista_dataframes <- list()

print("🚀 Iniciando el proceso de unificación de datos...")


# ==============================================================================
# 2. BUCLE ETL: RECORRER CARPETAS POR REGIÓN
# ==============================================================================

# Obtenemos las subcarpetas de la ruta base
carpetas_region <- list.dirs(
  path = RUTA_BASE,
  recursive = FALSE,
  full.names = TRUE
)

for (ruta_region in carpetas_region) {

  # Nombre de la región a partir del nombre de la carpeta
  carpeta_region <- basename(ruta_region)

  # Ruta del archivo listings.csv
  archivo_listings <- file.path(ruta_region, "listings.csv")

  # Comprobamos si existe el archivo
  if (file.exists(archivo_listings)) {

    print(paste(
      "📦 Procesando región:",
      toupper(carpeta_region), "..."
    ))

    tryCatch({

      # Lectura del archivo CSV
      df_region <- read_csv(
        archivo_listings,
        col_select = any_of(COLUMNAS_CLAVE),
        show_col_types = FALSE,
        progress = FALSE
      )

      # Comprobamos si faltan columnas críticas
      columnas_faltantes <- setdiff(
        COLUMNAS_CLAVE,
        names(df_region)
      )

      if (length(columnas_faltantes) > 0) {
        warning(
          paste(
            "Columnas ausentes en", carpeta_region, ":",
            paste(columnas_faltantes, collapse = ", ")
          )
        )
      }

      # Añadimos la columna de procedencia regional
      df_region <- df_region %>%
        mutate(region_origen = str_to_title(carpeta_region))

      # Guardamos el dataset en la lista
      lista_dataframes[[carpeta_region]] <- df_region

      print(paste(
        "   ✅ Leídas", nrow(df_region),
        "filas con éxito."
      ))

    }, error = function(e) {

      # Si falla la lectura, mostramos el error y continuamos
      message(
        "   ❌ Error al procesar ",
        carpeta_region, ": ", conditionMessage(e)
      )

    })

  } else {

    print(paste(
      "⚠️ No se encontró listings.csv en:",
      carpeta_region
    ))

  }
}


# ==============================================================================
# 3. CONSOLIDACIÓN: UNIR TODOS LOS DATASETS
# ==============================================================================

if (length(lista_dataframes) > 0) {

  print("\n🔄 Uniendo todos los datasets en un archivo maestro...")

  # Combinamos las filas de todas las regiones
  df_francia_maestro <- bind_rows(lista_dataframes)


  # ==============================================================================
  # 4. LIMPIEZA INICIAL DE DATOS
  # ==============================================================================

  print("🧹 Aplicando limpieza inicial...")

  # Eliminamos duplicados por ID de alojamiento
  df_francia_maestro <- df_francia_maestro %>%
    distinct(id, .keep_all = TRUE)

  # Limpiamos el precio y lo convertimos a número
  df_francia_maestro <- df_francia_maestro %>%
    mutate(
      price = as.character(price),
      price = str_replace_all(price, fixed("$"), ""),
      price = str_replace_all(price, fixed(","), ""),
      price = str_trim(price),
      price = na_if(price, ""),
      price = as.numeric(price)
    )


  # ==============================================================================
  # 5. ALMACENAMIENTO DEL DATASET MAESTRO
  # ==============================================================================

  archivo_salida <- file.path(
    RUTA_BASE,
    "airbnb_francia_master.csv"
  )

  write_csv(df_francia_maestro, archivo_salida)

  print("\n🎉 ¡Proceso completado con éxito!")

  print(paste(
    "📊 Total de alojamientos consolidados en Francia:",
    nrow(df_francia_maestro), "filas."
  ))

  print(paste(
    "💾 Archivo guardado en:",
    archivo_salida
  ))

} else {

  print(
    "⚠️ No se encontraron archivos listings.csv para unificar."
  )

}
# Cómo debe estar organizada tu carpeta de Francia
# El script espera una estructura de carpetas como esta:
