
# =====================================================================
# EJERCICIO 9: ÍNDICE DE PRECIO MEDIO POR TIPO DE ALOJAMIENTO
# Zonas: Burdeos, París, País Vasco francés y Lyon
# =====================================================================

cat("\n📌 9. ÍNDICE DE PRECIO MEDIO POR TIPO DE ALOJAMIENTO\n")

# 1. Cargamos el dataset
df <- read.csv(
  "airbnb_francia_master.csv",
  stringsAsFactors = FALSE
)

# 2. Limpiamos y convertimos los precios a formato numérico
df$price <- as.character(df$price)
df$price <- gsub("\\$", "", df$price)
df$price <- gsub("€", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- trimws(df$price)

df$price <- suppressWarnings(as.numeric(df$price))

# 3. Comprobamos las columnas necesarias
columnas_necesarias <- c(
  "price",
  "region_origen",
  "room_type"
)

faltantes <- setdiff(columnas_necesarias, names(df))

if (length(faltantes) > 0) {

  cat(
    "Faltan estas columnas en el dataset:",
    paste(faltantes, collapse = ", "),
    "\n"
  )

} else {

  # 4. Filtramos precios válidos
  df <- df[
    !is.na(df$price) & df$price > 0,
  ]

  # Eliminamos espacios en los nombres de las regiones y tipos
  df$region_origen <- trimws(as.character(df$region_origen))
  df$room_type <- trimws(as.character(df$room_type))

  # 5. Seleccionamos las cuatro zonas
  # Ajusta estos nombres si no coinciden con los del CSV
  zonas <- list(
    "Burdeos" = c("Burdeos", "Bordeaux"),
    "París" = c("París", "Paris"),
    "País Vasco francés" = c(
      "País Vasco francés",
      "Pays Basque",
      "French Basque Country"
    ),
    "Lyon" = c("Lyon")
  )

  # Normalizamos texto para comparar sin distinguir mayúsculas
  df$region_normalizada <- tolower(df$region_origen)

  # Creamos una lista para almacenar los resultados
  resultados <- list()

  for (zona in names(zonas)) {

    nombres_normalizados <- tolower(zonas[[zona]])

    datos_zona <- df[
      !is.na(df$region_normalizada) &
        df$region_normalizada %in% nombres_normalizados,
    ]

    if (nrow(datos_zona) == 0) {

      cat(
        "\nAviso: no se encontraron registros para",
        zona,
        ". Comprueba los valores de region_origen.\n"
      )

      next
    }

    # Precio medio general de la zona
    precio_medio_zona <- mean(datos_zona$price, na.rm = TRUE)

    # Precio medio y número de alojamientos por tipo
    tipos <- unique(datos_zona$room_type)

    resumen <- do.call(
      rbind,
      lapply(tipos, function(tipo) {

        if (is.na(tipo)) {
          datos_tipo <- datos_zona[is.na(datos_zona$room_type), ]
          tipo_resultado <- NA_character_
        } else {
          datos_tipo <- datos_zona[
            !is.na(datos_zona$room_type) &
              datos_zona$room_type == tipo,
          ]
          tipo_resultado <- tipo
        }

        data.frame(
          room_type = tipo_resultado,
          precio_medio = mean(datos_tipo$price, na.rm = TRUE),
          numero_alojamientos = nrow(datos_tipo),
          stringsAsFactors = FALSE
        )
      })
    )

    rownames(resumen) <- NULL

    # Índice con base 100 en la media de la zona
    resumen$indice_precio <-
      (resumen$precio_medio / precio_medio_zona) * 100

    resumen$zona <- zona

    resultados[[zona]] <- resumen
  }

  # 6. Mostramos los resultados
  if (length(resultados) > 0) {

    indice_precios <- do.call(rbind, resultados)
    rownames(indice_precios) <- NULL

    # Seleccionamos las columnas y ordenamos los resultados
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
        -indice_precios$indice_precio,
        na.last = TRUE
      ),
    ]

    cat("\n🏠 ÍNDICE DE PRECIO MEDIO POR ZONA Y TIPO:\n")

    # Redondeamos las columnas numéricas a 2 decimales
    resultado_mostrar <- indice_precios
    columnas_numericas <- c(
      "numero_alojamientos",
      "precio_medio",
      "indice_precio"
    )

    resultado_mostrar[columnas_numericas] <- lapply(
      resultado_mostrar[columnas_numericas],
      round,
      digits = 2
    )

    print(resultado_mostrar, row.names = FALSE)

    # 7. Tabla comparativa: una fila por tipo de alojamiento
    tipos_alojamiento <- unique(indice_precios$room_type)
    zonas_resultado <- unique(indice_precios$zona)

    tabla_comparativa <- matrix(
      NA_real_,
      nrow = length(tipos_alojamiento),
      ncol = length(zonas_resultado),
      dimnames = list(tipos_alojamiento, zonas_resultado)
    )

    for (i in seq_len(nrow(indice_precios))) {
      tipo <- indice_precios$room_type[i]
      zona <- indice_precios$zona[i]
      valor <- indice_precios$indice_precio[i]

      if (!is.na(tipo)) {
        tabla_comparativa[tipo, zona] <- valor
      }
    }

    cat("\n📊 COMPARACIÓN DE ÍNDICES ENTRE ZONAS:\n")
    print(round(tabla_comparativa, 2))

    # 8. Exportamos los resultados
    write.csv(
      indice_precios,
      "ejercicio9_indice_precios.csv",
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )

    cat(
      "\nResultados guardados en ejercicio9_indice_precios.csv\n"
    )

  } else {

    cat("\nNo se han encontrado datos para las zonas seleccionadas.\n")

  }
}