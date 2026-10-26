
# =====================================================================
# EJERCICIO 10: ÍNDICE DE PRECIOS POR TIPO DE ALOJAMIENTO
# Zonas: Burdeos, París, País Vasco francés y Lyon
# =====================================================================

cat("\n📊 EJERCICIO 10: ÍNDICE DE PRECIOS POR TIPO DE ALOJAMIENTO\n")

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

# 3. Comprobamos que existen las columnas necesarias
columnas_necesarias <- c(
  "price",
  "region_origen",
  "room_type"
)

faltantes <- setdiff(columnas_necesarias, names(df))

if (length(faltantes) > 0) {

  cat(
    "Faltan columnas necesarias:",
    paste(faltantes, collapse = ", "),
    "\n"
  )

} else {

  # 4. Limpiamos los datos
  df <- df[
    !is.na(df$price) & df$price > 0,
  ]

  df$region_origen <- trimws(as.character(df$region_origen))
  df$room_type <- trimws(as.character(df$room_type))

  # Eliminamos tipos de alojamiento vacíos o ausentes
  df <- df[
    !is.na(df$room_type) &
      df$room_type != "",
  ]

  # 5. Definimos las zonas que queremos analizar
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

  # Normalizamos los nombres de las regiones
  df$region_normalizada <- tolower(df$region_origen)

  resultados <- list()

  # 6. Calculamos el índice para cada zona
  for (zona in names(zonas)) {

    nombres_normalizados <- tolower(zonas[[zona]])

    datos_zona <- df[
      !is.na(df$region_normalizada) &
        df$region_normalizada %in% nombres_normalizados,
    ]

    if (nrow(datos_zona) == 0) {

      cat(
        "\nAviso: no se encontraron datos para",
        zona,
        ". Revisa los nombres de region_origen.\n"
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

    # Índice de precios con base 100
    resumen$indice_precio <-
      (resumen$precio_medio / precio_medio_zona) * 100

    resumen$zona <- zona

    resultados[[zona]] <- resumen
  }

  # 7. Unimos los resultados
  if (length(resultados) > 0) {

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

    # 8. Mostramos la tabla de resultados
    cat("\nÍNDICE DE PRECIOS POR ZONA Y TIPO DE ALOJAMIENTO\n")

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

    # 9. Creamos la tabla comparativa
    tipos_alojamiento <- unique(indice_precios$room_type)
    zonas_resultado <- names(zonas)[
      names(zonas) %in% indice_precios$zona
    ]

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

      tabla_comparativa[tipo, zona] <- valor
    }

    cat("\nCOMPARACIÓN DE ÍNDICES ENTRE ZONAS\n")
    print(round(tabla_comparativa, 2))

    # 10. Exportamos los resultados a CSV
    write.csv(
      indice_precios,
      "ejercicio10_indice_precios.csv",
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )

    # 11. Preparamos los datos para el gráfico
    # Instalamos ggplot2 si todavía no está instalado
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      install.packages("ggplot2")
    }

    library(ggplot2)

    tabla_grafico <- indice_precios

    # 12. Generamos el gráfico de barras
    grafico <- ggplot(
      tabla_grafico,
      aes(
        x = room_type,
        y = indice_precio,
        fill = zona
      )
    ) +
      geom_col(
        position = position_dodge(width = 0.8),
        width = 0.8
      ) +

      # Línea de referencia: índice 100
      geom_hline(
        yintercept = 100,
        color = "red",
        linetype = "dashed",
        linewidth = 1.2
      ) +

      # Título y etiquetas
      labs(
        title = "Índice de precios por tipo de alojamiento y zona",
        x = "Tipo de alojamiento",
        y = "Índice de precios (base 100)",
        fill = "Zona"
      ) +

      # Paleta de colores similar a viridis de Python
      scale_fill_viridis_d() +

      # Tema y formato
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 14,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 20,
          hjust = 1
        ),
        legend.position = "right",
        panel.grid.major.y = element_line(
          linetype = "dotted",
          color = "grey70"
        )
      )

    # 13. Guardamos el gráfico
    ggsave(
      filename = "ejercicio10_grafico_indice_precios.png",
      plot = grafico,
      width = 12,
      height = 7,
      dpi = 300
    )

    # 14. Mostramos el gráfico
    print(grafico)

    cat("\nArchivos generados correctamente:\n")
    cat("- ejercicio10_indice_precios.csv\n")
    cat("- ejercicio10_grafico_indice_precios.png\n")

  } else {

    cat(
      "\nNo se han encontrado datos válidos ",
      "para ninguna de las cuatro zonas.\n",
      sep = ""
    )

  }
}