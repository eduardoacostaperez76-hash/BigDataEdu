import pandas as pd
import matplotlib.pyplot as plt

# =====================================================================
# EJERCICIO 10: ÍNDICE DE PRECIOS POR TIPO DE ALOJAMIENTO
# Zonas: Burdeos, París, País Vasco francés y Lyon
# =====================================================================

print("\n📊 EJERCICIO 10: ÍNDICE DE PRECIOS POR TIPO DE ALOJAMIENTO")

# 1. Cargamos el dataset
df = pd.read_csv("airbnb_francia_master.csv", low_memory=False)

# 2. Limpiamos y convertimos los precios a formato numérico
df["price"] = pd.to_numeric(
    df["price"]
    .astype(str)
    .str.replace("$", "", regex=False)
    .str.replace("€", "", regex=False)
    .str.replace(",", "", regex=False)
    .str.strip(),
    errors="coerce"
)

# 3. Comprobamos que existen las columnas necesarias
columnas_necesarias = ["price", "region_origen", "room_type"]
faltantes = [
    col for col in columnas_necesarias
    if col not in df.columns
]

if faltantes:
    print(f"Faltan columnas necesarias: {faltantes}")

else:
    # 4. Limpiamos los datos
    df = df[df["price"] > 0].copy()

    df["region_origen"] = (
        df["region_origen"].astype("string").str.strip()
    )

    df["room_type"] = (
        df["room_type"].astype("string").str.strip()
    )

    # Eliminamos tipos de alojamiento vacíos
    df = df[
        df["room_type"].notna()
        & (df["room_type"] != "")
    ].copy()

    # 5. Definimos las zonas que queremos analizar
    zonas = {
        "Burdeos": ["Burdeos", "Bordeaux"],
        "París": ["París", "Paris"],
        "País Vasco francés": [
            "País Vasco francés",
            "Pays Basque",
            "French Basque Country"
        ],
        "Lyon": ["Lyon"]
    }

    # Normalizamos los nombres de las regiones
    df["region_normalizada"] = (
        df["region_origen"].str.casefold()
    )

    resultados = []

    # 6. Calculamos el índice para cada zona
    for zona, nombres in zonas.items():

        nombres_normalizados = [
            nombre.casefold() for nombre in nombres
        ]

        datos_zona = df[
            df["region_normalizada"].isin(nombres_normalizados)
        ].copy()

        if datos_zona.empty:
            print(
                f"\nAviso: no se encontraron datos para {zona}. "
                "Revisa los nombres de region_origen."
            )
            continue

        # Precio medio general de la zona
        precio_medio_zona = datos_zona["price"].mean()

        # Precio medio y número de alojamientos por tipo
        resumen = (
            datos_zona
            .groupby("room_type")
            .agg(
                precio_medio=("price", "mean"),
                numero_alojamientos=("price", "size")
            )
            .reset_index()
        )

        # Índice de precios con base 100
        resumen["indice_precio"] = (
            resumen["precio_medio"]
            / precio_medio_zona
        ) * 100

        resumen["zona"] = zona

        resultados.append(resumen)

    # 7. Unimos los resultados
    if resultados:

        indice_precios = pd.concat(
            resultados,
            ignore_index=True
        )

        indice_precios = indice_precios[
            [
                "zona",
                "room_type",
                "numero_alojamientos",
                "precio_medio",
                "indice_precio"
            ]
        ]

        indice_precios = indice_precios.sort_values(
            ["zona", "indice_precio"],
            ascending=[True, False]
        )

        # 8. Mostramos la tabla de resultados
        print("\nÍNDICE DE PRECIOS POR ZONA Y TIPO DE ALOJAMIENTO")

        print(
            indice_precios.round(2).to_string(index=False)
        )

        # 9. Creamos la tabla comparativa
        tabla_comparativa = indice_precios.pivot(
            index="room_type",
            columns="zona",
            values="indice_precio"
        )

        print("\nCOMPARACIÓN DE ÍNDICES ENTRE ZONAS")

        print(tabla_comparativa.round(2).to_string())

        # 10. Exportamos los resultados a CSV
        indice_precios.to_csv(
            "ejercicio10_indice_precios.csv",
            index=False,
            encoding="utf-8-sig"
        )

        # 11. Preparamos los datos para el gráfico
        tabla_grafico = indice_precios.pivot(
            index="room_type",
            columns="zona",
            values="indice_precio"
        )

        # 12. Generamos el gráfico de barras
        ax = tabla_grafico.plot(
            kind="bar",
            figsize=(12, 7),
            width=0.8,
            colormap="viridis"
        )

        # Línea de referencia: índice 100
        ax.axhline(
            y=100,
            color="red",
            linestyle="--",
            linewidth=1.5,
            label="Media de la zona (índice 100)"
        )

        # Título y etiquetas
        ax.set_title(
            "Índice de precios por tipo de alojamiento y zona",
            fontsize=14,
            fontweight="bold"
        )

        ax.set_xlabel("Tipo de alojamiento")
        ax.set_ylabel("Índice de precios (base 100)")

        ax.tick_params(
            axis="x",
            labelrotation=20
        )

        ax.legend(
            title="Zona",
            bbox_to_anchor=(1.02, 1),
            loc="upper left"
        )

        ax.grid(
            axis="y",
            linestyle=":",
            alpha=0.4
        )

        plt.tight_layout()

        # 13. Guardamos el gráfico
        plt.savefig(
            "ejercicio10_grafico_indice_precios.png",
            dpi=300,
            bbox_inches="tight"
        )

        # 14. Mostramos el gráfico
        plt.show()

        print("\nArchivos generados correctamente:")
        print("- ejercicio10_indice_precios.csv")
        print("- ejercicio10_grafico_indice_precios.png")

    else:
        print(
            "\nNo se han encontrado datos válidos "
            "para ninguna de las cuatro zonas."
        )