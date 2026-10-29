import pandas as pd

# =====================================================================
# EJERCICIO 9: ÍNDICE DE PRECIO MEDIO POR TIPO DE ALOJAMIENTO
# Zonas: Burdeos, París, País Vasco francés y Lyon
# =====================================================================

print("\n📌 9. ÍNDICE DE PRECIO MEDIO POR TIPO DE ALOJAMIENTO")

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

# 3. Comprobamos las columnas necesarias
columnas_necesarias = ["price", "region_origen", "room_type"]
faltantes = [col for col in columnas_necesarias if col not in df.columns]

if faltantes:
    print(f"Faltan estas columnas en el dataset: {faltantes}")

else:
    # 4. Filtramos precios válidos
    df = df[df["price"] > 0].copy()

    # Eliminamos espacios en los nombres de las regiones y tipos
    df["region_origen"] = df["region_origen"].astype("string").str.strip()
    df["room_type"] = df["room_type"].astype("string").str.strip()

    # 5. Seleccionamos las cuatro zonas
    # Ajusta estos nombres si no coinciden con los del CSV
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

    # Normalizamos texto para comparar sin distinguir mayúsculas
    df["region_normalizada"] = (
        df["region_origen"].str.casefold()
    )

    resultados = []

    for zona, nombres in zonas.items():

        nombres_normalizados = [n.casefold() for n in nombres]

        datos_zona = df[
            df["region_normalizada"].isin(nombres_normalizados)
        ].copy()

        if datos_zona.empty:
            print(
                f"\nAviso: no se encontraron registros para {zona}. "
                "Comprueba los valores de region_origen."
            )
            continue

        # Precio medio general de la zona
        precio_medio_zona = datos_zona["price"].mean()

        # Precio medio y número de alojamientos por tipo
        resumen = (
            datos_zona
            .groupby("room_type", dropna=False)
            .agg(
                precio_medio=("price", "mean"),
                numero_alojamientos=("price", "size")
            )
            .reset_index()
        )

        # Índice con base 100 en la media de la zona
        resumen["indice_precio"] = (
            resumen["precio_medio"] / precio_medio_zona
        ) * 100

        resumen["zona"] = zona

        resultados.append(resumen)

    # 6. Mostramos los resultados
    if resultados:
        indice_precios = pd.concat(resultados, ignore_index=True)

        indice_precios = indice_precios[
            [
                "zona",
                "room_type",
                "numero_alojamientos",
                "precio_medio",
                "indice_precio"
            ]
        ].sort_values(["zona", "indice_precio"], ascending=[True, False])

        print("\n🏠 ÍNDICE DE PRECIO MEDIO POR ZONA Y TIPO:")
        print(
            indice_precios.round(2).to_string(index=False)
        )

        # 7. Tabla comparativa: una fila por tipo de alojamiento
        tabla_comparativa = indice_precios.pivot(
            index="room_type",
            columns="zona",
            values="indice_precio"
        )

        print("\n📊 COMPARACIÓN DE ÍNDICES ENTRE ZONAS:")
        print(tabla_comparativa.round(2).to_string())

        # 8. Exportamos los resultados
        indice_precios.to_csv(
            "ejercicio9_indice_precios.csv",
            index=False,
            encoding="utf-8-sig"
        )

        print("\nResultados guardados en ejercicio9_indice_precios.csv")