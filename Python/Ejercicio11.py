import pandas as pd

# =====================================================================
# EJERCICIO 12: DISPERSIÓN DE LOS PRECIOS DE AIRBNB
# Zonas: Burdeos, París, País Vasco francés y Lyon
# Tres variables estadísticas
# =====================================================================

print("\nEJERCICIO 12: DISPERSIÓN DE LOS PRECIOS")

# 1. Cargamos el dataset
df = pd.read_csv(
    "airbnb_francia_master.csv",
    low_memory=False
)

# 2. Comprobamos las columnas necesarias
columnas = ["price", "region_origen"]
faltantes = [col for col in columnas if col not in df.columns]

if faltantes:
    print(f"Faltan columnas: {faltantes}")
    raise SystemExit(1)

# 3. Limpiamos los precios
df["price"] = pd.to_numeric(
    df["price"]
    .astype(str)
    .str.replace("$", "", regex=False)
    .str.replace("€", "", regex=False)
    .str.replace(",", "", regex=False)
    .str.strip(),
    errors="coerce"
)

df = df[
    (df["price"] > 0)
    & df["region_origen"].notna()
].copy()

df["region_normalizada"] = (
    df["region_origen"]
    .astype("string")
    .str.strip()
    .str.casefold()
)

# 4. Definimos las zonas
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

resultados = []

# 5. Calculamos las tres variables estadísticas
for zona, nombres in zonas.items():

    nombres = [nombre.casefold() for nombre in nombres]

    precios = df.loc[
        df["region_normalizada"].isin(nombres),
        "price"
    ]

    if precios.empty:
        print(f"Aviso: no hay datos para {zona}.")
        continue

    resultados.append({
        "zona": zona,
        "precio_medio": precios.mean(),
        "precio_mediano": precios.median(),
        "desviacion_tipica": precios.std()
    })

# 6. Mostramos y guardamos los resultados
if resultados:

    tabla = pd.DataFrame(resultados)

    columnas_numericas = [
        "precio_medio",
        "precio_mediano",
        "desviacion_tipica"
    ]

    tabla[columnas_numericas] = (
        tabla[columnas_numericas].round(2)
    )

    print("\nRESULTADOS POR ZONA\n")
    print(tabla.to_string(index=False))

    tabla.to_csv(
        "ejercicio12_variabilidad_precios.csv",
        index=False,
        encoding="utf-8-sig"
    )

    print(
        "\nResultados guardados en "
        "ejercicio12_variabilidad_precios.csv"
    )

else:
    print("No se encontraron datos para las zonas seleccionadas.")