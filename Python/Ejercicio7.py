import pandas as pd

# Cargamos el dataset preprocesado y limpio
df = pd.read_csv("airbnb_francia_master.csv", low_memory=False)
print("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):")
# 1. Elimina espacios en blanco y caracteres no numéricos si los hubiera (ej: $, €, comas)
df['price'] = df['price'].astype(str).str.replace('$', '', regex=False).str.replace(',', '', regex=False).str.strip()
df["price"] = pd.to_numeric(df["price"])

# =====================================================================
# EJERCICIO 7: ALOJAMIENTO CON MEJOR RELACIÓN CALIDAD-PRECIO
# Objetivo: encontrar los alojamientos mejor valorados en relación
# con su precio.
# Criterios: precio y puntuación de las reseñas.
# =====================================================================

print("\n📌 7. ALOJAMIENTOS CON MEJOR RELACIÓN CALIDAD-PRECIO:")

# 1. Creamos una copia del dataset original
df_calidad_precio = df.copy()

# 2. Convertimos las variables a formato numérico
df_calidad_precio["price"] = pd.to_numeric(
    df_calidad_precio["price"]
    .astype(str)
    .str.replace("$", "", regex=False)
    .str.replace(",", "", regex=False)
    .str.strip(),
    errors="coerce"
)

df_calidad_precio["review_scores_rating"] = pd.to_numeric(
    df_calidad_precio["review_scores_rating"],
    errors="coerce"
)

# 3. Filtramos los alojamientos con datos válidos
df_calidad_precio = df_calidad_precio[
    (df_calidad_precio["price"] > 0)
    & (df_calidad_precio["review_scores_rating"] > 0)
].copy()

# 4. Calculamos el índice de relación calidad-precio
# Cuanto mayor sea la puntuación por euro, mejor será la relación.
df_calidad_precio["calidad_precio"] = (
    df_calidad_precio["review_scores_rating"]
    / df_calidad_precio["price"]
)

# 5. Ordenamos los alojamientos de mejor a peor
ranking_calidad_precio = df_calidad_precio.sort_values(
    by="calidad_precio",
    ascending=False
)

# 6. Seleccionamos las columnas que queremos mostrar
columnas_resultado = [
    "name",
    "region_origen",
    "price",
    "review_scores_rating",
    "calidad_precio"
]

# Mostramos las columnas disponibles en el dataset
columnas_resultado = [
    columna for columna in columnas_resultado
    if columna in ranking_calidad_precio.columns
]

# 7. Mostramos los 10 alojamientos con mejor relación calidad-precio
print("\n🏆 TOP 10 ALOJAMIENTOS CON MEJOR RELACIÓN CALIDAD-PRECIO:")

print(
    ranking_calidad_precio[columnas_resultado]
    .head(10)
    .round(3)
    .to_string(index=False)
)

# 8. Destacamos el alojamiento que ocupa la primera posición
if not ranking_calidad_precio.empty:

    mejor = ranking_calidad_precio.iloc[0]

    print("\n🥇 MEJOR ALOJAMIENTO SEGÚN LA RELACIÓN CALIDAD-PRECIO:")
    print(f"Nombre: {mejor['name']}")
    
    if "region_origen" in ranking_calidad_precio.columns:
        print(f"Región: {mejor['region_origen']}")

    print(f"Precio: {mejor['price']:.2f} €")
    print(
        f"Valoración: "
        f"{mejor['review_scores_rating']:.2f}/5"
    )
    print(
        f"Índice calidad-precio: "
        f"{mejor['calidad_precio']:.4f}"
    )

else:
    print("No se encontraron alojamientos con datos válidos.")