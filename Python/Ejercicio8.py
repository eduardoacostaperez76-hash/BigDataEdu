import pandas as pd

# Cargamos el dataset preprocesado y limpio
df = pd.read_csv("airbnb_francia_master.csv", low_memory=False)
print("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):")
# 1. Elimina espacios en blanco y caracteres no numéricos si los hubiera (ej: $, €, comas)
df['price'] = df['price'].astype(str).str.replace('$', '', regex=False).str.replace(',', '', regex=False).str.strip()
df["price"] = pd.to_numeric(df["price"])

# =====================================================================
# EJERCICIO 8: INDICADOR DE REFERENCIA
# Variables: precio (50 %), número de camas (30 %),
# valoración de reseñas (20 %).
# =====================================================================

print("\n📌 8. RANKING DE ALOJAMIENTOS: INDICADOR DE REFERENCIA")

# 1. Creamos una copia del dataset
df_referencia = df.copy()

# 2. Convertimos las variables a formato numérico
for columna in ["price", "beds", "review_scores_rating"]:
    df_referencia[columna] = pd.to_numeric(
        df_referencia[columna]
        .astype(str)
        .str.replace("$", "", regex=False)
        .str.replace(",", "", regex=False)
        .str.strip(),
        errors="coerce"
    )

# 3. Eliminamos registros sin datos válidos
df_referencia = df_referencia[
    (df_referencia["price"] > 0)
    & (df_referencia["beds"] > 0)
    & (df_referencia["review_scores_rating"] > 0)
].copy()

if df_referencia.empty:
    print("No hay alojamientos con datos válidos para calcular el indicador.")

else:
    # 4. Normalizamos las variables entre 0 y 1

    # Precio: menor precio = mejor puntuación
    precio_min = df_referencia["price"].min()
    precio_max = df_referencia["price"].max()

    if precio_max == precio_min:
        df_referencia["score_precio"] = 1.0
    else:
        df_referencia["score_precio"] = (
            (precio_max - df_referencia["price"])
            / (precio_max - precio_min)
        )

    # Número de camas: más camas = mejor puntuación
    camas_min = df_referencia["beds"].min()
    camas_max = df_referencia["beds"].max()

    if camas_max == camas_min:
        df_referencia["score_camas"] = 1.0
    else:
        df_referencia["score_camas"] = (
            (df_referencia["beds"] - camas_min)
            / (camas_max - camas_min)
        )

    # Reseñas: mejor valoración = mejor puntuación
    df_referencia["score_resenas"] = (
        df_referencia["review_scores_rating"] / 5
    ).clip(0, 1)

    # 5. Calculamos el INDICADOR DE REFERENCIA sobre 100
    df_referencia["indicador_referencia"] = 100 * (
        0.50 * df_referencia["score_precio"]
        + 0.30 * df_referencia["score_camas"]
        + 0.20 * df_referencia["score_resenas"]
    )

    # 6. Ordenamos los alojamientos de mayor a menor indicador
    ranking_referencia = df_referencia.sort_values(
        by="indicador_referencia",
        ascending=False
    )

    # 7. Mostramos el Top 10
    columnas_resultado = [
        "name",
        "region_origen",
        "price",
        "beds",
        "review_scores_rating",
        "indicador_referencia"
    ]

    columnas_resultado = [
        col for col in columnas_resultado
        if col in ranking_referencia.columns
    ]

    print("\n🏆 TOP 10 ALOJAMIENTOS SEGÚN EL INDICADOR DE REFERENCIA:")

    print(
        ranking_referencia[columnas_resultado]
        .head(10)
        .round(2)
        .to_string(index=False)
    )

    # 8. Identificamos el alojamiento mejor clasificado
    mejor = ranking_referencia.iloc[0]

    print("\n🥇 ALOJAMIENTO CON MEJOR INDICADOR DE REFERENCIA:")
    print(f"Nombre: {mejor['name']}")
    print(f"Precio: {mejor['price']:.2f} €")
    print(f"Número de camas: {mejor['beds']:.0f}")
    print(f"Valoración: {mejor['review_scores_rating']:.2f}/5")
    print(
        f"Indicador de referencia: "
        f"{mejor['indicador_referencia']:.2f}/100"
    )