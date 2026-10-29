import pandas as pd

# Cargamos el dataset preprocesado y limpio
df = pd.read_csv("airbnb_francia_master.csv", low_memory=False)
print("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):")
# 1. Elimina espacios en blanco y caracteres no numéricos si los hubiera (ej: $, €, comas)
df['price'] = df['price'].astype(str).str.replace('$', '', regex=False).str.replace(',', '', regex=False).str.strip()
df["price"] = pd.to_numeric(df["price"])

# =====================================================================
# EJERCICIO 6: Alojamiento superbarato en habitación privada, con un precio igual o 
#inferior a 40 y con reseñas igual o superiores a 4,2
# =====================================================================
print("\n📌 6. Alojamiento superbarato (Filtro Cruzado Complejo):")
filtro_economico = (
    (df["room_type"] == "Private room")
    & (df["price"] < 40)
    & (df["review_scores_rating"] >= 4.2)
)

oportunidades = df[filtro_economico][['room_type', 'neighbourhood_cleansed', 'price', 'review_scores_rating']]
print(f"🎯 Se han encontrado {len(oportunidades)} opciones que cumplen los requisitos del inversor.")
print(oportunidades.head(10).to_string(index=False))  # Mostramos solo las primeras 10 para no saturar la pantalla