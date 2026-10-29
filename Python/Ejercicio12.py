import pandas as pd
import matplotlib.pyplot as plt

# =====================================================================
# EJERCICIO 12: DISPERSIÓN DE LOS PRECIOS DE AIRBNB
# Zonas: Burdeos, París, País Vasco francés y Lyon
# Variables: precio medio, precio mediano y desviación típica
# =====================================================================

print("\nEJERCICIO 12: DISPERSIÓN DE LOS PRECIOS DE AIRBNB")

# 1. Cargar el dataset
archivo = "airbnb_francia_master.csv"

try:
    df = pd.read_csv(archivo, low_memory=False)
except FileNotFoundError:
    print(f"Error: no se encuentra el archivo {archivo}")
    raise SystemExit(1)

# 2. Comprobar las columnas necesarias
columnas_necesarias = ["price", "region_origen"]

faltantes = [
    columna for columna in columnas_necesarias
    if columna not in df.columns
]

if faltantes:
    print(f"Faltan columnas necesarias: {faltantes}")
    raise SystemExit(1)

# 3. Limpiar y convertir los precios a formato numérico
df["price"] = pd.to_numeric(
    df["price"]
    .astype(str)
    .str.replace("$", "", regex=False)
    .str.replace("€", "", regex=False)
    .str.replace(",", "", regex=False)
    .str.strip(),
    errors="coerce"
)

# Conservar precios positivos y regiones válidas
df = df[
    (df["price"] > 0)
    & df["region_origen"].notna()
].copy()

# Normalizar los nombres de las regiones
df["region_normalizada"] = (
    df["region_origen"]
    .astype("string")
    .str.strip()
    .str.casefold()
)

# 4. Definir las zonas
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

# 5. Calcular las tres variables estadísticas
for zona, nombres in zonas.items():

    nombres_normalizados = [
        nombre.casefold() for nombre in nombres
    ]

    precios = df.loc[
        df["region_normalizada"].isin(nombres_normalizados),
        "price"
    ]

    if precios.empty:
        print(
            f"Aviso: no se encontraron datos para {zona}. "
            "Comprueba los nombres de region_origen."
        )
        continue

    resultados.append({
        "zona": zona,
        "precio_medio": precios.mean(),
        "precio_mediano": precios.median(),
        "desviacion_tipica": precios.std()
    })

# 6. Mostrar y exportar los resultados
if not resultados:
    print("\nNo se encontraron datos para las zonas seleccionadas.")
    raise SystemExit(1)

tabla = pd.DataFrame(resultados)

columnas_numericas = [
    "precio_medio",
    "precio_mediano",
    "desviacion_tipica"
]

# Redondeamos a dos decimales
tabla[columnas_numericas] = (
    tabla[columnas_numericas].round(2)
)

print("\nRESULTADOS ESTADÍSTICOS POR ZONA\n")
print(tabla.to_string(index=False))

# Guardar la tabla en CSV
tabla.to_csv(
    "ejercicio12_variabilidad_precios.csv",
    index=False,
    encoding="utf-8-sig"
)

print("\nTabla guardada en ejercicio12_variabilidad_precios.csv")

# =====================================================================
# 7. GRÁFICO 1: PRECIO MEDIO Y PRECIO MEDIANO
# =====================================================================

datos_precios = tabla.set_index("zona")[
    ["precio_medio", "precio_mediano"]
]

ax = datos_precios.plot(
    kind="bar",
    figsize=(10, 6),
    color=["#3478B8", "#F0A44B"],
    width=0.75
)

ax.set_title(
    "Precio medio y mediano de Airbnb por zona",
    fontsize=14,
    fontweight="bold"
)

ax.set_xlabel("Zona")
ax.set_ylabel("Precio (€)")
ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=0
)

ax.legend(
    ["Precio medio", "Precio mediano"]
)

ax.grid(
    axis="y",
    linestyle="--",
    alpha=0.3
)

plt.tight_layout()

plt.savefig(
    "ejercicio12_precios_medios_medianas.png",
    dpi=300,
    bbox_inches="tight"
)

plt.show()
plt.close()

# =====================================================================
# 8. GRÁFICO 2: DESVIACIÓN TÍPICA
# =====================================================================

ax = tabla.plot(
    x="zona",
    y="desviacion_tipica",
    kind="bar",
    legend=False,
    color="#55A889",
    figsize=(9, 5)
)

ax.set_title(
    "Dispersión de los precios de Airbnb por zona",
    fontsize=14,
    fontweight="bold"
)

ax.set_xlabel("Zona")
ax.set_ylabel("Desviación típica (€)")
ax.tick_params(axis="x", labelrotation=0)

ax.grid(
    axis="y",
    linestyle="--",
    alpha=0.3
)

plt.tight_layout()

plt.savefig(
    "ejercicio12_desviacion_tipica.png",
    dpi=300,
    bbox_inches="tight"
)

plt.show()
plt.close()

# =====================================================================
# 9. FINALIZACIÓN
# =====================================================================

print("\nEJERCICIO 12 FINALIZADO")
print("Archivos generados:")
print("- ejercicio12_variabilidad_precios.csv")
print("- ejercicio12_precios_medios_medianas.png")
print("- ejercicio12_desviacion_tipica.png")