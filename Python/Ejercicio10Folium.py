import pandas as pd
import folium
from folium.plugins import Fullscreen, MiniMap
import html

# =====================================================================
# EJERCICIO 10: ÍNDICE DE PRECIOS AIRBNB CON MAPA INTERACTIVO
# Zonas: Burdeos, París, País Vasco francés y Lyon
# =====================================================================

print("\nEJERCICIO 10: MAPA INTERACTIVO DEL ÍNDICE DE PRECIOS")

# ---------------------------------------------------------------------
# 1. CARGAR EL DATASET
# ---------------------------------------------------------------------

archivo = "airbnb_francia_master.csv"

try:
    df = pd.read_csv(archivo, low_memory=False)
except FileNotFoundError:
    print(f"Error: no se encuentra el archivo {archivo}")
    raise SystemExit(1)

# ---------------------------------------------------------------------
# 2. COMPROBAR LAS COLUMNAS NECESARIAS
# ---------------------------------------------------------------------

columnas_necesarias = ["price", "region_origen", "room_type"]

faltantes = [
    columna for columna in columnas_necesarias
    if columna not in df.columns
]

if faltantes:
    print(f"Faltan columnas en el CSV: {faltantes}")
    raise SystemExit(1)

# ---------------------------------------------------------------------
# 3. LIMPIAR LOS PRECIOS
# ---------------------------------------------------------------------

df["price"] = pd.to_numeric(
    df["price"]
    .astype(str)
    .str.replace("$", "", regex=False)
    .str.replace("€", "", regex=False)
    .str.replace(",", "", regex=False)
    .str.strip(),
    errors="coerce"
)

df = df[df["price"] > 0].copy()

# Limpiar los nombres de regiones y tipos de alojamiento
df["region_origen"] = (
    df["region_origen"]
    .astype("string")
    .str.strip()
)

df["room_type"] = (
    df["room_type"]
    .astype("string")
    .str.strip()
)

df = df[
    df["region_origen"].notna()
    & df["room_type"].notna()
    & (df["room_type"] != "")
].copy()

# Normalizar los nombres para comparar sin distinguir mayúsculas
df["region_normalizada"] = (
    df["region_origen"].str.casefold()
)

# ---------------------------------------------------------------------
# 4. DEFINIR ZONAS, COORDENADAS Y COLORES
# ---------------------------------------------------------------------

zonas = {
    "Burdeos": {
        "nombres": ["Burdeos", "Bordeaux"],
        "coordenadas": [44.8378, -0.5792],
        "color": "blue"
    },
    "París": {
        "nombres": ["París", "Paris"],
        "coordenadas": [48.8566, 2.3522],
        "color": "red"
    },
    "País Vasco francés": {
        "nombres": [
            "País Vasco francés",
            "Pays Basque",
            "French Basque Country"
        ],
        # Bayona como referencia geográfica
        "coordenadas": [43.4929, -1.4748],
        "color": "green"
    },
    "Lyon": {
        "nombres": ["Lyon"],
        "coordenadas": [45.7640, 4.8357],
        "color": "purple"
    }
}

# ---------------------------------------------------------------------
# 5. CALCULAR LOS ÍNDICES DE PRECIOS
# ---------------------------------------------------------------------

resultados = []

for zona, info in zonas.items():

    nombres_normalizados = [
        nombre.casefold()
        for nombre in info["nombres"]
    ]

    datos_zona = df[
        df["region_normalizada"].isin(nombres_normalizados)
    ].copy()

    if datos_zona.empty:
        print(
            f"Aviso: no hay registros para {zona}. "
            "Comprueba los nombres de region_origen."
        )
        continue

    # Media de todos los alojamientos de la zona
    precio_medio_zona = datos_zona["price"].mean()

    # Resumen por tipo de alojamiento
    resumen = (
        datos_zona
        .groupby("room_type")
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

# ---------------------------------------------------------------------
# 6. COMPROBAR QUE EXISTEN RESULTADOS
# ---------------------------------------------------------------------

if not resultados:
    print(
        "\nNo se han encontrado datos para las zonas seleccionadas."
    )
    print(
        "Revisa los valores de la columna region_origen del CSV."
    )
    raise SystemExit(1)

indice_precios = pd.concat(resultados, ignore_index=True)

# Ordenar los resultados
indice_precios = indice_precios[
    [
        "zona",
        "room_type",
        "numero_alojamientos",
        "precio_medio",
        "indice_precio"
    ]
].sort_values(
    ["zona", "indice_precio"],
    ascending=[True, False]
)

# ---------------------------------------------------------------------
# 7. MOSTRAR Y EXPORTAR LOS RESULTADOS
# ---------------------------------------------------------------------

print("\nÍNDICE DE PRECIOS POR ZONA Y TIPO DE ALOJAMIENTO\n")
print(indice_precios.round(2).to_string(index=False))

archivo_csv = "ejercicio10_indice_precios.csv"

indice_precios.to_csv(
    archivo_csv,
    index=False,
    encoding="utf-8-sig"
)

print(f"\nTabla guardada en: {archivo_csv}")

# ---------------------------------------------------------------------
# 8. CREAR EL MAPA BASE DE FRANCIA
# ---------------------------------------------------------------------

mapa = folium.Map(
    location=[46.6, 2.5],
    zoom_start=6,
    tiles=None,
    control_scale=True
)

# Capa cartográfica de OpenStreetMap
folium.TileLayer(
    tiles="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attr="© OpenStreetMap contributors",
    name="OpenStreetMap",
    overlay=False,
    control=True
).add_to(mapa)

# Controles de navegación
Fullscreen(
    position="topright"
).add_to(mapa)

MiniMap(
    toggle_display=True
).add_to(mapa)

# ---------------------------------------------------------------------
# 9. AÑADIR MARCADORES Y VENTANAS INFORMATIVAS
# ---------------------------------------------------------------------

for zona, info in zonas.items():

    datos_zona = indice_precios[
        indice_precios["zona"] == zona
    ]

    # Si no hay datos, no ponemos marcador
    if datos_zona.empty:
        continue

    # Recuperamos el precio medio general de la zona
    nombres_normalizados = [
        nombre.casefold()
        for nombre in info["nombres"]
    ]

    datos_originales = df[
        df["region_normalizada"].isin(nombres_normalizados)
    ]

    precio_medio_zona = datos_originales["price"].mean()
    numero_total = len(datos_originales)

    # Construimos las filas de la tabla HTML
    filas_html = ""

    for _, fila in datos_zona.iterrows():

        tipo = html.escape(str(fila["room_type"]))
        precio = fila["precio_medio"]
        indice = fila["indice_precio"]
        cantidad = int(fila["numero_alojamientos"])

        filas_html += f"""
        <tr>
            <td style="padding:6px;border:1px solid #ddd;">
                {tipo}
            </td>
            <td style="padding:6px;border:1px solid #ddd;">
                {precio:.2f} €
            </td>
            <td style="padding:6px;border:1px solid #ddd;">
                {indice:.1f}
            </td>
            <td style="padding:6px;border:1px solid #ddd;">
                {cantidad}
            </td>
        </tr>
        """

    # Ventana emergente
    contenido = f"""
    <div style="
        width: 440px;
        max-width: 100%;
        font-family: Arial, sans-serif;
        font-size: 13px;
    ">
        <h3 style="color:{info['color']};margin-bottom:8px;">
            {html.escape(zona)}
        </h3>

        <p>
            <b>Precio medio general:</b>
            {precio_medio_zona:.2f} €
        </p>

        <p>
            <b>Número de alojamientos:</b>
            {numero_total}
        </p>

        <p>
            <b>Índice de referencia:</b> 100
        </p>

        <div style="overflow-x:auto;">
            <table style="
                width:100%;
                border-collapse:collapse;
                text-align:center;
            ">
                <thead>
                    <tr style="background:#eeeeee;">
                        <th style="padding:6px;border:1px solid #ddd;">
                            Tipo
                        </th>
                        <th style="padding:6px;border:1px solid #ddd;">
                            Precio medio
                        </th>
                        <th style="padding:6px;border:1px solid #ddd;">
                            Índice
                        </th>
                        <th style="padding:6px;border:1px solid #ddd;">
                            N.º
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {filas_html}
                </tbody>
            </table>
        </div>

        <p style="font-size:12px;color:#555;">
            Índice superior a 100: precio por encima
            de la media de la zona.
        </p>
    </div>
    """

popup = folium.Popup(
    folium.IFrame(
        html=contenido,
        width=480,
        height=320
    ),
    max_width=500
)

    folium.Marker(
        location=info["coordenadas"],
        tooltip=f"Consultar precios: {zona}",
        popup=popup,
        icon=folium.Icon(
            color=info["color"],
            icon="home",
            prefix="fa"
        )
    ).add_to(mapa)

# ---------------------------------------------------------------------
# 10. AÑADIR TÍTULO Y LEYENDA
# ---------------------------------------------------------------------

titulo = """
<div style="
    position:fixed;
    top:10px;
    left:50%;
    transform:translateX(-50%);
    z-index:9999;
    background:white;
    padding:10px 14px;
    border:1px solid #ccc;
    border-radius:6px;
    box-shadow:0 2px 6px #aaa;
    font-family:Arial,sans-serif;
    font-size:15px;
    font-weight:bold;
    text-align:center;
    max-width:80%;
">
    Índice de precios Airbnb en Francia
</div>
"""

mapa.get_root().html.add_child(
    folium.Element(titulo)
)

# Control para activar/desactivar las capas
folium.LayerControl(
    position="topright"
).add_to(mapa)

# ---------------------------------------------------------------------
# 11. GUARDAR EL MAPA
# ---------------------------------------------------------------------

archivo_html = "ejercicio10_mapa_precios.html"

mapa.save(archivo_html)

print("\nArchivos generados correctamente:")
print(f"- {archivo_csv}")
print(f"- {archivo_html}")

print(
    "\nAbre el archivo HTML en Chrome, Edge o Firefox "
    "para explorar el mapa interactivo."
)