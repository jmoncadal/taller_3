# Predicción de precios de vivienda en Bogotá (Taller 3 – Big Data y ML)

Este repositorio contiene el código y la organización de archivos del proyecto de predicción de precios de vivienda desarrollado para el Problem Set 3 **“Making Money with ML?”** del curso **Big Data and Machine Learning para Economía Aplicada (MECA 4107)**. El objetivo es construir un flujo reproducible para estimar el precio de oferta de propiedades residenciales en Bogotá (con énfasis en Chapinero) a partir de características estructurales, variables espaciales y contenido textual de los avisos, y evaluar diferentes algoritmos de *machine learning* en un concurso de Kaggle.

---

## 1. Propósito del repositorio

- Implementar un pipeline completo de **preprocesamiento**, **ingeniería de variables** (estructurales, espaciales y textuales) y **entrenamiento de modelos** para predecir precios de vivienda.
- Comparar distintos algoritmos (OLS, Elastic Net, árboles, Random Forest, Boosting, redes neuronales y SuperLearner) con métricas de desempeño homogéneas.
- Generar las **submissions para Kaggle** y documentar un flujo de trabajo **reproducible** y fácilmente navegable.

---

## 2. Origen de los datos

### 2.1. Datos base

- **Fuente principal**: base de datos de propiedades residenciales en Bogotá proveniente de **Properati**, suministrada por el curso.
- El conjunto incluye información de:
  - Características físicas (área, número de habitaciones, baños, tipo de propiedad, etc.).
  - Ubicación geográfica (latitud, longitud).
  - Títulos y descripciones de los avisos.

### 2.2. Variables espaciales (OpenStreetMap)

Para enriquecer la información del entorno, usamos el paquete **`osmdata`** en R (el proyecto lo refiere como “osdata”) para consultar **OpenStreetMap (OSM)** y construir nuevas variables espaciales, por ejemplo:

- Distancias a cafés, estaciones de bus y otros puntos de interés.
- Conteos de amenidades en buffers alrededor de cada propiedad.
- Variables de cercanía a centros comerciales y características del barrio.

Estas variables se calculan en los scripts y se almacenan en la carpeta `stores/` (ver sección de estructura).

### 2.3. Variables textuales

A partir de los campos de **título** y **descripción** de los avisos, se construyen variables de texto, por ejemplo:

- Número de palabras.
- Presencia de palabras clave (remodelado, garaje, terraza, cercanía, etc.).
- Otras transformaciones sencillas a partir del análisis de texto.

La extracción y limpieza de palabras se realiza en el notebook `text_analysis.ipynb`.

---

## 3. Estructura del repositorio

La estructura principal del repositorio es la siguiente:

```txt
.
├── scripts/   # Código ejecutable principal
├── stores/    # Bases de datos (raw y procesadas)
└── views/     # Resultados: figuras, mapas y submissions de Kaggle
```

### 3.1. `scripts/`

Contiene todos los **scripts ejecutables** necesarios para reproducir el proyecto:

- `taller_3.R`
- `modelospt2.R`
- `text_analysis.ipynb`

(ver detalles en la sección 4).

### 3.2. `stores/`

Carpeta destinada a las bases de datos:

- **Raw data**: archivos originales (por ejemplo, training y test de Properati).
- **Datos procesados**: datasets resultantes del preprocesamiento, unión con variables espaciales, variables textuales, etc.

> Nota: por temas de tamaño o confidencialidad, algunos archivos raw pueden no estar incluidos en el repositorio público. En ese caso, deben ubicarse aquí siguiendo los nombres esperados por los scripts.

### 3.3. `views/`

Carpeta con los **resultados finales**:

- Gráficos, mapas y tablas relevantes (en formatos como `.png`, `.html`, etc.).
- Archivos de **submissions de Kaggle** generados por los modelos (por ejemplo, `.csv` listos para subir a Kaggle).

---

## 4. Scripts principales

### 4.1. `taller_3.R`

Script central del proyecto. Sus tareas principales son:

- **Preprocesamiento de datos**:
  - Carga de bases raw desde `stores/`.
  - Limpieza de variables, manejo de missing values, recodificación de factores.
- **Adición de variables espaciales**:
  - Conversión a objetos `sf`.
  - Cálculo de distancias y conteos de amenidades usando OpenStreetMap (`osmdata`).
- **Construcción de modelos**:
  - Entrenamiento de varios modelos de predicción (incluyendo el mejor modelo según la métrica de Kaggle).
  - Implementación de estrategias de validación (por ejemplo, CV estándar y/o espacial).
- **Generación de submissions**:
  - Creación de los archivos `.csv` con las predicciones sobre el set de test.
  - Guardado de estos archivos en `views/` para subirlos a Kaggle.

### 4.2. `modelospt2.R`

Script orientado a la **exploración y evaluación adicional de modelos**:

- Prueba y comparación de diferentes algoritmos y especificaciones.
- Ajuste de hiperparámetros y posibles modificaciones a la base (por ejemplo, filtrado, nuevas transformaciones de variables).
- Análisis de desempeño (métricas, rankings de modelos) y apoyo para seleccionar el modelo final.

Este script es útil para documentar el proceso iterativo de modelación más allá del mejor modelo.

### 4.3. `text_analysis.ipynb`

Notebook de análisis de texto (Jupyter):

- Limpieza y procesamiento de los campos de **título** y **descripción**.
- Extracción de palabras clave y construcción de indicadores (por ejemplo, `tiene_balcon`, `tiene_terraza`, `menciona_cercania_txt`, etc.).
- Generación de variables agregadas (conteos, longitudes, etc.) que después se integran a la base principal y se usan en los modelos.

---

## 5. Requisitos y dependencias

### 5.1. Versión de R

- **R >= 4.3**

Se recomienda trabajar con **RStudio** para facilitar la ejecución de los scripts.

### 5.2. Paquetes de R

Los scripts usan `pacman` para cargar/instalar automáticamente las librerías necesarias. Fragmento relevante:

```r
rm(list = ls())

# Importing libraries -----------------------------------------------------

library(pacman)
p_load(
  rio, writexl, readxl, tidyverse, caret, keras,
  reticulate, tidymodels, sf, gt, gtsummary, osmdata,
  gridExtra, plotly, skimr, leaflet, lwgeom
)
p_load("spatialsample")

p_load("caret")
p_load("xgboost")
p_load(leaps)
p_load(xgboost, nnls)

# Instalamos la versión más reciente de sl3 de GitHub. 
if (!require(sl3)) {
  remotes::install_github("tlverse/sl3")
  library(sl3)
  library(origami) # Validación cruzada diseñada para sl3.
}
```

En resumen, se requiere (lista no exhaustiva):

- **Infraestructura general**: `pacman`, `rio`, `readxl`, `writexl`, `tidyverse`, `caret`, `tidymodels`, `skimr`
- **Modelos ML**: `xgboost`, `nnls`, `leaps`, `sl3`, `origami`, `keras`
- **Espacial**: `sf`, `lwgeom`, `osmdata`, `leaflet`, `spatialsample`
- **Tablas y visualización**: `gt`, `gtsummary`, `gridExtra`, `plotly`
- **Integración con Python**: `reticulate` (para usar `keras` y/o apoyar el análisis de texto)

Además, para algunos modelos y el notebook de texto puede ser necesario:

- **Python instalado** (para `keras` y/o ejecución directa de `text_analysis.ipynb`).
- Jupyter Notebook o VSCode/RStudio con soporte para `.ipynb`.

---

## 6. Cómo replicar los resultados

1. **Clonar el repositorio**  
   ```bash
   git clone <URL_DEL_REPO>
   cd <nombre_del_repo>
   ```

2. **Preparar el entorno en R**  
   - Abrir el proyecto en RStudio (o fijar el directorio de trabajo en la carpeta raíz del repo).
   - Verificar que la versión de R sea ≥ 4.3.
   - Ejecutar en la consola:
     ```r
     source("scripts/taller_3.R")
     ```
     (o abrir el script y correrlo por secciones).

3. **Ubicar los datos**  
   - Colocar los archivos raw de Properati en la carpeta `stores/` (y subcarpetas si el script así lo requiere).
   - Verificar en `taller_3.R` los nombres exactos de los archivos esperados.

4. **Generar bases procesadas y variables adicionales**  
   - `taller_3.R`:
     - Lee los datos.
     - Limpia y transforma variables.
     - Calcula las variables espaciales usando `osmdata` y `sf`.
     - Integra variables textuales (provenientes del notebook o de archivos intermedios en `stores/`).

5. **Ejecutar el notebook de texto** (opcional pero recomendado si quieres reconstruir todo desde cero)  
   - Abrir `scripts/text_analysis.ipynb` en Jupyter o VSCode.
   - Ejecutar todas las celdas para generar las variables textuales y guardar el resultado en `stores/`.

6. **Explorar modelos adicionales**  
   - Correr `scripts/modelospt2.R` para:
     - Evaluar más especificaciones.
     - Comparar algoritmos y métricas.
     - Documentar la selección del mejor modelo.

7. **Revisar resultados**  
   - Explorar la carpeta `views/`:
     - Submissions (`.csv`) listos para Kaggle.
     - Figuras, mapas y tablas generadas en el análisis.

---

## 7. Notas finales

- Este repositorio está diseñado para ser **totalmente reproducible**, sujeto a:
  - Disponibilidad de los datos originales.
  - Instalación correcta de las librerías y versiones de R/Python.
- El trabajo se alinea con los requerimientos del problema, incluyendo:
  - Incorporación de variables externas (OSM) y textuales.
  - Uso de múltiples familias de algoritmos (lineales, árboles, boosting, redes, superlearner).
  - Generación de múltiples submissions en Kaggle y análisis comparativo de desempeño.

Puedes adaptar este README agregando:

- Nombre del equipo y de los/las integrantes.
- Puntaje final obtenido en Kaggle.
- Cualquier comentario adicional sobre limitaciones o posibles extensiones del proyecto.
