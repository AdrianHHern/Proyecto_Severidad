# Proyecto_Severidad
Ajustar modelos de seguridad por tipo de evento para determinar la pérdida esperada en caso de un evento futuro
## Objetivo
Ajustar modelos de severidad por tipo de evento para estimar la pérdida esperada ante la ocurrencia de un evento futuro.
--
# Para efectos de este proyecto, se considerarán únicamente eventos **hidrometeorológicos**.

## Metodología

### 1. Exploración y limpieza de datos
- Realizar un análisis preliminar de la base de datos.
- Aplicar procesos de limpieza, ajustes y modificaciones según sea necesario.

### 2. Análisis descriptivo
- Generar estadísticas descriptivas para comprender el comportamiento de los datos.

### 3. Ajuste por inflación
- Actualizar los montos de pérdida considerando la inflación anual.
- Expresar todos los valores en términos del año **2024**.

### 4. Transformación y preparación de datos
- Agrupar los datos según criterios relevantes.
- Aplicar técnicas de suavizamiento en caso necesario.
- Se recomienda aplicar una **transformación logarítmica** a las pérdidas actualizadas.

### 5. Modelado de severidad
- Estimar los parámetros de distintas distribuciones de probabilidad.
- Evaluar la posibilidad de segmentar los datos para generar grupos homogéneos.
- Ajustar uno o varios modelos según los hallazgos.

### 6. Validación de modelos
- Aplicar pruebas de bondad de ajuste para validar la adecuación de los modelos.

### 7. Estimación final
- Determinar el **costo promedio por evento** para un nuevo siniestro proyectado al año **2025**.

---

##  Notas
- La selección del modelo dependerá del comportamiento observado en los datos.
- La segmentación puede mejorar la precisión de las estimaciones.
