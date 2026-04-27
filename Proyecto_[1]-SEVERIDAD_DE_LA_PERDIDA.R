# =========================================================
# PROYECTO 1 - SEVERIDAD DE LA PÉRDIDA
# =========================================================

# Limpieza y construcción de la base para severidad
# ---------------------------------------------------------
# 0. Librerías
# ---------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(moments) 
library(actuar)
library(ggplot2)
library(fitdistrplus) 
library(goftest)# BOnDAD DE AJUSTE 
library(rriskDistributions)
# ---------------------------------------------------------
# 1. Cargar la base desde Excel
# ---------------------------------------------------------
data_raw <- read_excel("C:/Users/Sofia/Downloads/Basehistorica_2000_a_2024 (1).xlsx")

# Hacemos una copia de trabajo para no modificar la base original
data <- data_raw

# ---------------------------------------------------------
# 2. Filtrar solo eventos hidrometeorológicos
# ---------------------------------------------------------
# El proyecto pide trabajar únicamente con eventos hidrometeorológicos.
data <- data %>%
  filter(`Clasificación del fenómeno` == "Hidrometeorológico")


# ---------------------------------------------------------
# 3. Limpieza inicial de texto
# ---------------------------------------------------------
# Quitamos espacios al inicio y final en columnas clave.
data <- data %>%
  mutate(
    Estado = str_trim(Estado),
    `Tipo de fenómeno` = str_trim(`Tipo de fenómeno`)
  )


# ---------------------------------------------------------
# 4. Homologar el tipo de fenómeno
# ---------------------------------------------------------
# Aquí corregimos etiquetas inconsistentes o ambiguas:
# - "Lluvia" y "Lluvias" se unifican en "Lluvia"
# - "Sin clasificación" se reclasifica como "Inundación"
#   (porque en la revisión manual el caso correspondía a agua)
# - "Otros" se marca como "Eliminar"
# - "Bajas Temperaturas" se unifica con "Bajas temperaturas"
data <- data %>%
  mutate(
    Tipo_fenomeno_limpio = case_when(
      `Tipo de fenómeno` %in% c("Lluvia", "Lluvias") ~ "Lluvia",
      `Tipo de fenómeno` == "Sin clasificación" ~ "Inundación",
      `Tipo de fenómeno` == "Otros" ~ "Eliminar",
      `Tipo de fenómeno` == "Bajas Temperaturas" ~ "Bajas temperaturas",
      TRUE ~ `Tipo de fenómeno`
    )
  )


# ---------------------------------------------------------
# 5. Crear grupo de fenómeno
# ---------------------------------------------------------
# Se forman dos bolsas homogéneas principales:
# - Agua
# - Temperatura
#
# También se incorpora "Temperatura extrema" a la bolsa de Temperatura.
data <- data %>%
  mutate(
    Grupo_fenomeno = case_when(
      Tipo_fenomeno_limpio %in% c(
        "Inundación", "Lluvia", "Mar de fondo", "Marea de tormenta",
        "Ciclón tropical", "Tormenta tropical", "Tormenta severa",
        "Tormenta eléctrica", "Tornado", "Fuertes vientos", "Huracán"
      ) ~ "Agua",
      
      Tipo_fenomeno_limpio %in% c(
        "Helada", "Nevada", "Bajas temperaturas", "Granizada",
        "Sequía", "Temperatura extrema"
      ) ~ "Temperatura",
      
      TRUE ~ "Revisar"
    )
  )


# ---------------------------------------------------------
# 6. Corregir nombres de estado
# ---------------------------------------------------------
# Durante la revisión manual se observó que "México" se refería
# a "Estado de México", ya que los municipios asociados eran:
# Chalco, Ixtapaluca, Tlalnepantla, Valle de Chalco, etc.
data <- data %>%
  mutate(
    Estado = if_else(Estado == "México", "Estado de México", Estado)
  )


# ---------------------------------------------------------
# 7. Crear grupo geográfico de estado
# ---------------------------------------------------------
# Se clasifican los estados en:
# - Norte con costa
# - Norte sin costa
# - Sur con costa
# - Sur sin costa
#
# Esta clasificación se usó como segundo driver para construir
# portafolios más homogéneos.
data <- data %>%
  mutate(
    Grupo_estado = case_when(
      Estado %in% c("Baja California", "Baja California Sur", "Sonora",
                    "Sinaloa", "Tamaulipas") ~ "Norte con costa",
      
      Estado %in% c("Chihuahua", "Coahuila", "Nuevo León", "Durango",
                    "Zacatecas", "San Luis Potosí") ~ "Norte sin costa",
      
      Estado %in% c("Nayarit", "Jalisco", "Colima", "Michoacán",
                    "Guerrero", "Oaxaca", "Chiapas", "Veracruz",
                    "Tabasco", "Campeche", "Yucatán", "Quintana Roo") ~ "Sur con costa",
      
      Estado %in% c("Aguascalientes", "Guanajuato", "Querétaro", "Hidalgo",
                    "Estado de México", "Ciudad de México", "Morelos",
                    "Puebla", "Tlaxcala") ~ "Sur sin costa",
      
      TRUE ~ "Revisar"
    )
  )


# ---------------------------------------------------------
# 8. Crear portafolio final
# ---------------------------------------------------------
# Se combina el grupo de fenómeno con el grupo geográfico.
data <- data %>%
  mutate(
    Portafolio_final = paste(Grupo_fenomeno, Grupo_estado, sep = " - ")
  )


# ---------------------------------------------------------
# 9. Eliminar registros marcados como "Otros"
# ---------------------------------------------------------
# Estos registros no podían reclasificarse de forma confiable.
data <- data %>%
  filter(Tipo_fenomeno_limpio != "Eliminar")


# ---------------------------------------------------------
# 10. Revisar registros pendientes
# ---------------------------------------------------------
# Aquí se inspeccionan los casos que quedaron en "Revisar"
# para decidir si podían corregirse o si debían eliminarse.
revisar_df <- data %>%
  filter(Grupo_fenomeno == "Revisar" | Grupo_estado == "Revisar")

#View(revisar_df)


# ---------------------------------------------------------
# 11. Resolver fechas
# ---------------------------------------------------------
# Las fechas venían en formato mixto:
# - algunas como serial de Excel (ej. 36677)
# - otras como texto
# - algunas faltantes
#
# Primero convertimos los seriales de Excel a Date.
data <- data %>%
  mutate(
    fecha_inicio_corregida = as.Date(as.numeric(`Fecha de Inicio`), origin = "1899-12-30"),
    fecha_fin_corregida    = as.Date(as.numeric(`Fecha de Fin`), origin = "1899-12-30")
  )

# Después, si falta una fecha, se reemplaza con la otra:
# - si falta inicio, usar fin
# - si falta fin, usar inicio
data <- data %>%
  mutate(
    fecha_inicio_final = if_else(
      is.na(fecha_inicio_corregida),
      fecha_fin_corregida,
      fecha_inicio_corregida
    ),
    fecha_fin_final = if_else(
      is.na(fecha_fin_corregida),
      fecha_inicio_corregida,
      fecha_fin_corregida
    )
  )

# Finalmente, se elimina el único registro cuya fecha era
# irrecuperable (ej. "29-Sep" sin año).
data <- data %>%
  filter(!is.na(fecha_inicio_final), !is.na(fecha_fin_final))


# ---------------------------------------------------------
# 12. Eliminar registros multiestado o geográficamente ambiguos
# ---------------------------------------------------------
# Se detectaron registros como:
# - "Varios Estados"
# - "Tamaulipas, Veracruz"
# - "Veracruz y Tamaulipas"
# - listas largas de varios estados
#
# Esos registros no podían asignarse de manera unívoca
# a una sola región geográfica, por lo que se eliminan
# para conservar la homogeneidad del portafolio.
data <- data %>%
  filter(Grupo_estado != "Revisar")


# ---------------------------------------------------------
# 13. Confirmar la clasificación final
# ---------------------------------------------------------
table(data$Grupo_estado)
table(data$Portafolio_final)


# ---------------------------------------------------------
# 14. Revisar la variable de pérdida
# ---------------------------------------------------------
# Se inspecciona la variable:
# "Total de daños (millones de pesos)"
summary(data$`Total de daños (millones de pesos)`)
sum(is.na(data$`Total de daños (millones de pesos)`))
sum(data$`Total de daños (millones de pesos)` == 0, na.rm = TRUE)
sum(data$`Total de daños (millones de pesos)` < 0, na.rm = TRUE)

# También se revisa por portafolio
data %>%
  group_by(Portafolio_final) %>%
  summarise(
    n_total = n(),
    n_na = sum(is.na(`Total de daños (millones de pesos)`)),
    n_ceros = sum(`Total de daños (millones de pesos)` == 0, na.rm = TRUE),
    n_negativos = sum(`Total de daños (millones de pesos)` < 0, na.rm = TRUE),
    n_positivos = sum(`Total de daños (millones de pesos)` > 0, na.rm = TRUE)
  )


# ---------------------------------------------------------
# 15. Crear el dataframe de severidad
# ---------------------------------------------------------
# Para modelar severidad se usan solo pérdidas positivas.
data_severidad <- data %>%
  filter(
    !is.na(`Total de daños (millones de pesos)`),
    `Total de daños (millones de pesos)` > 0
  )

#View(data_severidad)

# Conteo por portafolio en la base de severidad
data_severidad %>%
  count(Portafolio_final)


# Seleccionar únicamente las columnas de interés
datos_severidad <-  data_severidad%>%
  dplyr::select(
    fecha_inicio_final,fecha_fin_final,Año,Tipo_fenomeno_limpio,
    Estado,`Clasificación del fenómeno`,
    Grupo_fenomeno,Grupo_estado,Portafolio_final,
    `Total de daños (millones de pesos)`
  )



# -----------------------------
# 16.Ajuste por inflación a 2024
# -----------------------------
inflacion <- read_excel(file.choose())
inflacion <- inflacion %>%
  arrange(desc(Año)) %>%
  mutate(
    factor = 1 + (Tasa / 100),              # Convertir tasa a factor
    factor_acumulado = cumprod(factor),     # Inflación compuesta
    factor_acumulado = factor_acumulado / first(factor_acumulado) # Base 2024 = 1
  )

# -----------------------------
# Unión y ajuste de daños
# -----------------------------

datos_severidad <- datos_severidad %>%
  left_join(inflacion %>% dplyr::select(Año, factor_acumulado), by = "Año") %>%
  mutate(
    `daños_2024(pesos)` = `Total de daños (millones de pesos)` * factor_acumulado, # Ajuste
    `daños_2024(pesos)` = `daños_2024(pesos)` * 1e6,  # Convertir a pesos
    log_daños = log(`daños_2024(pesos)`)     # Transformación logarítmica
  )

# Analisis Descriptivo del portafolio -------------------------------------

datos_severidad %>%
  summarise(
    media_log = mean(log_daños, na.rm = TRUE),     # Media de los daños en log
    mediana_log = median(log_daños, TRUE), # Mediana
    sd_log = sd(log_daños,TRUE),          # Desviación estándar
    min_log = min(log_daños, TRUE),        # Valor mínimo
    max_log = max(log_daños, TRUE)         # Valor máximo
  )


# -----------------------------
# Otras Estadísticos descriptivos básicos
# -----------------------------
p <- datos_severidad$log_daños  
max(p, na.rm = TRUE) - min(p, na.rm = TRUE)  # Rango de la variable

quantile(p, c(0.25, 0.5, 0.75), na.rm = TRUE)  # Cuartiles (Q1, mediana, Q3)

var(p, na.rm = TRUE)     # Varianza
sd(p, na.rm = TRUE)      # Desviación estándar (volatilidad de pérdidas)

# Intervalo simple: media ± desviación estándar
cat(mean(p, na.rm = TRUE) - sd(p, na.rm = TRUE),
    mean(p, na.rm = TRUE) + sd(p, na.rm = TRUE))

sd(p, na.rm = TRUE) / mean(p, na.rm = TRUE)  
# Coeficiente de variación: mide la volatilidad relativa


skewness(p, na.rm = TRUE)  
# Coeficiente de asimetría:
# ≈ 0 → simétrica (normal)
# > 0 → sesgo a la derecha
# < 0 → sesgo a la izquierda

kurtosis(p, na.rm = TRUE)  
# Curtosis:
# ≈ 3 → normal
# > 3 → colas pesadas (más valores extremos)
# < 3 → distribución más plana

# Frecuencia por tipo de fenómeno
table(datos_severidad$Tipo_fenomeno_limpio)

# Frecuencia por estado
table(datos_severidad$Estado)

#Daños promedio por grupo
datos_severidad %>%
  group_by(Grupo_fenomeno) %>%
  summarise(
    promedio_log = mean(log_daños, na.rm = TRUE),  # Promedio en escala log
    n = n()                                         # Número de observaciones
  )

# -----------------------------
# Análisis gráfico
# -----------------------------

hist(p, breaks = 30, main = "Histograma de perdidas", xlab = "Perdidas")
# Distribución de la variable (frecuencia)

boxplot(p, main = "Boxplot de Perdida")
# Detecta valores atípicos (outliers) y dispersión

plot(ecdf(p), main = "Función de distribución empírica")
# Muestra la acumulación de probabilidades
# La pendiente indica concentración de datos


# ANALISIS DE LOS FENOMENOS Y ESTIMACIONES DE DISTRIBUCIONES DE PROBABILIDAD
# -------------------------------------------------------------------------
# /// Segmentamos por tipo de fenómeno (AGUA) y luego por grupo de estado \\\


# Filtrar solo fenómenos de Agua
datos_agua <- datos_severidad %>%
  filter(Grupo_fenomeno == "Agua")

hist(datos_agua$log_daños, breaks = 30,
     main = "Log daños - Agua",
     xlab = "Log daños")

ggplot(datos_agua, aes(x = log_daños)) +
  geom_histogram(bins = 25) +
  facet_wrap(~Grupo_estado) +
  theme_minimal() +
  labs(title = "Distribución de log daños por grupo de estado (Agua)")

# Conteo por grupo de estado
datos_agua %>%
  count(Grupo_estado)

# Conteo por portafolio
datos_agua %>%
  count(Portafolio_final)

# MODELO GLOBAL AGUA
# -------------------------------------------------------------------------

summary(datos_agua$log_daños)

hist(datos_agua$log_daños,
     breaks = 30,
     main = "Log daños - Agua",
     xlab = "Perdidas")

# MODELOS
# Normal
fit_normal_global <- fitdist(
  datos_agua$log_daños,
  "norm"
)

# Logística
fit_logistica_global <- fitdist(
  datos_agua$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal_global, fit_logistica_global))

# modelos alternativos
fit.cont(datos_agua$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal_global, fit_logistica_global),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal_global, fit_logistica_global),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal_global, fit_logistica_global),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal_global, fit_logistica_global),
       legendtext = c("Normal", "Logística"))

# // Filtrar solo fenómenos de Agua Sur sin costa \\

datos_agua_sur_sc <- datos_agua %>%
  filter(
    Portafolio_final == "Agua - Sur sin costa"
  )

summary(datos_agua_sur_sc$log_daños)

hist(datos_agua_sur_sc$log_daños,
     breaks = 30,
     main = "Log daños - Agua Sur sin Costa",
     xlab = "Perdidas")

#MODELOS
# Normal
fit_normal <- fitdist(
  datos_agua_sur_sc$log_daños,
  "norm"
)

# Logística
fit_logistica <- fitdist(
  datos_agua_sur_sc$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal, fit_logistica))

#modelos alternativos
fit.cont(datos_agua_sur_sc$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal, fit_logistica),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal, fit_logistica),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
## SE AJUSTA UNA DIST LOGISTICA?


# // Filtrar solo fenómenos de Agua Sur con costa \\

datos_agua_sur_cc <- datos_agua %>%
  filter(
    Portafolio_final == "Agua - Sur con costa"
  )

summary(datos_agua_sur_cc$log_daños)

hist(datos_agua_sur_cc$log_daños,
     breaks = 30,
     main = "Log daños - Agua Sur con Costa",
     xlab = "Perdidas")

# MODELOS
# Normal
fit_normal <- fitdist(
  datos_agua_sur_cc$log_daños,
  "norm"
)

# Logística
fit_logistica <- fitdist(
  datos_agua_sur_cc$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal, fit_logistica))

#modelos alternativos
fit.cont(datos_agua_sur_cc$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal, fit_logistica),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal, fit_logistica),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
#AUN NO ES CLARO??


# // Filtrar solo fenómenos de Agua Norte sin costa \\
datos_agua_norte_sc <- datos_agua %>%
  filter(
    Portafolio_final == "Agua - Norte sin costa"
  )

summary(datos_agua_norte_sc$log_daños)

hist(datos_agua_norte_sc$log_daños,
     breaks = 30,
     main = "Log daños - Agua Norte sin Costa",
     xlab = "Perdidas")

# MODELOS
# Normal
fit_normal <- fitdist(
  datos_agua_norte_sc$log_daños,
  "norm"
)

# Logística
fit_logistica <- fitdist(
  datos_agua_norte_sc$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal, fit_logistica))

#modelos alternativos
fit.cont(datos_agua_norte_sc$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal, fit_logistica),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal, fit_logistica),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
# SE AJUSTA UNA UNIFORME??


# // Filtrar solo fenómenos de Agua Norte con costa \\

datos_agua_norte_cc <- datos_agua %>%
  filter(
    Portafolio_final == "Agua - Norte con costa"
  )

summary(datos_agua_norte_cc$log_daños)

hist(datos_agua_norte_cc$log_daños,
     breaks = 30,
     main = "Log daños - Agua Norte con Costa",
     xlab = "Perdidas")

# MODELOS
# Normal
fit_normal <- fitdist(
  datos_agua_norte_cc$log_daños,
  "norm"
)

# Logística
fit_logistica <- fitdist(
  datos_agua_norte_cc$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal, fit_logistica))

#modelos alternativos
fit.cont(datos_agua_norte_cc$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal, fit_logistica),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal, fit_logistica),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
# SE AJUSTA UNA UNIFORME?

# -------------------------------------------------------------------------
# /// segmentamos por tipo de fenómeno (AGUA) y region del Golfo de México y del Pacifico \\\

# / Estados del Golfo de México \
estados_golfo <- c(
  "Tamaulipas", "Veracruz", "Tabasco", "Campeche", "Yucatán", "Quintana Roo"
)

# / Estados del Pacífico \
estados_pacifico <- c(
  "Baja California", "Baja California Sur", "Sonora", "Sinaloa",
  "Nayarit", "Jalisco", "Colima", "Michoacán", "Guerrero", "Oaxaca", "Chiapas"
)

# // Filtrar solo fenómenos de Agua  Golfo de México \\

datos_agua_golfo <- datos_agua %>%
  filter(Estado %in% estados_golfo)

hist(datos_agua_golfo$log_daños, breaks = 30,
     main = "Log daños - Agua Golfo de México",
     xlab = "Log daños")

summary(datos_agua_golfo$log_daños)

# MODELOS
# Normal
fit_normal <- fitdist(
  datos_agua_golfo$log_daños,
  "norm"
)

# Logística
fit_logistica <- fitdist(
  datos_agua_golfo$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal, fit_logistica))

#modelos alternativos
fit.cont(datos_agua_golfo$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal, fit_logistica),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal, fit_logistica),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
#se ajusta una Normal?



# // Filtrar solo fenómenos de Agua Pacifico \\

datos_agua_pacifico <- datos_agua %>%
  filter(Estado %in% estados_pacifico)

hist(datos_agua_pacifico$log_daños, breaks = 30,
     main = "Log daños - Agua pacifico de México",
     xlab = "Log daños")

summary(datos_agua_pacifico$log_daños)

# MODELOS
# Normal
fit_normal <- fitdist(
  datos_agua_pacifico$log_daños,
  "norm"
)

# Logística
fit_logistica <- fitdist(
  datos_agua_pacifico$log_daños,
  "logis"
)

# Comparación
gofstat(list(fit_normal, fit_logistica))

#modelos alternativos
fit.cont(datos_agua_pacifico$log_daños)

# Gráficas de ajuste
denscomp(list(fit_normal, fit_logistica),
         legendtext = c("Normal", "Logística"))
qqcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))
cdfcomp(list(fit_normal, fit_logistica),
        legendtext = c("Normal", "Logística"))
ppcomp(list(fit_normal, fit_logistica),
       legendtext = c("Normal", "Logística"))

# Se ajusta a una lognormal o loglogistica

# -------------------------------------------------------------------------
# Parámetros estimados y elección de segmentación final
# -------------------------------------------------------------------------

# Parámetros estimados para AGUA global
parametros_global <- data.frame(
  Segmentacion = c("AGUA global", "AGUA global"),
  Modelo = c("Normal", "Logística"),
  Parametro_1 = c("mean", "location"),
  Valor_1 = c(fit_normal_global$estimate["mean"],
              fit_logistica_global$estimate["location"]),
  Parametro_2 = c("sd", "scale"),
  Valor_2 = c(fit_normal_global$estimate["sd"],
              fit_logistica_global$estimate["scale"]),
  AIC = c(fit_normal_global$aic, fit_logistica_global$aic),
  BIC = c(fit_normal_global$bic, fit_logistica_global$bic),
  n = c(length(datos_agua$log_daños), length(datos_agua$log_daños))
)

parametros_global


# Resultados por portafolio final
portafolios_agua <- unique(datos_agua$Portafolio_final)

resultados_portafolio <- data.frame()

for (p in portafolios_agua) {
  
  datos_temp <- datos_agua %>%
    filter(Portafolio_final == p)
  
  x <- datos_temp$log_daños
  
  fit_normal <- fitdist(
    x,
    "norm"
  )
  
  fit_logistica <- fitdist(
    x,
    "logis"
  )
  
  resultados_portafolio <- rbind(
    resultados_portafolio,
    data.frame(
      Segmentacion = p,
      Modelo = "Normal",
      Parametro_1 = "mean",
      Valor_1 = fit_normal$estimate["mean"],
      Parametro_2 = "sd",
      Valor_2 = fit_normal$estimate["sd"],
      AIC = fit_normal$aic,
      BIC = fit_normal$bic,
      n = length(x)
    ),
    data.frame(
      Segmentacion = p,
      Modelo = "Logística",
      Parametro_1 = "location",
      Valor_1 = fit_logistica$estimate["location"],
      Parametro_2 = "scale",
      Valor_2 = fit_logistica$estimate["scale"],
      AIC = fit_logistica$aic,
      BIC = fit_logistica$bic,
      n = length(x)
    )
  )
}

resultados_portafolio


# Mejor modelo por portafolio según AIC
mejor_aic_portafolio <- resultados_portafolio %>%
  group_by(Segmentacion) %>%
  slice_min(order_by = AIC, n = 1, with_ties = FALSE)

mejor_aic_portafolio


# Mejor modelo por portafolio según BIC
mejor_bic_portafolio <- resultados_portafolio %>%
  group_by(Segmentacion) %>%
  slice_min(order_by = BIC, n = 1, with_ties = FALSE)

mejor_bic_portafolio

# Conteo por portafolio para decidir si la segmentación es viable
conteo_portafolio <- datos_agua %>%
  count(Portafolio_final) %>%
  mutate(
    criterio_muestra = if_else(n >= 30, "Suficiente", "Poca muestra")
  )

conteo_portafolio


# Resumen global para comparar con portafolios
resumen_global <- data.frame(
  Segmentacion = "AGUA global",
  Modelo = c("Normal", "Logística"),
  AIC = c(fit_normal_global$aic, fit_logistica_global$aic),
  BIC = c(fit_normal_global$bic, fit_logistica_global$bic),
  n = nrow(datos_agua)
)

resumen_global

# Conclusión según los resultados
mejor_global_aic <- resumen_global %>%
  slice_min(order_by = AIC, n = 1, with_ties = FALSE)

mejor_global_bic <- resumen_global %>%
  slice_min(order_by = BIC, n = 1, with_ties = FALSE)

n_portafolios_total <- nrow(conteo_portafolio)
n_portafolios_suficientes <- sum(conteo_portafolio$criterio_muestra == "Suficiente")

if (n_portafolios_suficientes == n_portafolios_total) { 
  cat("Conclusion final:\n\n")  
  cat("Para el grupo de fenomenos AGUA se comparo el modelo global con la segmentacion por Portafolio_final.\n")
  cat("Al revisar los conteos, se observo que todos los portafolios tienen un tamaño de muestra suficiente, por lo que esta segmentacion se puede considerar adecuada.\n")
  cat("Ademas, se estimaron los parametros de las distribuciones Normal y Logistica sobre la variable log_daños en cada portafolio.\n")
  cat("Con apoyo de los criterios AIC y BIC, se identifico el modelo que presenta el mejor ajuste en cada subgrupo.\n")
  cat("Por lo tanto, se conserva la segmentacion por Portafolio_final para el analisis final del conjunto de AGUA.\n\n")
  
  cat("Mejor modelo de AGUA global segun AIC:", mejor_global_aic$Modelo, "\n")
  cat("Mejor modelo de AGUA global segun BIC:", mejor_global_bic$Modelo, "\n\n")
  
  cat("Modelo ganador por portafolio segun AIC:\n")
  print(mejor_aic_portafolio)
  
  cat("\nModelo ganador por portafolio segun BIC:\n")
  print(mejor_bic_portafolio)
}

# -------------------------------------------------------------------------
# Determinar el costo promedio por evento para un nuevo siniestro en 2025
# -------------------------------------------------------------------------

# Criterio para elegir el modelo final
criterio_final <- "AIC"

# Inflación para 2025 tomada de la tabla
inflacion_2025 <- inflacion %>%
  mutate(Año_chr = as.character(Año)) %>%
  filter(Año_chr == "2025") %>%
  summarise(tasa = dplyr::first(Tasa)) %>%
  pull(tasa)

factor_2025 <- 1 + inflacion_2025 / 100


# Función para obtener el costo promedio en la escala original
costo_promedio_evento <- function(modelo, valor_1, valor_2) {
  
  if (modelo == "Normal") {
    return(exp(valor_1 + (valor_2^2) / 2))
  }
  
  if (modelo == "Logística") {
    if (valor_2 >= 1) {
      return(NA_real_)
    } else {
      return(exp(valor_1) * (pi * valor_2) / sin(pi * valor_2))
    }
  }
  
  return(NA_real_)
}


# Si todos los portafolios tienen muestra suficiente, se usa Portafolio_final
if (all(conteo_portafolio$criterio_muestra == "Suficiente")) {
  
  if (criterio_final == "AIC") {
    modelos_finales <- mejor_aic_portafolio
  } else {
    modelos_finales <- mejor_bic_portafolio
  }
  
  costos_portafolio <- modelos_finales %>%
    mutate(
      costo_promedio_2024 = mapply(costo_promedio_evento, Modelo, Valor_1, Valor_2),
      probabilidad = n / sum(n),
      costo_promedio_2025 = costo_promedio_2024 * factor_2025
    )
  
  costos_portafolio
  
  costo_promedio_final_2024 <- sum(costos_portafolio$probabilidad * costos_portafolio$costo_promedio_2024, na.rm = TRUE)
  costo_promedio_final_2025 <- sum(costos_portafolio$probabilidad * costos_portafolio$costo_promedio_2025, na.rm = TRUE)
  
  cat("Inflación 2025 usada:", inflacion_2025, "%\n")
  cat("Costo promedio por evento en pesos de 2024:", costo_promedio_final_2024, "\n")
  cat("Costo promedio por evento para 2025:", costo_promedio_final_2025, "\n")
  cat("Costo promedio por evento en millones de pesos para 2025:", costo_promedio_final_2025 / 1e6, "\n")
}

mean(datos_severidad$`daños_2024(pesos)`, na.rm = TRUE)
median(datos_severidad$`daños_2024(pesos)`, na.rm = TRUE)

quantile(datos_severidad$`daños_2024(pesos)`, probs = c(0.75, 0.90, 0.95, 0.99), na.rm = TRUE)


datos_severidad %>%
  arrange(desc(`daños_2024(pesos)`)) %>%
  dplyr::select(Año, Estado, Tipo_fenomeno_limpio, Portafolio_final, `daños_2024(pesos)`) %>%
  head(10)



# =========================================================
# ANALISIS DE TEMPERATURA
# =========================================================

# ---------------------------------------------------------
# 1. Crear subconjunto de Temperatura
# ---------------------------------------------------------
datos_temperatura <- datos_severidad %>%
  filter(Grupo_fenomeno == "Temperatura")


# ---------------------------------------------------------
# 2. Revisión descriptiva básica de Temperatura
# ---------------------------------------------------------
nrow(datos_temperatura)

datos_temperatura %>% count(Grupo_estado)

datos_temperatura %>% count(Portafolio_final)

summary(datos_temperatura$log_daños)

hist(datos_temperatura$log_daños,
     breaks = 30,
     main = "Log daños - Temperatura",
     xlab = "Log daños")


# ---------------------------------------------------------
# 3. Ajuste global de modelos para Temperatura
# ---------------------------------------------------------
fit_normal_temp_global <- fitdist(
  datos_temperatura$log_daños,
  "norm"
)

fit_logistica_temp_global <- fitdist(
  datos_temperatura$log_daños,
  "logis"
)

gofstat(list(fit_normal_temp_global, fit_logistica_temp_global))


# ---------------------------------------------------------
# 4. Parámetros estimados a nivel global para Temperatura
# ---------------------------------------------------------
parametros_temp_global <- data.frame(
  Segmentacion = c("TEMPERATURA global", "TEMPERATURA global"),
  Modelo = c("Normal", "Logística"),
  Parametro_1 = c("mean", "location"),
  Valor_1 = c(fit_normal_temp_global$estimate["mean"],
              fit_logistica_temp_global$estimate["location"]),
  Parametro_2 = c("sd", "scale"),
  Valor_2 = c(fit_normal_temp_global$estimate["sd"],
              fit_logistica_temp_global$estimate["scale"]),
  AIC = c(fit_normal_temp_global$aic, fit_logistica_temp_global$aic),
  BIC = c(fit_normal_temp_global$bic, fit_logistica_temp_global$bic),
  n = c(length(datos_temperatura$log_daños),
        length(datos_temperatura$log_daños))
)

parametros_temp_global


# ---------------------------------------------------------
# 5. Ajuste por portafolio para Temperatura
# ---------------------------------------------------------
portafolios_temperatura <- unique(datos_temperatura$Portafolio_final)

resultados_temp_portafolio <- data.frame()

for (p in portafolios_temperatura) {
  
  datos_temp <- datos_temperatura %>%
    filter(Portafolio_final == p)
  
  x <- datos_temp$log_daños
  
  fit_normal <- fitdist(x, "norm")
  fit_logistica <- fitdist(x, "logis")
  
  resultados_temp_portafolio <- rbind(
    resultados_temp_portafolio,
    data.frame(
      Segmentacion = p,
      Modelo = "Normal",
      Parametro_1 = "mean",
      Valor_1 = fit_normal$estimate["mean"],
      Parametro_2 = "sd",
      Valor_2 = fit_normal$estimate["sd"],
      AIC = fit_normal$aic,
      BIC = fit_normal$bic,
      n = length(x)
    ),
    data.frame(
      Segmentacion = p,
      Modelo = "Logística",
      Parametro_1 = "location",
      Valor_1 = fit_logistica$estimate["location"],
      Parametro_2 = "scale",
      Valor_2 = fit_logistica$estimate["scale"],
      AIC = fit_logistica$aic,
      BIC = fit_logistica$bic,
      n = length(x)
    )
  )
}

resultados_temp_portafolio


# ---------------------------------------------------------
# 6. Mejor modelo por portafolio según AIC y BIC
# ---------------------------------------------------------
mejor_aic_temp_portafolio <- resultados_temp_portafolio %>%
  group_by(Segmentacion) %>%
  slice_min(order_by = AIC, n = 1, with_ties = FALSE)

mejor_bic_temp_portafolio <- resultados_temp_portafolio %>%
  group_by(Segmentacion) %>%
  slice_min(order_by = BIC, n = 1, with_ties = FALSE)

mejor_aic_temp_portafolio
mejor_bic_temp_portafolio


# ---------------------------------------------------------
# 7. Verificar suficiencia muestral de la segmentación
# ---------------------------------------------------------
conteo_temp_portafolio <- datos_temperatura %>%
  count(Portafolio_final) %>%
  mutate(
    criterio_muestra = if_else(n >= 30, "Suficiente", "Poca muestra")
  )

conteo_temp_portafolio


# ---------------------------------------------------------
# 8. Función para calcular costo promedio en escala original
# ---------------------------------------------------------
costo_promedio_evento <- function(modelo, valor_1, valor_2) {
  
  if (modelo == "Normal") {
    return(exp(valor_1 + (valor_2^2) / 2))
  }
  
  if (modelo == "Logística") {
    if (valor_2 >= 1) {
      return(NA_real_)
    } else {
      return(exp(valor_1) * (pi * valor_2) / sin(pi * valor_2))
    }
  }
  
  return(NA_real_)
}


# ---------------------------------------------------------
# 9. Selección final de modelos para Temperatura
# ---------------------------------------------------------
criterio_final_temp <- "AIC"

if (criterio_final_temp == "AIC") {
  modelos_temp_finales <- mejor_aic_temp_portafolio
} else {
  modelos_temp_finales <- mejor_bic_temp_portafolio
}

# quitar agrupamiento para que probabilidad se calcule bien
modelos_temp_finales <- modelos_temp_finales %>%
  ungroup()

modelos_temp_finales


# ---------------------------------------------------------
# 10. Ajuste práctico para modelos no válidos en costo esperado
# ---------------------------------------------------------
# Si el mejor modelo es Logística pero scale >= 1, se reemplaza por Normal
# del mismo portafolio para poder calcular un valor esperado finito.

normales_temp <- resultados_temp_portafolio %>%
  filter(Modelo == "Normal") %>%
  dplyr::select(Segmentacion, Modelo_normal = Modelo,
                Parametro_1_normal = Parametro_1,
                Valor_1_normal = Valor_1,
                Parametro_2_normal = Parametro_2,
                Valor_2_normal = Valor_2,
                AIC_normal = AIC,
                BIC_normal = BIC)

modelos_temp_ajustados <- modelos_temp_finales %>%
  left_join(normales_temp, by = "Segmentacion") %>%
  mutate(
    Modelo = if_else(Modelo == "Logística" & Valor_2 >= 1, Modelo_normal, Modelo),
    Parametro_1 = if_else(Modelo == "Normal", Parametro_1_normal, Parametro_1),
    Valor_1 = if_else(Modelo == "Normal", Valor_1_normal, Valor_1),
    Parametro_2 = if_else(Modelo == "Normal", Parametro_2_normal, Parametro_2),
    Valor_2 = if_else(Modelo == "Normal", Valor_2_normal, Valor_2)
  ) %>%
  dplyr::select(Segmentacion, Modelo, Parametro_1, Valor_1,
                Parametro_2, Valor_2, n)

modelos_temp_ajustados


# ---------------------------------------------------------
# 11. Calcular costo promedio por portafolio para Temperatura
# ---------------------------------------------------------
costos_temp_portafolio <- modelos_temp_ajustados %>%
  mutate(
    costo_promedio_2024 = mapply(costo_promedio_evento, Modelo, Valor_1, Valor_2),
    probabilidad = n / sum(n),
    costo_promedio_2025 = costo_promedio_2024 * factor_2025
  )

costos_temp_portafolio


# ---------------------------------------------------------
# 12. Calcular costo promedio final para Temperatura
# ---------------------------------------------------------
costo_promedio_temp_2024 <- sum(costos_temp_portafolio$probabilidad *
                                  costos_temp_portafolio$costo_promedio_2024,
                                na.rm = TRUE)

costo_promedio_temp_2025 <- sum(costos_temp_portafolio$probabilidad *
                                  costos_temp_portafolio$costo_promedio_2025,
                                na.rm = TRUE)

cat("Costo promedio por evento de TEMPERATURA en pesos de 2024:",
    costo_promedio_temp_2024, "\n")

cat("Costo promedio por evento de TEMPERATURA para 2025:",
    costo_promedio_temp_2025, "\n")

cat("Costo promedio por evento de TEMPERATURA en millones de pesos para 2025:",
    costo_promedio_temp_2025 / 1e6, "\n")

costos_temp_portafolio

costo_promedio_temp_2025

fit.cont(datos_temperatura$log_daños)

mean(datos_temperatura$`daños_2024(pesos)`, na.rm = TRUE)
median(datos_temperatura$`daños_2024(pesos)`, na.rm = TRUE)

quantile(datos_temperatura$`daños_2024(pesos)`,
         probs = c(0.75, 0.90, 0.95, 0.99),
         na.rm = TRUE)

datos_temperatura %>%
  arrange(desc(`daños_2024(pesos)`)) %>%
  dplyr::select(Año, Estado, Tipo_fenomeno_limpio, Portafolio_final, `daños_2024(pesos)`) %>%
  head(10)

gofstat(list(fit_normal_temp_global, fit_logistica_temp_global))

mejor_aic_temp_portafolio
mejor_bic_temp_portafolio