## Borro la memoria
rm(list=ls())

# Instalar paquetes necesarios (si no están instalados)
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringi")) install.packages("stringi")
if (!require("corrplot")) install.packages("corrplot")
if (!require("sandwich")) install.packages("sandwich", dependencies=TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies=TRUE)

# Cargar bibliotecas
library(dplyr)
library(stringi)
library(corrplot)
library(car)  
library(gridExtra) 
library(ggplot2)
library(tidyr)
library(sandwich)
library(lmtest)

# Ruta de los archivos
ruta <- "C:/Users/ivan_/OneDrive/Documents"
file_fm <- paste0(ruta, "/FootballManager_Updated.csv")
file_tm <- paste0(ruta, "/TransferMarket_Updated.csv")
file_salary <- paste0(ruta, "/FootballManager_Salary_Goals.csv")

# Cargar archivos con verificación
df_fm <- read.csv(file_fm, stringsAsFactors = FALSE)
df_tm <- read.csv(file_tm, stringsAsFactors = FALSE)
df_salary <- read.csv(file_salary, stringsAsFactors = FALSE)

#####################################################################
######## TRANSFORMACIONES PARA GENERAR BASE DE ANALISIS #############
#####################################################################

# Verificar que las columnas "Name_Detailed" y "Jugador" existen en ambos archivos
if (!"Name_Detailed" %in% names(df_fm) | !"Jugador" %in% names(df_tm)) {
  stop("Error: La columna 'Name_Detailed' o 'Jugador' no está presente en uno o ambos archivos.")
}

# Reemplazar valores NA en las columnas correspondientes
df_fm$Name_Detailed <- ifelse(is.na(df_fm$Name_Detailed), "", df_fm$Name_Detailed)
df_tm$Jugador <- ifelse(is.na(df_tm$Jugador), "", df_tm$Jugador)
df_salary$Salary <- ifelse(is.na(df_salary$Salary), "0", df_salary$Salary)

# Asegurar que Salary es numérico
df_salary$Salary <- as.numeric(gsub("[^0-9]", "", df_salary$Salary))

# Función para normalizar nombres
normalize_text <- function(text) {
  text <- stri_trans_general(text, "Latin-ASCII")  # Reemplaza caracteres especiales
  return(text)
}

# Aplicar normalización en los nombres
df_fm$Name_Detailed <- sapply(df_fm$Name_Detailed, normalize_text)
df_tm$Jugador <- sapply(df_tm$Jugador, normalize_text)

# Eliminar filas donde los nombres sean vacíos
df_fm <- df_fm[df_fm$Name_Detailed != "", ]
df_tm <- df_tm[df_tm$Jugador != "", ]

# Renombrar la columna en df_fm para que coincida con df_tm
df_fm <- df_fm %>% rename(Jugador = Name_Detailed)

# Unir archivos por la columna "Jugador"
df_merged <- inner_join(df_fm, df_tm, by = "Jugador")

# Eliminar filas con Unique_ID duplicado o nulo
if ("Unique_ID" %in% names(df_merged)) {
  df_merged <- df_merged %>%
    filter(!is.na(Unique_ID) & Unique_ID != "") %>%
    distinct(Unique_ID, .keep_all = TRUE)
}

# Prescindiremos de algunas observaciones que no pudieron cruzarse inequivocamente al utilizar nombre
# Contar la cantidad de veces que aparece cada jugador
jugador_counts <- df_merged %>% count(Jugador)

# Filtrar solo los jugadores que aparecen una sola vez
df_merged <- df_merged %>%
  filter(Jugador %in% jugador_counts$Jugador[jugador_counts$n == 1])

# Unir con df_salary usando la columna UID y seleccionando variables específicas
df_merged <- left_join(df_merged, df_salary %>% select(UID, Salary, AT.Apps, AT.Gls, Min.Fee.Rls), by = c("Unique_ID" = "UID"))

# Eliminar filas donde "Posición" sea nulo o vacío
df_merged <- df_merged %>% filter(!is.na(Posición) & Posición != "")

# Eliminar filas donde "Posición" sea "Goalkeeper"
df_merged <- df_merged %>% filter(Posición != "Goalkeeper")

# Eliminar filas donde "Valor de Mercado" o "Salary" sean nulos o "-"
df_merged <- df_merged %>% filter(!is.na(Valor.de.Mercado) & Valor.de.Mercado != "-")
df_merged <- df_merged %>% filter(!is.na(Salary) & Salary != "-")

# Eliminar columnas innecesarias
drop_columns <- c("Valor.de.Mercado", "Equipo", "Liga", "Temporada", "URL.Perfil", "URL.Rendimiento", "Unique_ID", "Scout_recommendation", "Kicking", "Scout_recommendation", "Player_Status_Information_General")
df_merged <- df_merged %>% select(-one_of(intersect(names(df_merged), drop_columns)))

# Convertir Minutos.Jugados y Partidos en números
df_merged$Minutos.Jugados <- as.numeric(gsub("['.]", "", df_merged$Minutos.Jugados))
df_merged$Partidos <- as.numeric(df_merged$Partidos)
df_merged$AT.Apps <- as.numeric(df_merged$AT.Apps)
df_merged$AT.Gls <- as.numeric(df_merged$AT.Gls)

# Transformar Amarillas, Segundas_Amarillas y Rojas extrayendo solo números y forzando a numeric
df_merged <- df_merged %>% mutate(
  Amarillas = as.numeric(ifelse(grepl("\\d+", Amarillas), gsub("\\D", "", Amarillas), "0")),
  Segundas_Amarillas = as.numeric(ifelse(grepl("\\d+", Segundas_Amarillas), gsub("\\D", "", Segundas_Amarillas), "0")),
  Rojas = as.numeric(ifelse(grepl("\\d+", Rojas), gsub("\\D", "", Rojas), "0"))
)

# Transformar Goles, Asistencias, Partidos temporada y Partidos historicos
df_merged$Goles <- as.numeric(ifelse(is.na(df_merged$Goles) | df_merged$Goles == "-" | df_merged$Goles == "", 0, df_merged$Goles))
df_merged$Asistencias <- as.numeric(ifelse(is.na(df_merged$Asistencias) | df_merged$Asistencias == "-" | df_merged$Asistencias == "", 0, df_merged$Asistencias))
df_merged$AT.Apps <- as.numeric(ifelse(is.na(df_merged$AT.Apps) | df_merged$AT.Apps == "-" | df_merged$AT.Apps == "", 0, df_merged$AT.Apps))
df_merged$AT.Gls <- as.numeric(ifelse(is.na(df_merged$AT.Gls) | df_merged$AT.Gls == "-" | df_merged$AT.Gls == "", 0, df_merged$AT.Gls))
df_merged$Partidos <- as.numeric(ifelse(is.na(df_merged$Partidos) | df_merged$Partidos == "-" | df_merged$Partidos == "", 0, df_merged$Partidos))
df_merged$Minutos.Jugados <- as.numeric(ifelse(is.na(df_merged$Minutos.Jugados) | df_merged$Minutos.Jugados == "-" | df_merged$Minutos.Jugados == "", 0, df_merged$Minutos.Jugados))

# Convertir variables binarias en numéricas
df_merged <- df_merged %>% mutate(across(where(~ all(. %in% c("0", "1"))), as.numeric))

# Mapeo de posiciones
position_map <- list(
  "Attacking Midfield" = "Mediocampista",
  "Centre-Forward" = "Delantero",
  "Defensive Midfield" = "Mediocampista",
  "Right Winger" = "Delantero",
  "Left Winger" = "Delantero",
  "Centre-Back" = "Defensor",
  "Central Midfield" = "Mediocampista",
  "Right-Back" = "Defensor",
  "Second Striker" = "Delantero",
  "Left-Back" = "Defensor",
  "Right Midfield" = "Mediocampista",
  "Left Midfield" = "Mediocampista",
  "Midfielder" = "Mediocampista"
)
## Reemplazamos
df_merged$Posición <- unlist(lapply(df_merged$Posición, function(x) ifelse(x %in% names(position_map), position_map[[x]], x)))

# Crear variable dummy para Posición
df_merged <- df_merged %>% mutate(Posicion_dummy = ifelse(Posición == "Defensor", 1, 0))
df_merged$Posicion_dummy[is.na(df_merged$Posicion_dummy)] <- 0

# Crear variables GolesPorPartido y AsistenciasPorPartido
df_merged <- df_merged %>% mutate(
  GolesPorPartido = ifelse(Partidos > 0, Goles / Partidos, 0),
  AsistenciasPorPartido = ifelse(Partidos > 0, Asistencias / Partidos, 0)
)

#####################################################################
##### ANALISIS ESTADISTICO DE VARIABLES INCLUYENDO OUTLIERS #########
#####################################################################

# Identificamos tipos de variables
categoricas <- c("Continente", "Posición", "National_Team", "Preferred_Foot", "Continente")
numericas <- c("Height", "Age", "Agility", "Jumping_Reach", "Heading", "Finishing", 
               "Dribbling", "Balance", "Anticipation", "Aerial_Reach", "Acceleration",
               "One_On_Ones", "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
               "Strength", "Vision", "Partidos", "Goles", "Asistencias", "Amarillas",
               "Segundas_Amarillas", "Rojas", "Minutos.Jugados", "Salary", 
               "AT.Apps", "AT.Gls")

## Variables categóricas: Tablas de frecuencia
cat_summary <- lapply(df_merged[categoricas], function(x) table(x))
cat_summary

## Generar gráficos de barras

cat_plots <- lapply(categoricas, function(var) {
  ggplot(df_merged, aes(x = .data[[var]])) +
    geom_bar(fill = "steelblue") +
    labs(title = paste("Frecuencia de", var), x = var, y = "Frecuencia") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas
})

# Mostrar los gráficos de barras en una cuadrícula
do.call("grid.arrange", c(cat_plots, ncol = 2))

## Variables numéricas: Resumen estadístico
num_summary <- summary(df_merged[numericas])
print(num_summary)

### 2. Detección de Outliers en Boxplots ###
# Detectar outliers usando el método del Rango Intercuartílico (IQR)
outlier_counts <- sapply(numericas, function(var) {
  Q1 <- quantile(df_merged[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_merged[[var]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  sum(df_merged[[var]] < lower_bound | df_merged[[var]] > upper_bound, na.rm = TRUE)
})

# Imprimir el número de outliers detectados en cada variable numérica
print("Número de outliers detectados en cada variable numérica (Boxplots):")
print(outlier_counts)

# Transformar el dataset a formato largo para facilitar el uso de facet_wrap()
df_long <- df_merged %>%
  select(all_of(numericas)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Número de gráficos por página
num_por_pagina <- 6
num_paginas <- ceiling(length(numericas) / num_por_pagina)

# Generar y mostrar los gráficos por páginas
for (i in 1:num_paginas) {
  start_index <- ((i - 1) * num_por_pagina) + 1
  end_index <- min(i * num_por_pagina, length(numericas))
  
  subset_vars <- numericas[start_index:end_index]
  
  p <- ggplot(df_long %>% filter(Variable %in% subset_vars), aes(y = Valor)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16) +
    facet_wrap(~ Variable, scales = "free", ncol = 3) +  # Máx. 3 columnas por fila
    labs(title = paste("Boxplots - Página", i)) +
    theme_minimal()
  
  print(p)
}

### 3. Detección de outliers multivariados con distancia de Mahalanobis ###
# Filtrar solo las variables numéricas
df_num <- df_merged[numericas]

# Remover filas con NA para evitar errores
df_num <- na.omit(df_num)

# Calcular la media y la matriz de covarianza
media <- colMeans(df_num)
cov_matrix <- cov(df_num)

# Calcular la distancia de Mahalanobis
mahalanobis_dist <- mahalanobis(df_num, center = media, cov = cov_matrix)

# Eliminar filas con NA en Mahalanobis_Dist
df_merged <- df_merged %>% filter(!is.na(mahalanobis_dist))

# Umbral de significancia (p < 0.001 para considerar un outlier)
umbral <- qchisq(0.999, df = length(numericas))  # Grados de libertad = número de variables numéricas

# Aplicar unique() para asegurarnos de que no haya duplicados
outliers_mahalanobis <- unique(which(mahalanobis_dist > umbral))

# Mostrar el número total de outliers y los índices únicos
outlier_nombres <- df_merged$Jugador[outliers_mahalanobis]
print(paste("Número total de outliers detectados con Mahalanobis:", length(outlier_nombres)))
print("Nombres de los jugadores detectados como outliers multivariados:")
print(outlier_nombres)

# Gráfico de distancias de Mahalanobis
ggplot(df_merged, aes(x = 1:nrow(df_merged), y = mahalanobis_dist)) +
  geom_point(aes(color = mahalanobis_dist > umbral)) +
  geom_hline(yintercept = umbral, color = "red", linetype = "dashed") +
  labs(title = "Distancia de Mahalanobis para detección de outliers multivariados",
       x = "Índice de la observación",
       y = "Distancia de Mahalanobis") +
  theme_minimal()

#####################################################################
############# ANALISIS DE CORRELACION Y REGRESIONES #################
#####################################################################

# Calcular coeficiente de correlación de cada variable con "Salary"
numeric_vars <- df_merged %>% select(where(is.numeric)) %>%  select(-Salary)
corr_values <- sapply(numeric_vars, function(x) cor(x, as.numeric(df_merged$Salary), use = "complete.obs"))
print(corr_values)

# Aplicar logaritmo a las variables seleccionadas (evitando log(0))
df_merged <- df_merged %>% mutate(
  log_Salary = log(Salary)
)

# Seleccionar variables numéricas
# Definir las variables que quieres incluir en la regresión
selected_vars <- c("Height", "Age", "Agility", "Jumping_Reach", "Heading", "Finishing", 
                   "Dribbling", "Balance", "Anticipation", "Aerial_Reach", "Acceleration", "One_On_Ones",
                   "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
                   "Strength", "Vision", "AT.Gls", "Europa", "Posicion_dummy", 
                   "National_Team_dummy", "X5_grandes_ligas", "GolesPorPartido", "AsistenciasPorPartido", "Preferred_Foot_dummy")

# Crear la fórmula para la regresión lineal
formula <- as.formula(paste("log_Salary ~", paste(selected_vars, collapse = " + ")))
modelo <- lm(formula, data = df_merged)
modelo_nulo <- lm(log_Salary ~ 1, data = df_merged)
summary(modelo)

#####################################################################
############# ANALISIS DE SUPUESTOS DE REGRESION ####################
#####################################################################

# 1. Comprobación de la normalidad de los residuos
par(mfrow = c(1, 2))  # Configurar dos gráficos en una fila
hist(residuals(modelo), main = "Histograma de residuos", xlab = "Residuos", breaks = 30)
qqnorm(residuals(modelo))
qqline(residuals(modelo), col = "red")

# 2. Prueba de normalidad de los residuos con Shapiro-Wilk
shapiro.test(residuals(modelo)) ### No tengo un problema de normalidad

# 3. Comprobación de la homocedasticidad (Test de Breusch-Pagan)
library(lmtest)
bptest(modelo) ##Tengo un problema muy fuerte de Heterocedasticidad. Vamos a arreglarlo:


#######################IC Mantengo problema, seguir desde aca

# 4. Gráfico de residuos vs valores ajustados
plot(fitted(modelo), residuals(modelo), main = "Residuos vs Valores Ajustados",
     xlab = "Valores Ajustados", ylab = "Residuos", pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 5. Comprobación de la independencia de los residuos (Test de Durbin-Watson)
dwtest(modelo)

# 6. Diagnóstico de multicolinealidad con VIF (Variance Inflation Factor)
library(car)
vif(modelo)

# 7.Calcular la distancia de Cook
cooksd <- cooks.distance(modelo)

# Definir umbral comúnmente usado para valores influyentes (4/n)
threshold <- 4 / length(cooksd)

# Filtrar observaciones con Distancia de Cook mayor al umbral
outliers <- which(cooksd > threshold)

# Eliminar duplicados (asegurando que sean únicos)
outliers_unique <- unique(outliers)

# Imprimir los valores atípicos únicos
print(outliers_unique)

# Visualización de la Distancia de Cook con etiquetas en los valores más altos
plot(cooksd, type = "h", main = "Distancia de Cook", ylab = "Cook's Distance", col = "black")
abline(h = threshold, col = "red")  # Línea de referencia

# Etiquetar los valores más influyentes en el gráfico
text(x = outliers_unique, y = cooksd[outliers_unique], labels = outliers_unique, col = "blue", pos = 3, cex = 0.8)

# Restaurar la disposición gráfica original
par(mfrow = c(1, 1))

#####################################################################
############# REPROCESAMIENTO DEL MODELO ############################
#####################################################################

# Aplicamos los 3 tipos de ajuste

library(MASS)

# 1️⃣ Selección Stepwise (ambas direcciones)
# -------------------------------
modelo_stepwise <- stepAIC(modelo, direction = "both", trace = FALSE)
modelo_forward <- stepAIC(modelo, direction = "forward", scope = formula, trace = FALSE)
modelo_backward <- stepAIC(modelo, direction = "backward", trace = FALSE)

# -------------------------------
# Mostrar resumen de cada modelo
# -------------------------------
cat("\n **Modelo Stepwise:**\n")
summary(modelo_stepwise)

cat("\n **Modelo Forward Selection:**\n")
summary(modelo_forward)

cat("\n **Modelo Backward Selection:**\n")
summary(modelo_backward)
