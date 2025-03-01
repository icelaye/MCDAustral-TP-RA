
#####################################################################
######## PREPARACION DEL AMBIENTE ###################################
#####################################################################

## Borro la memoria
rm(list=ls())

# Aumentar la penalización para usar notación científica y ajustar la cantidad de dígitos a mostrar
options(scipen = 999)
options(digits = 4)

# Instalar paquetes necesarios (si no están instalados)
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringi")) install.packages("stringi")
if (!require("corrplot")) install.packages("corrplot")
if (!require("sandwich")) install.packages("sandwich", dependencies=TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies=TRUE)
if (!require("rvest")) install.packages("rvest")
if (!require("xml2")) install.packages("xml2")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("nnet")) install.packages("nnet", dependencies=TRUE)
if (!require("randomForest")) install.packages("randomForest", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("pROC")) install.packages("pROC", dependencies=TRUE)
if (!require("caret")) install.packages("caret", dependencies=TRUE)
if (!require("rpart")) install.packages("rpart", dependencies=TRUE)
if (!require("rpart.plot")) install.packages("rpart.plot", dependencies=TRUE)
if (!require("ROSE")) install.packages("ROSE", dependencies=TRUE)
if (!require("glmnet")) install.packages("glmnet", dependencies=TRUE)
if (!require("ResourceSelection")) install.packages("ResourceSelection", dependencies=TRUE)
if (!require("e1071")) install.packages("e1071", dependencies=TRUE)
if (!require("gamlss")) install.packages("gamlss", dependencies=TRUE)
if (!require("boot")) install.packages("boot", dependencies=TRUE)

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
library(scales)
library(rvest)
library(xml2)
library(stringr)
library(nnet)  
library(randomForest)
library(ggplot2)
library(pROC)
library(caret)
library(rpart)
library(rpart.plot)
library(ROSE)
library(glmnet)
library(ResourceSelection)
library(e1071)
library(MASS)
library(gamlss)
library(boot)
library(xgboost)
library(gamlss)
library(parallel)
library(doParallel)

#####################################################################
############# IMPORT HTML FM ########################################
#####################################################################

# Leer el archivo HTML con codificación explícita
archivo_html <- read_html("C:/Program Files (x86)/Steam/steamapps/common/Football Manager 2024 Editor/Habilidades Fisicas BBDD TP.html", encoding = "UTF-8")
archivo_html_salarios <- read_html("C:/Program Files (x86)/Steam/steamapps/common/Football Manager 2024 Editor/salary_xg.html", encoding = "UTF-8")

# Extraer la tabla del HTML
tabla <- html_node(archivo_html, "table") %>% html_table(fill = TRUE)
# Extraer la tabla del HTML de salarios
tabla_salarios <- html_node(archivo_html_salarios, "table") %>% html_table(fill = TRUE)

# Definir nombres de las columnas
nombres_columnas <- c("Unique_ID", "Name_Detailed", "Division", "Nationality", "Height", "Age", 
                      "Preferred_Foot", "National_Team", "Agility", "Kicking", "Jumping_Reach", 
                      "Heading", "Finishing", "Dribbling", "Balance", "Anticipation", "Aerial_Reach", 
                      "Acceleration", "One_On_Ones", "Tackling", "Stamina", "Pace", "Passing", 
                      "Positioning", "Strength", "Vision", "Scout_recommendation", 
                      "Player_Status_Information_General")

#####################################################################
############# TRANSFORMACION DATOS FM################################
#####################################################################

# Renombrar las columnas
tabla <- setNames(tabla, nombres_columnas)

# Asignar continentes a Nationality
continentes <- list(
  "Europa" = c("BUL", "IRL", "LUX", "HUN", "ROU", "ALB", "MDA", "MAD", "SCO", "NIR", "KOS", "MKD", "WAL", "GRE", "CTA", "CZE", "SRB", "SVN", "SVK", "ESP", "FRA", "GER", "ITA", "ENG", "POR", "NED", "BEL", "SUI", "SWE", "DEN", "NOR", "RUS", "CRO", "POL", "TUR", "UKR", "AUT"),
  "Sudamerica" = c("ARG", "BRA", "URU", "CHI", "COL", "PAR", "ECU", "PER", "VEN", "BOL", "CUW"),
  "Norteamerica" = c("USA", "MEX", "CAN", "CRC", "HON", "JAM", "PAN", "SLV", "GUA", "TRI"),
  "Africa" = c("STP", "NGA", "CIV", "EGY", "CMR", "GHA", "MAR", "ALG", "SEN", "TUN", "RSA", "NIG", "MLI", "GAM", "GNB", "CPV", "ISR"),
  "Asia" = c("JPN", "KOR", "CHN", "IRN", "AUS", "QAT", "UAE", "SAU", "THA", "UZB", "IRQ", "CYP", "MOZ", "ZAM"),
  "Oceania" = c("NZL", "FIJ", "TGA", "PNG", "SOL"),
  "Otros" = c()
)

tabla$Continente <- sapply(tabla$Nationality, function(nat) {
  continente <- names(continentes)[sapply(continentes, function(paises) nat %in% paises)]
  ifelse(length(continente) > 0, continente, "Otros")
})

# Modificar Preferred_Foot
tabla$Preferred_Foot <- ifelse(tabla$Preferred_Foot == "Right Only", "Right",
                               ifelse(tabla$Preferred_Foot == "Left Only", "Left", tabla$Preferred_Foot))

# Crear columna Preferred_Foot_dummy
tabla$Preferred_Foot_dummy <- ifelse(tabla$Preferred_Foot == "Left", 1, 0)

# Crear columna National_Team_dummy
tabla$National_Team_dummy <- ifelse(!is.na(tabla$National_Team) & tabla$National_Team != "", 1, 0)

library(stringr)

# Convertir altura de formato "X'Y" a pulgadas
convertir_altura_pulgadas <- function(altura) {
  match <- str_match(altura, "(\\d+)'(\\d+)")
  if (!is.na(match[1])) {
    pies <- as.numeric(match[2])
    pulgadas <- as.numeric(match[3])
    # 1 pie = 12 pulgadas
    return(pies * 12 + pulgadas)
  } else {
    return(NA)
  }
}

tabla$Height <- sapply(tabla$Height, convertir_altura_pulgadas)

# Agregar columna "Europa"
tabla$Europa <- ifelse(tabla$Continente == "Europa", 1, 0)

# Agregar columna "5 grandes ligas"
tabla$`5_grandes_ligas` <- ifelse(tabla$Division %in% c("English Premier Division", "Bundesliga", "Spanish First Division", "Italian Serie A", "Ligue_1_Uber_Eats"), 1, 0)

# Guardar en CSV
write.csv(tabla, "C:/Users/ivan_/OneDrive/Documents/FootballManager_Updated.csv", row.names = FALSE)
write.csv(tabla_salarios, "C:/Users/ivan_/OneDrive/Documents/FootballManager_Salary_Goals.csv", row.names = FALSE)

#####################################################################
############# IMPORT CSV TRANSFERMARKT###############################
#####################################################################


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
df_merged <- left_join(df_merged, df_salary %>% dplyr::select(UID, Salary, AT.Apps, AT.Gls), by = c("Unique_ID" = "UID"))

# Eliminar filas donde "Posición" sea nulo o vacío
df_merged <- df_merged %>% filter(!is.na(Posición) & Posición != "")

# Eliminar filas donde "Posición" sea "Goalkeeper"
df_merged <- df_merged %>% filter(Posición != "Goalkeeper")

# Eliminar filas donde "Valor de Mercado" o "Salary" sean nulos o "-"
df_merged <- df_merged %>% filter(!is.na(Valor.de.Mercado) & Valor.de.Mercado != "-")
df_merged <- df_merged %>% filter(!is.na(Salary) & Salary != "-")

# Eliminar columnas innecesarias
drop_columns <- c("Valor.de.Mercado", "Equipo", "Liga", "Temporada", "URL.Perfil", "URL.Rendimiento", "Unique_ID", "Scout_recommendation", "Kicking", "Scout_recommendation", "Player_Status_Information_General")
df_merged <- df_merged %>% dplyr::select(-one_of(intersect(names(df_merged), drop_columns)))

# Convertir Minutos.Jugados y Partidos en números
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
categoricas <- c("Continente", "Posición", "National_Team", "Preferred_Foot", "Division")
numericas <- c("Height", "Age", "Agility", "Jumping_Reach", "Heading", "Finishing", 
               "Dribbling", "Balance", "Anticipation", "Acceleration",
                "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
               "Strength", "Vision", "Amarillas",
               "Rojas", "Salary", "AT.Gls", "GolesPorPartido", "AsistenciasPorPartido", "AT.Apps")

# Función para calcular frecuencias
freq_tables <- function(var) {
  # Frecuencia absoluta
  freq_abs <- table(var)
  
  # Frecuencia relativa
  freq_rel <- prop.table(freq_abs)
  
  # Frecuencia acumulada absoluta
  freq_cum_abs <- cumsum(freq_abs)
  
  # Frecuencia acumulada relativa
  freq_cum_rel <- cumsum(freq_rel)
  
  # Construimos un data frame con los resultados
  data.frame(
    Frecuencia_Absoluta = as.vector(freq_abs),
    Frecuencia_Relativa = round(as.vector(freq_rel), 4),
    Frecuencia_Acumulada_Absoluta = as.vector(freq_cum_abs),
    Frecuencia_Acumulada_Relativa = round(as.vector(freq_cum_rel), 4),
    row.names = names(freq_abs)
  )
}

# Aplicamos la función a cada variable categórica
cat_summary <- lapply(df_merged[categoricas], freq_tables)
cat_summary

## Generar gráficos de barras

cat_plots <- lapply(categoricas, function(var) {
  p <- ggplot(df_merged, aes(x = .data[[var]])) +
    geom_bar(fill = "steelblue") +
    labs(title = paste("Frecuencia de", var), x = var, y = "Frecuencia") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Si la variable es Division, modificamos las etiquetas del eje x
  if(var == "Division") {
    p <- p + scale_x_discrete(labels = function(x) substr(x, 1, 3))
  }
  
  return(p)
})

# Mostrar los gráficos de barras en una cuadrícula
do.call("grid.arrange", c(cat_plots, ncol = 2))

## Variables numéricas: Resumen estadístico

# Definimos una función que retorne un vector con los estadísticos que deseamos
my_summary <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  c(
    N        = length(na.omit(x)),           # Cantidad de valores no NA
    NA_Count = sum(is.na(x)),               # Cantidad de valores perdidos
    Min      = min(x, na.rm = TRUE),
    Q1       = quantile(x, 0.25, na.rm = TRUE),
    Median   = median(x, na.rm = TRUE),
    Mean     = m,
    Q3       = quantile(x, 0.75, na.rm = TRUE),
    Max      = max(x, na.rm = TRUE),
    Var      = var(x, na.rm = TRUE),        # Varianza
    Skew     = skewness(x, na.rm = TRUE),   # Asimetría (skewness)
    Kurt     = kurtosis(x, na.rm = TRUE),   # Curtosis
    CV       = ifelse(m == 0, NA, s / m)    # Coeficiente de variación (SD/Mean)
  )
}

# Aplicamos la función a cada variable numérica
num_summary_table <- sapply(df_merged[numericas], my_summary)

# Transponemos para que cada fila sea una variable y cada columna un estadístico
num_summary_table <- t(num_summary_table)

# Función para generar un histograma según la variable y un parámetro que define si usar binwidth = 1 o no
plot_distribution <- function(var_name, binwidth_param = 0) {
  
  # Construimos la base del ggplot
  p <- ggplot(df_merged, aes(x = .data[[var_name]])) +
    labs(title = paste("Distribución de", var_name), x = var_name, y = "Frecuencia") +
    theme_minimal()
  
  # Si binwidth_param == 1, usamos binwidth = 1; de lo contrario, dejamos el predeterminado
  if (binwidth_param == 1) {
    p <- p + 
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black")
  } else {
    p <- p + 
      geom_histogram(fill = "steelblue", color = "black")
  }
  
  return(p)
}

# Realizamos para cada variable 
p_salary   <- plot_distribution("Salary", binwidth_param = 0)  # binwidth por defecto
p_height   <- plot_distribution("Height", binwidth_param = 1)  # binwidth = 1
p_age      <- plot_distribution("Age", binwidth_param = 1)     # binwidth = 1
p_Agility <- plot_distribution("Agility", binwidth_param = 1)     # binwidth = 1
p_Jumping_Reach <- plot_distribution("Jumping_Reach", binwidth_param = 1)     # binwidth = 1
p_Heading      <- plot_distribution("Heading", binwidth_param = 1)     # binwidth = 1
p_Finishing      <- plot_distribution("Finishing", binwidth_param = 1)     # binwidth = 1
p_Dribbling      <- plot_distribution("Dribbling", binwidth_param = 1)     # binwidth = 1
p_Balance      <- plot_distribution("Balance", binwidth_param = 1)     # binwidth = 1
p_Anticipation      <- plot_distribution("Anticipation", binwidth_param = 1)     # binwidth = 1
p_Acceleration      <- plot_distribution("Acceleration", binwidth_param = 1)     # binwidth = 1
p_Tackling      <- plot_distribution("Tackling", binwidth_param = 1)     # binwidth = 1
p_Stamina      <- plot_distribution("Stamina", binwidth_param = 1)     # binwidth = 1
p_Pace      <- plot_distribution("Pace", binwidth_param = 1)     # binwidth = 1
p_Passing      <- plot_distribution("Passing", binwidth_param = 1)     # binwidth = 1
p_Positioning      <- plot_distribution("Positioning", binwidth_param = 1)     # binwidth = 1
p_Strength      <- plot_distribution("Strength", binwidth_param = 1)     # binwidth = 1
p_Vision   <- plot_distribution("Vision", binwidth_param = 1)  # binwidth por defecto
p_Amarillas      <- plot_distribution("Amarillas", binwidth_param = 1)     # binwidth = 1
p_Rojas      <- plot_distribution("Rojas", binwidth_param = 1)     # binwidth = 1
p_AT.Gls   <- plot_distribution("AT.Gls", binwidth_param = 0)  # binwidth por defecto
p_GolesPorPartido   <- plot_distribution("GolesPorPartido", binwidth_param = 0)  # binwidth por defecto
p_AsistenciasPorPartido   <- plot_distribution("AsistenciasPorPartido", binwidth_param = 0)  # binwidth por defecto
p_AT.Apps   <- plot_distribution("AT.Apps", binwidth_param = 0)  # binwidth por defecto

grid.arrange(p_salary, p_height, p_age, ncol = 3)

grid.arrange(p_Jumping_Reach, p_Heading, p_Finishing, p_Dribbling, p_Balance, ncol = 2)

grid.arrange(p_Pace, p_Passing, p_Positioning, 
             p_Strength, p_Vision, ncol = 2)

grid.arrange(p_Anticipation, p_Acceleration, p_Tackling, p_Stamina, p_Agility, ncol = 2)

grid.arrange(p_Amarillas, p_Rojas, p_AT.Gls, p_GolesPorPartido, p_AsistenciasPorPartido, p_AT.Apps, ncol = 2)

### 2. Detección de Outliers en Boxplots ###

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

outlier_table <- data.frame(
  Variable = names(outlier_counts),
  Outlier_Count = outlier_counts,
  row.names = NULL
)

# Transformar el dataset a formato largo para facilitar el uso de facet_wrap()
df_long <- df_merged %>%
  dplyr::select(all_of(numericas)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Número de gráficos por página
num_por_pagina <- 5
num_paginas <- 3

# Generar y mostrar los gráficos por páginas
for (i in 1:num_paginas) {
  start_index <- ((i - 1) * num_por_pagina) + 1
  end_index <- min(i * num_por_pagina, length(numericas))
  
  subset_vars <- c("Pace", "Passing", "Positioning", "Strength", "Vision")
  
  p <- ggplot(df_long %>% filter(Variable %in% subset_vars), aes(y = Valor)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16) +
    facet_wrap(~ Variable, scales = "free", ncol = 3) +  # Máx. 3 columnas por fila
    labs(title = paste("Boxplots - Atributos")) +
    theme_minimal()
  
  print(p)
}


#####################################################################
##### DETECCION DE OUTLIERS MULTIVARIADOS DE MAHALANOBIS #########
#####################################################################

# Filtrar solo las variables numéricas
df_num <- df_merged[numericas]
df_num <- df_num[, !names(df_num) %in% "Salary"] ## Excluimos Salary por no ser regresora

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

# Matriz de correlaciones y corrplot

# Calcular la matriz de correlación usando solo observaciones completas
cor_matrix <- cor(df_merged[numericas], use = "complete.obs")

# Redondeamos a 2 decimales para facilitar la lectura
cor_matrix <- round(cor_matrix, 2)

# Imprimir la matriz en la consola
print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = NULL)

# Seleccionar variables finales del modelo lineal
selected_vars <- c("Height", "Age", "Agility", "Jumping_Reach", "Heading", "Finishing", 
                   "Dribbling", "Balance", "Anticipation", "Acceleration",
                   "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
                   "Strength", "Vision", "AT.Gls", "Europa", "Posicion_dummy", "AT.Apps",
                   "National_Team_dummy", "GolesPorPartido", "AsistenciasPorPartido", "Preferred_Foot_dummy", "Amarillas", "expulsion", "X5_grandes_ligas")


# Seleccionar variables finales del modelo lineal con log sin APPs
selected_vars_log <- c("Height", "Age", "Agility", "Heading", "Finishing", "Jumping_Reach",
                   "Dribbling", "Balance", "Anticipation", "Acceleration",
                   "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
                   "Strength", "Vision", "log_AT.Gls", "Europa", "Posicion_dummy",
                   "National_Team_dummy", "GolesPorPartido", "AsistenciasPorPartido", "Preferred_Foot_dummy", "log_Amarillas", "expulsion", "X5_grandes_ligas", "log_AT.Apps")

df_merged$expulsion <- ifelse(df_merged$Rojas == 0, 0, 1)

# Seleccionar solo las columnas requeridas, nos quedamos con el .csv de la base final
df_procesada <- df_merged %>% dplyr::select(Jugador, all_of(selected_vars), Salary)
write.csv(df_procesada, "df_procesada.csv", row.names = FALSE)

# Aplicar logaritmo a las variables seleccionadas (evitando log(0))
df_procesada <- df_procesada %>% mutate(
  log_Salary = log(Salary),
  log_AT.Apps = log(AT.Apps + 1),
  log_Amarillas = log(Amarillas + 1),
  log_AT.Gls = log(AT.Gls + 1)
)

############################################
#####MODELOS PREDICTIVOS####################
############################################

#### Ahora haremos uno de predicción en lugar de descripción
# Fijar semilla para reproducibilidad
set.seed(187)  

# Obtener índices aleatorios para entrenamiento (70%)
train_indices <- sample(seq_len(nrow(df_procesada)), size = round(0.7 * nrow(df_procesada)), replace = FALSE)

# Dividir los datos en entrenamiento y prueba
df_train <- df_procesada[train_indices, ]
df_test <- df_procesada[-train_indices, ]

# Crear la fórmula para la regresión lineal
formula <- as.formula(paste("Salary ~", paste(selected_vars, collapse = " + ")))

# Entrenar modelo en conjunto de entrenamiento
modelo_train <- lm(formula, data = df_train)
summary(modelo_train)

# Crear la fórmula para la regresión lineal
formula <- as.formula(paste("log_Salary ~", paste(selected_vars_log, collapse = " + ")))

# Entrenar modelo en conjunto de entrenamiento
modelo_train <- lm(formula, data = df_train)
summary(modelo_train)

#####################################################################
############# ANALISIS DE SUPUESTOS DE REGRESION ####################
#####################################################################

# Graficar el Q-Q plot de los residuos
qqnorm(residuals(modelo_train), main = "Q-Q Plot de los Residuos")
qqline(residuals(modelo_train), col = "red")

shapiro.test(residuals(modelo_train)) 

# Evaluacion de autocorrelación de los residuos
dwtest(modelo_train)

# Evaluacion de multicolinealidad de los residuos
vif(modelo_train)
vif_values <- vif(modelo_train)
AIC(modelo_train)

# Ajustar márgenes para dar espacio a los nombres
par(mar = c(10, 4, 4, 2) + 1)

# Crear un gráfico de barras con los valores de VIF
barplot(vif_values,
        main = "Valores de VIF del Modelo",
        col = "skyblue",
        ylim = c(0, max(vif_values, 5) + 1),
        las = 2)

# Añadir una línea horizontal de umbral en 5
abline(h = 5, col = "red", lwd = 2, lty = 2)

# Añadir una línea horizontal de umbral en 1
abline(h = 1, col = "blue", lwd = 2, lty = 2)

# Graficar residuos vs. valores ajustados
plot(fitted(modelo_train), residuals(modelo_train),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs. Valores ajustados")
abline(h = 0, col = "red", lwd = 2)

bptest(modelo_train)

# 7.Calcular la distancia de Cook
cooksd <- cooks.distance(modelo_train) ## Cambiar modelo a evaluar

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

# Predicciones en el conjunto de prueba
df_test$pred_log_Salary <- predict(modelo_train, newdata = df_test)

# Evaluación en la escala logarítmica
MAE_log <- mean(abs(df_test$pred_log_Salary - df_test$log_Salary))  # Error Absoluto Medio en log
RMSE_log <- sqrt(mean((df_test$pred_log_Salary - df_test$log_Salary)^2))  # RMSE en log
R2_log <- cor(df_test$pred_log_Salary, df_test$log_Salary)^2  # R² en escala log

# Mostrar resultados corregidos
print(paste("MAE en log(Salary):", round(MAE_log, 4)))
print(paste("RMSE en log(Salary):", round(RMSE_log, 4)))
print(paste("R² en datos de prueba (log_Salary):", round(R2_log, 4)))

# Comparación visual en la escala logarítmica
ggplot(df_test, aes(x = log_Salary, y = pred_log_Salary)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "log(Salario) Real vs. log(Salario) Predicho",
       x = "log(Salario) Real", y = "log(Salario) Predicho") +
  theme_minimal()


############################################
##### MODELO STEPWISE ######################
############################################

# Seleccionar variables finales del modelo lineal con log sin APPs
selected_vars_log_2 <- c("Height", "Age", "Agility", "Jumping_Reach", "Heading", "Finishing", 
                       "Dribbling", "Balance", "Anticipation", "Acceleration",
                       "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
                       "Strength", "Vision", "log_AT.Gls", "Europa", "Posicion_dummy",
                       "National_Team_dummy", "GolesPorPartido", "AsistenciasPorPartido", "Preferred_Foot_dummy", "log_Amarillas", "expulsion", "X5_grandes_ligas")

# Crear la fórmula para la regresión lineal
formula_2 <- as.formula(paste("log_Salary ~", paste(selected_vars_log_2, collapse = " + ")))

# Entrenar modelo en conjunto de entrenamiento
modelo_train_2 <- lm(formula_2, data = df_train)

summary(modelo_train_2)

# Stepwise dplyr::selection (combinando forward y backward)
modelo_stepwise <- stepAIC(modelo_train_2, direction = "both", trace = FALSE)

# Backward dplyr::selection (eliminando variables una por una)
modelo_backward <- stepAIC(modelo_train_2, direction = "backward", trace = FALSE)

# Forward dplyr::selection (agregando variables una por una desde un modelo nulo)
modelo_nulo <- lm(log_Salary ~ 1, data = df_train)  # Modelo solo con el intercepto
modelo_forward <- stepAIC(modelo_nulo, scope = formula_2, direction = "forward", trace = FALSE)

# Comparar modelos
summary(modelo_stepwise)
summary(modelo_backward)
summary(modelo_forward)

# Comparar métricas de ajuste
ajuste <- data.frame(
  Modelo = c("Original", "Stepwise", "Backward", "Forward"),
  R2 = c(summary(modelo_train_2)$r.squared, summary(modelo_stepwise)$r.squared,
         summary(modelo_backward)$r.squared, summary(modelo_forward)$r.squared),
  R2_Ajustado = c(summary(modelo_train_2)$adj.r.squared, summary(modelo_stepwise)$adj.r.squared,
                  summary(modelo_backward)$adj.r.squared, summary(modelo_forward)$adj.r.squared),
  AIC = c(AIC(modelo_train_2), AIC(modelo_stepwise), AIC(modelo_backward), AIC(modelo_forward)),
  BIC = c(BIC(modelo_train_2), BIC(modelo_stepwise), BIC(modelo_backward), BIC(modelo_forward))
)

print(ajuste)

# Predicciones en el conjunto de prueba
df_test$pred_log_Salary <- predict(modelo_train_2, newdata = df_test)

# Predicciones en el conjunto de prueba
df_test$pred_log_Salary <- predict(modelo_stepwise, newdata = df_test)

# Evaluación en la escala logarítmica
MAE_log <- mean(abs(df_test$pred_log_Salary - df_test$log_Salary))  # Error Absoluto Medio en log
RMSE_log <- sqrt(mean((df_test$pred_log_Salary - df_test$log_Salary)^2))  # RMSE en log
R2_log <- cor(df_test$pred_log_Salary, df_test$log_Salary)^2  # R² en escala log

# Mostrar resultados corregidos
print(paste("MAE en log(Salary):", round(MAE_log, 4)))
print(paste("RMSE en log(Salary):", round(RMSE_log, 4)))
print(paste("R² en datos de prueba (log_Salary):", round(R2_log, 4)))

#####################################################################
############# ANALISIS DE SUPUESTOS DE REGRESION ####################
#####################################################################

# 1. Comprobación de la normalidad de los residuos
par(mfrow = c(1, 2))  # Configurar dos gráficos en una fila
hist(residuals(modelo_stepwise), main = "Histograma de residuos", xlab = "Residuos", breaks = 30)
qqnorm(residuals(modelo_stepwise))
qqline(residuals(modelo_stepwise), col = "red")

# 2. Prueba de normalidad de los residuos con Shapiro-Wilk
shapiro.test(residuals(modelo_stepwise))

# 3. Comprobación de la homocedasticidad (Test de Breusch-Pagan)
bptest(modelo_stepwise)

# 4. Gráfico de residuos vs valores ajustados
plot(fitted(modelo_stepwise), residuals(modelo_stepwise), main = "Residuos vs Valores Ajustados",
     xlab = "Valores Ajustados", ylab = "Residuos", pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 5. Comprobación de la independencia de los residuos (Test de Durbin-Watson)
## Probamos autocorrelacion
dwtest(modelo_train)
dwtest(modelo_stepwise)

# 6. Diagnóstico de multicolinealidad con VIF (Variance Inflation Factor)
library(car)
vif(modelo_train)
vif(modelo_stepwise)

# 7.Calcular la distancia de Cook
cooksd <- cooks.distance(modelo_stepwise) ## Cambiar modelo a evaluar

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


############################################
####### MODELO GAMLSS ######################
############################################



# Crear la fórmula para la regresión
formula_gamlss <- as.formula(paste("log_Salary ~", paste(selected_vars_log, collapse = " + ")))

# Ajustar el modelo GAMLSS. Vemos primero la distribución de log_Salary

# Histograma con densidad
ggplot(df_train, aes(x = log_Salary)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  labs(title = "Distribución de log_Salary", x = "log_Salary", y = "Densidad") +
  theme_minimal()

# Boxplot para detectar outliers
ggplot(df_train, aes(y = log_Salary)) +
  geom_boxplot(fill = "lightblue", color = "red") +
  labs(title = "Boxplot de log_Salary", y = "log_Salary") +
  theme_minimal()


# Instalar y cargar el paquete GAMLSS si no está instalado
if (!require(gamlss)) install.packages("gamlss", dependencies = TRUE)
library(gamlss)

modelo_gamlss <- gamlss(log_Salary ~ Height + Age + Agility + Finishing + 
                            Dribbling + Balance + Anticipation + Acceleration + Tackling + 
                            Stamina + Pace + Passing + Positioning + Strength + log_AT.Gls + 
                            Posicion_dummy + National_Team_dummy + GolesPorPartido + 
                            AsistenciasPorPartido + log_Amarillas + X5_grandes_ligas, sigma.formula = ~ X5_grandes_ligas + Age + Posicion_dummy, data = df_train, family = SHASHo,
                        control = gamlss.control(n.cyc = 1000))

summary(modelo_gamlss, robust = TRUE)

# Predicciones en el conjunto de prueba
df_test$pred_log_Salary <- NULL
df_test$pred_log_Salary <- predict(modelo_gamlss, newdata = df_test, type = "response")

# Evaluación del modelo
MAE_log <- mean(abs(df_test$pred_log_Salary - df_test$log_Salary))
RMSE_log <- sqrt(mean((df_test$pred_log_Salary - df_test$log_Salary)^2))
R2_gamlss <- cor(df_test$pred_log_Salary, df_test$log_Salary)^2  # R² en datos de prueba

# Calcular AIC y BIC
AIC_gamlss <- AIC(modelo_gamlss)
BIC_gamlss <- BIC(modelo_gamlss)

# Imprimir resultados
print(paste("MAE (GAMLSS):", round(MAE_log, 4)))
print(paste("RMSE (GAMLSS):", round(RMSE_log, 4)))
print(paste("R² en datos de prueba (GAMLSS):", round(R2_gamlss, 4)))
print(paste("AIC (GAMLSS):", round(AIC_gamlss, 4)))
print(paste("BIC (GAMLSS):", round(BIC_gamlss, 4)))

# Comparar gráficamente predicciones vs valores reales
ggplot(df_test, aes(x = log_Salary, y = pred_log_Salary)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Salario Real vs. Predicho (GAMLSS)", x = "Salario Real", y = "Salario Predicho") +
  theme_minimal()

dwtest(modelo_gamlss)
shapiro.test(residuals(modelo_gamlss))
bptest(modelo_gamlss)
vif(modelo_gamlss)

# Ajustar márgenes para dar espacio a los nombres
par(mar = c(10, 4, 4, 2) + 1)

# Crear un gráfico de barras con los valores de VIF
barplot(vif(modelo_gamlss),
        main = "Valores de VIF del Modelo",
        col = "skyblue",
        ylim = c(0, max(vif_values, 5) + 1),
        las = 2)

# Añadir una línea horizontal de umbral en 5
abline(h = 5, col = "red", lwd = 2, lty = 2)

# Añadir una línea horizontal de umbral en 1
abline(h = 1, col = "blue", lwd = 2, lty = 2)

############################################
########## MODELO REGRESION ROBUSTA ########
############################################

# Instalar y cargar el paquete
if (!require(robustbase)) install.packages("robustbase", dependencies = TRUE)
library(robustbase)

# Ajustar modelo robusto con MM-estimadores
modelo_mm <- rlm(log_Salary ~ Height + Age + Agility + Finishing + 
                     Dribbling + Balance + Anticipation + Acceleration + Tackling + 
                     Stamina + Pace + Passing + Positioning + Strength + log_AT.Gls + 
                     Posicion_dummy + National_Team_dummy + GolesPorPartido + 
                     AsistenciasPorPartido + log_Amarillas + X5_grandes_ligas, 
                   data = df_train)

# Resumen del modelo MM
summary(modelo_mm)

# Predicciones en el conjunto de prueba
df_test$pred_log_Salary_mm <- predict(modelo_mm, newdata = df_test)

# Evaluación del modelo
MAE_mm <- mean(abs(df_test$pred_log_Salary_mm - df_test$log_Salary))
RMSE_mm <- sqrt(mean((df_test$pred_log_Salary_mm - df_test$log_Salary)^2))
R2_mm <- cor(df_test$pred_log_Salary_mm, df_test$log_Salary)^2

# Imprimir resultados
print(paste("MAE (MM):", round(MAE_mm, 4)))
print(paste("RMSE (MM):", round(RMSE_mm, 4)))
print(paste("R² en datos de prueba (MM):", round(R2_mm, 4)))

dev.off()
# Comparar gráficamente predicciones vs valores reales
ggplot(df_test, aes(x = log_Salary, y = pred_log_Salary_mm)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Salario Real vs. Predicho (MM-Estimadores)", x = "Salario Real", y = "Salario Predicho") +
  theme_minimal()

# 1️⃣ Verificar Linealidad: Gráfico de Residuos vs. Valores Predichos
plot(fitted(modelo_mm), resid(modelo_mm), 
     xlab = "Valores Predichos", ylab = "Residuos", 
     main = "Residuos vs. Valores Predichos (Regresión Robusta)")
abline(h = 0, col = "red", lty = 2)

# 2️⃣ Prueba de Independencia de Errores (Durbin-Watson)
library(lmtest)
dwtest(modelo_mm)

############################################
####### MODELO XGBOOST Y VAL. CRUZADA#######
############################################


set.seed(187)
num_folds <- 5
folds <- createFolds(df_train$log_Salary, k = num_folds, list = TRUE)

# Variables para XGBoost
vars_xgb <- c("Height", "Age", "Agility", "Heading", "Finishing", "Jumping_Reach",
              "Dribbling", "Balance", "Anticipation", "Acceleration",
              "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
              "Strength", "Vision", "log_AT.Gls", "Europa", "Posicion_dummy",
              "National_Team_dummy", "GolesPorPartido", "AsistenciasPorPartido", 
              "Preferred_Foot_dummy", "log_Amarillas", "expulsion", 
              "X5_grandes_ligas", "log_AT.Apps")

# Funciones para Métricas

calc_rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
calc_mae <- function(actual, predicted) mean(abs(actual - predicted))
calc_mape <- function(actual, predicted) mean(abs((actual - predicted) / actual)) * 100
calc_median_ae <- function(actual, predicted) median(abs(actual - predicted))
calc_bias <- function(actual, predicted) mean(actual - predicted)

calc_r2 <- function(y_true, y_pred) {
  ss_total <- sum((y_true - mean(y_true))^2)
  ss_residual <- sum((y_true - y_pred)^2)
  return(1 - (ss_residual / ss_total))
}

# Convertir datos a formato matriz
train_xgb_final <- df_train[, vars_xgb]
dummies_final <- dummyVars(~ ., data = train_xgb_final)
train_xgb_matrix_final <- predict(dummies_final, newdata = train_xgb_final)
dtrain_final <- xgb.DMatrix(data = as.matrix(train_xgb_matrix_final), label = df_train$log_Salary) ## Cambiamos log_Salary

# Se dejan debajo los hiperparametros obtenidos por Gridsearch para ahorrar ejecucion

# # Definir grilla de hiperparámetros optimizada
# grid <- expand.grid(
#   nrounds = c(100, 500, 1000),  # Menos iteraciones para acelerar el proceso
#   eta = c(0.05, 0.1, 0.02),  # Tasa de aprendizaje razonable
#   max_depth = c(3, 6, 9),  # Profundidad reducida para evitar overfitting
#   min_child_weight = c(1, 5, 10),  
#   subsample = c(0.75, 1),  
#   colsample_bytree = c(0.75, 1),  
#   gamma = c(0, 1)  
# )
# 
# # Paralelización para acelerar Grid Search
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)
# 
# # Configuración de validación cruzada
# train_control <- trainControl(method = "cv", number = num_folds, allowParallel = TRUE, verboseIter = TRUE)
# 
# # Ejecutar Grid Search
# set.seed(187)  
# xgb_tune <- train(
#   x = as.matrix(train_xgb_matrix_final),
#   y = df_train$Salary,
#   method = "xgbTree",
#   trControl = train_control,
#   tuneGrid = grid,
#   metric = "RMSE"
# )
# 
# stopCluster(cl)  # Cerrar procesos paralelos
# 
# # Extraer mejores hiperparámetros
# best_params <- xgb_tune$bestTune
# print(best_params)

# --------------------
# 4️⃣ Entrenar Modelo XGBoost Final
# --------------------

# params_xgb <- list(
#   eta = best_params$eta,
#   max_depth = best_params$max_depth,
#   min_child_weight = best_params$min_child_weight,
#   subsample = best_params$subsample,
#   colsample_bytree = best_params$colsample_bytree,
#   gamma = best_params$gamma,
#   objective = "reg:squarederror"
# )

params_xgb <- list(
  eta =0.02,
  max_depth = 9,
  min_child_weight = 5,
  subsample = 0.75,
  colsample_bytree = 0.75,
  gamma = 0,
  objective = "reg:squarederror"
)

modelo_xgb_final <- xgb.train(params = params_xgb, data = dtrain_final, 
                              nrounds = 1000, 
                              watchlist = list(train = dtrain_final), 
                              early_stopping_rounds = 50, verbose = 0)

## Analisis SHAP
library(reshape2)

# Obtener SHAP values
shap_values <- predict(modelo_xgb_final, dtrain_final, predcontrib = TRUE)

# Convertir a dataframe
shap_df <- as.data.frame(shap_values)
shap_df$log_Salary <- df_train$log_Salary  # Agregar la variable objetivo

# Calcular importancia promedio de cada variable en SHAP
shap_importance <- colMeans(abs(shap_df[, -ncol(shap_df)]))

# Crear dataframe de importancia
shap_importance_df <- data.frame(Variable = names(shap_importance), Importance = shap_importance)

# Graficar importancia de SHAP values
ggplot(shap_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "SHAP Values - Importancia de Variables", x = "Variables", y = "Importancia")


# Cross-Validation para XGBoost y GAMLSS

xgb_rmse <- c()
xgb_r2 <- c()
gamlss_rmse <- c()
gamlss_r2 <- c()

for (i in 1:num_folds) {
  cat("Fold:", i, "\n")
  
  # Separar datos de entrenamiento y validación
  valid_idx <- folds[[i]]
  train_data <- df_train[-valid_idx, ]
  valid_data <- df_train[valid_idx, ]
  
  # XGBoost
  train_xgb <- train_data[, vars_xgb]
  valid_xgb <- valid_data[, vars_xgb]
  
  train_xgb_matrix <- predict(dummies_final, newdata = train_xgb)
  valid_xgb_matrix <- predict(dummies_final, newdata = valid_xgb)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_xgb_matrix), label = train_data$log_Salary)
  dvalid <- xgb.DMatrix(data = as.matrix(valid_xgb_matrix), label = valid_data$log_Salary)
  
  modelo_xgb <- xgb.train(params = params_xgb, data = dtrain, nrounds = 500,
                          watchlist = list(train = dtrain, valid = dvalid),
                          early_stopping_rounds = 50, verbose = 0)
  
  pred_xgb <- predict(modelo_xgb, dvalid)
  xgb_rmse <- c(xgb_rmse, calc_rmse(valid_data$log_Salary, pred_xgb))
  xgb_r2 <- c(xgb_r2, calc_r2(valid_data$log_Salary, pred_xgb))

  
  # Variables para XGBoost
  vars_xgb <- c("Height", "Age", "Agility", "Heading", "Finishing", "Jumping_Reach",
                "Dribbling", "Balance", "Anticipation", "Acceleration",
                "Tackling", "Stamina", "Pace", "Passing", "Positioning", 
                "Strength", "Vision", "log_AT.Gls", "Europa", "Posicion_dummy",
                "National_Team_dummy", "GolesPorPartido", "AsistenciasPorPartido", 
                "Preferred_Foot_dummy", "log_Amarillas", "expulsion", 
                "X5_grandes_ligas", "log_AT.Apps")
    
  # GAMLSS
  modelo_gamlss <- gamlss(log_Salary ~ Height + Age + Agility + Finishing + 
                            Dribbling + Balance + Anticipation + Acceleration + Tackling + 
                            Stamina + Pace + Passing + Positioning + Strength + log_AT.Gls + 
                            Posicion_dummy + National_Team_dummy + GolesPorPartido + 
                            AsistenciasPorPartido + log_Amarillas + X5_grandes_ligas, 
                          sigma.formula = ~ X5_grandes_ligas + Age + Posicion_dummy, 
                          data = train_data, family = NO)
  pred_gamlss <- predict(modelo_gamlss, newdata = valid_data, type = "response")
  gamlss_rmse <- c(gamlss_rmse, calc_rmse(valid_data$log_Salary, pred_gamlss))
  gamlss_r2 <- c(gamlss_r2, calc_r2(valid_data$log_Salary, pred_gamlss))
}

# Evaluación en Test Set


test_xgb <- df_test[, vars_xgb]
test_xgb_matrix <- predict(dummies_final, newdata = test_xgb)
dtest <- xgb.DMatrix(data = as.matrix(test_xgb_matrix))
pred_xgb_test <- predict(modelo_xgb_final, dtest)

xgb_rmse_test <- calc_rmse(df_test$log_Salary, pred_xgb_test)
xgb_mae_test <- calc_mae(df_test$log_Salary, pred_xgb_test)
xgb_mape_test <- calc_mape(df_test$log_Salary, pred_xgb_test)
xgb_r2_test <- calc_r2(df_test$log_Salary, pred_xgb_test)

df_test$pred_log_Salary <- NULL ## Ajustamos para tener las predicciones GAMLSS 
df_test$pred_log_Salary_mm <- NULL ## Ajustamos para tener las predicciones GAMLSS 

pred_gamlss_test <- predict(modelo_gamlss, newdata = df_test, type = "response")
gamlss_rmse_test <- calc_rmse(df_test$log_Salary, pred_gamlss_test)
gamlss_mae_test <- calc_mae(df_test$log_Salary, pred_gamlss_test)
gamlss_mape_test <- calc_mape(df_test$log_Salary, pred_gamlss_test)
gamlss_r2_test <- calc_r2(df_test$log_Salary, pred_gamlss_test)

#  Mostrar Resultados


cat("\nResultados en el Test Set:\n")
cat("\nXGBoost:\nRMSE:", xgb_rmse_test, "MAE:", xgb_mae_test, "MAPE:", xgb_mape_test, "% R²:", xgb_r2_test, "\n")
cat("\nGAMLSS:\nRMSE:", gamlss_rmse_test, "MAE:", gamlss_mae_test, "MAPE:", gamlss_mape_test, "% R²:", gamlss_r2_test, "\n")


############################################
######### MODELO DE CLASIFICACION ##########
############################################

# Fijar semilla para reproducibilidad
set.seed(187)

# Seleccionar solo las columnas requeridas
df_clasificacion <- df_merged %>% dplyr::select(all_of(selected_vars), Salary)

# # Aplicar k-means con 2 clusters ## Dejamos con k-means para futuros experimentos con mas datos y regresiones multinomiales
# kmeans_result <- kmeans(df_clasificacion$Salary, centers = 2, nstart = 25)
# 
# # Calcular la mediana de los salarios en cada cluster
# median_cluster1 <- median(df_clasificacion$Salary[kmeans_result$cluster == 1])
# median_cluster2 <- median(df_clasificacion$Salary[kmeans_result$cluster == 2])
# 
# # Determinar cuál cluster corresponde a "Bajo" y cuál a "Alto"
# cluster_bajo <- ifelse(median_cluster1 < median_cluster2, 1, 2)
# cluster_alto <- ifelse(cluster_bajo == 1, 2, 1)
# 
# # Asignar clases basadas en los clusters corregidos
# df_clasificacion$Salary_Class <- ifelse(kmeans_result$cluster == cluster_bajo, "Bajo", "Alto")
# 
# # Convertir a factor asegurando el orden correcto
# df_clasificacion$Salary_Class <- factor(df_clasificacion$Salary_Class, levels = c("Bajo", "Alto"))
# 
# # Verificar la distribución
# table(df_clasificacion$Salary_Class)

 percentil_7 <- quantile(df_clasificacion$Salary, 0.7)  # Calcular el percentil 7
 df_clasificacion$Salary_Class <- ifelse(df_clasificacion$Salary <= percentil_7, "Bajo", "Alto")
 df_clasificacion$Salary_Class <- factor(df_clasificacion$Salary_Class, levels = c("Bajo", "Alto"))  # Asegurar que es factor

# Dividir en conjunto de entrenamiento y prueba
train_indices <- sample(seq_len(nrow(df_clasificacion)), size = round(0.7 * nrow(df_clasificacion)), replace = FALSE)
df_train <- df_clasificacion[train_indices, ]
df_test <- df_clasificacion[-train_indices, ]
df_test$Salary_Class <- relevel(df_test$Salary_Class, ref = "Alto")

#Aplicar Oversampling con upSample para Árbol de Decisión, Regresión Logística y Random Forest (OS)
df_train_balanced_os <- upSample(x = df_train %>% dplyr::select(-Salary_Class), 
                                 y = df_train$Salary_Class)
colnames(df_train_balanced_os)[ncol(df_train_balanced_os)] <- "Salary_Class"

#Aplicar ROSE para Random Forest (ROSE)
df_train_balanced_rf_rose <- ROSE(Salary_Class ~ ., data = df_train, seed = 187)$data

# Crear dataframes para contar la cantidad de clases
df_balance_original <- as.data.frame(table(df_train$Salary_Class))
df_balance_os <- as.data.frame(table(df_train_balanced_os$Salary_Class))
df_balance_rose <- as.data.frame(table(df_train_balanced_rf_rose$Salary_Class))

# Agregar etiquetas para cada dataset
df_balance_original$Dataset <- "Original"
df_balance_os$Dataset <- "Oversampling"
df_balance_rose$Dataset <- "ROSE"

# Combinar los datos
df_balance_total <- rbind(df_balance_original, df_balance_os, df_balance_rose)
colnames(df_balance_total) <- c("Clase", "Cantidad", "Dataset")

# Gráfico de barras
ggplot(df_balance_total, aes(x = Clase, y = Cantidad, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribución de Clases Antes y Después del Balanceo", x = "Clase", y = "Cantidad") +
  theme_minimal()


# Definir fórmula para clasificación binaria
formula_class <- as.formula(paste("Salary_Class ~", paste(setdiff(selected_vars, c("Salary")), collapse = " + ")))

# Árbol de Decisión (Oversampling)

dev.off()
modelo_arbol <- rpart(formula_class, data = df_train_balanced_os, method = "class")
rpart.plot(modelo_arbol, main = "Árbol de Decisión")

df_test$pred_Salary_Class_arbol <- predict(modelo_arbol, newdata = df_test, type = "class")
df_test$prob_Salary_Class_arbol <- predict(modelo_arbol, newdata = df_test, type = "prob")[, 2]  
tabla_arbol <- confusionMatrix(df_test$pred_Salary_Class_arbol, df_test$Salary_Class)

# Regresión Logística con Lasso (Oversampling)

# Convertir variable objetivo a numérica
y_train <- as.numeric(df_train_balanced_os$Salary_Class) - 1

# Crear matriz de diseño sin la variable Age
X_train <- model.matrix(formula_class, data = df_train_balanced_os)[, -1]
X_train <- X_train[, !colnames(X_train) %in% "Age"]

# Ajustar modelo con validación cruzada
set.seed(187)
modelo_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)  # Lasso -> alpha = 1

# Obtener el mejor lambda encontrado
lambda_optimo <- modelo_lasso$lambda.min

# Modelo final con mejor lambda
modelo_logistico_lasso <- glmnet(X_train, y_train, family = "binomial", alpha = 1, lambda = lambda_optimo)

summary(modelo_logistico_lasso)

# Evaluar en el conjunto de prueba
X_test <- model.matrix(formula_class, data = df_test)[, -1]
X_test <- X_test[, !colnames(X_test) %in% "Age"]

df_test$prob_Salary_Class <- predict(modelo_logistico_lasso, newx = X_test, type = "response")
df_test$pred_Salary_Class <- ifelse(df_test$prob_Salary_Class > 0.7, "Alto", "Bajo")
df_test$pred_Salary_Class <- factor(df_test$pred_Salary_Class, levels = c("Bajo", "Alto"))

tabla_lasso <- confusionMatrix(df_test$pred_Salary_Class, df_test$Salary_Class)
print(tabla_lasso)
exp(coef(modelo_logistico_lasso))

## Evaluamos supuestos

#### 1 VIF

# Extraer coeficientes del modelo Lasso y convertirlos en matriz
coeficientes <- as.matrix(coef(modelo_logistico_lasso))

# Obtener las variables seleccionadas (excluyendo las que tienen coeficiente 0 y el intercepto)
variables_seleccionadas <- rownames(coeficientes)[coeficientes[,1] != 0]
variables_seleccionadas <- variables_seleccionadas[variables_seleccionadas != "(Intercept)"]

X_train_lasso <- as.data.frame(X_train[, variables_seleccionadas, drop = FALSE])
modelo_vif <- lm(formula = paste(variables_seleccionadas[1], "~", 
                                 paste(variables_seleccionadas[-1], collapse = " + ")), 
                 data = X_train_lasso)

# Calcular VIF con car::vif()
library(car)  # Asegúrate de tener el paquete car cargado
vif_values <- car::vif(modelo_vif)

print("Valores de VIF para cada variable seleccionada por Lasso:")
print(vif_values)

## Vemos ajuste (Homer-Lemeshow)
# Calcular probabilidades predichas en el conjunto de prueba
prob_predichas <- predict(modelo_logistico_lasso, newx = X_test, type = "response")

df_test$Salary_Class_Num <- as.numeric(df_test$Salary_Class) - 1

# Test de Hosmer-Lemeshow
hl_test <- hoslem.test(df_test$Salary_Class_Num, prob_predichas, g = 10)  # g = número de grupos
print(hl_test)

# Random Forest (Oversampling)

set.seed(187)
modelo_rf_os <- randomForest(formula_class, data = df_train_balanced_os, ntree = 100, importance = TRUE)

df_test$pred_Salary_Class_rf_os <- predict(modelo_rf_os, newdata = df_test)
df_test$prob_Salary_Class_rf_os <- predict(modelo_rf_os, newdata = df_test, type = "prob")[, 2]  
tabla_rf_os <- confusionMatrix(df_test$pred_Salary_Class_rf_os, df_test$Salary_Class)

# Random Forest (ROSE)
# --------------------
set.seed(187)
modelo_rf_rose <- randomForest(formula_class, data = df_train_balanced_rf_rose, ntree = 100, importance = TRUE)

df_test$pred_Salary_Class_rf_rose <- predict(modelo_rf_rose, newdata = df_test)
df_test$prob_Salary_Class_rf_rose <- predict(modelo_rf_rose, newdata = df_test, type = "prob")[, 2]  
tabla_rf_rose <- confusionMatrix(df_test$pred_Salary_Class_rf_rose, df_test$Salary_Class)

# Comparación de Modelos


# Calcular AUC-ROC para cada modelo
auc_arbol <- roc(df_test$Salary_Class, df_test$prob_Salary_Class_arbol)$auc
auc_lasso <- roc(df_test$Salary_Class, df_test$prob_Salary_Class)$auc
auc_rf_os <- roc(df_test$Salary_Class, df_test$prob_Salary_Class_rf_os)$auc
auc_rf_rose <- roc(df_test$Salary_Class, df_test$prob_Salary_Class_rf_rose)$auc

# Crear tabla de métricas incluyendo AUC-ROC
metricas_final <- rbind(
  data.frame(
    Modelo = "Árbol de Decisión (OS)", 
    Precisión = tabla_arbol$byClass["Precision"], 
    Exactitud = tabla_arbol$overall["Accuracy"], 
    Sensibilidad = tabla_arbol$byClass["Recall"], 
    F1 = tabla_arbol$byClass["F1"], 
    VPP = tabla_arbol$byClass["Pos Pred Value"], 
    VPN = tabla_arbol$byClass["Neg Pred Value"], 
    Especificidad = tabla_arbol$byClass["Specificity"], 
    `1-Especificidad` = 1 - tabla_arbol$byClass["Specificity"],
    AUC_ROC = auc_arbol
  ),
  data.frame(
    Modelo = "Regresión Logística (Lasso)", 
    Precisión = tabla_lasso$byClass["Precision"], 
    Exactitud = tabla_lasso$overall["Accuracy"], 
    Sensibilidad = tabla_lasso$byClass["Recall"], 
    F1 = tabla_lasso$byClass["F1"], 
    VPP = tabla_lasso$byClass["Pos Pred Value"], 
    VPN = tabla_lasso$byClass["Neg Pred Value"], 
    Especificidad = tabla_lasso$byClass["Specificity"], 
    `1-Especificidad` = 1 - tabla_lasso$byClass["Specificity"],
    AUC_ROC = auc_lasso
  ),
  data.frame(
    Modelo = "Random Forest (OS)", 
    Precisión = tabla_rf_os$byClass["Precision"], 
    Exactitud = tabla_rf_os$overall["Accuracy"], 
    Sensibilidad = tabla_rf_os$byClass["Recall"], 
    F1 = tabla_rf_os$byClass["F1"], 
    VPP = tabla_rf_os$byClass["Pos Pred Value"], 
    VPN = tabla_rf_os$byClass["Neg Pred Value"], 
    Especificidad = tabla_rf_os$byClass["Specificity"], 
    `1-Especificidad` = 1 - tabla_rf_os$byClass["Specificity"],
    AUC_ROC = auc_rf_os
  ),
  data.frame(
    Modelo = "Random Forest (ROSE)", 
    Precisión = tabla_rf_rose$byClass["Precision"], 
    Exactitud = tabla_rf_rose$overall["Accuracy"], 
    Sensibilidad = tabla_rf_rose$byClass["Recall"], 
    F1 = tabla_rf_rose$byClass["F1"], 
    VPP = tabla_rf_rose$byClass["Pos Pred Value"], 
    VPN = tabla_rf_rose$byClass["Neg Pred Value"], 
    Especificidad = tabla_rf_rose$byClass["Specificity"], 
    `1-Especificidad` = 1 - tabla_rf_rose$byClass["Specificity"],
    AUC_ROC = auc_rf_rose
  )
)

# Mostrar tabla de métricas con AUC-ROC
print(metricas_final)

print(tabla_arbol)
print(tabla_lasso)
print(tabla_rf_os)
print(tabla_rf_rose)

#  Curvas ROC
par(mfrow = c(2, 2))
plot.roc(roc(df_test$Salary_Class, df_test$prob_Salary_Class_arbol), col = "green", main = "ROC - Árbol de Decisión (OS)")
plot.roc(roc(df_test$Salary_Class, df_test$prob_Salary_Class), col = "red", main = "ROC - Regresión Logística (Lasso OS)")
plot.roc(roc(df_test$Salary_Class, df_test$prob_Salary_Class_rf_os), col = "blue", main = "ROC - Random Forest (OS)")
plot.roc(roc(df_test$Salary_Class, df_test$prob_Salary_Class_rf_rose), col = "purple", main = "ROC - Random Forest (ROSE)")

### Elegimos RF con OS

varImpPlot(modelo_rf_os, main = "Importancia de Variables - Random Forest (OS)")
