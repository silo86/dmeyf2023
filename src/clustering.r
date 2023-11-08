# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM
# my_seeds : 100019,100043,100049,100057,100069



# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection
#install.packages("ggplot2")
library("ggplot2")
require("data.table")
require("lightgbm")
library(dplyr)
library(randomForest)
library(dendextend)
library(data.table)
library(lubridate)

PARAM <- list()
PARAM$experimento <- "clustering_dataset"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"


PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105) # meses donde se entrena el modelo
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 100019

PARAM$finalmodel$num_iterations <- 4928
PARAM$finalmodel$learning_rate <- 0.0189943331895954
PARAM$finalmodel$feature_fraction <- 0.892623977897483
PARAM$finalmodel$min_data_in_leaf <- 785
PARAM$finalmodel$num_leaves <- 666


PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
#setwd("~/buckets/b1")
setwd("/Users/andres/Desktop/master/DM_EyF")

# cargo el dataset donde voy a entrenar

# cargo el dataset donde voy a entrenar

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
subset <- dataset[dataset$clase_ternaria == "BAJA+2", ]
subset <- subset[, !("clase_ternaria"), with = FALSE]
subset <- subset[!duplicated(subset$numero_de_cliente), ]
# Convierte las columnas de fecha a objetos Date


dataset$foto_mes <- as.Date(paste0(dataset$foto_mes, "01"), format = "%Y%m%d")
subset$foto_mes <- as.Date(paste0(subset$foto_mes, "01"), format = "%Y%m%d")


#Convertir a data.table
subset <- data.table(subset)
dataset <- data.table(dataset)

dataset_2 <- dataset[dataset$numero_de_cliente %in% subset$numero_de_cliente & dataset$foto_mes <= subset$foto_mes]




# Calculo la columna relative_month
dataset_2 <- dataset_2 %>%
  arrange(numero_de_cliente, foto_mes) %>%
  group_by(numero_de_cliente) %>%
  mutate(relative_month = as.numeric(interval(max(foto_mes), foto_mes) / months(1)))


######## Test#############
# dataset_2 %>%
#   filter(numero_de_cliente == 29199686) %>%
#   select(foto_mes, relative_month,clase_ternaria)
######## Test#############

# Escribo dataset_2
fwrite(dataset_2, file = "dataset_2.csv", sep = ",")

if (!exists("dataset_1")) {
  # Intenta leer dataset_1 desde el archivo CSV
  dataset_1 <- fread('dataset_1.csv', stringsAsFactors = TRUE)
}

if (!exists("dataset_1")) {
  # Si dataset_1 todavía no existe, ejecuta el código para crearlo
  subset$foto_mes <- as.numeric(subset$foto_mes)
  subset <- na.roughfix(subset)
  rf.fit <- randomForest(x = subset, y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE)
  hclust.rf <- hclust(as.dist(1 - rf.fit$proximity), method = "ward.D2")
  rf.cluster <- cutree(hclust.rf, k = 7)
  dataset_1 <- cbind(subset, rf.cluster)
  # Escribo dataset_1
  fwrite(dataset_1, file = "dataset_1.csv", sep = ",")
}


merged_dataset <- merge(dataset_2, dataset_1[, c("numero_de_cliente", "rf.cluster")], by.x = "numero_de_cliente", by.y = "numero_de_cliente", all.x = TRUE)



#result <- aggregate(. ~ rf.cluster, data = merged_dataset, FUN = mean)


#ggplot(merged_dataset, aes(relative_month, Master_delinquency, color = factor(rf.cluster))) +
#  geom_smooth()

#library(gridExtra)

# Crear un vector con los nombres de las columnas que deseas graficar (excepto 'relative_month')
#columns_to_plot <- colnames(merged_dataset)[-which(colnames(merged_dataset) == 'relative_month')]
#columns_to_plot <- c("Master_delinquency","Visa_mpagominimo")
columns_to_plot <- setdiff(
  colnames(merged_dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria", "rf.clusters", "clase_ternaria","relative_month")
)

# Inicializar una lista para almacenar las gráficas
plot_list <- list()

# Crear las gráficas y agregarlas a la lista
for (col in columns_to_plot) {
  p <- ggplot(merged_dataset, aes(relative_month, .data[[col]], color = factor(rf.cluster))) +
    geom_smooth() +
    labs(title = col)
  plot_list[[col]] <- p
}

# Guardar las gráficas en un archivo PDF
pdf("merged_dataset_plots.pdf", width = 8, height = 5)
do.call(grid.arrange, plot_list)
dev.off()




###### VERSION CLOUD

# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM
# my_seeds : 100019,100043,100049,100057,100069



# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection
#install.packages("ggplot2")
library("ggplot2")
require("data.table")
require("lightgbm")
library(dplyr)
library(randomForest)
library(dendextend)
library(data.table)
library(lubridate)

PARAM <- list()
PARAM$experimento <- "clustering_dataset"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"


PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105) # meses donde se entrena el modelo
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 100019

PARAM$finalmodel$num_iterations <- 4928
PARAM$finalmodel$learning_rate <- 0.0189943331895954
PARAM$finalmodel$feature_fraction <- 0.892623977897483
PARAM$finalmodel$min_data_in_leaf <- 785
PARAM$finalmodel$num_leaves <- 666


PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")
#setwd("/Users/andres/Desktop/master/DM_EyF")

# cargo el dataset donde voy a entrenar

# cargo el dataset donde voy a entrenar

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
subset <- dataset[dataset$clase_ternaria == "BAJA+2", ]
subset <- subset[, !("clase_ternaria"), with = FALSE]
subset <- subset[!duplicated(subset$numero_de_cliente), ]
# Convierte las columnas de fecha a objetos Date


dataset$foto_mes <- as.Date(paste0(dataset$foto_mes, "01"), format = "%Y%m%d")
subset$foto_mes <- as.Date(paste0(subset$foto_mes, "01"), format = "%Y%m%d")


#Convertir a data.table
subset <- data.table(subset)
dataset <- data.table(dataset)

dataset_2 <- dataset[dataset$numero_de_cliente %in% subset$numero_de_cliente & dataset$foto_mes <= subset$foto_mes]




# Calculo la columna relative_month
dataset_2 <- dataset_2 %>%
  arrange(numero_de_cliente, foto_mes) %>%
  group_by(numero_de_cliente) %>%
  mutate(relative_month = as.numeric(interval(max(foto_mes), foto_mes) / months(1)))


######## Test#############
# dataset_2 %>%
#   filter(numero_de_cliente == 29199686) %>%
#   select(foto_mes, relative_month,clase_ternaria)
######## Test#############

# Escribo dataset_2
fwrite(dataset_2, file = "dataset_2.csv", sep = ",")

if (!exists("dataset_1")) {
  # Intenta leer dataset_1 desde el archivo CSV
  dataset_1 <- fread('dataset_1.csv', stringsAsFactors = TRUE)
}

if (!exists("dataset_1")) {
  # Si dataset_1 todavía no existe, ejecuta el código para crearlo
  subset$foto_mes <- as.numeric(subset$foto_mes)
  subset <- na.roughfix(subset)
  rf.fit <- randomForest(x = subset, y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE)
  hclust.rf <- hclust(as.dist(1 - rf.fit$proximity), method = "ward.D2")
  rf.cluster <- cutree(hclust.rf, k = 7)
  dataset_1 <- cbind(subset, rf.cluster)
  # Escribo dataset_1
  fwrite(dataset_1, file = "dataset_1.csv", sep = ",")
}


merged_dataset <- merge(dataset_2, dataset_1[, c("numero_de_cliente", "rf.cluster")], by.x = "numero_de_cliente", by.y = "numero_de_cliente", all.x = TRUE)



#result <- aggregate(. ~ rf.cluster, data = merged_dataset, FUN = mean)


#ggplot(merged_dataset, aes(relative_month, Master_delinquency, color = factor(rf.cluster))) +
#  geom_smooth()

# Obtén las columnas a graficar
columns_to_plot <- setdiff(
  colnames(merged_dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria", "rf.clusters", "clase_ternaria", "relative_month")
)

# Inicializa el archivo PDF
pdf("merged_dataset_plots.pdf", width = 8, height = 5)

# Crea y guarda un gráfico en cada página
for (col in columns_to_plot) {
  p <- ggplot(merged_dataset, aes(relative_month, .data[[col]], color = factor(rf.cluster))) +
    geom_smooth() +
    labs(title = col)
  print(p)  # Imprime el gráfico en el PDF
  cat("\n\n")  # Añade una página en blanco
}

# Cierra el archivo PDF
dev.off()



