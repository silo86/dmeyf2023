# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM
# my_seeds : 100019,100043,100049,100057,100069



# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
library(dplyr)
library(randomForest)
library(dendextend)


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

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
#dataset[, foto_mes_num := as.numeric(sub("^([0-9]{4})([0-9]{2})$", "\\1\\2", foto_mes)) - 201900]#cami
subset <- dataset[dataset$clase_ternaria == "BAJA+2", ]
subset <- subset[!duplicated(subset$numero_de_cliente), ]



# Filtra los registros de dataset1 que cumplen con la condición
dataset_2 <- subset(dataset, numero_de_cliente %in% subset$numero_de_cliente & foto_mes <= subset$foto_mes)


# Identify numeric columns
numeric_columns <- names(subset)[sapply(subset, is.numeric)]

# # Replace missing values with the median for numeric columns
# for (col in numeric_columns) {
#   subset[is.na(subset[[col]]), (col) := median(subset[[col]], na.rm = TRUE)]
# }
# 
# # Identify character columns
# character_columns <- names(subset)[sapply(subset, is.character)]
# 
# # Impute missing values with random sampling from existing values for character columns
# #library(dplyr)
# 
# # Impute missing values in character columns
# 
# for (col in character_columns) {
#   # Find the non-missing values in the column
#   non_missing_values <- na.omit(subset$col)
#   
#   # If there are missing values in the column
#   if (length(non_missing_values) > 0) {
#     # Impute the missing values with a random sample of the non-missing values
#     imputed_values <- sample_frac(non_missing_values, 1, replace = TRUE)
#     
#     # Mutate the subset to replace the missing values
#     subset <- mutate(subset, col = replace_na(col, imputed_values))
#   }
# }


# Aplico Random Forest para clustering
subset.roughfix <- na.roughfix(subset)
rf.fit <- randomForest(x = subset.roughfix , y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE,na.action=na.roughfix)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=7)
# merge with subset
dataset_1 <- cbind(subset, rf.cluster)

fwrite(dataset_1, file = "dataset_2.csv", sep = ",")

result <- aggregate(. ~ rf.cluster, data = dataset_1, FUN = median)
result_table <- dcast(result, rf.cluster ~ variable_column_name)
result_table <- t(result_table)


