# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM
# my_seeds : 100019,100043,100049,100057,100069



# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

#install.packages("mice")
library(mice)
require("data.table")
require("lightgbm")
library(dplyr)
library(randomForest)
library(dendextend)


PARAM <- list()
PARAM$experimento <- "custom_KA5240_lag6"

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

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
subset <- dataset[dataset$clase_ternaria == "BAJA+2", ]
subset <- subset[!duplicated(subset$numero_de_cliente), ]


# Identify numeric columns
#numeric_columns <- names(subset)[sapply(subset, is.numeric)]

# Replace missing values with the median for numeric columns
#for (col in numeric_columns) {
#  subset[is.na(subset[[col]]), (col) := median(subset[[col]], na.rm = TRUE)]
#}

# Identify character columns
#character_columns <- names(subset)[sapply(subset, is.character)]

# Impute missing values with random sampling from existing values for character columns
#library(dplyr)

# Impute missing values in character columns

#for (col in character_columns) {
  # Find the non-missing values in the column
#  non_missing_values <- na.omit(subset$col)
  
  # If there are missing values in the column
#  if (length(non_missing_values) > 0) {
    # Impute the missing values with a random sample of the non-missing values
#    imputed_values <- sample_frac(non_missing_values, 1, replace = TRUE)
    
    # Mutate the subset to replace the missing values
#    subset <- mutate(subset, col = replace_na(col, imputed_values))
#  }
#}


# Aplico Random Forest para clustering 
rf.fit <- randomForest(x = subset, y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE,na.action=na.roughfix)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=7)
print(rf.cluster)

#table(rf.cluster, iris$Species)


# Ajusta un modelo de Random Forest
#rf_model <- randomForest(subset)

# Obtiene las proximidades entre las observaciones
#proximidades <- proximity(rf_model)

# Calcula la matriz de distancias a partir de las proximidades
#dist_matrix <- as.dist(1 - proximidades)

# Realiza clustering jerárquico con la matriz de distancias
#hc <- hclust(dist_matrix, method = "complete")


