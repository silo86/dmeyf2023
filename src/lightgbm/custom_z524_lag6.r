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

# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "custom_KA5240_lag_seis"

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

#--------------------------------------

# Define the columns to process
campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

# Function to replace zeros
replace_zeros <- function(column) {
  non_na_values <- column[!is.na(column)]
  if (length(non_na_values) > 0 && all(non_na_values == 0)) {
    print(column)
    column[column == 0] <- NaN
  }
  return(column)
}
# Loop through each column
for (campo in campos_buenos) {
  dataset[, (campo) := replace_zeros(get(campo)), by = foto_mes]
}


#--------------------------------------
#####Calculo Lag

# Define the columns to calculate lags for
lagged_columns <- colnames(dataset)[!(colnames(dataset) %in% c("numero_de_cliente", "foto_mes", "clase_ternaria"))]

dataset <- dataset %>%
  arrange(numero_de_cliente, foto_mes) %>%
  group_by(numero_de_cliente) %>%
  mutate(across(all_of(lagged_columns),
  list(Lag1 = ~lag(.x, 1), Lag2 = ~lag(.x, 2), Lag3 = ~lag(.x, 3), Lag4 = ~lag(.x, 4), Lag5 = ~lag(.x, 5), Lag6 = ~lag(.x, 6)), .names="lagged_{.col}_Lag{.fn}")) 



#--------------------------------------

#--------------------------------------
dataset <- as.data.table(dataset) # convert to data table
# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]


#Error in `:=`(clase01, ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"),  : 
#                                Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    max_bin = PARAM$finalmodel$max_bin,
    learning_rate = PARAM$finalmodel$learning_rate,
    num_iterations = PARAM$finalmodel$num_iterations,
    num_leaves = PARAM$finalmodel$num_leaves,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    seed = PARAM$finalmodel$semilla
  )
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)
#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 13000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")








