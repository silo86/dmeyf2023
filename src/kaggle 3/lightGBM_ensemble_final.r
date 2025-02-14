# Acá evaluar y escribir recomendación de recursos
# my_seeds : 100019,100043,100049,100057,100069
# Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")

# Defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()

# Nombre del experimento
PARAM$experimento <- "10seeds"
# Path donde se aloja el dataset (puede cargar su dataset preprocesado o puede hacerlo en el apartado de preprocesamiento de abajo)
PARAM$input$dataset <- "./datasets/dataset_exp.csv.gz"

# Meses donde se entrena el modelo
PARAM$input$training <- c( 201901, 201902, 201903, 201904, 201905, 202906,
  201907, 201908,201908, 201909, 201910, 201911, 201912, 202001, 202002,
  # hueco de la pandemia
  202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105,202106,202107)
# Mes donde aplico el modelo
PARAM$input$future <- c(202109)

# Defino parámetros:
cantidad_semillas = 10 
semillas <- as.integer(seq(10000, 100000, length.out = cantidad_semillas))

# Parámetros fijos obtenidos en la Optimización Bayesiana 
PARAM$finalmodel$num_iterations <- 675
PARAM$finalmodel$learning_rate <- 0.067
PARAM$finalmodel$feature_fraction <- 0.50
PARAM$finalmodel$min_data_in_leaf <- 6221
PARAM$finalmodel$num_leaves <- 433
PARAM$finalmodel$max_bin <- 33

# Aqui empieza el programa que voy a ejecutar para cada semilla
# Directorio de origen
setwd("~/buckets/b1/")

# Cargo el conjunto de datos
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# Configuro la variable target como binaria
# El criterio: POS = { BAJA+1, BAJA+2 }, NEG {CONTINUA}
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# Establezco qué datos usaré para entrenar
# Creo columna train con valor cero en todas sus filas
dataset[, train := 0L]

# Asigno un 1 a todas las filas correspondiente al foto_mes configurado en los parámetros de entrada
dataset[foto_mes %in% PARAM$input$training, train := 1L]

dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0("./exp/", PARAM$experimento, "/"))
# Dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

dapply <- dataset[foto_mes == PARAM$input$future]
predicciones <- dapply[, list(numero_de_cliente, foto_mes)]
for (semilla in semillas) {
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
      seed = semilla 
  )
  )


  # Aplico el modelo a los nuevos datos
  prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # Agrego columna con las predicciones de cada semilla
  col_name <- paste0("semilla_", semilla)
  predicciones[, (col_name) := prediccion] 
  cat("\n\nSemilla número", semilla , "hora:", Sys.time(), "\n")
 
}

# Guardo el archivo de probs
archivo_salida <- paste0(PARAM$experimento, "_predicciones_semillas.csv")
fwrite(predicciones, file = archivo_salida, sep = ",")

predicciones$proba_ensemble <- rowMeans(predicciones[, .SD, .SDcols = -(1:2)])

cat("\n\nEnsemble generado, hora:", Sys.time(), "\n")
# Ordeno por probabilidad descendente
setorder(predicciones, -proba_ensemble)
# Genero archivos variando la cantidad de estímulos
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  predicciones[, Predicted := 0L]
  predicciones[1:envios, Predicted := 1L]

  fwrite(predicciones[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado, hora:", Sys.time(),"\n")