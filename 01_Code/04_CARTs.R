
## Configuración inicial
# Nuevas variables para CARS

train_CARS <- train
test_CARS <- test

head(train_CARS)
head(test_CARS)

train_CARS <- droplevels(train_CARS) # Drop niveles no usados

set.speed(2025)

inTrain <- createDataPartition(
  y = train_CARS$Pobre, # La variable dependiente u objetivo 
  p = .7, # Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE
)

train_dum <- train_CARS[ inTrain,]
test_dum  <- train_CARS[-inTrain,] 

table(train_dum$Pobre)
table(test_dum$Pobre)

# Arbol de clasificación


complex_tree <- rpart(Pobre ~ ., 
                      data = train_dum , 
                      method = "class",
                      cp = 0)

minbucket_tree <- rpart::rpart(Pobre ~ ., 
                      data = train_dum , 
                      method = "class",
                      minbucket = 5,
                      cp = 0)

arbol  <-  rpart(Pobre ~ ., 
                 data = train_dum ,
                 method = "class"
                 )

arbol$control$cp 

prp(arbol, 
    under = TRUE, 
    branch.lty = 2, 
    yesno = 1, 
    faclen = 0, 
    varlen = 15,
    box.palette = "-RdYlGn")


# Se guard el ROC-AUC para el arbol

Pobre_dum <- ifelse(test_dum$Pobre=="Yes",1,0) 
pred_prob    <- predict(arbol, newdata = test_dum, type = "prob") 
aucval_arbol <- Metrics::auc(actual =Pobre_dum,predicted = pred_prob[,2]) #calcular el AUC

# Por validación cruzada

# fiveStats <- function(...) {
#   c(
#     twoClassSummary(...),
#     defaultSummary(...)
#   )
# }
# 
# ctrl<- trainControl(method = "cv",
#                     number = 5,
#                     summaryFunction = fiveStats, # nuestra función
#                     classProbs = TRUE,
#                     verbose = TRUE,
#                     savePredictions = TRUE)
# 
# grid <- expand.grid(cp = seq(0, 0.1, 0.001))

f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = lev[1])
  recall <- sensitivity(data$pred, data$obs, positive = lev[1])
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  out <- c(F1 = f1)
  return(out)
}

ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = f1Summary,
  classProbs = TRUE,
  savePredictions = TRUE
)

# Evaluar

grid <- expand.grid(cp = seq(0, 0.1, 0.1))

system.time({
cv_tree <- train(
    Pobre ~ ., 
    data = train_CARS,
    method = "rpart",
    trControl = ctrl,
    tuneGrid = grid,
    metric = "F1"
  )
})

cv_tree
