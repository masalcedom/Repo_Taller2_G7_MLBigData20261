
## Configuración inicial


str(train |> select(cat_educHead))

## Preprocesamiento

cat_educ_labels <- c(
  "0" = "No sabe",
  "1" = "Ninguno",
  "2" = "Preescolar",
  "3" = "Primaria",
  "4" = "Secundaria",
  "5" = "Media",
  "6" = "Universitaria"
)


train <- train %>%
  mutate(
    Pobre = factor(Pobre, levels = c("Yes", "No")),
    Dominio = factor(Dominio),
    cat_educHead = case_when(
      cat_educHead %in% c("No sabe", "Preescolar") ~ "Ninguno",
      TRUE ~ cat_educHead
    ),
    cat_educHead = factor(
      cat_educHead,
      levels = c("Ninguno", "Primaria", "Secundaria", "Media", "Universitaria")
    )
  )

test <- test %>%
  mutate(
    Dominio = factor(Dominio, levels = levels(train$Dominio)),
    cat_educHead = recode(as.character(cat_educHead), !!!cat_educ_labels),
    cat_educHead = case_when(
      cat_educHead %in% c("No sabe", "Preescolar") ~ "Ninguno",
      TRUE ~ cat_educHead
    ),
    cat_educHead = factor(cat_educHead, levels = levels(train$cat_educHead))
  )


 
str(train) 

table(train$Pobre)

head(train)

nrow(train)/(nrow(test)+nrow(train))

nrow(test)/(nrow(test)+nrow(train))

# Modelo benchmark
 

multiStats <- function(...){
  c(
    caret:::twoClassSummary(...),
    caret:::defaultSummary(...),
    caret:::prSummary(...)
  )
}


set.seed(2025)
train1 <- train
train1$Pobre <- ifelse(train1$Pobre == "Yes", 1, 0)

dummies <- dummyVars(~ ., data = train1) 
#train1 <- dummyVars(~ ., data = train1)
#train1 <- predict(dummies, newdata = data)

# dummies <- dummyVars(~ ., data = train1)
data_train_dum <- predict(dummies, newdata = train1)


train_pobre <- createFolds(train1$Pobre, k = 5, returnTrain = TRUE)

#esto se puede eliminar
# en_grid <- expand.grid(
#   alpha  = seq(0, 1, by = 0.1),
#   lambda = 10^seq(-3, 3, length = 5)
# )
###-------------------



control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

modelo_lpm <- train(
  Pobre ~ ., 
  data = data_train_dum,
  method = "lm",
  trControl = control
)


pred_df <- modelo_lpm$pred
head(pred_df)

pred_df$pred_class <- ifelse(pred_df$pred > 0.5, 1, 0)

confusionMatrix(
  factor(pred_df$pred_class),
  factor(pred_df$obs)
)

pred_df <- modelo_lpm$pred

## Se tiene entonces que para evaluar los threshold será:


evaluar_threshold <- function(th, data) {
  pred_class <- ifelse(data$pred > th, 1, 0)
  
  cm <- confusionMatrix(
    factor(pred_class),
    factor(data$obs),
    positive = "1"
  )
  
  c(
    threshold = th,
    Accuracy = cm$overall["Accuracy"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    F1 = cm$byClass["F1"]
  )
}

##  Probar tresholds

thresholds <- seq(0, 1, by = 0.01)

resultados <- t(sapply(thresholds, evaluar_threshold, data = pred_df))
resultados <- as.data.frame(resultados)

##  Mejor F1
best_f1 <- resultados[which.max(resultados$F1), ]
best_f1

## El mejor treshold es 

best_th <- best_f1$threshold

pred_final <- ifelse(pred_df$pred > best_th, 1, 0)

confusionMatrix(
  factor(pred_final),
  factor(pred_df$obs)
)


# plot 
plot(resultados$threshold, resultados$F1, type = "l", col = "blue")
lines(resultados$threshold, resultados$Accuracy, col = "red")
legend("bottomright", legend = c("F1", "Accuracy"), col = c("blue", "red"), lty = 1)

# Predicciones de validación cruzada
head(modelo_lpm$pred)

# Usar ROC

library(pROC)
roc_obj <- roc(pred_df$obs, pred_df$pred)
coords(roc_obj, "best", ret = "threshold")

## Creación de Kaggle

# Evaluación en el modelo de test

test1 <- test 

test1$Pobre <- NA

data_test_dum <- predict(dummies, newdata = test1)

pred_test <- predict(modelo_lpm, newdata = data_test_dum)

pred_class <- ifelse(pred_test > 0.2495515 , 1, 0)

test1$Pobre <- pred_class

predictSample_LM <- test1%>%
    select(id, Pobre)

head(predictSample_LM)  

# Elaborar el .csv para kagle
# 
# lambda_str <- gsub(
#   "\\.", "_", 
#   as.character(round(model1$bestTune$lambda, 4)))

#alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name <- paste0(
  "LM_PR", 
  ".csv"
) 

write.csv(predictSample_LM ,name, row.names = FALSE)




