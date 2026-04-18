
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

# Cración de la grilla de hiperparámetros para el 
set.seed(1999)
folds <- createFolds(train$Pobre, k = 5, returnTrain = TRUE)

en_grid <- expand.grid(
  alpha  = seq(0, 1, by = 0.1),
  lambda = 10^seq(-3, 3, length = 5)
)

#creación del control para el entrenamiento

ctrl_accuracy <- trainControl(
  method = "cv",                 # usamos validación cruzada
  number = 5,                    # dividimos los datos en 5 folds
  index = folds,                 # reutilizamos los mismos folds
  classProbs = TRUE,             # el modelo debe calcular probabilidades de clase
  savePredictions = "final",      # guarda predicciones solo del mejor tune
  summaryFunction = multiStats,
  allowParallel = TRUE
)


system.time(
  enet_bench <- train(
    Pobre ~ .,
    data = train,
    metric = "Accuracy",
    method = "glmnet",
    trControl = ctrl_accuracy,
    tuneGrid = en_grid,
  )
)
enet_bench

## Explorando con metrica F1

## Se crea el control para la métrica con F1

  ctrl_f1 <- trainControl(
    method = "cv",
    number = 5,
    index = folds,
    classProbs = TRUE,
    summaryFunction = multiStats,
    savePredictions = "final",
    verboseIter = TRUE
  )

# Elastic_net para la métrica de F
set.seed(2025)
system.time(
  elastic_net <- train(
    Pobre ~ .,
    data = train,
    method = "glmnet",
    family = "binomial",
    tuneGrid = en_grid,
    trControl = ctrl_f1,
    metric = "F"        # seleccionamos F1
  )
)

##elastic_net 

#valore reales 
yhat <- elastic_net$pred$obs[elastic_net$pred$lambda==elastic_net$bestTune$lambda]
#Probabilidades predichas por el modelo
phat <- elastic_net$pred$Yes[elastic_net$pred$lambda==elastic_net$bestTune$lambda]

## Se realiza el procedimiento para encontrar elastic_net con Elastic Net + un threshold óptimo PR
roc_elastic_net <-pROC::roc(
  response = yhat,  # Valores reales de la variable objetivo
  predictor = phat, # Probabilidades predichas por el modelo
  levels = c("No", "Yes"),  # referencia control y caso positivo (No = negativo, Yes = positivo)
  direction = "<")  # "<" significa que "Yes" es positivo

threshold_pr_metrics <- function(pred_df,
                                 prob_col = "Yes",
                                 positive = "Yes",
                                 thresholds = seq(0.05, 0.95, by = 0.01)) {
  negative <- setdiff(levels(pred_df$obs), positive)

  map_dfr(thresholds, function(threshold) {
    predicted_class <- factor(
      ifelse(pred_df[[prob_col]] >= threshold, positive, negative),
      levels = levels(pred_df$obs)
    )

    cm <- confusionMatrix(
      data = predicted_class,
      reference = pred_df$obs,
      positive = positive,
      mode = "prec_recall"
    )

    precision <- unname(cm$byClass["Precision"])
    recall <- unname(cm$byClass["Recall"])
    f1 <- unname(cm$byClass["F1"])

    precision <- ifelse(is.na(precision), 0, precision)
    recall <- ifelse(is.na(recall), 0, recall)
    f1 <- ifelse(is.na(f1), 0, f1)

    tibble(
      threshold = threshold,
      precision = precision,
      recall = recall,
      F1 = f1
    )
  })
}

# pr_cutoffs <- threshold_pr_metrics(elastic_oof)
pr_cutoffs <- data.frame(
  pROC::coords(roc_elastic_net,
               seq(0, 1, length = 100),
               ret = c('threshold', 'precision', 'recall'))
)
head(pr_cutoffs)

## Se agrega la columna del F1
pr_cutoffs<- pr_cutoffs  %>% mutate(F1=(2*precision*recall)/(precision+recall))
pr_cutoffs
## Se escoge el cutoff que máximiza el F1
best_cutoff <- pr_cutoffs %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff

# plot(pr_cutoffs)


## Graficar solo el F1

ggplot(pr_cutoffs, aes(x = threshold, y = F1)) +
  geom_line(color = "#1B175E", linewidth = 1) +
  geom_vline(
    xintercept = best_cutoff$threshold,
    color = "#b22222",
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = best_cutoff$threshold,
    y = best_cutoff$F1,
    label = paste0("Threshold óptimo = ", round(best_cutoff$threshold, 2)),
    fill = "white"
  ) +
  labs(
    title = "F1 según el punto de corte",
    x = "Threshold",
    y = "F1"
  ) +
  theme_minimal()

## Trade off Precision Recall

ggplot(pr_cutoffs, aes(x = recall, y = precision, color = F1)) +
  geom_path(linewidth = 1) +
  geom_point(
    data = best_cutoff,
    size = 3,
    color = "#b22222"
  ) +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Curva Precision-Recall para el mejor Elastic Net",
    x = "Recall",
    y = "Precision",
    color = "F1"
  ) +
  theme_minimal()

## Comparación de resultados

elastic_oof <- elastic_net$pred %>%
  filter(
    alpha == elastic_net$bestTune$alpha,
    lambda == elastic_net$bestTune$lambda
  ) %>%
  mutate(
    pred_05 = factor(
      ifelse(Yes >= 0.5, "Yes", "No"),
      levels = levels(obs)
    ),
    pred_pr = factor(
      ifelse(Yes >= best_cutoff$threshold, "Yes", "No"),
      levels = levels(obs)
    )
  )

cm_en_05 <- confusionMatrix(
  data = elastic_oof$pred_05,
  reference = elastic_oof$obs,
  positive = "Yes",
  mode = "prec_recall"
)

cm_en_pr <- confusionMatrix(
  data = elastic_oof$pred_pr,
  reference = elastic_oof$obs,
  positive = "Yes",
  mode = "prec_recall"
)

bind_rows(
  tibble(
    strategy = "Elastic Net con threshold 0.5",
    threshold = 0.5,
    precision = unname(cm_en_05$byClass["Precision"]),
    recall = unname(cm_en_05$byClass["Recall"]),
    F1 = unname(cm_en_05$byClass["F1"])
  ),
  tibble(
    strategy = "Elastic Net con threshold óptimo PR",
    threshold = best_cutoff$threshold,
    precision = unname(cm_en_pr$byClass["Precision"]),
    recall = unname(cm_en_pr$byClass["Recall"]),
    F1 = unname(cm_en_pr$byClass["F1"])
  )
)




## Rebalanceo de clases 

pos_weight <- sum(train$Pobre == "No") / sum(train$Pobre == "Yes")

case_weights <- ifelse(train$Pobre == "Yes", pos_weight, 1)

## Utilización de elastic NET con peso de clase

set.seed(1999)
elastic_net_weighted <- train(
  Pobre ~ .,
  data = train,
  metric = "F",
  method = "glmnet",
  trControl = ctrl_f1,
  family = "binomial",
  weights = case_weights,
  tuneGrid = en_grid
)

## Utilización de Downsampling o Submuestreo

ctrl_f1_down <- trainControl(
  method = "cv",
  number = 5,
  index = folds,
  classProbs = TRUE,
  summaryFunction = caret:::prSummary,
  savePredictions = "final",
  sampling = "down"
)

set.seed(2025)
elastic_net_down <- train(
  Pobre ~ .,
  data = train,
  metric = "F",
  method = "glmnet",
  trControl = ctrl_f1_down,
  family = "binomial",
  tuneGrid = en_grid
)

elastic_net_down

## Utilización de upsampling o sobremuestreo

ctrl_f1_up <- trainControl(
  method = "cv",
  number = 5,
  index = folds,
  classProbs = TRUE,
  summaryFunction = caret:::prSummary,
  savePredictions = "final",
  sampling = "up"
)

set.seed(2025)
elastic_net_up <- train(
  Pobre ~ .,
  data = train,
  metric = "F",
  method = "glmnet",
  trControl = ctrl_f1_up,
  family = "binomial",
  tuneGrid = en_grid
)

elastic_net_up

#Recopilación de todos los datos

extract_best_en_metrics <- function(model, strategy, threshold = 0.5) {
  best_row <- model$results %>%
    inner_join(model$bestTune, by = c("alpha", "lambda")) %>%
    slice(1)
  
  tibble(
    strategy = strategy,
    alpha = best_row$alpha,
    lambda = best_row$lambda,
    threshold = threshold,
    precision = best_row$Precision,
    recall = best_row$Recall,
    F1 = best_row$F
  )
}

en_comparison <- bind_rows(
  extract_best_en_metrics(elastic_net, "Elastic Net tuneado con F1"),
  tibble(
    strategy = "Elastic Net + threshold óptimo PR",
    alpha = elastic_net$bestTune$alpha,
    lambda = elastic_net$bestTune$lambda,
    threshold = best_cutoff$threshold,
    precision = best_cutoff$precision,
    recall = best_cutoff$recall,
    F1 = best_cutoff$F1
  ),
  extract_best_en_metrics(elastic_net_weighted, "Elastic Net + sample weighting"),
  extract_best_en_metrics(elastic_net_down, "Elastic Net + downsampling"),
  extract_best_en_metrics(elastic_net_up, "Elastic Net + upsampling")
) %>%
  arrange(desc(F1))

en_comparison %>%
  mutate(
    across(c(alpha, lambda, threshold, precision, recall, F1), round, 3)
  )

##Preparación de envío a Kagel

enet_bench_yhat <- test |>
  mutate(
    pobre = ifelse(predict(enet_bench, newdata = test) == "Yes", 1, 0)
  ) |>
  select(id, pobre)



# enet_bench_yhat <- test |>
#   mutate(
#     pobre_lab = caret:::predict.train(enet_bench,
#                                       newdata = test,
#                                       type = "raw")
#   ) |>
#   select(id,pobre_lab) |>
#   mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
#   select(id, pobre)

probs <- predict(enet_bench, newdata = test, type = "prob")$Yes

enet_bench_yhat <- test |>
  mutate(
    pobre = ifelse(probs > best_cutoff[1,1], 1, 0)
  ) |>
  select(id, pobre)


enet_bench_yhat <- test |>
  mutate(
    pobre = ifelse(probs > best_cutoff[1,1], 1, 0)
  ) |>
  select(id, pobre)

test |> mutate(
  pobre_lab = caret:::predict.train(enet_bench,
                                    test,
                                    'prob')$Yes > best_cutoff[1,1]
)|>
  select(id,pobre_lab) |>
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre)

head(enet_bench_yhat)

## Creación de archivo .csv para subir a Kaggle

lambda_str <- gsub(
  "\\.", "_",
  as.character(round(enet_bench$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(enet_bench$bestTune$alpha))

name <- paste0(
  "EN_PR_lambda_", lambda_str,
  "_alpha_" , alpha_str,
  ".csv"
)

write.csv(enet_bench_yhat,name, row.names = FALSE)

## Linear_R
train1 <- train 

train1$Pobre <- ifelse(train1$Pobre == "Yes", 1, 0)

dummies <- dummyVars(~ ., data = train1)

data_train_dum <- predict(dummies, newdata = train1)


train_transf <- predict(dummies, newdata = train1)

train_transf <- as.data.frame(train_transf)
#train_pobre <- createFolds(train1$Pobre, k = 5, returnTrain = TRUE)

data_train_dum <- createFolds(train1$Pobre, k = 5, returnTrain = TRUE)

## Se crea el control para el método LM

control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

## Se crea el benchmark para la evaluación del modelo
train_transf$Pobre <- train1$Pobre

modelo_lpm <- train(
  Pobre ~ ., 
  data =train_transf,
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


##  Probar tresholds

thresholds <- seq(0, 1, by = 0.01)

resultados <- t(sapply(thresholds, evaluar_threshold, data = pred_df))
resultados <- as.data.frame(resultados)

##  Mejor F1
best_f1 <- resultados[which.max(resultados$F1), ]
best_f1

## El mejor treshold es 

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



