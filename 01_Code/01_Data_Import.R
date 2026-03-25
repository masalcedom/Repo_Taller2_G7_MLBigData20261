  ## Data import 
  
  train_hogares <- read.csv("Data/train_hogares.csv")
  train_personas <- read.csv("Data/train_personas.csv")
  test_hogares  <- read.csv("data/test_hogares.csv")
  test_personas <- read.csv("data/test_personas.csv")
  
  
  
  ## Verificar los datos de id principal
  
  train_hogares %>% select(id) %>% head()
  
  table(train_hogares$Pobre)
  
  # Agrgar las dos columnas:
  # Ina: indica que las persona es inactiva
  # Pet: indica si la persona está en edad de trabajar
  
  new_hogar_variable <- train_personas  %>% 
    group_by(id) %>%
    summarize(h_Inactivos = sum(Ina, na.rm=TRUE),
              h_Pet = sum(Pet, na.rm = TRUE)) %>%
    mutate(h_Inactivosp = h_Inactivos/h_Pet) %>%
    ungroup()
  
  new_hogar_variable %>% head()
  
  ## Función de pre-procesamiento
  
  pre_process_personas <-  function(data, ...) {
    data <- data %>% 
      mutate(woman = ifelse(P6020==2,1,0), 
             head = ifelse(P6050== 1, 1, 0),
             minor = ifelse(P6040<=6,1,0), 
             cat_educ = ifelse(P6210==9,0,P6210), 
             occupied = ifelse(is.na(Oc),0,1) ) %>% 
      select(id, Orden, woman, head, minor, cat_educ, occupied)
    return(data)
  }
  
  train_personas <- pre_process_personas(train_personas)
  test_personas  <- pre_process_personas(test_personas)
  
  test_personas  %>% head()
  
  # Crear nueva columnas de;
  # Número de mujeres en el hogar
  # Número de menores de edad
  # Nivel educativo máximo dentro del hogar
  # Número de personas ocupadas
  
  # Creamos una base de datos de variables a nivel de hogar generadas a partir
  # de datos individuales.
  train_personas_nivel_hogar <- train_personas %>% 
    group_by(id) %>%
    summarize(num_women    = sum(woman, na.rm = TRUE),
              num_minors   = sum(minor, na.rm = TRUE),
              cat_maxEduc  = max(cat_educ, na.rm = TRUE),
              num_occupied = sum(occupied, na.rm = TRUE)) %>% 
    ungroup()
  
  ## Se cruzan las tabla de train_personas con train_personas_hogar
  
  
  train_personas_hogar <- train_personas %>% 
    filter(head == 1) %>%                # seleccionamos únicamente al jefe de cada hogar
    select(id, woman, cat_educ, occupied) %>% 
    rename(headWoman = woman,
           cat_educHead = cat_educ,
           occupiedHead = occupied) %>% 
    left_join(train_personas_nivel_hogar)    # añadimos las variables agregadas del hogar usando el id
  
  ## Se cruzan las tabla de train_personas con test_personas_hogar
  
  test_personas_nivel_hogar<- test_personas  %>% 
    group_by(id) %>%
    summarize(num_women    = sum(woman, na.rm = TRUE),
              num_minors   = sum(minor, na.rm = TRUE),
              cat_maxEduc  = max(cat_educ, na.rm = TRUE),
              num_occupied = sum(occupied, na.rm = TRUE)) %>% 
    ungroup()
  
  test_personas_hogar<- test_personas %>% 
    filter(head == 1) %>%
    select(id, woman, cat_educ, occupied) %>%
    rename(headWoman = woman,
           cat_educHead = cat_educ,
           occupiedHead = occupied) %>%
    left_join(test_personas_nivel_hogar)
  
  ## Variables de la base de hogares
  # Se agrega si arriendan o no
  
  train_hogares <- train_hogares %>%
    mutate(rent = ifelse(P5090 == 3,1,0)) %>%
    select(id, Dominio, rent, Pobre)
  
  test_hogares <- test_hogares %>% 
    mutate(rent = ifelse(P5090 == 3,1,0)) %>%
    select(id, Dominio, rent)
  
  # Construcción de la base final
  train <- train_hogares %>% 
    left_join(train_personas_hogar) %>%
    select(-id)
  
  test <- test_hogares %>% 
    left_join(test_personas_hogar)
  
  # Convertir a variables categóricas en el training 
  train <- train %>% 
    mutate(Pobre   = factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
           Dominio = factor(Dominio),
           cat_educHead = factor(
             cat_educHead, 
             levels = c(0:6),
             labels=c("No sabe",'Ninguno', 'Preescolar', 'Primaria',
                      'Secundaria', 'Media', 'Universitaria')
           ))
  
  # Convertir a variables categóricas en el test 
  test <- test %>%
    mutate(Dominio = factor(Dominio),
           cat_educHead = factor(
             cat_educHead, 
             levels = c(0:6),
             labels=c("No sabe",'Ninguno', 'Preescolar', 'Primaria',
                      'Secundaria', 'Media', 'Universitaria')
           ))
  # Estimación probabilidad
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    savePredictions = TRUE
  )
  
  #Evaluación del modelo
  set.seed(2025)
  
  model1 <- train(
    Pobre~.,
    data=train,
    metric = "Accuracy",
    method = "glmnet",
    trControl = ctrl, 
    tuneGrid = expand.grid(
      alpha  = seq(0, 1, by= 0.1),
      lambda = 10^seq(-3, 3, length = 10)
    )   
  )
  
  model1$pred  %>% head()
  
  # Ejemplo de observacionas 
  
  filtered_preds <- model1$pred %>%  filter(rowIndex %in% c(2,5))
  
  table(filtered_preds$pred,
        filtered_preds$obs, 
        dnn = c('pred', 'obs'))
  
  # True possitive rate
  
  ctrl <- trainControl(
    method = "cv",                 # usamos validación cruzada
    number = 5,                    # dividimos los datos en 5 folds
    classProbs = TRUE,             # el modelo debe calcular probabilidades de clase
    summaryFunction = twoClassSummary, # calcula métricas como TPR
    savePredictions = TRUE         # guarda las predicciones de cada fold
  )
  
  # Estimar por TRP, Utilizando Elastic Net
  
  set.seed(2025)                   # fijamos la semilla 
  
  model1 <- train(
    Pobre ~ .,                     # variable a predecir y predictores
    data = train,                  # base de datos de entrenamiento
    metric = "Sens",               # métrica que queremos maximizar (TPR)
    method = "glmnet",             # modelo Elastic Net
    trControl = ctrl,              # usamos la configuración definida arriba
    family = "binomial",           # modelo de clasificación binaria
    tuneGrid = expand.grid(        # grilla de hiperparámetros a explorar
      alpha  = seq(0, 1, by = 0.1),    # mezcla entre ridge (0) y lasso (1)
      lambda = 10^seq(-3, 3, length = 10) # intensidad de la penalización
    )   
  )
  
  model1                           # mostramos el modelo seleccionado
  
  ## Envío a Kaggle
  
  predictSample <- test %>%
    mutate(pobre_lab = predict(model1, newdata = test, type = "raw")) %>%
    select(id,pobre_lab)
  
  head(predictSample)
  
  
  # Recodificar 
  
  predictSample <- predictSample %>% 
    mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>%
    select(id, pobre)
  head(predictSample)  
  
  # Elaborar el .csv para kagle
  
  lambda_str <- gsub(
    "\\.", "_", 
    as.character(round(model1$bestTune$lambda, 4)))
  
  alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))
  
  name <- paste0(
    "EN_lambda_", lambda_str,
    "_alpha_" , alpha_str, 
    ".csv"
  ) 
  
  write.csv(predictSample,name, row.names = FALSE)
  