library(quanteda)
library(readxl)
library(skimr)
library(caret)
library(caretEnsemble)
library(pROC)
library(ggplot2)

set.seed(2018)
# tipos de  columnas para importar
  types <- c("numeric", "text", "text", "numeric",	"date",	"text",	"numeric",	"text",
             "numeric",	"date",	"text",	"numeric",	"text",	"text",	"text",	"text",	"text",
             "numeric",	"text",	"text",	"text",	"numeric",	"date",	"text",	"text",	"text"
  )
  gender <-read_excel("gender.xlsx", col_types=types)

  
#listas de nombres por genero
  male.names <- read.csv("male.txt", sep="\n")
  female.names <- read.csv("female.txt", sep="\n")

# eliminar los que tienen gender:confidence  < 1
  gender.cleaned <- gender[-which(gender$"gender:confidence" <1),]

#remover las columnas que no aportan
  
  dataset <- subset(gender.cleaned, select = -c(`_unit_id`, `_golden`, `_unit_state`, created, gender_gold,
                                                `_trusted_judgments`, `_last_judgment_at`, `gender:confidence`,
                                                `profile_yn:confidence`, created, gender_gold, profile_yn_gold,
                                                profileimage, tweet_created,  tweet_id, user_timezone, 
                                                tweet_location, profile_yn))
                                                
                    
################################################
############## DATA PREPARATION ################
################################################
  
  
#variables has_male_name y has_female_name
  dataset$has_male_name <- ifelse(apply(dataset[,"name"], 1, function(u) any(pmatch( unlist(male.names), u), na.rm=TRUE) ), 1, 0)
  dataset$has_female_name <- ifelse(apply(dataset[,"name"], 1, function(u) any(pmatch( unlist(female.names), u), na.rm=TRUE) ), 1, 0)
  
  dataset.cleaned <- dataset
  
  # armo la matrix df idf
  
  tokens <- tokens(	paste(dataset.cleaned$text, dataset.cleaned$description), what = "word",
                          remove_numbers = T, remove_punct = T,
                          remove_symbols = TRUE, remove_hyphens = T)
  tokens <- tokens_tolower(tokens)
  tokens <- tokens_select(	tokens, stopwords(),
                                 selection = "remove")
  
  tokens <- tokens_wordstem(tokens, language = "english")
  
  dfm <- dfm(tokens)
  #remove sparse terms
  dfm <- dfm_trim(dfm, min_termfreq = 10, min_docfreq = 50)
  
  #transforamr la matriz con TF - IDF
  dfm.tf.idf <- dfm_tfidf(dfm, scheme_tf = "prop") %>% round(digits = 2)
  df <- convert(dfm.tf.idf, to = "data.frame")
  
  #si hay casos que quedaron vacios, rellenar columnas con 0
  inc.cases.ix <- which(!complete.cases(df))
  df[inc.cases.ix, ] <- rep(0.0, ncol(df))
  #si quedaron nombres invalidos en R, fixearlos
  names(df) <- make.names(names(df))
  
  df <- subset(df, select = -c(document))
  
  
  
# remover temporalmente los fields donde vamos a hacer text mining
  dataset.cleaned <- subset(dataset.cleaned, select = -c(name, description, text))
  dataset.cleaned <- cbind.data.frame(dataset.cleaned, df)
  
# convert hex colors to in
  dataset.cleaned$link_color <- strtoi(dataset.cleaned$link_color, 16L)
  dataset.cleaned$sidebar_color <- strtoi(dataset.cleaned$sidebar_color, 16L)
  
  #imputar el gender de los na con la clase mayoritaria
  summary(dataset.cleaned$gender) # clase mayoritaria female
  dataset.cleaned$gender[which(is.na(dataset.cleaned$gender))] <- "female"
  
#sacar los dos registros que tienen tweets vacios, genedr unknown y gender NA
  #dataset.cleaned <- dataset.cleaned[-which(is.na(dataset.cleaned$gender)),]
  dataset.cleaned <- dataset.cleaned[-which(dataset.cleaned$gender=="unknown"),]
  dataset.cleaned <- dataset.cleaned[-which(is.na(dataset.cleaned$link_color)),]
  dataset.cleaned <- dataset.cleaned[-which(is.na(dataset.cleaned$sidebar_color)),]
  #dataset.cleaned <- dataset.cleaned[-which(is.na(dataset.cleaned$text)),]

# cambiar tweet_coord a un booleano si tiene seteadas coordenadas o no
  tweet_coord.na.indexes <- which(is.na(dataset.cleaned$tweet_coord))
  dataset.cleaned$tweet_coord[tweet_coord.na.indexes] <- FALSE
  dataset.cleaned$tweet_coord[-tweet_coord.na.indexes] <- TRUE
  dataset.cleaned$tweet_coord <- as.logical(dataset.cleaned$tweet_coord)
  

# set column types (AGREGAR los fields de TM)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  dataset.cleaned$gender <- as.factor(dataset.cleaned$gender)
  dataset.cleaned$link_color <- as.integer(dataset.cleaned$link_color)
  dataset.cleaned$sidebar_color <- as.integer(dataset.cleaned$sidebar_color)
  dataset.cleaned$fav_number <- as.integer(dataset.cleaned$fav_number)
  dataset.cleaned$retweet_count <- as.integer(dataset.cleaned$retweet_count)
  dataset.cleaned$tweet_count <- as.integer(dataset.cleaned$tweet_count)
  dataset.cleaned$has_male_name <- as.logical(dataset.cleaned$has_male_name)
  dataset.cleaned$has_female_name <- as.logical(dataset.cleaned$has_female_name)


# resumen del dataset 
  #skimmed <- skim_to_wide(dataset.cleaned)

  
# Split en 500 de competencia
  ix.competencia <- sample(nrow(dataset.cleaned), 500)
  compData <- dataset.cleaned[ix.competencia,]
  
  # las saco del dataset
  dataset.cleaned <- dataset.cleaned[-ix.competencia, ]
  
  
# separo training y test manteniendo la relacion entre clases
  
  # Step 1: Get row numbers for the training data
  trainRowNumbers <- createDataPartition(dataset.cleaned$gender, p=0.9, list=FALSE)
  
  # Step 2: Create the training  dataset
  trainData <- dataset.cleaned[trainRowNumbers,]
  
  # Step 3: Get the remaining rows (going to be splitted in 2)
  testData <- dataset.cleaned[-trainRowNumbers,]

  #compruebo las proporciones
  prop.table(table(trainData$gender))
  prop.table(table(testData$gender))
  
  
 ################################################
 ################## MODELOS #####################
 ################################################
  
 # objeto de control de training
  fitControl <- trainControl(method = 'cv',                   # k-fold cross validation
                             number = 5,                      # number of folds
                             savePredictions = 'final',       # saves predictions for optimal tuning parameter
                             classProbs = T,                  # should class probabilities be returned
                             #summaryFunction=twoClassSummary  # results summary function
                             
                             verboseIter = T,
                             trim=T,
                             returnData = F,
                             allowParallel =T,
                             
                             index = createFolds(trainData$gender, 5)
  )

# especifico los modelos y los parametros a tunear

  #J48_params_grid <- expand.grid(C = seq(0.05, 0.30, 0.05), M = seq(2, 10, 2))
  regLogistic_params <- expand.grid(cost = seq(0.25,4,0.5), loss="L1", epsilon = 1e-03 )
  
  model_list_big <- caretList(
    gender~., trainData,
    trControl=fitControl,
    #methodList=list( "xgbLinear", "xgbTree", "lda", "LogitBoost", "regLogistic"),
    #methodList=list(  "LogitBoost", "rf", "C5.0Tree","gbm"),
    #methodList=list(  "LogitBoost", "rf","gbm"),
    methodList=list(   "LogitBoost", "gbm"),
    tuneLength=5,
    tuneList=list(
    regLogistic=caretModelSpec(method="regLogistic", tuneGrid=data.frame(regLogistic_params))
      #nb=caretModelSpec(method="naive_bayes", preProcess="conditionalX", tuneGrid=data.frame(nb_params_grid)),
      #kn=caretModelSpec(method="knn", tuneGrid=data.frame(knn_params_grid))
      )
  )
  

  saveRDS(model_list_big, "./final_model_5cv.rds")

  #model_list_big <- readRDS("./final_model.rds")
  

  
# pruebo los modelos en el dataset de TEST
  
  test_pred.1 <- predict(model_list_big$regLogistic, testData, type="raw")
  test_pred.2 <- predict(model_list_big$LogitBoost, testData, type="raw")
  test_pred.3 <- predict(model_list_big$gbm, testData, type="raw")

  ## ROC MODELO 1
  
  mod1_brand_vs_all <- cbind.data.frame(response = ifelse(test_pred.1 == "brand","brand",  "other"),
                                      target = ifelse(testData$gender == "brand",1,  0))
  mod1_female_vs_all <- cbind.data.frame(response= ifelse(test_pred.1 == "female","female",  "other"),
                                       target =  ifelse(testData$gender == "female",1,  0))
  mod1_male_vs_all <- cbind.data.frame(response = ifelse(  test_pred.1 == "male","male",  "other"),
                                     target = ifelse(testData$gender == "male",1,  0))
  
  mod1_roc_brand <- roc(mod1_brand_vs_all$response, mod1_brand_vs_all$target)
  mod1_roc_female <- roc(mod1_female_vs_all$response, mod1_female_vs_all$target)
  mod1_roc_male <- roc(mod1_male_vs_all$response, mod1_male_vs_all$target)
  
  ggroc(list(brand=mod1_roc_brand, female=mod1_roc_female, male=mod1_roc_male), size = 1)  
  auc(mod1_roc_brand)
  auc(mod1_roc_female)
  auc(mod1_roc_male)
  
  
  ## ROC MODELO 2
  mod2_brand_vs_all <- cbind.data.frame(response = ifelse(test_pred.2 == "brand","brand",  "other"),
                                        target = ifelse(testData$gender == "brand",1,  0))
  mod2_female_vs_all <- cbind.data.frame(response= ifelse(test_pred.2 == "female","female",  "other"),
                                         target =  ifelse(testData$gender == "female",1,  0))
  mod2_male_vs_all <- cbind.data.frame(response = ifelse(  test_pred.2 == "male","male",  "other"),
                                       target = ifelse(testData$gender == "male",1,  0))
  
  
  mod2_roc_brand <- roc(mod2_brand_vs_all$response, mod2_brand_vs_all$target)
  mod2_roc_female <- roc(mod2_female_vs_all$response, mod2_female_vs_all$target)
  mod2_roc_male <- roc(mod2_male_vs_all$response, mod2_male_vs_all$target)
  
  ggroc(list(brand=mod2_roc_brand, female=mod2_roc_female, male=mod2_roc_male), size = 1)  
  auc(mod2_roc_brand)
  auc(mod2_roc_female)
  auc(mod2_roc_male)
  

  
  ## ROC MODELO 3
  
  mod3_brand_vs_all <- cbind.data.frame(response = ifelse(test_pred.3 == "brand","brand",  "other"),
                                        target = ifelse(testData$gender == "brand",1,  0))
  mod3_female_vs_all <- cbind.data.frame(response= ifelse(test_pred.3 == "female","female",  "other"),
                                         target =  ifelse(testData$gender == "female",1,  0))
  mod3_male_vs_all <- cbind.data.frame(response = ifelse(  test_pred.3 == "male","male",  "other"),
                                       target = ifelse(testData$gender == "male",1,  0))
  
  
  mod3_roc_brand <- roc(mod3_brand_vs_all$response, mod3_brand_vs_all$target)
  mod3_roc_female <- roc(mod3_female_vs_all$response, mod3_female_vs_all$target)
  mod3_roc_male <- roc(mod3_male_vs_all$response, mod3_male_vs_all$target)
  
  
  ggroc(list(brand=mod3_roc_brand, female=mod3_roc_female, male=mod3_roc_male), size = 1)  
  auc(mod3_roc_brand)
  auc(mod3_roc_female)
  auc(mod3_roc_male)
  
  # ACCURACY modelos en TEST
  caret::confusionMatrix(reference = testData$gender, data = test_pred.1)
  caret::confusionMatrix(reference = testData$gender, data = test_pred.2)
  caret::confusionMatrix(reference = testData$gender, data = test_pred.3)
  
  
  train_pred.1 <- predict(model_list_big$regLogistic, trainData, type="prob")
  #train_pred.3 <- predict(model_list_big$J48, trainData, type="prob")
  train_pred.2 <- predict(model_list_big$LogitBoost, trainData, type="prob")
  #train_pred.5 <- predict(model_list_big$C5.0Tree, trainData, type="prob")
  train_pred.3 <- predict(model_list_big$gbm, trainData, type="prob")

  
# para el ensameble, usamos promedio de las probabilidades de los modelos
  train_avg_probs <- cbind.data.frame( brand = ( train_pred.1$brand + train_pred.2$brand + train_pred.3$brand )/3,
                                       female = ( train_pred.1$female +  train_pred.2$female + train_pred.3$female )/3,
                                       male = ( train_pred.1$male +  train_pred.2$male + train_pred.3$male )/3,
                                       target= trainData$gender )
  
  

  #armo un data frame con las probabilidades medias y el target
  train_results <- cbind.data.frame(new_prediction=names(train_avg_probs)[max.col(train_avg_probs[,1:3])], trainData$gender)
  caret::confusionMatrix(reference = trainData$gender, data = train_results$new_prediction)
  
# pruebo el ensamble en el dataset de TEST

 
  #test_pred.1 <- predict(model_list_big$lda, testData, type="prob")
  test_pred.1 <- predict(model_list_big$LogitBoost, testData, type="prob")
  #test_pred.3 <- predict(model_list_big$J48, testData, type="prob")
  test_pred.2 <- predict(model_list_big$regLogistic, testData, type="prob")
  #test_pred.5 <- predict(model_list_big$C5.0Tree, testData, type="prob")
  test_pred.3 <- predict(model_list_big$gbm, testData, type="prob")

  
# para el ensameble, usamos promedio de las probabilidades de los modelos
  test_avg_probs <- cbind.data.frame( brand = ( test_pred.1$brand +  test_pred.2$brand +  test_pred.3$brand)/3,
                                       female = ( test_pred.1$female +  test_pred.2$female + test_pred.3$female)/3,
                                       male = ( test_pred.1$male + test_pred.2$male +  test_pred.3$male)/3,
                                       target= testData$gender )
  
#armo un data frame con las probabilidades medias y el target
  test_results <- cbind.data.frame(new_prediction=names(test_avg_probs)[max.col(test_avg_probs[,1:3])], testData$gender) # devuelve clases
  #final_results <- cbind.data.frame(new_prediction=apply(avg.probs[,1:3],1, max), trainData$gender) # devuelve probs
  
  # ROC one Vs All
  # se crea un dataframe para cada valor de la clase, en 
  # cada caso poniendo el nombre de una de las clases y "other" para las otras dos
  # y hago lo mismo con el target
  brand_vs_all <- cbind.data.frame(response = ifelse(test_results$new_prediction == "brand","brand",  "other"),
                                   target = ifelse(testData$gender == "brand",1,  0))
  female_vs_all <- cbind.data.frame(response= ifelse(test_results$new_prediction == "female","female",  "other"),
                                    target =  ifelse(testData$gender == "female",1,  0))
  male_vs_all <- cbind.data.frame(response = ifelse(test_results$new_prediction == "male","male",  "other"),
                                  target = ifelse(testData$gender == "male",1,  0))
  

  roc_brand <- roc(brand_vs_all$response, brand_vs_all$target)
  roc_female <- roc(female_vs_all$response, female_vs_all$target)
  roc_male <- roc(male_vs_all$response, male_vs_all$target)
  
  
  ggroc(list(brand=roc_brand, female=roc_female, male=roc_male), size = 1)  
  auc(roc_brand)
  auc(roc_female)
  auc(roc_male)
  
  
  caret::confusionMatrix(reference = testData$gender, data = test_results$new_prediction)

# pruebo el ensamble en el dataset de COMPETICION

  #comp_pred.1 <- predict(model_list_big$lda, compData, type="prob")
  comp_pred.1 <- predict(model_list_big$LogitBoost, compData, type="prob")
  #comp_pred.3 <- predict(model_list_big$J48, compData, type="prob")
  comp_pred.2 <- predict(model_list_big$regLogistic, compData, type="prob")
 # comp_pred.5 <- predict(model_list_big$C5.0Tree, compData, type="prob")
  comp_pred.3 <- predict(model_list_big$gbm, compData, type="prob")
  
  
  comp_avg_probs <- cbind.data.frame( brand = ( comp_pred.1$brand +  comp_pred.2$brand  + comp_pred.3$brand)/3,
                                      female = ( comp_pred.1$female +  comp_pred.2$female + comp_pred.3$female)/3,
                                      male = ( comp_pred.1$male +  comp_pred.2$male +  comp_pred.3$male)/3,
                                      target= compData$gender )
  comp_results <- cbind.data.frame(new_prediction=names(comp_avg_probs)[max.col(comp_avg_probs[,1:3])], compData$gender) # devuelve clases
  
  #RESULTADO FINAL COMPETICION 
  caret::confusionMatrix(reference = compData$gender, data = comp_results$new_prediction)
  

  #ggplot(model_list_big$J48)
  #ggsave("J48.png", dpi=400, height=6, width=12)
  #ggsave()  #ggplot(model_list_big$lda)
  ggplot(model_list_big$LogitBoost)
  ggsave("LogitBoost.png", dpi=400, height=6, width=12)
  ggplot(model_list_big$regLogistic)
  ggsave("regLogistic.png", dpi=400, height=6, width=12)
  #ggplot(model_list_big$C5.0Tree)
  ggplot(model_list_big$gbm)
  ggsave("gbm.png", dpi=400, height=6, width=12)
  