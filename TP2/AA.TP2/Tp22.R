
library(readxl)
library(skimr)
library(caret)
#library(RANN)
#library(plyr)
#library(caretEnsemble)


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

#variables
# "gender" # CLASE ver que hacer con unknowns y blanks                
# "description" # yo lo cambiaria a un booleano de si existe o no descripcion o sino hacer TM aca tambien        
# "fav_number"          
# "link_color"          
# "name"                 
# "retweet_count"    
# "sidebar_color" 
# "text"                 
# "tweet_coord"   #cambiar a booleano si tiene coordenadas o no
# "tweet_count"    

# variables que no aportan
# "created"    # sacar, es la fecah de creacion del perfil
# "gender_gold"   #sacar      
# "profile_yn_gold" #sacar   
# "tweet_created"  # no aporta sacar
# "tweet_id"      #el id del tweet no aporta datos  
# "tweet_location" # no aporta sacar        
# "user_timezone" # no aporta sacar
# "profileimage"

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
  
# remover temporalmente los fields donde vamos a hacer text mining
  dataset.cleaned <- subset(dataset.cleaned, select = -c(name, description, text))
# convert hex colors to in
  dataset.cleaned$link_color <- strtoi(dataset.cleaned$link_color, 16L)
  dataset.cleaned$sidebar_color <- strtoi(dataset.cleaned$sidebar_color, 16L)
  
  
#sacar los dos registros que tienen tweets vacios, genedr unknown y gender NA
  dataset.cleaned <- dataset.cleaned[-which(is.na(dataset.cleaned$gender)),]
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

#cambiar la descripcion para los que tienen na, poner un string con un -1
  #dataset.cleaned$description[which(is.na(dataset.cleaned$description))] <- "-1"


#imputar el gender de los na con la clase mayoritaria
  summary(dataset.cleaned$gender) # clase mayoritaria female
  dataset.cleaned$gender[which(is.na(dataset.cleaned$gender))] <- "female"

# resumen del dataset 
  skimmed <- skim_to_wide(dataset.cleaned)

  
# Split en 500 de competencia
  ix.competencia <- sample(nrow(dataset.cleaned), 500)
  competencia <- dataset.cleaned[ix.competencia,]
  
  # las saco del dataset
  dataset.cleaned <- dataset.cleaned[-ix.competencia, ]
  
  
# separo training y test manteniendo la relacion entre clases
  
  # Step 1: Get row numbers for the training data
  trainRowNumbers <- createDataPartition(dataset.cleaned$gender, p=0.8, list=FALSE)
  
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
                             trim=F,
                             returnData = T,
                             allowParallel =T,
                             index = createFolds(trainData$gender, 5) # importante setear los indices que coincidan con el CV
  )

# especifico los modelos y los parametros a tunear

  
  J48_params_grid <- expand.grid(C = seq(0.05, 0.50, 0.05), M = seq(2, 4, 2))
  knn_params_grid <- expand.grid(k= seq(1, 10, 1))
  nb_params_grid <- expand.grid(fL=c(0.1,0.5,1.0), usekernel = c(TRUE, FALSE), adjust=c(0.1,0.5,1.0))
  
  model_list_big <- caretList(
    gender~., trainData,
    trControl=fitControl,
    methodList=list("cforest"),
    tuneList=list(
      j48=caretModelSpec(method="J48", tuneGrid=data.frame(J48_params_grid)),
      #nb=caretModelSpec(method="nb", tuneGrid=data.frame(nb_params_grid)),
      nb=caretModelSpec(method="J48", tuneGrid=data.frame(J48_params_grid)),
      kn=caretModelSpec(method="knn", tuneGrid=data.frame(knn_params_grid))
      )
  )
  
  
  #j48.probs <- predict(model_list_big$j48, trainData, type="prob")
  #kn.probs <- predict(model_list_big$kn, trainData, type="prob")
  
# pruebo los modelos en el dataset de TRAIN
  train_pred_j48 <- predict(model_list_big$j48, trainData, type="prob")
  train_pred_kn <- predict(model_list_big$kn, trainData, type="prob")
  train_pred_nb <- predict(model_list_big$nb, trainData, type="prob")
  
  train_pred_forest <- predict(model_list_big$cforest, trainData, type="prob")
  
  
  # para el ensameble, usamos promedio de las probabilidades de los modelos
  train_avg_probs <- cbind.data.frame( brand = (train_pred_j48$brand + train_pred_kn$brand + train_pred_nb$brand + train_pred_forest$brand)/4,
                                      female = (train_pred_j48$female + train_pred_kn$female + train_pred_nb$female + train_pred_forest$female)/4,
                                      male = (train_pred_j48$male + train_pred_kn$male + train_pred_nb$male + train_pred_forest$male)/4,
                                      target= trainData$gender )
  
  #armo un data frame con las probabilidades medias y el target
  train_results <- cbind.data.frame(new_prediction=names(train_avg_probs)[max.col(train_avg_probs[,1:3])], trainData$gender)
  caret::confusionMatrix(reference = trainData$gender, data = train_results$new_prediction)
  
# pruebo los modelos en el dataset de TEST
  test_pred_j48 <- predict(model_list_big$j48, testData, type="prob")
  test_pred_kn <- predict(model_list_big$kn, testData, type="prob")
  test_pred_nb <- predict(model_list_big$nb, testData, type="prob")
  
  test_pred_svm <- predict(model_list_big$cforest, testData, type="prob")
  

# para el ensameble, usamos promedio de las probabilidades de los modelos
  test_avg_probs <- cbind.data.frame( brand = (test_pred_j48$brand + test_pred_kn$brand + test_pred_nb$brand + test_pred_forest$brand)/4,
                                 female = (test_pred_j48$female + test_pred_kn$female + test_pred_nb$female + test_pred_forest$female)/4,
                                 male = (test_pred_j48$male + test_pred_kn$male + test_pred_nb$male + test_pred_forest$female)/4,
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

  