# Trabalho de Disciplina

library(mlbench)
data(Satellite)

# Separando os dados espectrais para o pixel central
data <- data.frame(Satellite[c("x.17", "x.18", "x.19", "x.20", "classes")])

# Separando a base de treine e teste
library("caret")
indices <- createDataPartition(data$classes, p=0.80, list=FALSE)
treino <- data[indices,]
teste <- data[-indices,]

set.seed(7)

############################## Criando Modelos ##############################
rf <- train(classes~., data=treino, method="rf")
svm <- train(classes~., data=treino, method="svmRadial")
rna <- train(classes~., data=treino, method="nnet", trace=FALSE)

########################### Predição dos Modelos ############################
predicoes.rf <- predict(rf, teste)
predicoes.svm <- predict(svm, teste)
predicoes.rna <- predict(rna, teste)

###################### Gerando as Matrizes de Confusão ######################
confusionMatrix(predicoes.rf, teste$classes)
confusionMatrix(predicoes.svm, teste$classes)
confusionMatrix(predicoes.rna, teste$classes)

# Matriz de Confusão para o modelo Random Forest

# Prediction            red soil cotton crop grey soil damp grey soil vegetation stubble very damp grey soil
# red soil                 296           0         1              0                  5                   0
# cotton crop                0         129         0              0                  2                   0
# grey soil                  8           0       255             33                  1                  18
# damp grey soil             0           2        10             52                  1                  35
# vegetation stubble         2           8         0              0                118                   7
# very damp grey soil        0           1         5             40                 14                 241

# Accuracy : 0.8497  


# Matriz de Confusão para o modelo SVM

# Prediction            red soil cotton crop grey soil damp grey soil vegetation stubble very damp grey soil
# red soil                 294           0         0              0                 12                   0
# cotton crop                0         126         0              0                  0                   0
# grey soil                 10           0       260             36                  0                  13
# damp grey soil             0           0        11             57                  1                  29
# vegetation stubble         2          10         0              0                107                   5
# very damp grey soil        0           4         0             32                 21                 254

# Accuracy : 0.8551 


# Matriz de Confusão para o modelo RNA

# Prediction            red soil cotton crop grey soil damp grey soil vegetation stubble very damp grey soil
# red soil                 282           0         1              0                 11                   0
# cotton crop                1         123         0              0                  3                   0
# grey soil                 16           0       108             31                  0                  18
# damp grey soil             0           0         0              0                  0                   0
# vegetation stubble         7          16         2              2                 98                  10
# very damp grey soil        0           1       160             92                 29                 273

# Accuracy : 0.6885


############################ Escolha do modelo ############################
# Dentre os 3 modelos testados o que apresentou melhor resultado em acurácia foi: SVM

######################## Retreinando melhor modelo ########################
svmFinal <- train(classes~., data=data, method="svmRadial")
predicoes.svmFinal <- predict(svmFinal, teste)
confusionMatrix(predicoes.svmFinal, teste$classes)

################## Matriz de confusão do modelo escolhido #################

# Prediction            red soil cotton crop grey soil damp grey soil vegetation stubble very damp grey soil
# red soil                 297           0         0              0                 10                   0
# cotton crop                0         128         0              0                  0                   0
# grey soil                  7           0       260             36                  0                  13
# damp grey soil             0           1        11             58                  1                  29
# vegetation stubble         2          10         0              0                110                   4
# very damp grey soil        0           1         0             31                 20                 255

# Accuracy : 0.8629

################ Verificando os parâmetros do modelo final ################
# Support Vector Machines with Radial Basis Function Kernel 
# 
# 5151 samples
# 4 predictor
# 6 classes: 'red soil', 'cotton crop', 'grey soil', 'damp grey soil', 'vegetation stubble', 'very damp grey soil' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 5151, 5151, 5151, 5151, 5151, 5151, ... 
# Resampling results across tuning parameters:
#   
#   C     Accuracy   Kappa    
# 0.25  0.8570144  0.8228464
# 0.50  0.8587932  0.8250666
# 1.00  0.8598336  0.8263926
# 
# Tuning parameter 'sigma' was held constant at a value of 0.86087
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 0.86087 and C = 1.

library("kernlab")
final_model <- ksvm(
  type="C-svc",
  classes~.,
  data=data,
  kernel="rbfdot",
  C=1.0,
  kpar=list(sigma=0.86087)
)

# Gerando a predição do modelo usando a base total de dados
final_predict.svm <- predict(final_model, data)

 
# Vendo a nova matriz de confusão desse modelo total
confusionMatrix(final_predict.svm, data$classes)

# Salvando o modelo em um arquivo .RDS
saveRDS(final_model, "satellite_svm.rds")




