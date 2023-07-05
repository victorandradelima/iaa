## Estatística Aplicada II

# Primeira Lista de Exercícios

# Com a base de dados “imoveiscwbav” obter os seguintes resultados com o auxílio do “R”
# 1.	Elaborar os modelos Ridge, Lasso e ElasticNet e estimar uma predição para cada um dos modelos
# (elabore seus próprios valores para a predição). Apresente os parâmetros estimados e os valores
# resultantes da predição.


# #### Preparando os dados ####

load("imoveiscwbav.RData")
imoveis <- imoveiscwbav

# Visualizador de dados
library(dplyr)
glimpse(imoveis)

# Fixando randomização
set.seed(999)

# Separando os indices para treino e teste (80%)
index = sample(1:nrow(imoveis),0.8*nrow(imoveis))
train = imoveis[index,] # Dados de treino
test = imoveis[-index,] # Dados de teste

# Selecionando as colunas que devem ser padronizadas
cols = c('price', 'age', 'parea', 'tarea', 'bath', 'ensuit', 'garag', 'plaz', 'park', 'trans', 'kidca', 'school',
         'health', 'bike')

# Padronizando
library(caret)
pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

# Criando um vetor com as variáveis em uso
cols_reg = c('price', 'age', 'parea', 'tarea', 'bath', 'ensuit', 'garag', 'plaz','park', 'trans', 'kidca',
             'school', 'health', 'bike', 'barb', 'balc', 'elev', 'fitg', 'party', 'categ')

# Gerando dummies para organizar o conjunto de dados no objeto matriz
dummies <- dummyVars(price ~ age+parea+tarea+bath+ensuit+garag+plaz+park+trans+kidca+school+health+bike+
                       barb+balc+elev+fitg+party+categ,data = imoveis[,cols_reg])
train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])

# O conjunto de dados da matriz para o modelo
x = as.matrix(train_dummies)
y_train = train$price
x_test = as.matrix(test_dummies)
y_test = test$price

# Previsão para nosso exemplo
# Como os valores do conjunto de dados são padronizados, também temos que padronizar os dados que queremos prever
age = (14-pre_proc_val[["mean"]][["age"]])/pre_proc_val[["std"]][["age"]] # age = 14
parea = (120-pre_proc_val[["mean"]][["parea"]])/pre_proc_val[["std"]][["parea"]] # parea = 120
tarea = (180-pre_proc_val[["mean"]][["tarea"]])/pre_proc_val[["std"]][["tarea"]] # tarea = 180
bath = (3-pre_proc_val[["mean"]][["bath"]])/pre_proc_val[["std"]][["bath"]] # bath = 3
ensuit = (3-pre_proc_val[["mean"]][["ensuit"]])/pre_proc_val[["std"]][["ensuit"]] # ensuit = 3
garag = (2-pre_proc_val[["mean"]][["garag"]])/pre_proc_val[["std"]][["garag"]] # garag = 2
plaz = (0-pre_proc_val[["mean"]][["plaz"]])/pre_proc_val[["std"]][["plaz"]] # plaz = 0
park = (2-pre_proc_val[["mean"]][["park"]])/pre_proc_val[["std"]][["park"]] # park = 2
trans = (2-pre_proc_val[["mean"]][["trans"]])/pre_proc_val[["std"]][["trans"]] # trans = 2
kidca = (1.5-pre_proc_val[["mean"]][["kidca"]])/pre_proc_val[["std"]][["kidca"]] # kidca = 1.5
school = (1-pre_proc_val[["mean"]][["school"]])/pre_proc_val[["std"]][["school"]] # school = 0.5
health = (0.5-pre_proc_val[["mean"]][["health"]])/pre_proc_val[["std"]][["health"]] # health = 0.5
bike = (0.5-pre_proc_val[["mean"]][["bike"]])/pre_proc_val[["std"]][["bike"]] # bike = 0.5
barb = 0 # barb = 0
balc = 0 # balc = 0
elev = 0 # elev = 0
fitg = 0 # fitg = 0
party = 1 # party = 1
categ = 1 # categ = 1

# Construindo uma matriz de dados para previsão
our_pred = as.matrix(data.frame(age=age, 
                                parea=parea,
                                tarea=tarea,
                                bath=bath,
                                ensuit=ensuit,
                                garag=garag,
                                plaz=plaz,
                                park=park,
                                trans=trans,
                                kidca=kidca,
                                school=school,
                                health=health,
                                bike=bike,
                                barb=barb,
                                balc=balc,
                                elev=elev,
                                fitg=fitg,
                                party=party,
                                categ=categ))

# Rotina para calcular R^2 a partir de valores verdadeiros e previstos
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Métricas de desempenho do modelo
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}







# #### Regressão Ridge #### 

# Cálculo do valor lambda ótimo;
# alpha = "0", é para Regressão de Ridge
library(glmnet)
lambdas_ridge <- 10^seq(2, -3, by = -.1)
ridge_lamb <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas_ridge)
best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

# Modelo de estimativa
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = best_lambda_ridge)
summary(ridge_reg)

# O valor dos parâmetros
ridge_reg[["beta"]]

# Previsão e avaliação de dados de treino
predictions_train_ridge <- predict(ridge_reg, s = best_lambda_ridge, newx = x)
eval_results(y_train, predictions_train_ridge, train)

# Previsão e avaliação em dados de teste
predictions_test_ridge <- predict(ridge_reg, s = best_lambda_ridge, newx = x_test)
eval_results(y_test, predictions_test_ridge, test)

# Fazendo a previsão
predict_our_ridge <- predict(ridge_reg, s = best_lambda_ridge, newx = our_pred)
predict_our_ridge

# O resultado é um valor padronizado, vamos convertê-lo em valor nominal, consistente com o banco de dados original
price_pred_ridge=(predict_our_ridge*pre_proc_val[["std"]][["price"]])+pre_proc_val[["mean"]][["price"]]
price_pred_ridge # 1112961 (R$ 1.112.961,00)
# Este é o valor do preço do imóvel previsto

# Intervalos de confiança para nosso exemplo
n_ridge <- nrow(train)
m_ridge <- price_pred_ridge
s_ridge <- pre_proc_val[["std"]][["price"]]
dam_ridge <- s_ridge/sqrt(n_ridge)

CIlwr_ridge <- m_ridge + (qnorm(0.025))*dam_ridge
CIupr_ridge <- m_ridge - (qnorm(0.025))*dam_ridge 
CIlwr_ridge # 1063056 (R$ 1.063.056,00)
CIupr_ridge # 1162867 (R$ 1.162.867,00)






# #### Regressão Lasso #### 

# Encontrando o melhor lambda e definindo alfa = 1 para implementar a regressão de Lasso
lambdasLasso <- 10^seq(2, -3, by = -.1)

# Definindo alfa = 1 para implementar a regressão de Lasso
lasso_lamb <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdasLasso, standardize = TRUE, nfolds = 5)

# Melhor lambda
best_lambda_lasso <- lasso_lamb$lambda.min 
best_lambda_lasso
lasso_model <- glmnet(x, y_train, alpha = 1, lambda = best_lambda_lasso, standardize = TRUE)

# Como visualizar os parâmetros estimados
lasso_model[["beta"]]

# Fazendo as previsões e avaliando o modelo de laço
predictions_train_Lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x)
eval_results(y_train, predictions_train_Lasso, train)
predictions_test_Lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)
eval_results(y_test, predictions_test_Lasso, test)

## Previsão para o nosso exemplo
# Fazendo as previsões com base nos mesmos parâmetros da regressão de crista
predict_our_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = our_pred)
predict_our_lasso

# Novamente, a informação que ele retorna é padronizada, temos que convertê-la para
# um valor compatível com o conjunto de dados original
price_pred_lasso=(predict_our_lasso*pre_proc_val[["std"]][["price"]])+pre_proc_val[["mean"]][["price"]]
price_pred_lasso # 1127815 (R$ # 1.127.815,00)

# Intervalos de confiança para o nosso exemplo
n_lasso <- nrow(train)
m_lasso <- price_pred_lasso
s_lasso <- pre_proc_val[["std"]][["price"]]
dam_lasso <- s_lasso/sqrt(n_lasso)

CIlwr_lasso <- m_lasso + (qnorm(0.025))*dam_lasso
CIupr_lasso <- m_lasso - (qnorm(0.025))*dam_lasso 
CIlwr_lasso # 1063056 (R$ 1.063.056,00)
CIupr_lasso # 1162867 (R$ 1.162.867,00)






# #### Regressão ElasticNet ####
library(caret)
# Definir controle de treinamento
train_cont_elasticNet <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random", verboseIter = FALSE, returnData = FALSE)

# Treinamento modelo
# Não temos o parâmetro "alpha", porque a Regressão ElasticNet vai encontrá-lo automaticamente,
# cujo valor estará entre 0 e 1 (para Ridge ==> alpha = 0; para Lasso ==> alpha = 1).
# O parâmetro "lambda" também é escolhido por validação cruzada.
elastic_reg <- train(price ~ age+parea+tarea+bath+ensuit+garag+plaz+park+trans+kidca+
                       school+health+bike+barb+balc+elev+fitg+party+categ,
                     data = train,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = train_cont_elasticNet)

# Melhor parâmetro de ajuste
elastic_reg$bestTune

# E os parâmetros são:
# elastic_reg[["finalModel"]][["beta"]]

# Fazendo previsões e avaliando o modelo

# Faça previsões no conjunto de treinamento
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Faça previsões no conjunto de teste
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

## Previsão para o nosso exemplo
predict_our_elastic <- predict(elastic_reg,our_pred)
predict_our_elastic

# Novamente, o resultado é padronizado, temos que reverter para o original valores de nível
price_pred_elastic=(predict_our_elastic*pre_proc_val[["std"]][["price"]])+pre_proc_val[["mean"]][["price"]]
price_pred_elastic # 1124279 (R$ 1.124.279,00)

# Intervalos de confiança para o nosso exemplo
n_elasticnet <- nrow(train)
m_elasticnet <- price_pred_elastic
s_elasticnet <- pre_proc_val[["std"]][["price"]]
dam_elasticnet <- s_elasticnet/sqrt(n_elasticnet)

CIlwr_elasticnet <- m_elasticnet + (qnorm(0.025))*dam_elasticnet
CIupr_elasticnet <- m_elasticnet - (qnorm(0.025))*dam_elasticnet 
CIlwr_elasticnet # 1063056 (R$ 1.063.056,00)
CIupr_elasticnet # 1162867 (R$ 1.162.867,00)






# #### RESULTADOS ####
price_pred_ridge # 1112961 (R$ 1.112.961,00)
CIlwr_ridge # 1063056 (R$ 1.063.056,00)
CIupr_ridge # 1162867 (R$ 1.162.867,00)

price_pred_lasso # 1127815 (R$ # 1.127.815,00)
CIlwr_lasso # 1063056 (R$ 1.063.056,00)
CIupr_lasso # 1162867 (R$ 1.162.867,00)

price_pred_elastic # 1124279 (R$ 1.124.279,00)
CIlwr_elasticnet # 1063056 (R$ 1.063.056,00)
CIupr_elasticnet # 1162867 (R$ 1.162.867,00)

results <- data.frame(model=c("ridge", "lasso", "elasticnet"), predict=c(price_pred_ridge, price_pred_lasso, price_pred_elastic), lowerConfidence=c(CIlwr_ridge, CIlwr_lasso, CIlwr_elasticnet), upperConfidence=c(CIupr_ridge, CIupr_lasso, CIupr_elasticnet), stringsAsFactors=FALSE)
results


