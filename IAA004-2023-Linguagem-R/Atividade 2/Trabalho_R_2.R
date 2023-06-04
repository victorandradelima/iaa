# 1 - Carregar o arquivo Volumes.csv
volumesData <- read.csv2("Volumes.csv")

# 2 - Eliminar a coluna NR, que só apresenta um número sequencial
volumes <- volumesData[,c(2:ncol(volumesData))]

set.seed(999)

# 3 - Criar partição de dados: treinamento 80%, teste 20%
library("caret")
indices <- createDataPartition(volumes$DAP, p=0.80, list=FALSE)
treino <- volumes[indices,]
teste <- volumes[-indices,]

# 4 - Usando o pacote "caret", treinar os modelos: Random Forest (rf), SVM (svmRadial), Redes Neurais (neuralnet) e o modelo alométrico de SPURR
rf <- train(VOL~., data=treino, method="rf") # Randoim Forest
svm <- train(VOL~., data=treino, method="svmRadial") # SVM
rna <- train(VOL~., data=treino, method="nnet", trace=FALSE) # Redes Neurais

# 5 - O modelo alométrico é dado por: Volume = b0 + b1 * dap2 * Ht
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, volumes, start=list(b0=0.5, b1=0.5))

# 6 - Efetue as predições nos dados de teste
predicoes.rf <- predict(rf, teste)
predicoes.svm <- predict(svm, teste)
predicoes.rna <- predict(rna, teste)
predicoes.alom <- predict(alom, teste)

# 7 - Crie funções e calcule as seguintes métricas entre a predição e os dados observados

# 7.1 - Índice do Erro quadrático médio
rmseResults <- c()
rmseResults["rf"] <- RMSE(predicoes.rf, teste$VOL)
rmseResults["svm"] <- RMSE(predicoes.svm, teste$VOL)
rmseResults["rna"] <- RMSE(predicoes.rna, teste$VOL)
rmseResults["alom"] <- RMSE(predicoes.alom, teste$VOL)
print(rmseResults)

# 7.2 - Coeficiente de determinação: R2
r2 <- function(valorObservado, valorPredito) {
  mediaValores <- mean(valorObservado)
  return(1-((sum((valorObservado-valorPredito)^2))/(sum((valorObservado-mediaValores)^2))))
}

r2Results <- c()
r2Results['rf'] <- r2(teste$VOL, predicoes.rf)
r2Results['svm'] <- r2(teste$VOL, predicoes.svm)
r2Results['rna'] <- r2(teste$VOL, predicoes.rna)
r2Results['alom'] <- r2(teste$VOL, predicoes.alom)
print(r2Results)

# 7.3 - Erro padrão da estimativa: Syx
syx <- function(valorObservado, valorPredito) {
  n <- length(valorObservado)
  return (sqrt(sum((valorObservado-valorPredito)^2)/(n-2)))
}

syxResults <- c()
syxResults['rf'] <- syx(teste$VOL, predicoes.rf)
syxResults['svm'] <- syx(teste$VOL, predicoes.svm)
syxResults['rna'] <- syx(teste$VOL, predicoes.rna)
syxResults['alom'] <- syx(teste$VOL, predicoes.alom)
print(syxResults)

# 8 - Escolha o melhor modelo
# Baseado nas métricas calculadas de RSME, R2 e SYX, podemos afirmar que o modelo
# Alométrico foi o mais eficaz já que teve o valor mais próximo do ideal das
# 3 métricas realizadas




