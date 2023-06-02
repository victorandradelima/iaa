# 1 - Carregar o arquivo Volumes.csv
volumesData <- read.csv2("Volumes.csv")

# 2 - Eliminar a coluna NR, que só apresenta um número sequencial
volumes <- volumesData[,c(2:ncol(volumesData))]

# 3 - Criar partição de dados: treinamento 80%, teste 20%
library("caret")
indices <- createDataPartition(volumes$DAP, p=0.80, list=FALSE)
treino <- volumes[indices,]
teste <- volumes[-indices,]

# 4 - Usando o pacote "caret", treinar os modelos: Random Forest (rf), SVM (svmRadial), Redes Neurais (neuralnet) e o modelo alométrico de SPURR
set.seed(99)
library("caret")
rf <- train(VOL~., data=treino, method="rf") # Randoim Forest
svm <- train(VOL~., data=treino, method="svmRadial") # SVM
rna <- train(VOL~., data=treino, method="nnet") # Redes Neurais

# 5 - O modelo alométrico é dado por: Volume = b0 + b1 * dap2 * Ht
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, volumes, start=list(b0=0.5, b1=0.5))

# 6 - Efetue as predições nos dados de teste
predicoes.rf <- predict(rf, teste)
predicoes.svm <- predict(svm, teste)
predicoes.rna <- predict(rna, teste)
predicoes.alom <- predict(alom, teste)

# 7 - Crie funções e calcule as seguintes métricas entre a predição e os dados observados
rmse.rf <- RMSE(predicoes.rf, teste$VOL)
rmse.svm <- RMSE(predicoes.svm, teste$VOL)
rmse.rna <- RMSE(predicoes.rna, teste$VOL)
rmse.alom <- RMSE(predicoes.alom, teste$VOL)

# 8 - Escolha o melhor modelo
melhor_modelo <- min(c(rmse.rf, rmse.svm, rmse.rna, rmse.alom))

# O melhor modelo foi o alométrico, que retornou o menor coeficiente de erro quadrático médio