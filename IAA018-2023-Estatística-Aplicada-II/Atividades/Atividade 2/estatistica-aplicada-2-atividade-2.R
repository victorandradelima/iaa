# Segunda Lista de Exercícios

# Com a base de dados “prodbebidas” (dados mensais do índice de produção de bebidas no Brasil)
# obter os seguintes resultados com o auxílio do “R”

# Fazer a todos os testes estatísticos e gráficos necessários e a predição para os próximos 6
# meses do índice de produção de bebidas para os seguintes modelos:

# i.	ETS;
# ii.	ARIMA OU SARIMA (verificar se existe sazonalidade ou não e decidir qual modelo é mais adequado)
# Obs: separe os últimos 12 meses da série para testar o modelo.




# Carregando dados prodbebidas
library(readxl)
prodbebidas <- read_excel('prodbebidas.xls')





# Convertendo os valores da coluna "Datas" para o tipo data
dates <- unlist(prodbebidas[,1])
datesFormat <- c()
for (date in dates) {
  year <- substr(date, start = 1, stop = 4)
  month <- substr(date, start = 6, stop = 8)
  # cat(month, '/' , year, '\n')
  datesFormat <- c(datesFormat, paste0(year,'-',month,'-01'))
}
prodbebidas <- subset(prodbebidas, select = -Data)
prodbebidas <- cbind(prodbebidas, Datas = as.Date(datesFormat))





# Criando uma série temporal com a tabela de dados
prodbebidas_ts <- ts(data = prodbebidas[, 1], start = c(2002, 1), end = c(2022, 4), frequency = 12)




# Protando um gráfico para ver a distribuição dos valores no tempo
library('ggplot2')
library('forecast')
prodbebidas %>%
  ggplot() +
  geom_line(aes(x = Datas, y = Prodbebidas, group = TRUE, color = "Prodbebidas"), size = 0.8) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")


# Decompondo a produção de bebidas pelo modelo aditivo
decprodbebidas <- decompose(x = prodbebidas_ts, type = "additive")
# decprodbebidas <- decompose(x = prodbebidas_ts, type = "multiplicative")


# Transformando o objeto decprodbebidas em um dataframe
library('dplyr')
decprodbebidas_df <- data.frame(tempo = prodbebidas$Datas,
                        serie = unlist(prodbebidas$Prodbebidas),
                        tendencia = unlist(decprodbebidas$trend),
                        sazonalidade = unlist(decprodbebidas$seasonal),
                        dessazonalizada = prodbebidas_ts - decprodbebidas$seasonal,
                        erro = unlist(decprodbebidas$random)) %>% rename(tempo = 1,
                                                                         serie = 2,
                                                                         tendencia = 3,
                                                                         sazonalidade = 4,
                                                                         dessazonalizada = 5,
                                                                         erro = 6)




# Plotando todos os dados da decomposição juntos
decprodbebidas_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Serie values"), size = 1.2) +
  geom_line(aes(x = tempo, y = tendencia, color = "Trend"), size = 1) +
  geom_line(aes(x = tempo, y = sazonalidade, color = "Seasonality"), size = 1.2) +
  geom_line(aes(x = tempo, y = erro, color = "Error"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legend:", x = NULL, y = NULL) +
  scale_color_manual(values = c("#440154FF", "#3CBB75FF", "#39568CFF", "#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")





# Preparando dados gráficos dos objetos separados da decompisição da série temporal

# Valores em série
decprodbebidas_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Serie values")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Serie values", x = NULL, y = NULL) +
  scale_color_manual(values = c("#39568CFF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_serie

# Sazonalidade
decprodbebidas_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = sazonalidade, color = "Seasonality")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seasonality", x = NULL, y = NULL) +
  scale_color_manual(values = c("#3CBB75FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_sazonalidade

# ==> Trend
decprodbebidas_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = tendencia, color = "Trend")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Trend", x = NULL, y = NULL) +
  scale_color_manual(values = c("#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black",  fill = NA),
        legend.position = "none") -> decomp_tendencia

# Error
decprodbebidas_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = erro, color = "Error")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Error", x = NULL, y = NULL) +
  scale_color_manual(values = c("#440154FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_erro

# The parts of the decomposition
library('gridExtra')
grid.arrange(decomp_serie,
             decomp_sazonalidade,
             decomp_tendencia,
             decomp_erro,
             ncol = 1)





# Comparação gráfica dos dados com a decomposição sem sazonalidade
decprodbebidas_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Dados"), size = 1.2) +
  geom_line(aes(x = tempo, y = dessazonalizada, color = "Sem sazonalidade"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legenda:", x = NULL, y = NULL) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")




############################## ETS MODEL #######################################

library(readxl)
prodbebidas <- read_excel('prodbebidas.xls')

# Isolando apenas os valores
prodbebidasvalues=prodbebidas[2]

# Criando a série temporal
prodbebidasvaluests=ts(prodbebidasvalues, start=c(2002, 1), end=c(2022, 4), frequency=12)

# Separando os dados de treino e teste do modelo
# Foi solicitado que os último 12 meses, devem ser usados para teste

# Separando os dados de treino
prodbebidastreino=window(prodbebidasvaluests,start=c(2002,1), end=c(2021,4))
# plot(prodbebidastreino)
# length(prodbebidastreino)


# Separando os dados de teste
prodbebidasteste=window(prodbebidasvaluests,start=c(2021,5), end=c(2022,4))
# plot(prodbebidasteste)
# length(prodbebidasteste)


# Estimando o modelo ETS
prodbebidastreino.ets <- ets(prodbebidastreino)
summary(prodbebidastreino.ets)

# Fazendo a previsao no modelo ETS para 12 meses 
prodbebidasvaluests.ets.forecasts <- forecast.ets(prodbebidastreino.ets, h = 12)
summary(prodbebidasvaluests.ets.forecasts)

# Plotting graph with the predictions
#X11(width = 10, height = 12)
plot(prodbebidasvaluests.ets.forecasts)

library('plotly')
ggplotly(
  autoplot(prodbebidastreino.ets)+
    autolayer(prodbebidasteste, serie="Real values")+
    autolayer(prodbebidasvaluests.ets.forecasts$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# Estatísticas de precisão no conjunto de dados de teste
library('forecast')
forecast::accuracy(prodbebidasvaluests.ets.forecasts$mean,prodbebidasteste)


# Plotando os resíduos estimados
plot(prodbebidastreino.ets$residuals)


# Plotando o gráfico de autocorrelação dos resíduos
acf(prodbebidastreino.ets$residuals)
# Pela visualização do gráfico podemos afirmar que não existe correlaçao significante entre
# os resíduos, já que nenhum resultado (além do primeiro) ultrapassa a tolerância da média
# dos resíduos

# Verificando se o modelo possui desajustes
Box.test(prodbebidastreino.ets$residuals, lag=1, type=c("Ljung-Box"))

# Como o valor do p é maior que 0.05, podemos afirmar que o mesmo não possui desajustes.
# Box-Ljung test
# data:  prodbebidastreino.ets$residuals
# X-squared = 2.5776, df = 1, p-value = 0.1084







############################ ARIMA/SARIMA MODEL ######################################

# vamos usar os mesmos valores de treino e teste do modelo ETS
# prodbebidasvaluests # série temporal
# prodbebidastreino # dados de treino
# prodbebidasteste # dados de teste

# Teste de sazonalidade 
# Vamos verificar a sazonalidade do da série, pois o modelo ARIMA não é utilizado
# em séries com sazonalidade, para isso usamos o modelo SARIMA
library(seastests)
seastests::combined_test(prodbebidasvaluests)
# Test used:  WO 
# Test statistic:  1 
# P-value:  0 0 0

# Como os valores de P são menores do que 0,01 (QS-test) e 0,002 (kw-test)
# respectivamente, então podemos afirmar que se trata de uma série com sazonalidade.

# Desta forma vamos usar o modelo SARIMA







# Teste Estacionário

# Teste Dickey-Fuller
# Ho: A série não é estacionária
# HA: A série é estacionária
prodbebidastreinodf=ur.df(prodbebidastreino, selectlags = 'BIC', type = 'none')
summary(prodbebidastreinodf)
prodbebidastreinodf=ur.df(prodbebidastreino, selectlags = 'BIC', type = 'drift')
summary(prodbebidastreinodf)
prodbebidastreinodf=ur.df(prodbebidastreino, selectlags = 'BIC', type = 'trend')
summary(prodbebidastreinodf)

# Pelos testes não podemos afimar se a série é estacionária ou não. Já que os valores
# de z.lag.1 nos testes Dickey-Fuller são siginificativamente diferentes.

# Outra forma de identificar se uma série não é estacionária
# Caso o número de diferenciações seja maior que 0, ela não é estacionária.
ndiffs(prodbebidastreino)
# [1] 1
# Logo como temos um número não nulo de diferenciações ela não é estacionária

# Aplicando a função para torna-la estacionária
difprodbebidastreino <- diff(prodbebidastreino)
ggtsdisplay(difprodbebidastreino)


# Repetindo o teste Dickey-Fuller para verificar se está estacionária
difprodbebidastreinodf=ur.df(difprodbebidastreino, selectlags = 'BIC', type = 'none')
summary(difprodbebidastreinodf)
difprodbebidastreinodf=ur.df(difprodbebidastreino, selectlags = 'BIC', type = 'drift')
summary(difprodbebidastreinodf)
difprodbebidastreinodf=ur.df(difprodbebidastreino, selectlags = 'BIC', type = 'trend')
summary(difprodbebidastreinodf)

# Agora podemos verificar que os valores de z.lag.1 são siginificativamente iguais.
# Logo podemos afirmar que a série após aplicar as diferenciações estão estacionárias.


# Vamos aplicar a função auto.arima, que sózinha identifica que existe a sazonalidade
# e aplica o SARIMA
arimaprodbebidas=auto.arima(prodbebidastreino, trace=T)





# Teste de resíduo de Ljung-Box
checkresiduals(arimaprodbebidas)

# Ljung-Box test
# 
# data:  Residuals from ARIMA(1,0,2)(2,1,2)[12]
# Q* = 34.597, df = 17, p-value = 0.007024

# No teste de resíduo estamos verificando se os mesmos nao são correlacionados.
# Logo, como o valor de p (0.007024) maior do que 0.05 podemos aceitar a hipótese
# de que os resíduos não são correlacionados.




# Teste de Normalidade
# 2. Residuals Normality
ks.test(arimaprodbebidas$residuals, "pnorm", mean(arimaprodbebidas$residuals), sd(arimaprodbebidas$residuals))

# One-sample Kolmogorov-Smirnov test
# 
# data:  arimaprodbebidas$residuals
# D = 0.067978, p-value = 0.234
# alternative hypothesis: two-sided

# Como o valor de p é maior do que 0.05, podemos afirmar a hipótese de que existe normalidade nos resíduos







# Teste de estacionariedade da variância
# Verificando de existe o efeito ARCH/GARCH, ou seja, evidência de heterocedasticidade
ArchTest(arimaprodbebidas$residuals)

# ARCH LM-test; Null hypothesis: no ARCH effects
# 
# data:  arimaprodbebidas$residuals
# Chi-squared = 31.514, df = 12, p-value = 0.001644

# Como o Valor de p () é menos do que 0.05, podemos afirmar que a hipótese válida é que 
# existe heterocedasticidade da variância.






# Realizando a predição dos valores
prevprodbebidas=forecast::forecast(arimaprodbebidas, h=12)
prevprodbebidas
autoplot(prevprodbebidas) + theme_bw()






# Verificando a acurácia do modelo
forecast::accuracy(prevprodbebidas, prodbebidasteste)

#               ME        RMSE     MAE       MPE        MAPE     MASE      ACF1      Theil's U
# Training set  0.2754421 5.032150 3.576535  0.04480483 4.228811 0.6297333 0.0302036 NA
# Test set     -1.4487435 8.049874 6.915202 -1.62271540 7.259174 1.2175843 0.2701566 0.9459907





# Plotando gráfico de valores Preditos e valores reais
ggplotly(
  autoplot(prodbebidastreino)+
    autolayer(prodbebidasteste,serie="Valores Reais")+
    autolayer(prevprodbebidas$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)













